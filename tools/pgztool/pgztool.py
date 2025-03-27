import argparse
import xml.etree.ElementTree as ET

class Pgz:
    def __init__(self):
        self.segments = list()

    @classmethod
    def from_manifest(cls, manifest):
        pgz_file = cls()
        pgzroot = manifest.getroot()
        outputfile = pgzroot.attrib['outputfile']
        address = pgzroot.find('start').attrib['address']
        pgz_file.startaddress = address
        print("output:", outputfile)

        print("Adding segments")
        for f in pgzroot.findall('segments/segment'):
            fname = f.attrib['filename']
            address = f.attrib['address']

            print(fname, address)
            with open(fname, 'rb') as f:
                data = f.read()
            print("filesize:", len(data))
            pgz_file.segments.append(Segment(fname, data, address))

        # create final start address segment
        #fname = "start"
        #data = bytearray()
        #address = pgz_file.startaddress
        pgz_file.segments.append(Segment("start", bytearray(), pgz_file.startaddress))
        return pgz_file


    def export(self, filename):
        with open(filename, 'wb') as f:
            # write pgz header
            f.write("Z".encode('ascii'))
            for segment in self.segments:
                print("export seg ",segment.name," start", f"${segment.address:06x}")
                #f.write((segment.address // 16) & 255)
                f.write((segment.address).to_bytes(3, byteorder = 'little'))    # segment address
                f.write(len(segment.data).to_bytes(3, byteorder = 'little'))    # segment length
                f.write(segment.data)


class Segment:

    def __init__(self, name, data, address):
        self.address = int(address, 0)
        self.data = data
        self.name = name


def parse_arguments():
    parser = argparse.ArgumentParser(description='Foenix pgz file compilation tool')
    parser.add_argument('manifest')
    parser.add_argument('--directory', '-d', action=argparse.BooleanOptionalAction, help = "input and output are directories")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    args = parse_arguments()
    print(args)

    tree = ET.parse(args.manifest)
    print(tree)
    pgz = Pgz.from_manifest(tree)
    fn_outputfile = tree.getroot().attrib['outputfile']
    print("outfile:", fn_outputfile)
    pgz.export(fn_outputfile)

