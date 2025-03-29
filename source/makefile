### Tools ###
AS := 64tass
PGZTOOL := python ../tools/pgztool/pgztool.py
RM := rm

### Tool flags ###
#ASFLAGS := --quiet


phony: loderunner

loderunner: game.bin lr_manifest.xml
	$(PGZTOOL) lr_manifest.xml

game.bin: game.asm it.asm constants.h archdep.h
	$(AS) -b -L lr.listing -o $@ $<

clean:
	-$(RM) loderunner.pgz
	-$(RM) game.bin

