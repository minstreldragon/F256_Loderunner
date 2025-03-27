### Tools ###
AS := 64tass
RM := rm

### Tool flags ###
#ASFLAGS := --quiet

loderunner.bin: it.asm constants.h archdep.h
	$(AS) $(ASFLAGS) -b -L loderunner.listing -o $@ $<

clean:
	-$(RM) loderunner.bin

