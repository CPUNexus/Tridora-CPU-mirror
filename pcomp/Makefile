PCOMP=./pcomp
SASM=./sasm
LSYMGEN=./lsymgen
LIBGEN=./libgen

.SUFFIXES:
.SUFFIXES: .pas .o

.pas:
	fpc -Mobjfpc -gl $<

all: pcomp sasm sdis libgen lsymgen

libs: pcomp sasm lsymgen libgen
	$(SASM) ../lib/coreloader.s
	$(LSYMGEN) ../lib/coreloader.sym
	$(PCOMP) -n ../lib/stdlib.pas
	$(LIBGEN) ../lib/stdlib.s
	$(LIBGEN) ../lib/runtime.s
	$(LIBGEN) ../lib/float32.s

nativecomp: pcomp sasm libs
	$(PCOMP) sasm.pas
	$(PCOMP) pcomp.pas
	$(PCOMP) lsymgen.pas
	$(PCOMP) libgen.pas

nativeprogs: nativecomp
	$(PCOMP) ../progs/shell.pas
	$(PCOMP) ../progs/editor.pas
	$(PCOMP) ../progs/reclaim.pas
	$(PCOMP) ../progs/dumpdir.pas
	$(PCOMP) ../progs/partmgr.pas
	$(PCOMP) ../progs/xfer.pas
	$(SASM) ../lib/rommon.s
	$(SASM) -A ../lib/rommon.s ../lib/rom.mem
examples: nativecomp
	$(PCOMP) ../tests/readtest.pas
	$(PCOMP) ../tests/readchartest.pas
	$(PCOMP) ../tests/timetest.pas
	$(PCOMP) ../tests/test133.pas
	-$(PCOMP) ../examples/chase.pas
	$(PCOMP) ../tests/cchangetest.pas
	$(PCOMP) ../tests/tree.pas
	
clean:
	rm -f pcomp sasm sdis libgen lsymgen *.o *.s
