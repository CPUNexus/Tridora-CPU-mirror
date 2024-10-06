del *.s
del ..\lib\*.lib
del ..\lib\stdlib.s

fpc -Mobjfpc -gl pcomp.pas
fpc -gl sasm.pas
fpc -gl lsymgen.pas

sasm ..\lib\coreloader.s
lsymgen ..\lib\coreloader.sym
py pcomp.py -n ..\lib\stdlib.pas
libgen ..\lib\stdlib.s
libgen ..\lib\runtime.s
libgen ..\lib\float32.s

py pcomp.py sasm.pas
py pcomp.py pcomp.pas
py pcomp.py lsymgen.pas
py pcomp.py libgen.pas

rem exit /b

py pcomp.py ..\progs\shell.pas
py pcomp.py ..\progs\editor.pas
py pcomp.py ..\progs\reclaim.pas
py pcomp.py ..\progs\dumpdir.pas
py pcomp.py ..\progs\partmgr.pas
py pcomp.py ..\progs\xfer.pas
sasm ..\lib\rommon.s
sasm -A ..\lib\rommon.s ..\lib\rom.mem

rem exit /b

py pcomp.py ..\tests\readtest.pas
py pcomp.py ..\tests\readchartest.pas
py pcomp.py ..\tests\timetest.pas
py pcomp.py ..\tests\test133.pas
py pcomp.py ..\examples\chase.pas
py pcomp.py ..\tests\cchangetest.pas
py pcomp.py ..\tests\tree.pas
