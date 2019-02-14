@echo off

set origwd=%cd%

if "%1" == "" goto showusage
goto run


:showusage
echo Usage:
echo nmexec ctlfile
goto exit

:run
set nmfe=C:\\nm74g64\\run\\nmfe74.bat
set I_MPI_AUTH_METHOD=delegate
rem set paraopts="-parafile=C:\nm73g64\run\mpiwini8.pnm" "[nodes]=3"
rem set paraopts="-parafile=%origwd%\mpiwini8.pnm" "[nodes]=3"
set run_dir=%~n1
set ctl_file=%run_dir%.ctl
set lst_file=%run_dir%.lst

if not exist %run_dir% mkdir %run_dir%

if exist %ctl_file% perl -pe "$_ =~ s/(\$DATA\s+)(\S+)/$1..\/$2/g;" %ctl_file% > %run_dir%\%ctl_file%

cd %run_dir%

rem call %nmfe% %ctl_file% %lst_file% %paraopts%
call %nmfe% %ctl_file% %lst_file%

rem Remove temporary files
if exist  temp_dir rmdir /q /s temp_dir
if exist  background.set          del background.set          2>trash.out
if exist  compile.lnk             del compile.lnk             2>trash.out
if exist  FCON                    del FCON                    2>trash.out
if exist  FDATA                   del FDATA                   2>trash.out
if exist  FMSG                    del FMSG                    2>trash.out
if exist  FREPORT                 del FREPORT                 2>trash.out
if exist  FSIZES                  del FSIZES                  2>trash.out
if exist  FSTREAM                 del FSTREAM                 2>trash.out
if exist  FSUBS*                  del FSUBS*                  2>trash.out
if exist  fsubs.f90               del fsubs.f90               2>trash.out
if exist  fsubs.obj               del fsubs.obj               2>trash.out
if exist  FSUBS_MU.F90            del FSUBS_MU.F90            2>trash.out
if exist  garbage.out             del garbage.out             2>trash.out
if exist  ifort.txt               del ifort.txt               2>trash.out
if exist  INTER                   del INTER                   2>trash.out
if exist  licfile.set             del licfile.set             2>trash.out
if exist  LINK.LNK                del LINK.LNK                2>trash.out
if exist  linkc.lnk               del linkc.lnk               2>trash.out
if exist  newline                 del newline                 2>trash.out
if exist  nmexec.set              del nmexec.set              2>trash.out
if exist  nmprd4p.mod             del nmprd4p.mod             2>trash.out
if exist  nonmem.exe              del nonmem.exe              2>trash.out
if exist  norundir.set            del norundir.set            2>trash.out
if exist  parafile.set            del parafile.set            2>trash.out
if exist  prcompile.set           del prcompile.set           2>trash.out
if exist  prdefault.set           del prdefault.set           2>trash.out
if exist  PRDERR                  del PRDERR                  2>trash.out
if exist  prsame.set              del prsame.set              2>trash.out
if exist  prsizes.f90             del prsizes.f90             2>trash.out
if exist  rundir.set              del rundir.set              2>trash.out
if exist  runpdir.set             del runpdir.set             2>trash.out
if exist  temporaryfile.xml       del temporaryfile.xml       2>trash.out
if exist  trash.tmp               del trash.tmp               2>trash.out
if exist  trskip.set              del trskip.set              2>trash.out
if exist  worker.set              del worker.set              2>trash.out
if exist  xmloff.set              del xmloff.set              2>trash.out
if exist  GFCOMPILE.BAT           del GFCOMPILE.BAT           2>trash.out
if exist  locfile.set             del locfile.set             2>trash.out
if exist  maxlim.set              del maxlim.set              2>trash.out
if exist  nmpathlist.txt          del nmpathlist.txt          2>trash.out
if exist  tprdefault.set          del tprdefault.set          2>trash.out
if exist  condor.set              del condor.set              2>trash.out
if exist  condorarguments.set     del condorarguments.set     2>trash.out
if exist  condoropenmpiscript.set del condoropenmpiscript.set 2>trash.out
if exist  flushtime.set           del flushtime.set           2>trash.out
if exist  nobuild.set             del nobuild.set             2>trash.out
if exist  parafprint.set          del parafprint.set          2>trash.out
if exist  simparon.set            del simparon.set            2>trash.out

rem goto render
goto exit

:render
call render_results.bat
goto exit

:exit
cd %origwd%
