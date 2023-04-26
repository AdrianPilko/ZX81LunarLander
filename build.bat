REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del lander.p
del *.lst
del *.sym

call zxasm lander

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run

if exist lander.p (
  call lander.p
  exit
) else (
  pause
)



