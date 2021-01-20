gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c MoonSun.f90 -o MoonSun.o
gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c SOFA.f90 -o SOFA.o
gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c NOVAS.f90 -o NOVAS.o
gfortran -shared -static -Wl,--output-def=StaticMoonSunC.def -Wl,--out-implib=StaticMoonSunC.a -Wl,--dll MoonSun.o SOFA.o NOVAS.o -o StaticMoonSunC.dll -s
