gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c MoonSun.f95 -o MoonSun.o
gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c SOFA.f95 -o SOFA.o
gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c SOFAnutate.FOR -o SOFAnutate.o
gfortran.exe -Wall -DBUILD_DLL -O2  -ffree-line-length-none -Wall -c NOVAS.f95 -o NOVAS.o
gfortran -shared -static -Wl,--output-def=StaticMoonSunC.def -Wl,--out-implib=StaticMoonSunC.a -Wl,--dll MoonSun.o SOFA.o SOFAnutate.o NOVAS.o -o StaticMoonSunC.dll -s