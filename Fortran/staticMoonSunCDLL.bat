gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c SOFAnutate.f95 -o SOFAnutate.o
gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c SOFA.f95 -o SOFA.o
gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c MoonSunLib.f95 -o MoonSunLib.o
gfortran.exe -shared -Wl,--output-def=libMoonSunDLL.def -Wl,--out-implib=libMoonSunDLL.a -Wl,--dll SOFAnutate.o SOFA.o MoonSunLib.o -static -o StaticMoonSunC32.dll -s -O3 -s 
