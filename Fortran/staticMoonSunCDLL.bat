gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c SOFAnutate.f95 -o SOFAnutate.o
gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c SOFA.f95 -o SOFA.o
gfortran.exe -Wall -DBUILD_DLL -O2  -fexpensive-optimizations -ffree-form -Wall -fcheck=bounds -static -c MoonSun.f95 -o MoonSun.o
gfortran.exe -shared -Wl,--output-def= MoonSunDLL.def -Wl,--out-implib= MoonSunDLL.a -static -Wl,--dll SOFAnutate.o SOFA.o MoonSun.o -o StaticMoonSunC.dll -s -O3 -s 
