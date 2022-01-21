gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c SOFAnutate.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c MSISGTD.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c SOFA.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c MoonSun.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c Main.f90
gfortran SOFA.o SOFAnutate.o MSISGTD.o MoonSun.o Main.o -O3 -Wextra -Wall -pedantic -ffree-form -static -o Calendar.exe -s

 