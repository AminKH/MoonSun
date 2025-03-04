gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c SOFAnutate.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c MSISGTD.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c SOFA.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c MoonSun.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -static -c Main.f90
gfortran SOFA.o SOFAnutate.o MSISGTD.o MoonSun.o Main.o -O3 -Wextra -Wall -pedantic -ffree-form -static -o Calendar.exe -s

 