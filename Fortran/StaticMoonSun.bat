gfortran -Wall  -O2  -ffree-line-length-none -Wall -c SOFA.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c SOFAnutate.FOR
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c NOVAS.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MoonSun.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MSISGTD.FOR
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c Main.f90
gfortran SOFA.o SOFAnutate.o NOVAS.o MSISGTD.o MoonSun.o Main.o -Wextra -Wall -pedantic -ffree-form -static -o Calendar.exe -s

