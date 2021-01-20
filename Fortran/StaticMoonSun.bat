gfortran -Wall  -O2  -ffree-line-length-none -Wall -c SOFA.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c NOVAS.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MoonSun.f90
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MSISGTD.FOR
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c Main.f90
gfortran SOFA.o NOVAS.o MSISGTD.o MoonSun.o Main.o -Wextra -Wall -pedantic -ffree-form -static -o Calendar.exe -s

