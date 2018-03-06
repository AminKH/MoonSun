mingw32-gfortran -Wall  -O2  -ffree-line-length-none -Wall -c Main.f95
mingw32-gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MoonSun.f95
mingw32-gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MSISGTD.FOR
mingw32-gfortran Main.o MoonSun.o MSISGTD.o -Wextra -Wall -pedantic -ffree-form -static -o Calendar.exe -s

