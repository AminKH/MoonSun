
mingw32-gfortran -O2 -c -o MSISGTD.o MSISGTD.FOR 
mingw32-gfortran -shared -static -Wl,--output-def=StaticMSISE.def -Wl,--out-implib=StaticMSISE.a -Wl,--dll MSISGTD.o -o StaticMSISE.dll -s