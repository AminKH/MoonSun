The Moon and Sun
Is about predicting sun and moon position, at observer’s sky All procedures or algorithms are from Jean Meeus book of “Astronomical Algorithms”. At beginning of project, it was about sun position, but later I have noticed that for indicating astronomical event we need calendar dates. Calendars as far as I know are based on sun or moon movement in observer’s sky.

Iranian calendar is solar based calendar, and new year is at spring equinox. For centuries mathematician and astronomers worked on tables to predict on time equinox and therefore leap year, but it is still difficult to predict some leap years are in 4 years interval or 5 years interval. Therefore after some thinking, I have come to conclusion of predicting Iranian leap year from purely astronomical algorithms of spring equinox and the rule that is: “ leap year is the year that equinox happens before noon and next year equinox happens after noon” .   Subroutine IranCalendar is about this

Hijri lunar based calendar is important for traditional and religious matters. Visibility of new crescent  moon is based on recent year studies of Yallop and Odeh. First day of each Hijri month is due to visibility of new crescent moon. 

Procedure for sun is same as : Solar Position Algorithm for Solar Radiation Applications
from  https://www.nrel.gov/docs/fy08osti/34302.pdf

For this project I have used following packages for nutation, precession and UTS and TT time differences
NOVAS from http://aa.usno.navy.mil/software/novas/novas_info.php
SOFA from http://www.iausofa.org/
MSISE from https://ccmc.gsfc.nasa.gov/pub/modelweb/atmospheric/msis/nrlmsise00/

C#
For convenience and to be user friendly (kind of ) , selected routines imported from dynamic library (dll) in ClassMoonSun.cs . An example of combination Fortran fast , user friendly graphical interface. Complete project file is TheMoonandSun 
