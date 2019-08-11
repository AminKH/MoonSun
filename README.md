The Moon and Sun Is about predicting sun and moon position, at observer’s sky. Algorithms are from “Astronomical Algorithms”. Track of sun in the sky, has been used in estimating amount solar energy received at any location.

Subroutine Moon_Day_Rise_Set gives rise and set time in local times. Moon rise and set happens when upper edge of apparent moon disk is touches horizon, that is when moon center is approximately 34 arc minutes or 0.5666 degree below horizon. MoonAngle is the elevation of moon center at rise and set. If user sets this angle to zero (0.0) degree, software will calculate half diameter of apparent moon. The software checks difference of moon elevation or altitude with precision of 0.0001degree. Resulting rise time is within one minute of time given by NOVAS website, and Time and Date web site, but set time has 3 to 5 minute difference.  
 
Routines from following locations are Incorporated in software.

IAU SOFA http://www.iausofa.org/.

NOVAS http://aa.usno.navy.mil/software/novas/novas_info.php

MSISE from https://ccmc.gsfc.nasa.gov/pub/modelweb/atmospheric/msis/nrlmsise00/

A dynamic library link and kind of user friendly created. Error eliminated more accurate and faster processes used. Any critics or comments appreciated.


