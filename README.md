The Moon and Sun Is about predicting sun and moon position, at observer’s sky. Algorithms are from “Astronomical Algorithms”. Track of sun in the sky, has been used in estimating amount solar energy received at any location.

On Moon Rise and Moon Set 
I have used Jean Meeus Astronomical Algorithms to find moon position in the sky from an observer’s view. SOFA software for nutation and precession. Rise and set of celestial bodies happen when upper edge of then will be at horizon. Atmosphere due to refraction of light during passing through atmosphere, causes bodies to be seen above the real physical location. Therefore, upper edge will be visible when center of the celestial body is below horizon plus accounting for atmospheric effects. In Astronomical Algor recommended location for sun is -0.8333 degree.  For moon is more complicated because of variation of moon semidiameter and parallax. The formula I have used is : semidiameter of the moon + atmospheric effect.
The calculation ids performed at local time that is Julian date plus time zone of area. Calculating preliminary transit time by Meeus algorithm and performing polynomial interpolation for a few minutes of interval times. I get subroutine POLINT from  Numerical Recipes (Fortran) . Result is accurate transit time. By knowing transit time, approximate time of moon rise and set calculated. By same method, I find with polynomial interpolation, accurate time of rise and set. At higher latitudes, that is above 60 degrees, sometimes moon does not set as per definition, and remains slightly above horizon. For these situations. One should take care for latitudes above 60 degrees.
To find occurrence of moon rise, transit and set of the moon in one day, all occurrences of the moon previous day and day after calculated and checked which one is within the time frame of the day. 

Routines from following locations are Incorporated in software.

IAU SOFA http://www.iausofa.org/.

MSISE from https://ccmc.gsfc.nasa.gov/pub/modelweb/atmospheric/msis/nrlmsise00/


