*******************************
MAP-Riviera project sonic data
*******************************
Dipl. Phys. Marco Andretta
Institute for Atmospheric
and Climate Science ETH
Winterthurerstr.190
CH-8057 Zurich
marco.andretta@ethz.ch

File names
----------
The sonic data were collected using Edisol in calibrated raw data mode (Gill R2 Mode 2). These binary files where then processed using ediflux2000, a fortran program developed at the Institute for Climate Research during my PhD work.

The first letter in the filename corresponds to each location [L] of one Gill R2 instrument:
Letter [L]             a     b     c      d       e     f     g     h
------------------------------------------------------------------------
Gill serial number    068   069   047    054     035   030   036   0103(Gill R3)
Krypton serial N.     1299   -   1300     -       -   1370    -     -  
Height [m]            3.9  15.9   28      5      14     22    28    5

Locations [L]: (A1) Bosco di sotto a,b,c  (B) Rored e,f,g
	       (C)  Pian Perdascio d      (E) Torrazza h
Comments:  - sonic f had also 2 more analogue inputs (Temperature and Humidity)


The three numbers following the location are the day in year 1999 [DDD], then the hours [HH] (CET-1) and minutes [MM] in that day. So [LDDDHHMM] indicates the first hour that was processed. This part of the file names corresponds also to the name of the hourly raw binary data files collected during the MAP-Riviera project field phase. [n] stays for number of hours in a row processed in one post processing run, it is followed by two numbers [NN]. Because of computational limits this number is always smaller or equal 36. The letter [a] indicates the averaging period, also used to determine then mean flow direction. The instument kartesian coordinate system is then rotated in the mean flow direction, with the x-axis in the mean wind direction, and the z-axis perpendicular to it. [a] is followed by two numbers [AA] expressing the averaging pariod in minutes, which is usually set to 30 minutes. The last three number following the letter [f] are the filter lenght expressed in seconds [FFF]. Meaning that a recursive filter has been applied to the raw timeseries, to remove the low frequency part of the signal. The turbulent component is then determinated subtracting from the instant parameters the low frequency value. The filter lenght default was 300 seconds.  

Formats
-------
'.frm' :Formated output, selfexplaining table/text output. Three tables for each half hour, one for each applied coordinate transformation (see below).

'.si1/.si2/.si3' :Simple output in columns format, easy to use for postprocessing. In '.si1' files, the cartesian (right hand) coordinate system used for processing tha data corresponds to the Meteorological coordinate system (x: W-->E, y S-->N, z: vertical). '.si2' data were processed after turning the coordinate system around the z-axis in the mean (block average during the choosen averaging period) wind direction, so that <v>=0. After performing a second coordinate transformation around the y-axis, so that <w>=0, the processed data are written in the '.si3' file. 



Simple output table format (.si1,.si2 and .si3) 
------------------------------------------
#############################################################################
#day,hour,min,teta,eps,tm,pm,rhm,rm,nm,(runind(k),k=1,5),(mean(k),k=1,5),
# 1   2    3    4   5   6  7   8  9 10    11-15              16-20
#
#(sigma(k),k=1,5),(skew(k),k=1,5),(uiuj(k),k=1,3),(uiui(k),k=1,3),
#    21-25           26-30            31-33           34-36
#                                  u`v`, v`w`, w`u`
#
#(uit(k),k=1,3),(uiq(k),k=1,3),(wusqr(k),k=1,3),qt,M,Hs,E,zoverl,ustar,tstar
#    37-39         40-42           43-45        46 47 48 49 50    51    52
#############################################################################
var(k=1)= u component of var
var(k=2)= v component of var
var(k=3)= w component of var
var(k=4)= T component of var
var(k=5)= q component of var

M.A.
