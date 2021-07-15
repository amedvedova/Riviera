;+
; NAME:
;	ethsonic.pro
;
; PURPOSE:
;	liest die ASCII-halbstundenrecords der eth-sonics (auch sonderfall 0054
;   mit keyword bav) in eine structure ein.
;
; CALLING SEQUENCE:
;   result=ethsonic(filename, /bav)
;
; INPUTS:
;	filename    : vollständiger pfad des ascii datenfiles
;   das ascii datenfile kann x-beliebige records des selben sensors enthalten.
;   bav         : bav-file keyword (nur für 0054)
;
; OUTPUT:
;	structure analog zu unibs
;
; REVISION HISTORY:
;   20.03.00 AC (MAP)
;   18.04.00 AC bav hinzugefügt
;-


;BEISPIEL ETH FORMAT
;*********** San Vittore 1999 *************************************************
; 1: before turning in wind direction
;  Date and location:  Location B   Day 193   Time 22:30
;  Wind direction   :  Eps= -1.02   Teta= 81.74
;------------------------------------------------------------------------------
;                          u          v          w          T          q
;  Averages         :  0.090808   0.625406  -0.011207  24.026001   0.000000
;  Sigma            :  0.841361   0.561479   0.158472   0.510025   0.000000
;  Skewness         : -0.153512   0.971099   0.200888   0.435718   0.000000
;  Kurtosis         : -1.205193   0.691947   3.661302  -0.578496   0.000000
;------------------------------------------------------------------------------
;2: after turning in wind direction
;                          u          v          w          T          q
;  Run test index   :      7          9          7         18          0
;  Averages         :  0.632060  -0.000002   0.000000  24.026001   0.000000
;  Sigma            :  0.623134   0.796774   0.158505   0.510025   0.000000
;  Skewness         :  0.764919   0.145120   0.183755   0.435718   0.000000
;  Kurtosis         :  0.099797  -1.089353   3.654346  -0.578496   0.000000
;------------------------------------------------------------------------------
;  u`v`, v`w`, w`u` :  -0.274E+00          -0.194E-01           0.350E-02
;  u`T`, v`T`, w`T` :   0.269E+00          -0.262E+00           0.610E-02
;  wu^2, wt^2, wq^2 :   0.286E+02           0.195E+03           0.000E+00
;  u`q`, v`q`, w`q` :   0.000E+00           0.000E+00           0.000E+00
;------------------------------------------------------------------------------
;  M[N/m2] H,E[W/m2]:  -0.022198            6.896201            0.000000
;  u*, T*, z/L, q`T`:   0.14045     -0.04341     -0.05231      0.00000
;******************************************************************************

;BEISPIEL ETH BAV FORMAT
;***************************************************************************************************
; STUNDE:  20   30   Sonic: 54   Versuch:  1  MET: bav  FL:     0  TETA: 331.9
;     RUN TEST         12          22          16          12
;     MEAN:        0.868E-01   0.000E+00   0.582E-10   0.177E+02
;     SIG:         0.454E+00   0.349E+00   0.468E-01   0.230E+00
;     UiUj:        0.467E-02   0.362E-02  -0.258E-01               ij =13,23,12
;     UiT, QT:     0.444E-01   0.132E-01   0.209E-02   0.000E+00    i =1,3
;    SKEWNESS:     0.663E+00  -0.806E+00   0.149E+01  -0.629E-01
;    <(iw)**2>     0.177E+02   0.201E+02   0.000E+00               i =u,t,q


function ethsonic, filename, bav=bav

	openu, lun, filename, /get_lun
	zeile=''
	if keyword_set(bav) then recstart='****************************' else recstart='*********** San Vittore 1999'
	startdoy=193
	bavoffset=[ 0,144,192] ; offsets für bav-file c (siehe header des files)
	anz_tage=5 ; 193-197
	anz_protag=48 ; 30min files

	; structure aufsetzen [analog uni bs]
	eth  =	{time:		dblarr(2,anz_protag*anz_tage), $ ;0=anfangszeit, 1=endzeit als doy.bruchteil doy
		     ens:		lonarr(anz_protag*anz_tage),   $
		     eta:		dblarr(anz_protag*anz_tage),   $
		     theta:		dblarr(anz_protag*anz_tage),   $
		     M_vec:		dblarr(anz_protag*anz_tage),   $ ;vektoriell
		     M_cup:		dblarr(anz_protag*anz_tage),   $ ;skalar
		     M_uvw:		dblarr(anz_protag*anz_tage),   $ ;vektoriell
		     mittel:	dblarr(8,anz_protag*anz_tage), $ ;mittelwerte u nach geographisch N und v nach geographisch E
		     std_dev:	dblarr(8,anz_protag*anz_tage), $ ;nach streamline
		     skewness:	dblarr(8,anz_protag*anz_tage), $ ;nach streamline
		     kurtosis:  dblarr(8,anz_protag*anz_tage), $ ;nach streamline
		     cov_mat:	dblarr(5,5,anz_protag*anz_tage)} ;nach streamline

		;structure mit fehlerwerten füllen

	   	eth.time(*,*)		= -9999
    	eth.ens(*)			= -9999
		eth.eta(*)			= -9999.
		eth.theta(*)		= -9999.
		eth.M_vec(*)		= -9999.
		eth.M_cup(*)		= -9999.
		eth.M_uvw(*)		= -9999.
		eth.mittel(*,*)		= -9999.
		eth.std_dev(*,*)	= -9999.
		eth.skewness(*,*)	= -9999.
		eth.kurtosis(*,*)	= -9999.
		eth.cov_mat(*,*,*)	= -9999.

;zeiten auffüllen
for t=0, (anz_tage*anz_protag)-1 do begin
   	eth.time(0,t)=startdoy+(t*(double(1)/48));startzeit
	eth.time(1,t)=eth.time(0,t)+(double(1)/48);endzeit
endfor

	; bis zum ersten datensatz (* suchen)
	repeat begin
	readf, lun, zeile
	print, zeile
	endrep until strmid(zeile,0,28) eq recstart or eof(lun) eq 1

if keyword_set(bav) eq 0 then begin ;** kein bav file sondern standard.
repeat begin
	;datensatz einlesen
	readf, lun, zeile
	if strcompress(strmid(zeile,0,3),/remove_all) eq '1:' then begin


;herausfinden welcher zeitpunkt
readf, lun, zeile
;0         1         2         3         4         5
;012345678901234567890123456789012345678901234567890123456789
;  Date and location:  Location B   Day 193   Time 23: 0
				doy =fix(strmid(zeile,39,3))
				stunde=fix(strmid(zeile,50,2))
				minu=fix(strmid(zeile,53,2))
				;sanvittore zeitrechnung beginnt doy 193, 0:00. 1=halbestunde

				pos=stunde*2+(48*(doy-startdoy))

				if minu eq 30 then pos=pos+1
				doydat=double(doy)+(double(stunde)/24)
				if minu eq 30 then doydat=doydat+(double(1)/48)

			  	eth.time(0,pos)=doydat;startzeit
			  	eth.time(1,pos)=doydat+(double(1)/48);endzeit

;eta und theta
readf, lun, zeile
;0         1         2         3         4         5
;012345678901234567890123456789012345678901234567890123456789
;  Wind direction   :  Eps= -1.02   Teta= 81.74
				eth.eta(pos)=double(strmid(zeile,40,6))
				eth.theta(pos)=double(strmid(zeile,26,6))

readf, lun, zeile ;---- zeile
readf, lun, zeile ;titel
;rawmean (geogr. n)
readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;  Averages         : -0.170559   0.485856  -0.025612  23.702484   0.000000
				eth.mittel(0,pos)=double(strmid(zeile,20,10)) ;u
				eth.mittel(1,pos)=double(strmid(zeile,31,10)) ;v
				eth.mittel(2,pos)=double(strmid(zeile,42,10)) ;w
				eth.mittel(4,pos)=double(strmid(zeile,53,10)) ;t
				eth.m_vec(pos) = sqrt(eth.mittel(0,pos)^2+eth.mittel(1,pos)^2);uv vectoriell
				eth.m_uvw(pos) = sqrt(eth.mittel(0,pos)^2+eth.mittel(1,pos)^2+eth.mittel(2,pos)^2);uvw vectoriell


for n=0, 8 do begin 	;9 zeilen auslassen
	readf, lun, zeile
endfor
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;  Sigma            :  0.519711   0.604774   0.088253   0.271521   0.000000
				eth.std_dev(0,pos)=double(strmid(zeile,20,10)) ;sigma u
				eth.std_dev(1,pos)=double(strmid(zeile,31,10)) ;sigma v
				eth.std_dev(2,pos)=double(strmid(zeile,42,10)) ;sigma w
				eth.std_dev(4,pos)=double(strmid(zeile,53,10)) ;sigma t
readf, lun, zeile
				eth.skewness(0,pos)=double(strmid(zeile,20,10)) ;skewness u
				eth.skewness(1,pos)=double(strmid(zeile,31,10)) ;skewness v
				eth.skewness(2,pos)=double(strmid(zeile,42,10)) ;skewness w
				eth.skewness(4,pos)=double(strmid(zeile,53,10)) ;skewness t
readf, lun, zeile
				eth.kurtosis(0,pos)=double(strmid(zeile,20,10)) ;kurtosis u
				eth.kurtosis(1,pos)=double(strmid(zeile,31,10)) ;kurtosis v
				eth.kurtosis(2,pos)=double(strmid(zeile,42,10)) ;kurtosis w
				eth.kurtosis(4,pos)=double(strmid(zeile,53,10)) ;kurtosis t
readf, lun, zeile ;---- zeile
readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;  u`v`, v`w`, w`u` :   0.142E+00          -0.130E-02           0.132E-03
;  u`T`, v`T`, w`T` :  -0.391E-01          -0.756E-01          -0.151E-03
;  wu^2, wt^2, wq^2 :   0.119E+04           0.100E+05           0.000E+00
;  u`q`, v`q`, w`q` :   0.000E+00           0.000E+00           0.000E+00
				eth.cov_mat(0,1,pos)  = double(strmid(zeile,21,11)) ;uv
				eth.cov_mat(1,0,pos)  = double(strmid(zeile,21,11)) ;
				eth.cov_mat(2,1,pos)  = double(strmid(zeile,41,11)) ;vw
				eth.cov_mat(1,2,pos)  = double(strmid(zeile,41,11)) ;
				eth.cov_mat(2,0,pos)  = double(strmid(zeile,61,11)) ;uw
				eth.cov_mat(0,2,pos)  = double(strmid(zeile,61,11)) ;
readf, lun, zeile
				eth.cov_mat(0,4,pos)  = double(strmid(zeile,21,11)) ;uv
				eth.cov_mat(4,0,pos)  = double(strmid(zeile,21,11)) ;
				eth.cov_mat(4,1,pos)  = double(strmid(zeile,41,11)) ;vw
				eth.cov_mat(1,4,pos)  = double(strmid(zeile,41,11)) ;
				eth.cov_mat(2,4,pos)  = double(strmid(zeile,61,11)) ;uw
				eth.cov_mat(4,2,pos)  = double(strmid(zeile,61,11)) ;
readf, lun, zeile
readf, lun, zeile
;finito

	endif else message, 'error in file format (1)'

	repeat begin
	readf, lun, zeile
	endrep until strmid(zeile,0,28) eq recstart or eof(lun) eq 1
endrep until eof(lun) eq 1

endif else begin ;** bav -------------------------------------------------------------------------

repeat begin
	;datensatz einlesen

;herausfinden welcher zeitpunkt
readf, lun, zeile
;0         1         2         3         4         5         6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
; STUNDE:  20   30   Sonic: 54   Versuch:  1  MET: bav  FL:     0  TETA: 331.9
				stunde = fix(strmid(zeile,10,2))
				minu   = fix(strmid(zeile,15,2))
				versuch= fix(strmid(zeile,41,3))

				if minu eq 30 then halbs=1 else halbs=0

				pos=bavoffset(versuch-1)+(stunde*2+halbs)
				print, pos
				if pos gt 239 then pos=0
				if pos gt 239 then print, 'error'

				;doydat=double(doy)+(double(stunde)/24)
				;if minu eq 30 then doydat=doydat+(double(1)/48)

			  	;eth.time(0,pos)=doydat;startzeit
			  	;eth.time(1,pos)=doydat+(double(1)/48);endzeit

				eth.eta(pos)=double(strmid(zeile,71,6))
;eta und theta
readf, lun, zeile
;0         1         2         3         4         5
;012345678901234567890123456789012345678901234567890123456789
;     RUN TEST         12          22          16          12
;nichts machen

readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;     MEAN:        0.868E-01   0.000E+00   0.582E-10   0.177E+02
				eth.mittel(0,pos)=double(strmid(zeile,17,10)) ;u
				eth.mittel(1,pos)=double(strmid(zeile,29,10)) ;v
				eth.mittel(2,pos)=double(strmid(zeile,41,10)) ;w
				eth.mittel(4,pos)=double(strmid(zeile,53,10)) ;t
				eth.m_vec(pos) = sqrt(eth.mittel(0,pos)^2+eth.mittel(1,pos)^2);uv vectoriell
				eth.m_uvw(pos) = sqrt(eth.mittel(0,pos)^2+eth.mittel(1,pos)^2+eth.mittel(2,pos)^2);uvw vectoriell


readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;     SIG:         0.454E+00   0.349E+00   0.468E-01   0.230E+00
				eth.std_dev(0,pos)=double(strmid(zeile,17,10)) ;sigma u
				eth.std_dev(1,pos)=double(strmid(zeile,29,10)) ;sigma v
				eth.std_dev(2,pos)=double(strmid(zeile,41,10)) ;sigma w
				eth.std_dev(4,pos)=double(strmid(zeile,53,10)) ;sigma t

readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;     UiUj:        0.467E-02   0.362E-02  -0.258E-01               ij =13,23,12
				eth.cov_mat(0,2,pos)  = double(strmid(zeile,17,10)) ;uw
				eth.cov_mat(1,2,pos)  = double(strmid(zeile,29,10)) ;vw
				eth.cov_mat(0,1,pos)  = double(strmid(zeile,41,10)) ;uv

readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;     UiT, QT:     0.444E-01   0.132E-01   0.209E-02   0.000E+00    i =1,3
				eth.cov_mat(0,4,pos)  = double(strmid(zeile,17,10)) ;ut
				eth.cov_mat(1,4,pos)  = double(strmid(zeile,29,10)) ;vt
				eth.cov_mat(2,4,pos)  = double(strmid(zeile,41,10)) ;wt

readf, lun, zeile
;0         1         2         3         4         5     	 6         7
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
;    SKEWNESS:     0.663E+00  -0.806E+00   0.149E+01  -0.629E-01
				eth.skewness(0,pos)=double(strmid(zeile,17,10)) ;skewness u
				eth.skewness(1,pos)=double(strmid(zeile,29,10)) ;skewness v
				eth.skewness(2,pos)=double(strmid(zeile,41,10)) ;skewness w
				eth.skewness(4,pos)=double(strmid(zeile,53,10)) ;skewness t

readf, lun, zeile
;^2-zeile weglassen

;finito

	repeat begin
		readf, lun, zeile
	endrep until strmid(zeile,0,28) eq recstart or eof(lun) eq 1
endrep until eof(lun) eq 1

endelse ;** ende bav-keyword
	close, lun & free_lun, lun
	return, eth
end