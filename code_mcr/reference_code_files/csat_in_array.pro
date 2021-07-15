;+
; NAME:
;	csat_in_array.pro
;
; PURPOSE:
;	read sonic raw data. reads binary data of csat3-format (labview-aquisition) into an array of
;   the dimensions u [m/s], v[m/s], w[m/s], t[°C]
;
;   u v w t
;   u v w t
;   . . . .
;   . . . .
;   . . . .
;
;   float-array(4,r), r is number of record.
;
; CALLING SEQUENCE:
;   result=csat_in_array(filepath, anz_records)
;
; INPUTS:
;	filepath    : full path of datafile pfad des datenfiles
;   anz_records : numbers of records to read into array. if this parameter is set to '0',
;                 then all data will be read (exept the 'broken' las record)
;
; OUTPUT:
;	array(4,r)  u,v,w,t
;
; EXAMPLE:
;   uvwt=csat_in_array('d:\map\data\mn\sonic\228\MN_N1_1999_228_010000.raw',10000)
;
; REVISION HISTORY:
;   28.08.99 AC (MAP)
;   18.01.00 AC wenn counter kontrolle nicht bestanden -> "-1" als result, da daten unbrauchbar.
;   23.02.00 EvG null elemente abfangen
;-


;CSAT FORMAT (1 character means 1 bit) see csat manual
;11111111 11111111 22222222 2222222 3333333 33333333 44444444 44444444 88999999 55556677
;1 = x-komponent
;2 = y-komponent
;3 = z-komponent
;4 = c
;5 = diagnostic flags
;6 = x-range
;7 = y-range
;8 = z-range
;9 = counter

function csat_in_array, fullpath, anz_records

	openr, lun, fullpath, /get_lun
	print,'message from <csat_in_array>: reading: '+fullpath
	if n_elements(anz_records) EQ 0 then begin ; anzahl records selber berechnen (wenn '0' gesetzt)
		fileinfo=fstat(lun)
		anz_records=fileinfo.size/10
	endif

	anz_records=anz_records-1 ; naja die jetztige version hat noch shifting-problem.... *

      recordr2={recordr2, wind_and_t: intarr(4), status: bytarr(2)}
      rawdata=replicate(recordr2, anz_records)

	  einbyte=0B
	  point_lun,lun,0
	  readu, lun, einbyte ; erstes byte gehört noch zu vorangehendem file *
	  readu, lun, rawdata
      close, lun
      free_lun, lun

	  ;skalierungsfaktoren, counter und status aus letztem integer (status=word4) rausholen
	  word4=bytarr(5,anz_records)
	  word4(0,*)=byte((rawdata.status(1,*) mod 16)/4)  ;ux-range  b11b10
	  word4(1,*)=(rawdata.status(1,*) mod 4)           ;uy-range  b09b08
	  word4(2,*)=(byte(rawdata.status(0,*)/64))        ;uz-range  b07b06
	  word4(3,*)=(byte(rawdata.status(1,*)/16))        ;diagnostic flags
	  word4(4,*)=(rawdata.status(0,*) mod 64)          ;counter

	fehler=moment(word4(3,*))
	if fehler(0) NE 0 then begin
		print, 'Error: Data aquisition failed (Obstacle)'
		uvwt=-1
	endif else begin

	  uvwt=fltarr(4,anz_records)
	  for i=0, 2 do begin
    	;umrechnung in meter pro sekunde mit skalierfaktoren aus word 4
    	uvwt(i,*)=(float(rawdata.wind_and_t(i,*))*0.001)*(2*(1/(2^(float(word4(i,*))))))
	  endfor

	   ;umrechnung in celsius
   		uvwt(3,*)=(float(rawdata.wind_and_t(3,*))*0.001+340.)^2/402.7-273.15

   	endelse

	return,uvwt

end