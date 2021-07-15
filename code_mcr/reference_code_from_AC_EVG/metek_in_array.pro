;+
; NAME:
;	metek_in_array.pro
;
; PURPOSE:
;	sonic-daten einlesen. liest ascii-files des meteck usa-1 (z.b. der labview-erfassung)
;   in einen float array der form u [m/s], v[m/s], w[m/s], t[°C]
;
;   u v w t
;   u v w t
;   . . . .
;   . . . .
;   . . . .
;
;   dimensionen: (4,r), wobei r die anzahl eingelesener records ist.
;
; CALLING SEQUENCE:
;   result=metek_in_array(filepath, anz_records)
;
; INPUTS:
;	filepath    : vollständiger pfad des datenfiles
;   anz_records : anzahl records, die eingelesen werden sollen, wenn '0', dann wird das ganze file von anfang bis ende
;                 eingelesen.
;
; OUTPUT:
;	array(4,r) mit u,v,w,t
;
; SIDE EFFECTS
;   braucht recht lange (jaja diese strings auseinandernehmen....)
;
; EXAMPLE:
;   uvwt=metek_in_array('d:\map\data\ro\sonic\235\RO_N1_1999_235_010000.raw',10000)
;
; REVISION HISTORY:
;   28.08.99 AC (MAP)
;   18.01.00 AC : Wenn Fehler -> Result =-1
;   24.03.00 Evg: nullfiles abfangen, message welches file eingelesen wird
;	27.03.99 Evg: zeilenversatz durch zeilen des typs M:x =   116 y =  -179 z =   -61 t=M:x =   116 y =  -179 z =   -61 t =  1679 abfangen
;   30.03.00 AC : Korrektur des Indexfehlers, i=i+1 ans ende getetzt. Erster Record nicht mehr 0.
;                 0er records werden abgefangen (fehler duch obstacle oder so)
;-


;METEK USA-1 FORMAT
;         1         2         3         4
;1234567890123456789012345678901234567890123
;-------------------------------------------
;M:x =   116 y =  -179 z =   -61 t =  1679
;42/43 RT/LF
;insgesamt 44 bytes per record

function metek_in_array, fullpath, anz_records

	openr, lun, fullpath, /get_lun
	print,'message from <metek_in_array.pro>:reading ' +fullpath

	if n_elements(anz_records) EQ 0 then begin ; anzahl records selber berechnen (wenn '0' gesetzt)
		fileinfo=fstat(lun)
		anz_records=fileinfo.size/43
	endif

	uvwt=fltarr(4,anz_records)
	zeile=''

	i=0 & fehler=0
	repeat begin
		readf, lun, zeile
		case 1 of
			strmid(zeile,0,2) ne 'M:':	begin ; datenüberprüfung
											print, 'error in <metek_in_array.pro>: unknown data format at record '+strcompress(string(i))+' in file :'+fullpath
											i=anz_records-1 ; springe an schluss damit nur 1 fehlermeldung
											uvwt=-1
									   	end
			strlen(zeile) gt 41 :		begin
											anz_records=anz_records-2
											uvwtneu=fltarr(4,anz_records) & uvwtneu(*,0:i)=uvwt(*,0:i)
											uvwt=uvwtneu & uvwtneu=0b
										end
		else:	begin
				 uvwt(0,i)=float(strcompress(strmid(zeile,6,6)))*0.01
				 uvwt(1,i)=float(strcompress(strmid(zeile,16,6)))*0.01
				 uvwt(2,i)=float(strcompress(strmid(zeile,26,6)))*0.01
				 uvwt(3,i)=float(strcompress(strmid(zeile,36,6)))*0.01
				 	if uvwt(0,i) eq 0 AND uvwt(1,i) eq 0 AND uvwt(2,i) eq 0 AND uvwt(3,i) eq 0 then begin ; da liegt ein messfehler vor
				 		fehler=fehler+1
				 		anz_records=anz_records-1 ;diesen fehlerwert überspringen
						uvwtneu=fltarr(4,anz_records) & uvwtneu(*,0:i)=uvwt(*,0:i)
						uvwt=uvwtneu & uvwtneu=0b
				 	endif
				end
		endcase
		i=i+1
	endrep until i ge anz_records
	if fehler gt 0 then print, $
		'messages from <metek_in_array.pro>: '+strcompress(string(fehler))+' records with errors detected.'
	close, lun
	free_lun, lun
	return,uvwt
end