;+
; NAME:
;	gillhs_in_array.pro
;
; PURPOSE:
;	sonic-daten einlesen. liest binärdaten des gillhs-formates der labview-erfassung in einen array der
;   form u [m/s], v[m/s], w[m/s], t[°C], ai1[unskaliert], ai2[unskaliert], ...
;
;   u v w t ai1 ai2 ...
;   u v w t ai1 ai2 ...
;   . . . .  .   .
;   . . . .  .   .
;   . . . .  .   .
;
;   float-array(4+anz_ai,r), wobei anz_ai die anzahl analog inputs ist und r die anzahl eingelesener
;   records. die id-bytes in den daten werden kontrolliert, ob der eingelesene datenblock vollständig ist.
;   inklinometer und statusinformationen werden nicht rausgenommen.
;
; CALLING SEQUENCE:
;   result=gillhs_in_array(filepath, anz_records, anz_ai=anz_ai, /inklino, /status)
;
; INPUTS:
;	filepath    : vollständiger pfad des datenfiles
;   anz_rec     : anzahl analog-inputs, die aufgeschaltet sind.
;   anz_records : keyword: anzahl records, die eingelesen werden sollen, beginnend vom anfang,
;                 wenn nicht gesetzt, dann wird das ganze file von anfang bis ende eingelesen.
;   inklino     : keyword ANSTATT uwvt wird das inklinometer ausgegeben.
;   status      : keyword zum plotten der statusinforamtion (1x), zusätzlich.
;
; OUTPUT:
;	array(4+anz_ai,r) mit u,v,w,t,ai1,ai2... oder
;   array (r/10,2) mit (r/10,0) inklinometer x, und (r/10,1) inklinometer y wenn keyword "/inklino" gesetzt
;
; EXAMPLE:
;   uvwt=gillhs_in_array('m:\data\mn\rohdaten\fast\228\MN_N8_1999_228_010000.raw',1,anz_rec=100, /status)
;
; REVISION HISTORY:
;   28.08.99 AC (MAP)
;   12.09.99 AC fehler beim einlesen mit 0 analog-inputs korrigiert
;   11.12.99 AC wenn id-byte kontrolle nicht bestanden -> "-1" als result, da daten unbrauchbar.
;   03.03.00 AC inklinometer und statusinformationen eingebaut. anz_rec als keyword
;	23.03.00 EvG option id_shift beigefügt
;-


;GILL HS FORMAT (1 zeichen entspricht 1 byte, ausgenommen []):
;1123445566[ss]77[aa]8
;
;1 = ID-Byte hex 2 * "BA"
;2 = status adresse (=counter 1-10)
;3 = status data (inklinometer und so)
;4 = wind x
;5 = wind y
;6 = wind z
;[ss] = speed of sound
;7 = absolut temp
;[aa] = Analog Inputs (pro stück 2 bytes)
;8 = Checksum

function gillhs_in_array, filepath,anz_ai,anz_rec=anz_rec,inklino=inklino,status=status

	openr, lun, filepath, /get_lun

	if anz_ai gt 0 then begin
		recordhs = {Recordhs1,id1: 0B, id2: 0B, status_adress:0B,status_data: 0B,wind: bytarr(6), temp: bytarr(2), ai: bytarr(anz_ai*2), checksum: 0B}
	endif else begin
		recordhs = {Recordhs2,id1: 0B, id2: 0B, status_adress:0B,status_data: 0B,wind: bytarr(6), temp: bytarr(2), checksum: 0B}
	endelse

	if keyword_set(anz_rec) then begin
		anz_records=anz_rec
	endif else begin ;berechnen
		fileinfo=fstat(lun)
		anz_records=fileinfo.size/(13+2*anz_ai)
	endelse

	rawdata = replicate (recordhs,anz_records)
	indexvector=indgen(anz_records, /long)

	readu, lun, rawdata
	close, lun
	free_lun, lun

	;data check
	winddata=fltarr(3,anz_records)

	;id_shift
	if mean(rawdata.id1) ne 186 then begin
		 test		= bytarr(fileinfo.size)
		;das ganze in eimem schlonz einlesen...
		openr, lun, filepath, /get_lun
		readu,lun,test
		close, lun
		free_lun, lun

		pointer=fltarr(fileinfo.size/(13.+2*anz_ai))

		anz_records=-1.d
		for i=0d,fileinfo.size-3 do begin
			if test(i) eq 186 and test(i+1) eq 186 and test(i+2) ge 0. and test(i+2) le 10. then begin
				pointer(i/(13.+2*anz_ai))=i
	  			anz_records=anz_records+1
			endif
		endfor

		print,'reading again',fileinfo.name,'byte shift detected'
		rawdata = replicate (recordhs,anz_records)

		;zuweisen das ganze
		;------------------
		for i = 0d,anz_records-1 do begin
			rawdata(i).id1				= test(pointer(i))
			rawdata(i).id2				= test(pointer(i)+1)
			rawdata(i).status_adress	= test(pointer(i)+2)
			rawdata(i).status_data		= test(pointer(i)+3)
			rawdata(i).wind				= test(pointer(i)+4:pointer(i)+9)
			rawdata(i).temp				= test(pointer(i)+10:pointer(i)+11)
			if anz_ai gt 0 then begin
				rawdata(i).ai = test(pointer(i)+12:pointer(i)+13)
				rawdata(i).checksum = test(pointer(i)+14)
			endif else rawdata(i).checksum = test(pointer(i)+12)
		endfor
	endif


	reihenfolge=intarr(10)
	for i=0, 9 do begin
		reihenfolge(i)=rawdata(i).status_adress
	endfor

if keyword_set(status) then begin
	b=intarr(8,10)
	for i=0, 9 do begin
	pos=where(reihenfolge eq i+1)
	if pos(0) ne (-1) then begin
		ad=strcompress(string(fix(i)),/remove_all)
		config=rawdata(pos(0)).status_data
		b(0,i)=fix((config mod   2) /   1) & ;print, 'Adress '+ad+' , Byte 00 '+string(fix(b(0,i)))
		b(1,i)=fix((config mod   4) /   2) & ;print, 'Adress '+ad+' , Byte 01 '+string(fix(b(1,i)))
		b(2,i)=fix((config mod   8) /   4) & ;print, 'Adress '+ad+' , Byte 02 '+string(fix(b(2,i)))
		b(3,i)=fix((config mod  16) /   8) & ;print, 'Adress '+ad+' , Byte 03 '+string(fix(b(3,i)))
		b(4,i)=fix((config mod  32) /  16) & ;print, 'Adress '+ad+' , Byte 04 '+string(fix(b(4,i)))
		b(5,i)=fix((config mod  64) /  32) & ;print, 'Adress '+ad+' , Byte 05 '+string(fix(b(5,i)))
		b(6,i)=fix((config mod 128) /  64) & ;print, 'Adress '+ad+' , Byte 06 '+string(fix(b(6,i)))
		b(7,i)=fix((config mod 256) / 128) & ;print, 'Adress '+ad+' , Byte 07 '+string(fix(b(7,i)))
	endif
	endfor

	;auswertung status bits

		print, 'Inclinometer fitted (yes=1, no=0):' +string(b(3,0))
		print, 'Axis definition (uvw=0, u!z(27)v!z(27)w!z(27)=1):' +string(b(4,0))
		print, 'Wind Mode (wind mode uvw=0, wind mode axis=1, polar 360 wrap=2, polar 540 wrap=3):' +string(b(0,1)+b(1,1)*2)
		print, 'Full Scale Deflection (+-10m/s=0, +-20m/s=1, +-30m/s=2, +-60m/s=3):' +string(b(2,1)+b(3,1)*2)
		print, 'SOS report (off=0, speed of sound=1, sonic temp K=2, sonic temp C=3):' +string(b(4,1)+b(5,1)*2)
		print, 'PRT Temp (off=0, temp K=1, temp C=2, reserved=3):' +string(b(6,1)+b(7,1)*2)
		print, 'Number of Analog Inputs (off=0, number=1-6):' +string(b(0,2)+b(1,2)*2+b(2,2)*4)
		print, 'Error History: Non volatile memory error (yes=1, no=0):' +string(b(4,3))
		print, 'Error History: PRT failed (yes=1, no=0):' +string(b(4,3))
		print, 'Transducer Gain Levels: Cannel pair 1 (nominal=0, 50%=1, 90%=2, 100%=3):' +string(b(0,4)+b(1,4)*2)
		print, 'Transducer Gain Levels: Cannel pair 2 (nominal=0, 50%=1, 90%=2, 100%=3):' +string(b(2,4)+b(3,4)*2)
		print, 'Transducer Gain Levels: Cannel pair 3 (nominal=0, 50%=1, 90%=2, 100%=3):' +string(b(4,4)+b(5,4)*2)
		print, 'Anemometer Type (single axis=0, omnidirectional or asymmetric=1, three axis horiz.=2):' +string(b(0,5)+b(1,5)*2+b(2,5)*4)
endif

if keyword_set(inklino) then begin
		;auswertung inklinometer offset
		inklx_msb_pos_offset=where(reihenfolge eq 6)
		inklx_lsb_pos_offset=where(reihenfolge eq 7)
		inkly_msb_pos_offset=where(reihenfolge eq 8)
		inkly_lsb_pos_offset=where(reihenfolge eq 9)

		inklx_msb_pos=where((indexvector-inklx_msb_pos_offset(0)) mod 10 eq 0)
		inklx_lsb_pos=where(((indexvector-inklx_lsb_pos_offset(0)) mod 10 eq 0) AND indexvector gt inklx_msb_pos(0))
		inkly_msb_pos=where(((indexvector-inkly_msb_pos_offset(0)) mod 10 eq 0) AND indexvector gt inklx_msb_pos(0))
		inkly_lsb_pos=where(((indexvector-inkly_lsb_pos_offset(0)) mod 10 eq 0) AND indexvector gt inklx_msb_pos(0))

		inklx_msb=255-rawdata(inklx_msb_pos).status_data
		inklx_lsb=255-rawdata(inklx_lsb_pos).status_data
		inkly_msb=255-rawdata(inkly_msb_pos).status_data
		inkly_lsb=255-rawdata(inkly_lsb_pos).status_data


		en=min([n_elements(inklx_msb),n_elements(inklx_lsb),n_elements(inkly_msb),n_elements(inkly_lsb)])
		inklinox=float(fix(inklx_msb(0:en-1))+fix(256)*fix(inklx_lsb(0:en-1)))/100
		inklinoy=float(fix(inkly_msb(0:en-1))+fix(256)*fix(inkly_lsb(0:en-1)))/100

		return, [[inklinox],[inklinoy]]
endif ;else begin


	uvwt=fltarr(4+anz_ai,anz_records)
	;scaling of wind speeds [m s^-1]
	windrawdata=intarr(3,anz_records)
	for i=0, 2 do begin
		windrawdata(i,*)=rawdata.wind(i*2)*256+rawdata.wind(i*2+1)
		uvwt(i,*)=windrawdata(i,*)*0.01
	endfor


	;scaling of temperature [°C]
	temprawdata=intarr(1,anz_records)
	temprawdata(0,*)=rawdata.temp(0)*256+rawdata.temp(1)
 	uvwt(3,*)=temprawdata(*,*)*0.01

	;scaling of ai
	if anz_ai GT 0 then begin
		for i=0, anz_ai-1 do begin
			airawdata=intarr(anz_ai,anz_records)
			airawdata(i,*)=rawdata.ai(i)*256+rawdata.ai(i+1)
			uvwt(4+i,*)=airawdata(i,*)
		endfor
	endif

		return, uvwt
		;endelse ;ende inklino
;	endelse; ende data check
end