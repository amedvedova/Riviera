;+
; NAME:
;   AML_SON_read_csat3.pro
;
; PURPOSE:
;   read sonic raw data. reads binary data of csat3-format (labview-aquisition, id-byte)
;   into an array of the dimensions u [m/s], v[m/s], w[m/s], t[degC].
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
;   result=AML_SON_read_csat3(fullpath, recs=recs, noid=noid)
;
; INPUTS:
;   fullpath    : string-path. full path of datafile according to operating system.
;   recs        : number. keyword. number of records to read into array. if not set,
;                 all data will be read (exept of 'broken' last records)
;   noid        : keyword. data format without id-bytes (10 bytes) instead of id-data with 12 bytes
;   eval        : keyword. maximum number of errorous values to be interpolated (default 50)
;   offset      : keyword. 1 oder 0. wenn gesetzt, wird das erste byte weggeworfen und erst ab dem
;                 zweiten eingelesen. das ist noetig bei den daten der labview erfassung bis
;                 und mit 1.2c (betrifft ausschliesslich MAP-Riviera), da dort
;                 ein shift-fehler aufgetreten ist. alle hoeheren versionen beoetigen default=0
;   softflag    : keyword. wenn gesetzt, dann wird anstelle des datenarrays word4[3,*] zurückgegeben.
;
; OUTPUT:
;   array[4,r]  u,v,w,t
;   ODER
;   array[r]    word 4 wird zurückgegeben (wenn keyword "softflag" gesetzt)
;
;   u,v und w sind ein rechtshändiges system.
;   u+ zeigt nach 180° wenn du von der befestigung zum sonic kopf schaust (sonic S)
;   v+ zeigt nach 90° (rechts) wenn du von der befestigung zum sonic kopf schaust (sonic E)
;   w+ zeigt nach oben.
;   d.h. ein wind mit u+ blässt vom kopf zur befestigung und ein wind mit v+ von links nach
;   rechts, wenn du an der befestigung stehst und zum kopf schaust.
;
; EXAMPLE:
;   uvwt=AML_SON_read_csat3('M:\!ebex\data\sonic\2000_227\EX_C2_2000_227_040000.raw')
;
; REVISION HISTORY:
;   28.08.99 AC  (MAP)
;   18.01.00 AC  wenn counter kontrolle nicht bestanden -> "-1" als result, da daten unbrauchbar.
;   02.02.00 AC  sonic 1.3a kompatibel (ohne lun). ältere files können nicht gelesen werden.
;   14.08.00 AC  CSAT mit ID-bytes werden standardmaessig gelesen (EBEX).
;   26.10.00 EvG double in abfrage der ID-bytes eingefuehrt (rundungsproblem->uvwt=-1).
;   27.09.01 CF  Auswertung der diagnostic flags, bis zu 50 erroneous values pro file werden
;                geduldet und Daten werden linear interpoliert
;   08.11.01 CF/AC keyword eval bestimmt anzahl fehlerwerte die erlaubt sind (default 50)
;   13.11.01 AC  keyword offset eingeführt. damit können map-daten wieder gelesen werden.
;   18.11.01 EvG output softflag (fehlerwerte) hinzugefügt.
;   13.12.01 AC  dokumentation verbessert.
;   06.06.02 EvG vor linearer interpolation:file mit fehlerwert beginnend -> file verkürzen (wie in aml_son_read_metek)
;-


;CSAT FORMAT (1 character means 1 bit) see csat manual
;00000000 00000000 11111111 11111111 22222222 2222222 3333333 33333333 44444444 44444444 88999999 55556677
;0 = id-byte (with keyword /noid files without id can be read)
;1 = x-component
;2 = y-component
;3 = z-component
;4 = c
;5 = diagnostic flags
;6 = x-range
;7 = y-range
;8 = z-range
;9 = counter

function AML_SON_read_csat3, fullpath, recs=recs, noid=noid, eval=eval, offset=offset, softflag=softflag

  if not keyword_set(eval)   then eval=50
  if not keyword_set(offset) then offset=0
  openr, lun, fullpath, /get_lun
  if keyword_set(noid) then nbyt=10 else nbyt=12
  if keyword_set(recs) EQ 0 then begin ; anzahl records selber berechnen (wenn '0' gesetzt)
    fileinfo=fstat(lun)
    anz_records=fileinfo.size/nbyt
  endif else begin
    anz_records=recs
  endelse
  if keyword_set(offset) then begin ;korrektur map-daten
    anz_records=anz_records-1
    einbyte=0B
    point_lun,lun,0
    readu, lun, einbyte ; erstes byte wegwerfen
  endif
  if keyword_set(noid) then begin
    recordcsi={wind_and_t: intarr(4), status: bytarr(2)}
    rawdata=replicate(recordcsi, anz_records)
  endif else begin
    recordcsiid={id: intarr(1), wind_and_t: intarr(4), status: bytarr(2)}
    rawdata=replicate(recordcsiid, anz_records)
  endelse

  readu, lun, rawdata
  close, lun
  free_lun, lun

  ;byteorder depending on platform
  if !version.os_family ne 'Windows' then begin
   rawdata.wind_and_t = SWAP_ENDIAN(rawdata.wind_and_t)
   if not keyword_set(noid) then rawdata.id = SWAP_ENDIAN(rawdata.id)
  endif

  ;skalierungsfaktoren, counter und status aus letztem integer (status=word4) rausholen
  word4=bytarr(5,anz_records)
  word4(0,*)=byte((rawdata.status(1,*) mod 16)/4)  ;ux-range  b11b10
  word4(1,*)=(rawdata.status(1,*) mod 4)           ;uy-range  b09b08
  word4(2,*)=(byte(rawdata.status(0,*)/64))        ;uz-range  b07b06
  word4(3,*)=(byte(rawdata.status(1,*)/16))        ;diagnostic flags
  word4(4,*)=(rawdata.status(0,*) mod 64)          ;counter

  ;neuer fehlercheck, einige (eval) errors werden zugelassen, CF
  ;2:poor signal lock 4:amplitude too high 8: amplitude too low
  if keyword_set(softflag) then return,word4(3,*)
  ix=where(word4(3,*) ne 0.,cnt)
  if cnt gt 0  and cnt le eval then message, strcompress(string(cnt))+ ' erroneous values detected, linear interpolation is performed.', /informational
  if cnt gt eval then begin
    message, 'ERROR: Data aquisition failed (obstacle). '+strcompress(string(cnt))+' erroneous values detected.', /informational
    uvwt=-1
  endif else begin
    uvwt=fltarr(4,anz_records)
    for i=0, 2 do begin
      ;umrechnung in meter pro sekunde mit skalierfaktoren aus word 4
      uvwt(i,*)=(float(rawdata.wind_and_t(i,*))*0.001)*(2*(1/(2^(float(word4(i,*))))))
    endfor
    ;umrechnung in celsius
    uvwt(3,*)=(float(rawdata.wind_and_t(3,*))*0.001+340.)^2/402.7-273.15
    ;file mit fehlerwert beginnend -> file verkürzen
    if ix[0] eq 0 then begin
      message, 'Datenfile beginnt mit Fehlerwerten. Datenfile wird verkuerzt.', /informational
      repeat begin
        uvwt=uvwt[*,1:*] ;array reduzieren
        ix=where(uvwt(0,*) eq 0 AND uvwt(1,*) eq 0 AND uvwt(2,*) eq 0 AND uvwt(3,*) eq 0,cnt)
      endrep until ix[0] ne 0
    endif
    ;linear interpol
    for i=0L,cnt-1 do for j=0,3 do begin
      k=1
      if i eq cnt-1 then k=0
      if (ix(i+k)-ix(i) ne 1) then begin
        tmp = interpol([uvwt(j,ix(i)-1),uvwt(j,ix(i)+1)],k+2)
        uvwt(j,ix(i))=tmp(1)
      endif else begin
        repeat begin
          k=k+1
          if (i+k eq cnt) then true = 1 else true = (ix(i+k)-ix(i) ne k)
        endrep until true eq 1
        tmp = interpol([uvwt(j,ix(i)-1),uvwt(j,ix(i)+k)],k+2)
        uvwt(j,ix(i):ix(i+k-1))=tmp(1:k)
        if (i+k eq cnt) then goto,endloop
        i=i+k
      endelse
    endfor
    endloop:
  endelse
  ;id check
  if keyword_set(noid) eq 0 then begin
    if mean(double(rawdata.id)) ne -21931 then begin
      print, 'Error: unknown data format (ID-byte test failed)'
      uvwt=-1
    endif
  endif
  return, uvwt
end