;+
; NAME:
;   AML_SON_read_metek.pro
;
; PURPOSE:
;   sonic-daten des METEK USA-1 einlesen. liest ascii-files und integer files ein
;   (labview-erfassung, keyword "format"). fehlerwerte werden interpoliert oder falls
;   groesser als ein schwellenwert wird das file nicht eingelesen.
;
;   output der daten in einen float array der form
;   u [m/s], v[m/s], w[m/s], t[degC], .. (a1[mV], a2[mV] etc.)
;
;   u v w t (a1 a2 ...)
;   u v w t (a1 a2 ...)
;   . . . .  .  .
;   . . . .  .  .
;   . . . .  .  .
;
;
; CALLING SEQUENCE:
;   result=AML_SON_read_metek(fullpath, recs=recs, noid=noid)
;
; INPUTS:
;   fullpath    : full path of datafile pfad des datenfiles
;   eval        : anzahl fehlerwerte die erlaubt sind (default 50)
;   format      : string. entweder 'i' für integer file oder 'a' für ascii file.
;   eval        : zahl. maximale anzahl zulaessige fehlerwerte, die noch interpoliert werden.
;                 treten mehr fehlerwerte auf, dann wird die datenreihe verworfen.
;   softflag    : keyword. wenn gesetzt gibt der output gibt nicht die daten, sondern die anzahl
;                 interpolierte fehlerwerte aus.
;
; OUTPUT:
;   array[4+anzahl analog inputs,r], wobei r die anzahl eingelesener
;   records ist. -1 wird ausgegeben wenn ein fehler aufgetreten ist
;   ODER
;   array[r] mit den positionen wo interploliert wurde, wenn keyword
;   "softflag" gesetzt ist.
;
;   u,v und w sind ein linkshaendiges system.
;   u+ zeigt nach 0deg bez. dem nordpfeil auf dem sonic
;   v+ zeigt nach 90deg bez. dem nordpfeil auf dem sonic
;   w+ zeigt nach oben
;   dies gilt nur, wenn im USA-1 kein azimuth programmiert wurde
;   also AZ=0 gesetzt ist!
;
; EXAMPLE:
;  *integer-file
;   data=AML_SON_read_metek('\\gibm-ntserver3\map\bari\data\sonic\2001_210\CA_N4_2001_210_050000.raw')
;   data=AML_SON_read_metek('Macintosh HD:Users:andi:Documents:EBEX:sonic:2000_220:EX_B2_2000_220_123000.raw')
;  *ascii-file mit fehlern (interpolation)
;   data=AML_SON_read_metek('\\gibm-ntserver3\map\data\ro\rohdaten\fast\240\RO_N1_1999_240_150000.raw', format='a')
;
; REVISION HISTORY:
;   28.08.99 AC     : (MAP)
;   18.01.00 AC     : Wenn Fehler -> Result =-1
;   24.03.00 Evg    : nullfiles abfangen, message welches file eingelesen wird
;   27.03.99 Evg    : zeilenversatz durch zeilen des typs M:x =   116 y =  -179 z =   -61 t=M:x =   116 y =  -179 z =   -61 t =  1679 abfangen
;   30.03.00 AC     : Korrektur des Indexfehlers, i=i+1 ans ende gesetzt. Erster Record nicht mehr 0.
;                     0 er records werden abgefangen (fehler duch obstacle oder so)
;   13.11.01 AC     : keywords format. integerfiles koennen retzt auch eingelesen werden.
;                     fehlerhafte werte werden interpoliert und nicht weggeworfen.
;   21.11.01 AC/IL  : fehler behoben bei interpolation, wenn erster record in einem file falsch
;                     ist, wird das file verkuerzt.
;   18.11.01 IL/EvG : output softflag (fehlerwerte) hinzugefuegt.
;   13.12.01 AC     : dokumentation verbessert
;   08.03.02 AC     : schneller und plattformunabhaengig gemacht (byteorder) bei integer-files.
;   17.05.03 AC     : Fehler in der skalierung von analog input daten behoben. analog inputs wurden bisher FALSCH
;                     skaliert!
;   15.09.03 AC     : In Sofia, Bulgarien: Analog Inputs to ASCII-data added.
;-
function AML_SON_read_metek, file, anz_ai=anz_ai, $
                             format=format, eval=eval,softflag=softflag
  if not keyword_set(anz_ai) then anz_ai=0     ; anzahl analog inputs
  if not keyword_set(eval)   then eval=50      ; anzahl zulaessige fehlerwerte
  if not keyword_set(format) then format='i'   ; default ist integer-file
  format = strlowcase(format)
  fak=(32768./10000)                           ; faktor fuer analog-input skalierung
  fehler=0L

  ; USA1 im integer format (ab labview-erfassung > 1.4) -------------------------------------------------------

  if strlowcase(format) eq 'i' then begin
    cols=4+anz_ai
    openr, lun_int, file, /get_lun
    info=fstat(lun_int)
    if info.size gt cols*2 then begin ; mehr als 1 record muss existieren
      raw=intarr(cols,info.size/(cols*2))
      readu, lun_int, raw
      if !version.os_family eq 'Windows' then byteorder, raw ; bytes unter intel umkehren, fuer unix und mac ok.
      close, lun_int
      free_lun, lun_int
    endif else return, -1
    data=float(raw) & raw=0B

  ; USA1 im ascii format (ab labview-erfassung 1.0) ------------------------------------------------------------

    endif else begin

    openr, lun_asc, file, /get_lun
    fileinfo=fstat(lun_asc)

    n_rec=fileinfo.size/43 ;43 bytes pro record wenn keine analog inputs
    if anz_ai gt 0 then n_rec=n_rec/2 ;with analog inputs

    data=fltarr(4+anz_ai,n_rec)
    zeile=''
    i=0L & byte_shift=0L
    repeat begin
      readf, lun_asc, zeile
      case 1 of
        strmid(zeile,0,2) ne 'M:': begin ; datenueberpruefung
                 message, 'Unbekanntes ASCII-datenformat oder Fehler waehrend der Datenerfassung  [record:'+strcompress(string(i))+', file :'+file+']', /informational
                 i=n_rec-1 ; springe an schluss damit nur 1 fehlermeldung
                 data=-1
                 end
        strlen(zeile) gt 41 and anz_ai eq 0 : begin ; byte shift
                 n_rec=n_rec-2
                 dataneu=fltarr(4,n_rec) & dataneu[*,0:i]=data[*,0:i]
                 data=dataneu & dataneu=0b
                 byte_shift=byte_shift+1L
                 end
        else: begin
                data(0,i)=float(strcompress(strmid(zeile,6,6)))  ;x
                data(1,i)=float(strcompress(strmid(zeile,16,6))) ;y
                data(2,i)=float(strcompress(strmid(zeile,26,6))) ;z
                data(3,i)=float(strcompress(strmid(zeile,36,6))) ;t
                if anz_ai gt 0 then begin
                  readf, lun_asc, zeile
                  for a=0, anz_ai-1 do begin
                    data(a+4,i)=float(strcompress(strmid(zeile,6+10*a,6))) ;analog inputs
                  endfor
                endif
              end
        endcase
        i=i+1
      endrep until i ge n_rec or eof(lun_asc)

      if i lt n_rec then message, 'Fehler: unerwartetes Ende des ASCII-files '+file, /informational
      close, lun_asc
      free_lun, lun_asc
      if byte_shift gt 0 then message, 'ACHTUNG : '+strcompress(string(byte_shift))+' Records mit einem Byte-Versatz detektiert. '+strcompress(string(byte_shift))+' halbe Records wurden eliminiert. ', /informational
    endelse
  ;metek-daten skalieren
  if n_elements(data) gt 4 then begin
    data[0:3,*]=data[0:3,*]/100
    if anz_ai gt 0 then data[4:(anz_ai+4)-1,*]=data[4:(anz_ai+4)-1,*]*fak ;analog inputs -> mV, fehler indizierung korrigiert 17.05.2003
    ;fehlerwerte
    ix=where(data(0,*) eq 0 AND data(1,*) eq 0 AND data(2,*) eq 0 AND data(3,*) eq 0,cnt)
    if keyword_set(softflag) then begin
      flag = intarr(n_elements(data[0,*]))
      IF cnt GT 0 THEN flag[ix] = 1
      return, flag
    endif
    if cnt gt eval then begin
      message, 'FEHLER: Datenerfassung ist missglueckt '+strcompress(string(cnt))+' ungueltige Werte detektiert!', /informational
      data=-1
    endif else begin
      if cnt gt 0  and cnt le eval then begin
        message, strcompress(string(cnt))+' ungueltige Werte detektiert, fehlende Werte werden linear interpoliert', /informational
        ;file mit fehlerwert beginnend -> file verkürzen
        if ix[0] eq 0 then begin
          message, 'Datenfile beginnt mit Fehlerwerten. Datenfile wird verkuerzt.', /informational
          repeat begin
            data=data[*,1:*] ;array reduzieren
            ix=where(data(0,*) eq 0 AND data(1,*) eq 0 AND data(2,*) eq 0 AND data(3,*) eq 0,cnt)
          endrep until ix[0] ne 0
        endif
        ;linear interpol
        for i=0L,cnt-1 do for j=0,3 do begin
          k=1
          if i eq cnt-1 then k=0
          if (ix(i+k)-ix(i) ne 1) then begin
            tmp = interpol([data(j,ix(i)-1),data(j,ix(i)+1)],k+2)
            data(j,ix(i))=tmp(1)
          endif else begin
            repeat begin
              k=k+1
              if (i+k eq cnt) then true = 1 else true = (ix(i+k)-ix(i) ne k)
            endrep until true eq 1
            tmp = interpol([data(j,ix(i)-1),data(j,ix(i)+k)],k+2)
            data(j,ix(i):ix(i+k-1))=tmp(1:k)
            if (i+k eq cnt) then goto,endloop
            i=i+k
          endelse
        endfor
        endloop:
      endif
    endelse
    return, data
  endif else return, -1
end