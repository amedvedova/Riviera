;+
; NAME:
;   gillr2_in_tc.pro
;
; PURPOSE:
;   sonic-daten einlesen. liest binärdaten des gillr2-transitcount-formates der
;   labview-erfassung in einen array mit den transitcounts
;
;   t1 t2 t3 t4 t5 t6 ai1 ai2
;   t1 t2 t3 t4 t5 t6
;   . . . .  .   .
;   . . . .  .   .
;   . . . .  .   .
;
;   float-array(6+anz_ai,r), r ist die anzahl eingelesener
;   records, ai anzahl analog inputs.
;
; CALLING SEQUENCE:
;   result=gillr2_in_tc(filepath, anz_records, anz_ai)
;
; INPUTS:
;   filepath    : vollständiger pfad des datenfiles
;   anz_records : anzahl records, die eingelesen werden sollen, beginnend vom anfang, wenn '0', dann wird
;                 das ganze file von anfang bis ende eingelesen.
;   anz_ai      : anzahl analog-inputs, die aufgeschaltet sind.
;
; OUTPUT:
;   array(6+anz_ai,r) mit t1,t2,t3,t4,t5,t6,ai1,ai2...
;
; EXAMPLE:
;   uvwt=gillr2_in_tc('m:\data\mn\sonic\237\MN_N5_1999_237_170000.raw',10000,0)
;
; REVISION HISTORY:
;   28.08.99 AC (MAP)
;   18.01.99 AC wenn ID-byte-kontrolle nicht bestanden -> "-1" als result, da daten unbrauchbar.
;   27.01.99 AC wenn datafile leer -> "-1" als result.
;   17.02.99 AC r2 billig rausgenommen und umbenannt zu "gillr2_in_tc". jetzt werden tc rausgegeben.
;               altes program aus kompatibilitästgründen weiterbehalten. keywords eingeführt.
;   23.03.00 EvG implementiert, dass byte shift abfangen wird
;   24.04.00 EvG fehlende analog inputs werden als byte shift interpretiert. neu als -1 ausgegeben
;   13.11.00 AC/EvG bei analog input zeile
; 			ai(0,*)=rawdata.ai(0)*1+rawdata.ai(1) 			auf 
;			ai(0,*)=rawdata.ai(0)*256+rawdata.ai(1)			geändert
;-

;GILL R2 TRANSIT COUNT FORMAT (1 zeichen entspricht 1 byte, ausgenommen []):
;1122334455667788[aa]99
;
;1 = start of record HEX "8181" = -32383
;2 = record number (Counter)
;3 = transit count t-b axis 1
;4 = transit count b-t axis 1
;5 = transit count t-b axis 2
;6 = transit Count b-t axis 2
;7 = transit Count t-b axis 3
;8 = transit Count b-t axis 3
;[a] = analog inputs
;9 = end of record HEX "8282" = -32126

function gillr2_in_tc, filepfad, anz_records=anz_records, anz_ai=anz_ai

    openr, lun, filepfad, /get_lun
    print, 'message from <gillr2_in_array.pro>: reading data file '+filepfad

    if not keyword_set(anz_ai) then anz_ai=0

    if anz_ai eq 0 then $
       r2 = {start_id: 0, counter: 0, tc: bytarr(12), end_id: 0} else $
       r2 = {start_id: 0, counter: 0, tc: bytarr(12), ai:bytarr(anz_ai*2), end_id: 0}

    fileinfo=fstat(lun)
    if not keyword_set(anz_records) then begin  ; anzahl records selber berechnen (wenn '0' gesetzt)
        anz_records=fileinfo.size/(18+2*anz_ai)
    endif

if fileinfo.size gt 200L then begin ;mindestens 2 records vorhanden in file

    rawdata = replicate (r2,anz_records)

    readu, lun, rawdata
    close, lun
    free_lun, lun

    ;data check
if mean(rawdata.start_id) ne (-32383.) then begin

    test        = bytarr(fileinfo.size)
    ;das ganze in eimem schlonz einlesen...
    openr, lun, filepfad, /get_lun
    readu,lun,test
    close, lun
    free_lun, lun

    pointer=fltarr(fileinfo.size/(18+2*anz_ai))

    anz_records=-1.d
    for i=0d,fileinfo.size-3 do begin
        if test(i) eq 129 and test(i+1) eq 129  then begin
            if (i/18.+2*anz_ai) lt n_elements(pointer) then pointer(i/(18.+2*anz_ai))=i else pointer=0
            anz_records=anz_records+1
        endif
    endfor

    if n_elements(pointer) gt anz_records and anz_records gt 0 then begin

    print,'reading again ',fileinfo.name,' byte shift detected'
    rawdata = replicate (r2,anz_records)

    ;zuweisen das ganze
    ;------------------
    for i = 0d,anz_records-12 do begin
        rawdata(i:i+1).start_id         = test(pointer(i):pointer(i)+1)
        rawdata(i:i+1).counter          = test(pointer(i)+2:pointer(i)+3)
        rawdata(i:i+11).tc              = test(pointer(i)+4:pointer(i)+15)
        if anz_ai gt 0 then begin
            rawdata(i:i+1).ai = test(pointer(i)+16:pointer(i)+17)
            rawdata(i:i+1).end_id = test(pointer(i)+18:pointer(i)+19)
        endif else rawdata(i:i+1).end_id = test(pointer(i)+16:pointer(i)+17)
    endfor
    endif else begin
        print,'error_message form <gillr2_in_array.pro>: array could not be repaired by byte shift'
        rettc=-1
        return,rettc
    endelse
endif; byte shift

    ;transit counts
    tc=intarr(6,anz_records)
    for i=0, 5 do begin
        tc(i,*)=rawdata.tc(i*2)*256+rawdata.tc(i*2+1)
    endfor


    ;evtl. analog inputs rausholen
    ;-----------------------------
    if anz_ai ne 0 then begin
        ai=intarr(anz_ai,anz_records)
        ;ai(0,*)=rawdata.ai(0)*1+rawdata.ai(1)
	ai(0,*)=rawdata.ai(0)*256+rawdata.ai(1)
        tc=[tc(*,*),ai]
    endif

    rettc=float(tc)

    endif else begin
        print, 'error in <gillr2_in_array.pro>: no data in data file '+filepfad
        rettc=-1
    endelse ;mindestens 2 records vorhanden in file

    return, rettc

end