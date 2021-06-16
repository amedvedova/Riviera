;+
; NAME:
;   gillr2_in_tc.pro
;
; PURPOSE:
;   read in transit counts

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


    ; dealing with analog inputs
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
