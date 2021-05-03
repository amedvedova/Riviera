;+
; NAME:
;   AML_SON_read_hs.pro
;
; PURPOSE:
;   sonic-daten einlesen. liest binaerdaten des sonics Gill HS (z.b. der labview-erfassung) in einen array der
;   form u [m/s], v[m/s], w[m/s], t[degC], ai1[unskaliert], ai2[unskaliert], ...
;
;   u v w t ai1 ai2 ...
;   u v w t ai1 ai2 ...
;   . . . .  .   .
;   . . . .  .   .
;   . . . .  .   .
;
;   float-array[4+anz_ai,r], wobei anz_ai die anzahl analog inputs ist und r die anzahl eingelesener
;   records. die id-bytes in den daten werden kontrolliert, ob der eingelesene datenblock vollstaendig ist.
;   inklinometer und statusinformationen werden nicht rausgenommen.
;
; CALLING SEQUENCE:
;   result=AML_SON_read_hs(filepath, anz_records, anz_ai=anz_ai, /inklino, /status)
;
; INPUTS:
;   filepath    : string. pfad. vollstaendiger pfad des datenfiles
;   anz_rec     : ganzzahl. anzahl analog-inputs, die aufgeschaltet sind.
;   anz_records : keyword. ganzzahl. anzahl records, die eingelesen werden sollen, beginnend vom anfang,
;                 wenn nicht gesetzt, dann wird das ganze file von anfang bis ende eingelesen.
;   inklino     : keyword. 1 oder 0. ANSTATT uwvt wird das inklinometer ausgegeben.
;   status      : keyword.  1 oder 0. zum plotten der statusinforamtion in den output log, zusaetzlich.
;
; OUTPUT:
;   array[4+anz_ai,r] mit u,v,w,t,ai1,ai2... oder
;   array[r/10,2] mit [r/10,0] inklinometer x, und [r/10,1] inklinometer y wenn keyword "/inklino" gesetzt
;
;   u,v und w sind ein rechtshaendiges system.
;   u+ zeigt nach 0deg wenn du von der kleinen kabelbox dem ausleger entlang schaust (Sonic N).
;   v+ zeigt nach 270deg (links) wenn du von der kleinen kabelbox dem ausleger entlang schaust (Sonic W).
;   w+ zeigt nach oben.
;   d.h. ein wind mit u+ blaesst von der kleinen kabelbox zum kopf
;   und ein wind mit v+ von rechts nach links, wenn du an der
;   kleinen kabelbox stehst und zum kopf schaust.
;   dies gilt nur wenn die ausrichtung auf "spar alignment" gesetzt ist
;   und nicht fuer "axis alignment" bei axis alignment ist das u+=330deg und v+=240deg
;
; EXAMPLE:
;   data=AML_SON_read_hs('m:\data\mn\rohdaten\fast\228\MN_N8_1999_228_010000.raw',1,anz_rec=100, /status)
;   data=AML_SON_read_hs('Macintosh HD:Users:andi:Documents:EBEX:sonic:2000_220:EX_A2_2000_220_060000.raw',3)
;
; REVISION HISTORY:
;   28.08.99 AC (MAP)
;   12.09.99 AC fehler beim einlesen mit 0 analog-inputs korrigiert
;   11.12.99 AC wenn id-byte kontrolle nicht bestanden -> "-1" als result, da daten unbrauchbar.
;   03.03.00 AC inklinometer und statusinformationen eingebaut. anz_rec als keyword
;   23.03.00 EvG option id_shift beigefügt
;   08.11.01 CF/AC mean durch where ersetzt bei ID kontrolle
;   13.12.01 AC dokumentation verbessert
;   30.04.03 AC (sehr seltener) fehler abgefangen der zum aubsturz führte, bei korrektur der rohdaten (out of range).
;-
;GILL HS STREAMING FORMAT (1 zeichen entspricht 1 byte, ausgenommen []):
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

function AML_SON_read_hs, filepath,anz_ai,anz_rec=anz_rec,inklino=inklino,status=status

    fak=10000./2.^14
    if anz_ai gt 0 then begin
        recordhs = {id1: 0B, id2: 0B, status_adress:0B,status_data: 0B,wind: bytarr(6), temp: bytarr(2), ai: bytarr(anz_ai*2), checksum: 0B}
    endif else begin
        recordhs = {id1: 0B, id2: 0B, status_adress:0B,status_data: 0B,wind: bytarr(6), temp: bytarr(2), checksum: 0B}
    endelse

    openr, lun, filepath, /get_lun

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
    ix=where(rawdata.id1 ne 186, cnt)
    if cnt ne 0 then begin
        test       = bytarr(fileinfo.size)
        ;das ganze in eimem schlonz einlesen...
        openr, lun, filepath, /get_lun
        readu,lun,test
        close, lun
        free_lun, lun
        pointer=fltarr(fileinfo.size/(13.+2*anz_ai))
        anz_records=-1.d
        for i=0d,fileinfo.size-3 do begin
            if test(i) eq 186 and test(i+1) eq 186 and test(i+2) ge 0. and test(i+2) le 10. then begin
                if n_elements(pointer)-1 gt i/(13.+2*anz_ai) then pointer(i/(13.+2*anz_ai))=i else begin
                 message, 'Fehler beim Versuch Fehlerhafte Rohdaten  zusammenzufügen.',/informational
                 return, -1
                endelse
                anz_records=anz_records+1
            endif
        endfor
        print,'reading again ',fileinfo.name,' byte shift detected'
        rawdata = replicate (recordhs,anz_records)
        ;zuweisen das ganze
        ;------------------
        for i = 0d,anz_records-1 do begin
            if i le n_elements(pointer)-1 then begin
            rawdata(i).id1              = test(pointer(i))
            rawdata(i).id2              = test(pointer(i)+1)
            rawdata(i).status_adress    = test(pointer(i)+2)
            rawdata(i).status_data      = test(pointer(i)+3)
            rawdata(i).wind             = test(pointer(i)+4:pointer(i)+9)
            rawdata(i).temp             = test(pointer(i)+10:pointer(i)+11)
            if anz_ai gt 0 then begin
                rawdata(i).ai = test(pointer(i)+12:pointer(i)+13)
                rawdata(i).checksum = test(pointer(i)+14)
            endif else rawdata(i).checksum = test(pointer(i)+12)
            endif else begin
                message, 'Fehler, Arrayposition ausserhalb zulässigem Range.',/informational
                return, -1
            endelse
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
        print, "Axis definition (uvw=0, u'v'w'=1):" +string(b(4,0))
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
    windrawdata=fltarr(3,anz_records)
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
            airawdata(i,*)=rawdata.ai(i*2)*256+rawdata.ai(i*2+1)
            uvwt(4+i,*)=airawdata(i,*)*fak
        endfor
    endif
   return, uvwt
   ;endelse ;ende inklino
;   endelse; ende data check
end