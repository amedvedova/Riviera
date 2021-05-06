;+
; NAME:
;   aml_son_gillcal.pro
;
; PURPOSE:
;   kalibriert rohdaten von gill R2 sonics anahnd der vom
;   hersteller mitgelieferten gill-files. das r2 muss im "uvw
;   uncalibrated" oder "transit count" modus betrieben worden
;   sein, im "uvw calibrated" mode machts kein sinn, da die
;   daten schon geraeteintern kalibriert sind, und die kalibrierung
;   quasi doppelt angewendet wuerde.
;
; CALLING SEQUENCE:
;   cal=aml_son_gillcal(data,serno,gilldir=gilldir)
;
; INPUTS:
;   data      : array[n,t] mit [0,*] der rohen u-komponente (u+ nach
;               150deg bez. dem nordpfeil auf dem r2), [1,*] der rohen
;               v-komponente (v+ nach 240deg bez. dem nordpfeil auf dem
;               r2) und [2,*] der rohen w-komonente (w+ nach oben) und
;               evtl.  weiteren spalten mit temperatur und analog inputs.
;   serno     : integer. seriennummer des R2.
;   gilldir   : directory wo sich die gill-kalibrierfiles befinden.
;               default : '\\gibm-ntserver3\messnetz\kalib\sonics\gill\'
;
; OUTPUT:
;   array[n,t] mit den gill-kalibrierten daten, gleiche dimensionen wie data.
;
; EXAMPLE:
;   datafile='\\gibm-ntserver3\map\data\ag\rohdaten\fast\234\AG_N2_1999_234_050000.raw'
;   data=AML_SON_read_r2tc(datafile)
;   cal=aml_son_gillcal(data,208)
;
; REVISION HISTORY:
;   13.12.2001 AC nach RV (MAP)
;   07.02.2006 IL matrizen haben eine neue heimat
;   08.04.2011 TM matrizen bekommen schon wieder ne neue heimat
;   14.10.2011 TM matrizen neu auf met-server0
;-
function AML_SON_gillcal, data, serno, gilldir=gilldir
    ;if not keyword_set(gilldir) then gilldir='\\gibm-ntserver3\messnetz\kalib\sonics\gill\'
    ;if not keyword_set(gilldir) then gilldir='\\met-server3\messnetz\kalib\sonics\gill\'
    ;if not keyword_set(gilldir) then gilldir='\\met-messnetz\db\calib\sonics\gill\'
    if not keyword_set(gilldir) then gilldir='\\met-server0.storage.p.unibas.ch\projects\messnetz\AML\calib\sonics\gill\'
    gill=AML_SON_gillfile(serno,gilldir)
    azi=(aml_met_azi(data[1,*],data[0,*])+90+30) mod 360 ;gill-winkel linksh�ndig!
    ;magnitude and direction calibration
    um  = data[0,*] - data[1,*] * gill.uv[azi,1]
    um  = TEMPORARY(um) * gill.uv[azi,0]
    vm  = data[1,*] + data[0,*] * gill.uv[azi,1]
    vm  = TEMPORARY(vm) * gill.uv[azi,0]
    data[0,*]=um & data[1,*]=vm
    ;gill w-korrektur
    ix = where (data[2,*] ge 0.0, cnt)
    if cnt gt 0 then data[2,ix] = TEMPORARY(data[2,ix]) * gill.w[azi[ix],0]
    ix = where (data[2,*] lt 0.0, cnt)
    if cnt gt 0 then data[2,ix] = TEMPORARY(data[2,ix]) * gill.w[azi[ix],1]
    return, data
end