;+
; NAME:
;      KH2OSCAL
; PURPOSE:
;      Scales mVoltage according to the instrument no.
; CATEGORY:
;
; CALLING SEQUENCE:
;       KH2Oscal(mVKH2O)
; INPUTS:
;       mV KH2O
; KEYWORDS:
; OUTPUTS:
;       Return array: absolute Humidity in [g/m3]
; COMMON BLOCKS:
; RESTRICTIONS:
; EXAMPLE:
; MODIFICATION HISTORY:
;       18.6.97  RV
;       11.7.97  RV
;       Average calculated for decision of humidity range was wrong. Corrected.
;       19.2.99  die Sache etwas vereinfacht, neue Instrumente eingetragen
function KH2Oscal,mVKH2O,KHNUM
  KHNUM=strupcase(KHNUM)
  case KHNUM of
  '1094A': Begin  ; Caldate: 9-Feb-92
             ; clean windows
             V0f = 3682d  &  Kwf = -0.123d ;full range
             V0l = 3469d  &  Kwl = -0.113d ; low range
             V0h = 4089d  &  Kwh = -0.130d ;high range
             ; scaled windows
             V0fs = 3066d  &  Kwfs = -0.129d  ;full range
             V0ls = 2891d  &  Kwls = -0.120d  ; low range
             V0hs = 3362d  &  Kwhs = -0.136d  ;high range
             path_len = 1.003d
           END
  '1094B': Begin  ; Caldate: 9-Feb-94
             V0f = 3945d  &  Kwf = -0.127d ;full range
             V0l = 3656d  &  Kwl = -0.114d ; low range
             V0h = 4407d  &  Kwh = -0.135d ;high range
             ;#1094  scaled windows
             V0fs = 3032d  &  Kwfs = -0.134d ;full range
             V0ls = 2901d  &  Kwls = -0.126d  ; low range
             V0hs = 3201d  &  Kwhs = -0.138d  ;high range
             path_len = 1.002d
           END
  '1094C': Begin  ; Caldate: 16-Dec-98
             V0f = 3538d  &  Kwf = -0.135d ;full range
             V0l = 3361d  &  Kwl = -0.126d ; low range
             V0h = 3798d  &  Kwh = -0.140d ;high range
             ;#1094  scaled windows
             V0fs = 2616d  &  Kwfs = -0.140d ;full range
             V0ls = 2590d  &  Kwls = -0.138d  ; low range
             V0hs = 2670d  &  Kwhs = -0.141d  ;high range
             path_len = 0.999d
           END

  '1138':  Begin ; Caldate: 15-Jan-92
             ;  clean windows
             V0f = 5604d  &  Kwf = -0.144d;full range
             V0l = 6004d  &  Kwl = -0.153d ; low range
             V0h = 5067d  &  Kwh = -0.139d ;high range
             ;  scaled windows
             V0fs = 3962d  &  Kwfs = -0.146d  ;full range
             V0ls = 4171d  &  Kwls = -0.153d  ; low range
             V0hs = 3807d  &  Kwhs = -0.144d  ;high range
             path_len = 1.360d
           END
  '1139':  Begin  ; Caldate: 15-Jan-92
             ; clean windows
             V0f = 4811d  &  Kwf = -0.144d ;full range
             V0l = 5163d  &  Kwl = -0.153d ; low range
             V0h = 4353d  &  Kwh = -0.139d ;high range
             ; scaled windows
             V0fs = 3514d  &  Kwfs = -0.145d  ;full range
             V0ls = 3689d  &  Kwls = -0.152d  ; low range
             V0hs = 3370d  &  Kwhs = -0.143d  ;high range
             path_len = 1.356d
           END
  '1199A': Begin  ; Caldate: 17-Mar-94
             ; clean windows
             V0f = 5188d  &  Kwf = -0.144d ;full range
             V0l = 5566d  &  Kwl = -0.155d ; low range
             V0h = 4750d  &  Kwh = -0.139d ;high range
             ; scaled windows
             V0fs = 3085d  &  Kwfs = -0.144d  ;full range
             V0ls = 3659d  &  Kwls = -0.155d  ; low range
             V0hs = 3057d  &  Kwhs = -0.139d  ;high range
             path_len = 1.370d
           END
  '1199B': Begin  ; Caldate: 20-Jul-95
             ; clean windows
             V0f = 5925d  &  Kwf = -0.142d ;full range
             V0l = 6309d  &  Kwl = -0.151d ; low range
             V0h = 5508d &  Kwh = -0.139d ;high range
             ; scaled windows
             V0fs = 3035d  &  Kwfs = -0.144d  ;full range
             V0ls = 3273d  &  Kwls = -0.154d  ; low range
             V0hs = 2759d  &  Kwhs = -0.139d  ;high range
             path_len = 1.317d
           END
  '1199C': Begin  ; Caldate: 16-Dec-98
             ; clean windows
             V0f = 5001d  &  Kwf = -0.146d ;full range
             V0l = 5221d  &  Kwl = -0.152d ; low range
             V0h = 4850d  &  Kwh = -0.144d ;high range
             ; scaled windows
             V0fs = 3723d  &  Kwfs = -0.143d  ;full range
             V0ls = 4000d  &  Kwls = -0.144d  ; low range
             V0hs = 3390d  &  Kwhs = -0.138d  ;high range
             path_len = 1.311d
           END

  ELSE: Print, 'Das KH2O existiert nicht !'
  ENDCASE
  tiny = 1d*1.0E-2 ;same number as in Davis
  bad_val = WHERE(mVKH2O LE 0, warning)
  IF warning NE 0 THEN PRINT,'negative KH2O-Voltages'
  IF warning NE 0 THEN mVKH2O(bad_val) = tiny
  if warning gt n_elements(mvKH2O)/5 then begin
    retarr=mVKH2O*0.000001
    goto,gump
  endif

  XKw = path_len * Kwf
  logV0 = ALOG(V0f)

  retarr = (ALOG(mVKH2O) - logV0) / XKw
  IF TOTAL(retarr)/float(n_elements(retarr)) GT 9. THEN BEGIN
    XKw = path_len * Kwh
    logV0 = ALOG(V0h)
    retarr = (ALOG(mVKH2O) - logV0) / XKw
  ENDIF ELSE BEGIN
    XKw = path_len * Kwl
    logV0 = ALOG(V0l)
    retarr = (ALOG(mVKH2O) - logV0) / XKw
  ENDELSE

  gump:
  return, retarr
  retarr=0B & mVKH2O=0B

end
