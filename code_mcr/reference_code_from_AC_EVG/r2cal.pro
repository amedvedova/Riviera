;+
; NAME:
;       r2cal.PRO
; PURPOSE:
;       Scales and calibrates an array of transit counts of a certain SONIC#
; CATEGORY:
;       Alles fuer Ihr SONIC-Anemometer
; CALLING SEQUENCE:
;       r2cal,tc,cson,tpfak,qq=qq,mat99=mat99,gill=gill,pure=pure, no_eff_path=no_eff_path,kelvin=kelvin
; INPUTS:
;       tc     : transit counts                                  (integer)
;       cson   : Number of the required SONIC                    (integer)
; OUTPUTS:
;
; KEYWORDS:
;       /gill		: Gill manufacturer calibration
;	    /mat99		: RV's Matrix-Kalibrierung in 4 Grad Schritten, unterscheidet zw.
;			      positiven und negativen w
;	    /pure		: no calibration at all
;rv     qq          : value for humidity correction of temperature. Can be one number (in case of a constant value e.g.)
;rv                   in wind tunnel or a vector (in case of Krypton measurements). Currently qq is interpreted as absolute
;rv                   humidity !!!!!!!!!!!!!!!!!!!!!!
;rv     /no_eff_path: do not use effective path length, instead 0.149m are used.
;rv     /Kelvin     : Temperature in Kelvin instead of Celsius

; SIDE EFFECTS:
;       Hopefully none.
; RESTRICTIONS:
;       valid for R2-SONICS provided, calibration information exists
; MODIFICATION HISTORY:
;       frei nach RV's baflux.pas,fluxun1.pas
;       17.2.00 RV, AC, EvG
;rv     18.2.00 das Programm zum Laufen gebracht. Keywort Kelvin eingefuehrt


function r2cal,tc,cson,tpfak,qq=qq,mat99=mat99,gill=gill,pure=pure,kelvin=kelvin

  if keyword_set(mat99) then k1=1 else k1=0
  if keyword_set(pure)  then k2=1 else k2=0
  if keyword_set(gill)  then k3=1 else k3=0

  if k1+k2+k3 ne 1 then begin
    message,'Nu Nu zuviel oder zuwenig Kalibrierungen '
    stop
  endif


 ; achsen- und schallgeschwindigkeiten
  a1  = reform(TPFAK(0)/tc(0,*)-TPFAK(0)/tc(1,*))
  a2  = reform(TPFAK(1)/tc(2,*)-TPFAK(1)/tc(3,*))
  a3  = reform(TPFAK(2)/tc(4,*)-TPFAK(2)/tc(5,*))

  ; umrechnen ins uvw-SONIC Koordinatensystem
  u = reform((2*a1 - a2 - a3)/2.1213D)
  v = reform((       a2 - a3)/1.2247D)
  w = reform(( -a1 - a2 - a3)/2.1213D)


  c1  = reform(TPFAK(0)/tc(0,*)+TPFAK(0)/tc(1,*))
  c2  = reform(TPFAK(1)/tc(2,*)+TPFAK(1)/tc(3,*))
  c3  = reform(TPFAK(2)/tc(4,*)+TPFAK(2)/tc(5,*))

  ; Speicher der Transitcounts tc freigeben
  tc = 0B



  ; azimuth in meteorological coordinate system (relative to north arrow of instrument)
  azi = round (metgink(u,v))
  ix=where(azi eq 360) & if ix(0) ne -1 then azi(ix)=0

  um=u & vm=v & wm=w


  ;*********** GILL-Calibration ************

  if keyword_set(gill) then begin


  ; magnitude and direction calibration
    um  = u - v * gill.uv(azi,1)
    um  = TEMPORARY(um) * gill.uv(azi,0)
    vm  = v + u * gill.uv(azi,1)
    vm  = TEMPORARY(vm) * gill.uv(azi,0)

  ; w Calibration
    ix = where (wm ge 0.0)
    if ix(0) ne -1 then wm(ix) = TEMPORARY(wm(ix)) * gill.w(azi(ix),0)
    ix = where (wm lt 0.0)
    if ix(0) ne -1 then wm(ix) = TEMPORARY(wm(ix)) * gill.w(azi(ix),1)
    u = 0B  &  v = 0B & w = 0B

  endif


  ;**************************************
  ; RV's Matrix-PLUSMINUS-Calibration
  ;**************************************

  if keyword_set(mat99) then begin
  ; die Matritzen stecken in mat99 (pmat und nmat)
    print,'PLUSMINUS-Kalibrierung'
    qa = azi/4                  ; Azimut-Viererklassen-Index

    umm=u & vmm=v & wmm=w
    po=Where(w ge 0.0) &  ng=Where(w lt 0.0)  ; Positiv or Negativ Index-Index

    if po(0) ne -1 then begin
      umm(po)=um(po)*mat99.p(0,qa(po))+vm(po)*mat99.p(1,qa(po))+wm(po)*mat99.p(2,qa(po)) +mat99.p(3,qa(po))
      vmm(po)=um(po)*mat99.p(4,qa(po))+vm(po)*mat99.p(5,qa(po))+wm(po)*mat99.p(6,qa(po)) +mat99.p(7,qa(po))
      wmm(po)=um(po)*mat99.p(8,qa(po))+vm(po)*mat99.p(9,qa(po))+wm(po)*mat99.p(10,qa(po))+mat99.p(11,qa(po))
    endif
    if ng(0) ne -1 then begin
      umm(ng)=um(ng)*mat99.n(0,qa(ng))+vm(ng)*mat99.n(1,qa(ng))+wm(ng)*mat99.n(2,qa(ng)) +mat99.n(3,qa(ng))
      vmm(ng)=um(ng)*mat99.n(4,qa(ng))+vm(ng)*mat99.n(5,qa(ng))+wm(ng)*mat99.n(6,qa(ng)) +mat99.n(7,qa(ng))
      wmm(ng)=um(ng)*mat99.n(8,qa(ng))+vm(ng)*mat99.n(9,qa(ng))+wm(ng)*mat99.n(10,qa(ng))+mat99.n(11,qa(ng))
    endif

  ; Winkel im meteorologischen Koordinatensystem berechnen
    azi = round(metgink(umm,vmm))
    ix=where(azi eq 360) & if ix(0) ne -1 then azi(ix)=0

    po=0 & ng=0 & qa=0
    um=umm & vm=vmm & wm=wmm
    u = 0B  &  v = 0B & w = 0B
    umm = 0B  &  vmm = 0B & wmm = 0B
  endif

; ******************************************************************************
;    ENDE Einzelne Kalibrierverfahren oder auch pure
; ******************************************************************************

  ;**************************************
  ; Temperaturen (ohne Feuchte-Korrektur)
  ;**************************************

  ; Geschwindigkeitskomponente normal zum Pfad berechnen
  vu = sqrt((um^2 + vm^2 + wm^2 - a1^2)>0)
  vv = sqrt((um^2 + vm^2 + wm^2 - a2^2)>0)
  vw = sqrt((um^2 + vm^2 + wm^2 - a3^2)>0)

  a1 = 0B &  a2 = 0B & a3 = 0B
  if keyword_set(kelvin) then anul=0. else anul = -273.15
  ; ... und die Pfad-Temperaturen
  if keyword_set(qq) then begin
    t1 = reform((c1^2 + vu^2)/(402.7d*(1.0d + 0.51*qq/1200d)))+anul
    t2 = reform((c2^2 + vv^2)/(402.7d*(1.0d + 0.51*qq/1200d)))+anul
    t3 = reform((c3^2 + vw^2)/(402.7d*(1.0d + 0.51*qq/1200d)))+anul
  endif

  t1v = reform((c1^2 + vu^2)/402.7d)+anul
  t2v = reform((c2^2 + vv^2)/402.7d)+anul
  t3v = reform((c3^2 + vw^2)/402.7d)+anul

  Tvmean = (t1v+t2v+t3v)/3D

  skawe=fltarr(7,n_elements(um))
  skawe(0,*)=um & skawe(1,*)=vm & skawe(2,*)=wm

  if keyword_set(qq) then begin
    skawe(3,*)=t1 & skawe(4,*)=t2 & skawe(5,*)=t3 & skawe(6,*)=tvmean
  endif else begin
    skawe(3,*)=t1v & skawe(4,*)=t2v & skawe(5,*)=t3v & skawe(6,*)=tvmean
  endelse

  cu = 0 & cv = 0 & cw = 0 & vu = 0 & vv = 0 & vw = 0
  um = 0 & vm = 0 & vw = 0 & t1 = 0 & t2 = 0 & t3 = 0
  t1v = 0 & t2v = 0 & t3v = 0 & tvmean=0B

  return,skawe

end
