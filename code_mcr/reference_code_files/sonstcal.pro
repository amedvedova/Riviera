;+
; NAME:
;       sonstcal.PRO
; PURPOSE:
;       wendet mat99 kalibrierung auf uvw array an
; CATEGORY:
;       Alles fuer Ihr SONIC-Anemometer
; CALLING SEQUENCE:
;       sonstcal,uvw,mat99=mat99
; INPUTS:
;       uvw    : windvektor                                  (mind. float)
; OUTPUTS:
;
; KEYWORDS:
;	    mat99		: darf nicht mir /mat99 aufgerufen werden, sondern nur mit mat99=mat99, da
;                     die Matrizen uebergeben werden
;                     RV's Matrix-Kalibrierung in 4 Grad Schritten, unterscheidet zw.
;			          positiven und negativen w

; SIDE EFFECTS:
;       Hopefully none.
; MODIFICATION HISTORY:
;rv     18.2.00 das Programm fuer nicht R2 gebaut.
;rv     29.2.00 Kommentare eingefuegt

function sonstcal,uvw,mat99=mat99

  if keyword_set(mat99) then begin
  ; die Matritzen stecken in mat99 (pmat und nmat)
    print,'PLUSMINUS-Kalibrierung neu'

  ; Winkel im meteorologischen Koordinatensystem berechnen
    azi = round(met_azi(reform(uvw(0,*)),reform(uvw(1,*))))
    ix=where(azi eq 360) & if ix(0) ne -1 then azi(ix)=0

   ; Azimut-Viererklassen-Index
    qa = azi/4
    um=reform(uvw(0,*)) & vm=reform(uvw(1,*)) & wm=reform(uvw(2,*))

   ; Positiv or Negativ Index-Index
    po=Where(wm ge 0.0) &  ng=Where(wm lt 0.0)

    if po(0) ne -1 then begin
      uvw(0,po)=um(po)*mat99.p(0,qa(po))+vm(po)*mat99.p(1,qa(po))+wm(po)*mat99.p(2,qa(po)) +mat99.p(3,qa(po))
      uvw(1,po)=um(po)*mat99.p(4,qa(po))+vm(po)*mat99.p(5,qa(po))+wm(po)*mat99.p(6,qa(po)) +mat99.p(7,qa(po))
      uvw(2,po)=um(po)*mat99.p(8,qa(po))+vm(po)*mat99.p(9,qa(po))+wm(po)*mat99.p(10,qa(po))+mat99.p(11,qa(po))
    endif
    if ng(0) ne -1 then begin
      uvw(0,ng)=um(ng)*mat99.n(0,qa(ng))+vm(ng)*mat99.n(1,qa(ng))+wm(ng)*mat99.n(2,qa(ng)) +mat99.n(3,qa(ng))
      uvw(1,ng)=um(ng)*mat99.n(4,qa(ng))+vm(ng)*mat99.n(5,qa(ng))+wm(ng)*mat99.n(6,qa(ng)) +mat99.n(7,qa(ng))
      uvw(2,ng)=um(ng)*mat99.n(8,qa(ng))+vm(ng)*mat99.n(9,qa(ng))+wm(ng)*mat99.n(10,qa(ng))+mat99.n(11,qa(ng))
    endif

    po=0 & ng=0 & qa=0
    um = 0B  &  vm = 0B & wm = 0B

  endif

  return,uvw

end
