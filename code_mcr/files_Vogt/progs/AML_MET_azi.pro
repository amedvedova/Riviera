;+
; NAME:
;       AML_MET_azi.pro
;
; PURPOSE:
;       Berechnet aus u (u+ = Ost) und v (v+ = Nord)
;       den Winkel im meteorologischen Koordinatensystem.
;       ein wind aus westen hat ein positives u
;       ein wind aus sueden hat ein positives v
;
; CALLING SEQUENCE:
;       retarr=AML_MET_azi(u,v)
;
; INPUTS:
;       u,v
;
; OUTPUTS:
;       Winkel im met.Koordinatensystem mit 0° = Nord und 90° = Ost
;
; MODIFICATION HISTORY:
;       30.07.1996   aus metwink abglaeutert
;       13.09.2001   aus met_azi umgeschrieben, so dass u+ Ost und v+ Nord
;-

function AML_MET_azi,u,v

 fehlerwert=-9999
 fehler=where(u eq fehlerwert or v eq fehlerwert, cnt)

 u=double(u) & v=double(v)
 retarr=fltarr(n_elements(u))
 index=where(u eq 0 and v gt 0)
 if index[0] ne -1 then retarr[index]=180 ;0
 index=where(u eq 0 and v lt 0)
 if index[0] ne -1 then retarr[index]=0 ;180
 index=where(u lt 0)
 if index[0] ne -1 then  $
 retarr[index]=( 90.0-(atan(v(index)/u(index))/!pi)*180.0)
 index=where(u gt 0)
 if index[0] ne -1 then $
 retarr[index]=(270.0-(atan(v(index)/u(index))/!pi)*180.0)

 if cnt gt 0 then retarr[fehler]=fehlerwert

 return,retarr
 retarr=0b

end

