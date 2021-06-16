;+
; NAME:
;       getsoncal_map
;
; PURPOSE:
;       Returns Calibration Data for the required SONIC
;       funktioniert zusammen mit r2cal (fuer die R2) und sonstcal (HS)
;
; CATEGORY:
;       Alles fuer Ihr SONIC-Anemometer
;
; CALLING SEQUENCE:
;       getsoncal_map,cson,lw,pfad  keywoerter
;
; INPUTS:
;       sono   : Number of the required SONIC           (integer)
;       lw     : drive,                                 string, e.g. m:\
;       pfad   : Pfad,                                  string  kalfiles\
;
; KEYWORDS
; Wichtig: !!!!!!!!!!! Keywoerter werden als arrays missbraucht
; Beim Aufruf werden sie gesetzt und das Program schreibt dann die Kalibrierdaten rein
;       gill                 Herstellerkalibrierung betifft im Moment nur die R2
;       mat99                Matrixkalibrierung betrifft alle
;       tpfak                so was wie Pfadlaengen fuer die R2
;       no_eff_path          Wahlmoeglichkeit fuer Pfadlaengen fuer die R2
;       vito                 Wahlmoeglichkeit fuer Pfadlaengen fuer die R2
;       HS                   Holt die caldaten fuer die HS nur HS46 und HS101
;       extra                Holt p-m-Matrizen, die auf HS-Daten im cal-Mode basieren
; OUTPUTS:
;       TPFAK   : aus Temperaturen berechnete Pfadlaengen*TFAK
;       Calibration Tables for required SONIC
;       gill_uv : GILL Calibration UV
;       gill_w  : GILL Calibration W
;       pmat,nmat:RV's PLUSMINUS-Matrix-Calibration
;
; COMMON BLOCKS:
;       Sonic_const
;
; SIDE EFFECTS:
;       Hopefully none.
;
; RESTRICTIONS:
;       valid fuer alle SONICs:
;       Holt Kalibriermatrizen und Pfadlaengen für die R2s: #43,#160,#208, #211, #212, #213
;
;       Holt nur Kalibriermatrizen fuer
;       C112, C118, C199    HS46   HS101   M9903006  M9812015
;
; MODIFICATION HISTORY:
;       frei nach RV "Alles fuer Ihr SONIC-Anemometer" aus son_inc.pas
;rv     14.2.2000
;rv     29.2.2000  Vittore Pfade mit eingebaut (keywort vito) und etwas kommentiert
;rv                bei den Kombinationen der keywoerter kann noch einiges abgefangen bzw vereinfacht werden
;evg	03.2.2000  neue Windkanalpfade eingetragen
;rv     17.3.2000  HS Kalfiles holen
;rv     20.3.2000  Zum Fruehlingsanfang eine Ausnahme eingebaut: betrifft nur das HS
;rv                mit dem keyword extra werden andere Matrizen fuer die Matrixkorrektur eingelesen
;rv                Sie basieren auf HS Daten, die im kalibrierten Mode gesammelt wurden

pro getsoncal_map,cson,lw,pfad, $
                            gill=gill,                     $
                            tpfak=tpfak,                   $
                            mat99=mat99,                   $
                            no_eff_path=no_eff_path,       $
                            vito=vito,                     $
                            hs=hs,                         $
                            extra=extra


  if strmid(cson,0,1) eq 'S' then begin
    TFAK = 29491200D*0.5        ; Quarzfrequenz halbe

    ; mal sehen ob's eins gibt
    s_index = ['S43','S160','S208','S211','S212','S213']
    s_index = where(s_index eq cson,soco)
    if soco eq 0 then message,'This R2 SONIC #'+cson+' is not available!!'
    ; Die aus Temperaturen berechneten Pfadlaengen

   if keyword_set(vito) then begin   ; alle nach AC San Vittore
    pfff =[[0.1472297,    0.1485552,    0.1478459],$     ;#43
           [0.1483970,    0.1468325,    0.1472208],$     ;#160
           [0.1443119,    0.1463585,    0.1460751],$     ;#208
           [0.1480876,    0.1478710,    0.1474957],$     ;#211
           [0.1446797,    0.1459218,    0.1466953],$     ;#212
           [0.1465218,    0.1474999,    0.1479391]]      ;#213
   endif else begin
    pfff =[[ 0.147087,0.148895, 0.148152],$    ;#43      ; WK99 EVG 2.3.00
          [0.148438	 ,0.146792, 0.147315],$	   ;#160	 ; WK99 EVG 2.3.00
          [0.149   , 0.149    , 0.149  ],$     ;#208
          [0.149   , 0.149    , 0.149  ],$     ;#211
          [0.145025, 0.146448 ,0.146982],$     ;#212     ; WK99 EVG 2.3.00
          [0.149   , 0.149    , 0.149  ]]      ;#213
   endelse
   if keyword_set(no_eff_path) then tpfak=dblarr(3)+0.149*tfak else TPFAK = pfff(*,s_index)*TFAK
  endif ; fuer die GillR2 sonics

  if keyword_set(gill) then begin
  ;GILL-Files
    uv_finam = ['0043rcal.h','0160rcal.h','0208rcal.h','0211rcal.h','0212rcal.h','0213rcal.h']
    w_fil  = 'wcal.h'

    gill_uv = dblarr(360,2) & gill_w  = dblarr(360,2)
    h = dblarr(361) ; dummy zum einlesen
    zeile   = ''

    openr,lun,lw+pfad+uv_finam(s_index),/get_lun
    readf,lun,zeile & readf,lun,zeile
    readf,lun,h
    gill_uv(*,0)= h(0:359)/65536.0d
    readf,lun,zeile & readf,lun,zeile
    readf,lun,h
    gill_uv(*,1)=h(0:359)/65536.0d
    free_lun,lun

    openr,lun,lw+pfad+w_fil,/get_lun
    readf,lun,zeile
    readf,lun,h
    gill_w(*,0)=h(0:359)/65536.0d
    readf,lun,zeile & readf,lun,zeile
    readf,lun,h
    gill_w(*,1)=h(0:359)/65536.0d
    free_lun,lun

    gill={uv : gill_uv, w : gill_w }
  endif
  if keyword_set(mat99) then begin
    ; Die PLUSMINUS Kalibrierung von RV, zwei dblarr(12,90)
    zeile=''
    PMAT = fltarr(12,90)
    NMAT = fltarr(12,90)
    file = strcompress('p_'+cson+'.004',/remove_all)
    if keyword_set(extra) and cson eq 'HS46' then file = strcompress('xp_'+cson+'.004',/remove_all)
    openr,lun,lw+pfad+file,/get_lun
    readf,lun,zeile & readf,lun,zeile
    readf,lun,PMAT
    free_lun,lun
    file = strcompress('n_'+cson+'.004',/remove_all)
    if keyword_set(extra) and cson eq 'HS46' then file = strcompress('xn_'+cson+'.004',/remove_all)
    openr,lun,lw+pfad+file,/get_lun
    print,lw+pfad+file
    readf,lun,zeile & readf,lun,zeile
    readf,lun,NMAT
    free_lun,lun

    PMAT = double(PMAT)
    NMAT = double(NMAT)

    mat99={p : pmat, n : nmat}
  endif

  if keyword_set(HS) then begin
    if cson eq 'HS46' then uv_finam = 'HS46.cal'
    gill_uv = dblarr(2,360)
    openr,lun,lw+pfad+uv_finam,/get_lun
    readf,lun,gill_uv
    gill_uv(0,*)=gill_uv(0,*)/1000.
    gill_uv(1,*)=(gill_uv(1,*)+960.)/1024
    HS=gill_uv

  endif


end
