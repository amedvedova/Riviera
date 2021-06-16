;+
; NAME:
;   masota.pro
;   masota=zu kurzer name für "MAp-SOnic-TAgesfiles"

;
; PURPOSE:
;   macht map-sonic-tagesfiles (structure und ascii). produziert dazu ein header-file
;   berechnet statistik mit normalem reynolds.jatzt auch mit aml_son_rotator
;
; CALLING SEQUENCE:
;    masota, source_dir, destination_dir, stationsid, sensorid, kalpfad=kalpfad, options=[a,b,c,d]
;
; INPUTS:
;   source_dir      : directory mit den files des tages
;   destination_dir : hier werden die  tagesfiles hingeschrieben (ascii / binary / header)
;   stationsid      : stationsid als string 'XX' z.b. 'MN'
;   sensorid        : sensor als string 'XX' z.b. 'N7'
;   options         : kalibrierung/behandlung als array der form [a,b,c,d]
;
;                     a ist METHODE
;                     1= pure (mit standardpfadlängen bei r2)
;                     2= gillr2 herstellerklaibrierung (nur r2)
;                     3= matrixkalibrerierung windkanal
;
;                     b ist behandlung der PFADLÄNGEN (nur wirksam bei x=1, 2 und 3 und ausschliesslich bei r2's)
;                     0= keine effektiven pfadlängen, standard 0.149 (default)
;                     1= windkanal99 pfadlängen
;                     2= sanvittore pfadlängen
;
;                     c ist behandlung des detrendings
;                     0= kein detrending
;                     1= lineares detrending
;
;                     d ist rotation/ausrichtung
;                     0= keine drehung (original geräteintern)
;                     1= u+ nach ggN, v+ nach ggE (GEOGRAPHIC)
;                     2= u+ longitudinal, v+ lateral linksdrehend (STREAMLINE)
;                     3= streamline mit dritter rotation so dass v'w'=0 (McMillen, 1988)
;
;
; OUTPUT:
;   tagesfiles als ascii (.asc) und binary (.stc) und header file
;
; EXAMPLES:
;   masota, 'm:\data\sonic\MAP 001\vgl\sonics1\195\','c:\temp\','V2','02', options=[3,2,0,2]
;
; REVISION HISTORY:
;   20.01.00 AC (MAP) vorerst erst für planare anordnung (rotation)
;   24.01.00 AC neu auch für 3d fall mit sonicN2ggN.pro verknüpfe. evg-kompatibel
;   30.01.00 AC fehler bei mittelung der temperatur r2 behoben (betr: höhere momente ab 2 und cov_mat mit t)
;   04.02.00 AC streamline-option eingebaut
;   17.02.00 AC kalibrierungsoptionen
;   29.02.00 AC kalibrierungsoptionen über array "options".
;   03.03.00 AC änderungen bei matrixkalibrierung.
;   24.03.00 AC gill hs wird mit keyword extra eingelesen. annahme, dass daten "uvw calibrated" sind.
;   30.03.00 AC metek wird neu ohne "0-record" eingelsen. änderung in metek_in_array.pro. V1.61
;   22.11.00 AC macmillen-rotation eingebaut.
;-

pro masota, source_dir, destination_dir, stationsid, sensorid, options=options


version='1.7 (22.11.00) AC <masota.pro>'

    ;structure definieren (evg-structure)

final=  {time:      dblarr(2,48),$   ;0=anfangszeit, 1=endzeit als doy.bruchteil doy
        ens:        lonarr(48), $
        eta:        dblarr(48), $
        theta:      dblarr(48), $
        rotang:     dblarr(3,48), $
        M_vec:      dblarr(48), $ ;vektoriell sqrt(u-quer^2+v-quer^2)
        M_cup:      dblarr(48), $ ;skalar     summe von x=0 bis anz_rec-1 von: (sqrt(ux^2+vx^2)) /anz_rec
        M_uvw:      dblarr(48), $ ;skalar
        mittel:     dblarr(8,48), $ ;u,v,w,M,t,t1,t2,t3
        std_dev:    dblarr(8,48), $ ;u,v,w,M,t,t1,t2,t3
        skewness:   dblarr(8,48), $ ;u,v,w,M,t,t1,t2,t3
        kurtosis:   dblarr(8,48), $ ;u,v,w,M,t,t1,t2,t3
        cov_mat :   dblarr(5,5,48)} ;u,v,w,M,t

    ;structure mit fehlerwerten füllen

        final.time(*,*)     = -9999
        final.ens(*)        = -9999
        final.eta(*)        = -9999.
        final.theta(*)      = -9999.
        final.rotang(*,*)   = -9999.
        final.M_vec(*)      = -9999.
        final.M_cup(*)      = -9999.
        final.M_uvw(*)      = -9999.
        final.mittel(*,*)   = -9999.
        final.std_dev(*,*)  = -9999.
        final.skewness(*,*) = -9999.
        final.kurtosis(*,*) = -9999.
        final.cov_mat(*,*,*)= -9999.

rotang=[-9999,-9999,-9999]

    ;logfiles suchen
    logfilelist=findfile(source_dir+stationsid+'*.log') ;array mit allen logfile-pfaden
    logfilelist=logfilelist(sort(logfilelist)) ;aufsteigend sortieren
    datafilelist=strarr(48)
    datfilestatus=strarr(48)
    zeiten=halbstundenstring() ;zeitinformation der nur ganzen zeitschritte
    logi=read_soniclogfile(logfilelist(0)) ;erstes logfile exemplarisch öffnen
    fn1=strmid(logi(0).logfilename,0,12)  ;beginn des logfilenamens 'MN_1999_237_' einfach erstes file

;-----------------------------------------------------------------------------------------------------

for fino=0, 47 do begin ;file für file durcharbeiten

    somuesseesheissen=source_dir+fn1+zeiten(fino,0)+'.log'

    ;zeiten berechnen
    filenameinfo=data_from_filename(strcompress(somuesseesheissen, /remove_all))
    starttime=filenameinfo.doy_dat ;als doydat
    endtime=filenameinfo.doy_dat+double(1)/48 ;start + 1/2 Stunde
    final.time(0,fino)  = starttime & final.time(1,fino)  = endtime ;zeiten einfüllen

    gibtesdas=findfile(somuesseesheissen)

if gibtesdas(0) ne '' then begin ;file exist if
    logi=read_soniclogfile(somuesseesheissen)   ;jetzt dieses einlesen
    pos=where(strupcase(sensorid) eq logi.sensor_id) ;gewünschter sensor suchen

if pos(0) ne -1 then begin ;kein sensor der so heisst - if

    ;datafilenamen aufnehmen
    datafile=logi(pos).datafilename
    datafilelist(fino)=datafile

    if logi(pos).no_of_errors gt 0 then begin
        print, 'message from <masota.pro>: fehler in '+datafile
    endif else begin
        print, 'message from <masota.pro>: '+datafile+' wird bearbeitet'

;rohdaten einlesen in 'xyzt' -----------------------------------------------------------------------

        rawIN=raw_reader(source_dir+logi(pos).datafilename,kalmet=[options(0),options(1)])
        if n_elements(rawIn) ne 1 then begin

;rotationen ------------------------------------------------------------------------------------------

            if options(3) ne 0 then begin
                raw=uvw2sonicN(rawIN(0:2,*),logi(pos).sensor_type)  ;raw -> sonicN
                raw=sonicN2ggN(raw,logi(pos).inklination,logi(pos).exposition, logi(pos).azimuth) ; sonicN -> ggN
                rawIn(0:2,*)=raw(0:2,*)
            endif else begin
                print, 'warning in <masota.pro>: -no rotation- selected.'
            endelse

            xyzt=rawIn

;halbstunden-statistik ------------------------------------------------------------------------------------------

            case options(3) of
            2 : begin ;streamline
                if options(2) eq 1 then begin ;detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens,eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat, /streamline, /detrend
                endif else begin ;no detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens,eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat, /streamline
                endelse
             end
             3 : begin ;streamline aber mit drei rotationen
                rotang=1
                xyzt(0:2,*)=aml_son_rotator(xyzt(0:2,*), rotang=rotang, /rot3)
                if options(2) eq 1 then begin ;detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens, eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat, /detrend
                endif else begin ;no detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens, eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat
                endelse
                rotang=rotang*!radeg
             end
             else : begin ;no streamline
                if options(2) eq 1 then begin ;detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens,eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat, /detrend
                endif else begin ;no detrend
                    REYNOLDS_AC, xyzt(0:3,*), ens,eta, theta, uv_len, uv_cup_speed, uvw_cup_speed, $ ;output vars
                    raw_meandata, std_dev, skewness, kurtosis, cov_mat, sonicstat
                endelse
            end
            endcase

;halbestunde-rausschreiben ---------------------------------------------------------------------------------------

            final.ens(fino)  = ens
            final.eta(fino)  = eta
            final.theta(fino)= theta
            final.rotang(*,fino)= rotang
            final.M_vec(fino)= uv_len
            final.M_cup(fino)= uv_cup_speed
            final.M_uvw(fino)= uvw_cup_speed
            ;uvwt                                         t (und 3,* leer lassen (damit kompatibel zu evg-format))
            final.mittel(0:2,fino)  = raw_meandata(0:2) & final.mittel(4,fino) = raw_meandata(3)
            final.std_dev(0:2,fino) = std_dev(0:2)      & final.std_dev(4,fino) = std_dev(3)
            final.skewness(0:2,fino)= skewness(0:2)     & final.skewness(4,fino)= skewness(3)
            final.kurtosis(0:2,fino)= kurtosis(0:2)     & final.kurtosis(4,fino)= kurtosis(3) ;uvw
            final.cov_mat(0:2,0:2,fino) = cov_mat(0:2,0:2) ;uu uv uw um ut
            final.cov_mat(4,0:2,fino)   = cov_mat(3,0:2)   ;vu vv vw vm vt
            final.cov_mat(0:2,4,fino)   = cov_mat(0:2,3)   ;wu wv ww wm wt
            final.cov_mat(4,4,fino)     = cov_mat(3,3)     ;mu mv mw mm mt
                                                           ;tu tv tw tm tt ;alle terme mit m sind hier leer!

            datfilestatus(fino)=zeiten(fino,1)+' : '+datafile+' ('+strcompress(string(ens), /remove_all)+' Records)'
    endif else begin
            datfilestatus(fino)=zeiten(fino,1)+' : '+datafile+' (Not Calculated. Aquisition Errors.)'
    endelse

        endelse
    endif else begin ;kein sensor der so heisst - if
            datfilestatus(fino)=zeiten(fino,1)+' : Error: Sensor in logfile not found (Not Calculated).'
    endelse ;kein sensor der so heisst - if
    endif else begin
            datfilestatus(fino)=zeiten(fino,1)+' : No datafile.'
            print, 'message from <masota.pro>: '+datfilestatus(fino)
    endelse
endfor

    ;resultate in files schreiben -----------------------------------------------------------------------

    dirstc=destination_dir+strmid(datafile,0,14)+'.stc' ;dblarr-file
    dirasc=destination_dir+strmid(datafile,0,14)+'.asc' ;asc-file
    dirhea=destination_dir+strmid(datafile,0,14)+'.txt' ;header-file

    openw, lun1,dirstc ,/get_lun
    openw, lun2,dirasc ,/get_lun
    openw, lun3,dirhea ,/get_lun

    ;Header file
    printf,lun3,' # HEADER INFORMATION FOR STRUCTURE MAP-SONIC-FILES'
    printf,lun3,' # Binary Structure-File: '+dirstc
    printf,lun3,' # ASCII-File           : '+dirasc
    printf,lun3,' # Files Calculated     : '+systime()
    printf,lun3,' # Calculation Version  : '+version

 case options(0) of
 1: printf,lun3,' # Calibration Method   : PURE
 2: printf,lun3,' # Calibration Method   : GILL HERSTELLER [R2]
 3: printf,lun3,' # Calibration Method   : MATRIX WK 99
 endcase

 case options(1) of
 0: printf,lun3,' # Path Handling [R2]   : no effective pathlength
 1: printf,lun3,' # Path Handling [R2]   : WK 99
 2: printf,lun3,' # Path Handling [R2]   : San Vittore 99 [196 00:00-04:00 REF CSAT 199 AC]
 endcase

 case options(2)  of
 0: printf,lun3,' # Detrending           : no detrending
 1: printf,lun3,' # Detrending           : linear detrending
 endcase

 case options(3)  of
 0: printf,lun3,' # Rotation             : NO ROTATION : (uvw = sensor xyz)
 1: printf,lun3,' # Rotation             : GEOGRAPHIC  : u+ nach ggN, v+ nach ggE, w=up
 2: printf,lun3,' # Rotation             : STREAMLINE  : u+ longitudinal, v+ lateral lefthanded, w=up (wq=0)
 3: printf,lun3,' # Rotation             : STREAMLINE  : with 3rd Rotation vw=0, u+ longitudinal, v+ lateral lefthanded, w=up (wq=0)
 endcase

    printf,lun3,' # Structure Definiton  : {time     : dblarr(2,48) - 0=Start, 1=End
    printf,lun3,' #                         ens      : lonarr(48)
    printf,lun3,' #                         eta      : dblarr(48)
    printf,lun3,' #                         theta    : dblarr(48)
    printf,lun3,' #                         M_vec    : dblarr(48)
    printf,lun3,' #                         M_cup    : dblarr(48)
    printf,lun3,' #                         M_uvw    : dblarr(48)
    printf,lun3,' #                         mittel   : dblarr(8,48)
    printf,lun3,' #                         std_dev  : dblarr(8,48)
    printf,lun3,' #                         skewness : dblarr(8,48)
    printf,lun3,' #                         kurtosis : dblarr(8,48)
    printf,lun3,' #                         cov_mat  : dblarr(5,5,48)}
    printf,lun3,' #'
    printf,lun3,' # STATION ID           : '+logi(pos).station_id
    printf,lun3,' # STATION NAME         : '+logi(pos).station_name
    printf,lun3,' # SENSOR ID            : '+logi(pos).sensor_id
    printf,lun3,' # SENSOR TYPE          : '+logi(pos).sensor_type
    printf,lun3,' # SENSOR SERIAL NO     : '+logi(pos).serial_no
    printf,lun3,' #'
    printf,lun3,' # CALCULATION INFO
    for i=0, 47 do begin
        printf, lun3, ' # '+datfilestatus(i)
    endfor

    ;ASCII-FILE

    printf, lun2,$
    +'time'$
    +','+'start_doydat'$
    +','+'end_doydat'$
    +','+'no_of_records'$
    +','+'eta'$
    +','+'theta'$
    +','+'rot1'$$
    +','+'rot2'$$
    +','+'rot3'$$
    +','+'m_vec'$ $
    +','+'m_cup'$ $
    +','+'M_uvw'$ $
    +','+'mean_u'$ $
    +','+'mean_v' $
    +','+'mean_w' $
    +','+'mean_m' $
    +','+'mean_T' $
    +','+'mean_t1' $
    +','+'mean_t2' $
    +','+'mean_t3' $
    +','+'std_dev_u'$ $
    +','+'std_dev_v' $
    +','+'std_dev_w' $
    +','+'std_dev_m' $
    +','+'std_dev_T' $
    +','+'std_dev_t1' $
    +','+'std_dev_t2' $
    +','+'std_dev_t3' $
    +','+'skewness_u'$ $
    +','+'skewness_v' $
    +','+'skewness_w' $
    +','+'skewness_m' $
    +','+'skewness_T' $
    +','+'skewness_t1' $
    +','+'skewness_t2' $
    +','+'skewness_t3' $
    +','+'kurtosis_u'$ $
    +','+'kurtosis_v' $
    +','+'kurtosis_w' $
    +','+'kurtosis_m' $
    +','+'kurtosis_T' $
    +','+'kurtosis_t1' $
    +','+'kurtosis_t2' $
    +','+'kurtosis_t3' $
    +','+'uv' $
    +','+'uw' $
    +','+'um' $
    +','+'uT' $
    +','+'vw' $
    +','+'vm' $
    +','+'vT' $
    +','+'wm' $
    +','+'wT' $
    +','+'mT'

    for i=0, 47 do begin
    printf, lun2,$
    +zeiten(i,1)$
    +','+(string(final.time(0,i)))$
    +','+(string(final.time(1,i)))$
    +','+(string(final.ens(i)))$
    +','+(string(final.eta(i)))$
    +','+(string(final.theta(i)))$
    +','+(string(final.rotang(0,i)))$
    +','+(string(final.rotang(1,i)))$
    +','+(string(final.rotang(2,i)))$
    +','+(string(final.m_vec(i))) $
    +','+(string(final.m_cup(i))) $
    +','+(string(final.m_uvw(i))) $
    +','+string(final.mittel(0,i)) $
    +','+string(final.mittel(1,i)) $
    +','+string(final.mittel(2,i)) $
    +','+string(final.mittel(3,i)) $
    +','+string(final.mittel(4,i)) $
    +','+string(final.mittel(5,i)) $
    +','+string(final.mittel(6,i)) $
    +','+string(final.mittel(7,i)) $
    +','+string(final.std_dev(0,i)) $
    +','+string(final.std_dev(1,i)) $
    +','+string(final.std_dev(2,i)) $
    +','+string(final.std_dev(3,i)) $
    +','+string(final.std_dev(4,i)) $
    +','+string(final.std_dev(5,i)) $
    +','+string(final.std_dev(6,i)) $
    +','+string(final.std_dev(7,i)) $
    +','+string(final.skewness(0,i)) $
    +','+string(final.skewness(1,i)) $
    +','+string(final.skewness(2,i)) $
    +','+string(final.skewness(3,i)) $
    +','+string(final.skewness(4,i)) $
    +','+string(final.skewness(5,i)) $
    +','+string(final.skewness(6,i)) $
    +','+string(final.skewness(7,i)) $
    +','+string(final.kurtosis(0,i)) $
    +','+string(final.kurtosis(1,i)) $
    +','+string(final.kurtosis(2,i)) $
    +','+string(final.kurtosis(3,i)) $
    +','+string(final.kurtosis(4,i)) $
    +','+string(final.kurtosis(5,i)) $
    +','+string(final.kurtosis(6,i)) $
    +','+string(final.kurtosis(7,i)) $
    +','+string(final.cov_mat(0,1,i)) $
    +','+string(final.cov_mat(0,2,i)) $
    +','+string(final.cov_mat(0,3,i)) $
    +','+string(final.cov_mat(0,4,i)) $
    +','+string(final.cov_mat(1,2,i)) $
    +','+string(final.cov_mat(1,3,i)) $
    +','+string(final.cov_mat(1,4,i)) $
    +','+string(final.cov_mat(2,3,i)) $
    +','+string(final.cov_mat(2,4,i) )$
    +','+string(final.cov_mat(3,4,i))
    endfor

    ;BINARY STRUCTURE FILE

    writeu,lun1,final

    close, /all
    free_lun, lun1
    free_lun, lun2
    free_lun, lun3

end