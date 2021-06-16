;+
; NAME:
;	raw_reader.pro
;
; PURPOSE:
;	liest eines oder mehrere sonic-rohdatenfiles (z.b. halbstundenfiles eines sensors) der labview-erfassung
;   hintereinander ein. unterst�tzte typen sind: Gill-HS Raw-files, Gill-R2 Raw-files, CSAT3 Raw-
;   files, METEK-Raw-ASCII-files. die daten werden zu uvwt umgerechnet. das programm raw_reader.pro
;   h�ngt die eingelesenen rohdaten zu einem einzigen fortlaufeden array zusammen. "das liebe programm
;   findet sogar selbst heraus, wieviel  analog-inputs angeh�ngt sind" (text entnommen aus: microsoft-
;   sonicreader-wizard-online-helper-toolbar-assistent...). die reihenfolge des einlesens wird
;   durch die reihenfolge der files im filepfad-array festgelegt. datenfiles, die nicht gefunden wurden,
;   werden nicht eingelesen (dadurch etsteht ggf. eine l�cke!). zur�ck kommt ein daten-array im format
;   u,v,w,t,(a1,a2,a3,...)
;
; CALLING SEQUENCE:
;   uvwt=raw_reader(filepatharray)
;
; INPUTS:
;	filepath    : filepatharray mit den vollen pfaden aller datenfiles, in derjenigen reihenfolge,
;                 in der sie aneinandergeh�ngt werden sollen. wenn der array nicht geliefert wird,
;                 erscheint eine dialogbox (dialog-pickfile)
;   kalmet      : kalibrierung als array der form [x,y]
;
;                 x ist METHODE
;                 0= nur transit-counts (default) bzw. originaldaten (alle)
;                 1= pure (mit standardpfadl�ngen r2)
;                 2= gillr2 herstellerklaibrierung
;                 3= matrixkalibrerierung windkanal
;
;                 y ist behandlung der PFADL�NGEN (nur wirksam bei x=1, 2 und 3 und ausschliesslich bei r2's)
;                 0= keine effektiven pfadl�ngen, standard 0.149 (default)
;                 1= windkanal99 pfadl�ngen
;                 2= sanvittore pfadl�ngen
;;
; OUTPUT:
;	array(x,r) mit x: u,v,w,t,(a1),(a2),...
;              und r: anzahl records
;
; REVISION HISTORY:
;   17.02.00 AC Bei R2 m�glichkeit nur transit counts (default) oder diverse kalibrierungen
;   29.02.00 AC Matrix-Kalibrierung nun auch m�glich.
;   03.03.00 AC gill hs angepasst an neues einlese programm, fehler korrigiert bei matrix.
;   24.03.00 AC gill hs matrix wird IMMER mit keyword extra ausgelesen. d.h. annahme dass daten schon gill-
;               kalibriert sind.


function raw_reader, filepfadarray, kalmet=kalmet, silent=silent

	kallw='m:'
	kalpfad='\data\mapcal\sonics\'
	kelvin2celsius=-273.15
	rrdata=0

	if keyword_set(kalmet) then begin
		kalmet=fix(kalmet)
	endif else begin
		kalmet=[0,0]
	endelse



	anz_files=n_elements(filepfadarray)

	for a=0, anz_files-1 do begin

		;allgemeine umschlaufung (aufgr. sensorvertauschung mn 231-264)
		;(1) datafilenamen -> logfilenamen.
		;(2) sensor id aus datafilenamen raussuchen -> sensor id im logfile suchen -> datenfile aus logfile lesen.
		;(3) alten datenfilenamen �berschreiben.

		;(1) daten aus filepfad lesen
		nami=data_from_filename(filepfadarray(a)) ;filenamen auseinandernehmen
		sensorid=nami.sensor_id

		;(1) logfile suchen (muss im selben directory sein)
		len=strlen(filepfadarray(a))
		logfilepfad=strmid(filepfadarray(a),0, len-22)+strmid(filepfadarray(a),len-19,15)+'.log' ;so muss das logfile heissen...

		;(2) daten aus logfile lesen
   		existenz=findfile(logfilepfad)
   		existenzsize=size(existenz)
   		if existenzsize(0) eq 0 then begin
   			print, 'error in <raw_reader.pro>: Konnte Log-File '+logfilepfad+' nicht finden. Daten aus '+filepfadarray(a)+' werden deshalb nicht eingelesen!'
		endif else begin
			logi=read_soniclogfile(logfilepfad)
			sensor=where(logi.sensor_id EQ sensorid)


		;datenfilenamen aus logfile lesen
		dir=strmid(filepfadarray(a),0, len-25)
		filepfadarray(a)=dir+logi(sensor).datafilename

		;daten aus [neuem, im normqlfall selben] daten filepfad lesen
		nami=data_from_filename(filepfadarray(a)) ;filenamen auseinandernehmen
		sensorid=nami.sensor_id

			anz_ai=total(logi(sensor).ai);anz_analog_inputs f�r diesen sensor
			sensor_type=(logi(sensor).sensor_type)
			serno=      (logi(sensor).serial_no)
			no_of_errors=(logi(sensor).no_of_errors)

			if no_of_errors eq 0 then begin

				;einlesen
				case logi(sensor).sensor_type of
					'Gill R2         ':  eingelesen=gillr2_in_tc(filepfadarray(a),anz_ai=anz_ai)
					'Gill HS         ':  eingelesen=gillhs_in_array(filepfadarray(a),anz_ai)
				endcase

				;sensornummer
				cson=make_cson(logi(sensor).serial_no,logi(sensor).sensor_type)

				;kalibrieren r2-------------------------------------------------------------------------------

				if logi(sensor).sensor_type eq 'Gill R2         ' and eingelesen(0) ne -1 then begin

					raw=eingelesen
					case kalmet(0) of
						2: begin
							gill=1
							getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak,/no_eff_path
							if kalmet(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak
							if kalmet(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak,/vito
							eingelesen=r2cal(raw,cson,tpfak,gill=gill)
							print, 'message from <raw_reader.pro>: Kalibriere mit Methode: GILL HERSTELLER'
						   end
						3: begin ;matrix
							mat99=1
							getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path
							if kalmet(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak
							if kalmet(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/vito
							eingelesen=r2cal(raw,cson,tpfak,mat99=mat99)
							print, 'message from <raw_reader.pro>: Kalibriere mit Methode: MATRIX WINDKANAL 99'
						   end
						else: begin
							eingelesen=raw
							print, 'message from <raw_reader.pro>: Kalibriere Daten nicht: TRANSIT COUNTS'
						   end
					endcase

					if kalmet(0) ne 0 then begin ;reihenfolge so dass u,v,w,tm,t1,t2,t3, bisher: u,v,w,t1,t2,t3,tm
						mitteltemp=eingelesen(6,*)
						eingelesen(4:6,*)=eingelesen(3:5,*)
						eingelesen(3,*)=mitteltemp
						eingelesen(3:6,*)=eingelesen(3:6,*)+kelvin2celsius
						mitteltemp=0
					endif
				endif


				;kalibrieren nicht r2-sonics (csat, hs, metek) -------------------------------------------------
					endif else begin ;alle andern f�lle
						if eingelesen(0) ne -1 then begin
							case kalmet(0) of
							3: begin ;3= matrixkalibrierung windkanal
								mat99=1
								raw=eingelesen
								;so drehen, dass u+ sonic S und v+ sonic W (f�r Matrix)
								case logi(sensor).sensor_type of
								'Campbell CSAT 3 ': raw(1,*)=-raw(1,*) ;v=-v
								'Gill HS         ': raw(0,*)=-raw(0,*) ;u=-u
								'METEK USA-1     ': begin
														raw(0,*)=-raw(0,*);u=-u
														raw(1,*)=-raw(1,*);v=-v
													end
								else: stop ;fehler, d�rfte nicht passieren
								endcase


								if logi(sensor).sensor_type eq 'Gill HS         ' then $
									getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path, /extra else $
								getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path
								eingelesen=sonstcal(raw,mat99=mat99)
								print, 'message form <raw_reader.pro>: Kalibriere mit Methode: MATRIX WINDKANAL 99'

						   	;zur�ckdrehen
								case logi(sensor).sensor_type of
								'Campbell CSAT 3 ': eingelesen(1,*)=-eingelesen(1,*) ;v=-v
								'Gill HS         ': eingelesen(0,*)=-eingelesen(0,*) ;u=-u
								'METEK USA-1     ': begin
														eingelesen(0,*)=-eingelesen(0,*);u=-u
														eingelesen(1,*)=-eingelesen(1,*);v=-v
													end
								else: stop ;fehler, d�rfte nicht passieren
								endcase

						  	 end
							else: print, 'message form <raw_reader.pro>: Kalibrier Daten nicht: UVW'
							endcase
						endif else begin
							eingelesen=-1 ;fehler beim einlesen von r2s
						endelse
					endelse
				endelse


				if n_elements(rrdata) lt 4 then begin
					rrdata=eingelesen ;beim ersten file "rrdata" neu erstellen
				endif else begin
					rrdata=[[rrdata],[eingelesen]] ; alle weiteren files werden angeh�ngt
				endelse

			endif
		endfor

	return, rrdata

end
