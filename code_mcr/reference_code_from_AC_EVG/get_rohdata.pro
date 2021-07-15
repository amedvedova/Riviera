pro get_rohdata,filename,info,kal, $;input
				uvwt				;output

kallw='m:'
kalpfad='\data\mapcal\sonics\'
kelvin2celsius=-273.15
tc=0b
uvwt=0b

;daten einlesen
CASE strupcase(strtrim(info.sensor_type)) OF
	strupcase(strtrim('Gill R2')) 			:begin
												tc		= gillr2_in_tc(filename,anz_ai=info.ai(0))
												if info.ai(0) gt 0 and n_elements(tc) gt 1 then q=KH2Oscal(tc(6,*),'1199C')
											 end
	strupcase(strtrim('Gill HS')) 			:begin
												uvwt	= gillhs_in_array(filename,info.ai(0))
												if info.ai(0) gt 0 then q=KH2Oscal(uvwt(4,*),'1094C')
											 end
	strupcase(strtrim('Campbell CSAT 3 '))  :uvwt	= csat_in_array(filename)
	strupcase(strtrim('Metek USA-1')) 		:uvwt	= metek_in_array(filename)
ELSE: print, 'error in <get_rohdata.pro>: unknown sensor type'+filename
ENDCASE

if n_elements(uvwt) gt 1 OR n_elements(tc) gt 1 then begin

;kalfilename
cson=make_cson(info.serial_no,info.sensor_type)
if cson ne '' then begin

;kalibrieren
if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Gill R2')) AND info.no_of_errors eq 0 AND info.anz_records gt 36000. then begin
 case kal(0) of
 	1: begin
		getsoncal_map,cson,kallw,kalpfad,tpfak=tpfak,/no_eff_path
		if kal(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,tpfak=tpfak
		if kal(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,tpfak=tpfak,/vito
		uvwt=r2cal(tc,cson,tpfak,/pure)
		print, 'message from <get_rohdata.pro>: PURE calibration mode: ',cson
	   end
	2: begin
		gill=1
		getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak,/no_eff_path
		if kal(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak
	    if kal(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak,/vito
		uvwt=r2cal(tc,cson,tpfak,gill=gill)
		print, 'message from <get_rohdata.pro>: GILL calibration for sonic '+cson
	   end
	3: begin
		mat99=1
		getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path
		if kal(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak
		if kal(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/vito
		uvwt=r2cal(tc,cson,tpfak,mat99=mat99)
		print, 'message from <get_rohdata.pro>: mat99 calibration for sonic '+cson
	   end
 else:print,'error message from <get_rohdata.pro>: unknown calibration mode'
 endcase
 if info.ai(0) gt 0 then  uvwt=[uvwt(0,*),uvwt(1,*),uvwt(2,*),uvwt(6,*),q(0,*)] else uvwt=[uvwt(0,*),uvwt(1,*),uvwt(2,*),uvwt(6,*)]
endif else if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Gill R2')) then uvwt=-3131.

if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Gill HS')) AND info.no_of_errors eq 0 AND info.anz_records gt 35000. then begin
  if kal(0) eq 3 then begin
  	uvwt(0,*)= -uvwt(0,*)	;in sonstcal definiert u=-u
  	mat99	= 1
  	getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path,/extra
	uvwt	=sonstcal(uvwt(0:3,*),mat99=mat99)
	if info.ai(0) gt 0 then uvwt=[uvwt(0:3,*),q(0,*)]
	uvwt(0,*)= -uvwt(0,*) ;back to righthanded
	print, 'message from <get_rohdata.pro>: mat99 calibration for sonic '+cson
  endif else print, 'message from <get_rohdata.pro>: pure calibration mode: ',cson
endif else if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Gill HS')) then uvwt=-3131.

if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Campbell CSAT 3 ')) AND info.no_of_errors eq 0 AND info.anz_records gt 35000. then begin
  if kal(0) eq 3 then begin
  	uvwt(1,*)= -uvwt(1,*)	;in sonstcal definiert v=-v
  	mat99	= 1
  	getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path
	uvwt	=sonstcal(uvwt(0:3,*),mat99=mat99)
	uvwt(1,*)= -uvwt(1,*) ;back to righthanded
	print, 'message from <get_rohdata.pro>: mat99 calibration for sonic '+cson
  endif else print, 'message from <get_rohdata.pro>: pure calibration mode: ',cson
endif else if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Campbell CSAT 3 ')) then uvwt=-3131.

if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('METEK USA-1 ')) AND info.no_of_errors eq 0 AND info.anz_records gt 17000. then begin
  if kal(0) eq 3 then begin
  	uvwt(0,*)= -uvwt(0,*)	;in sonstcal definiert u=-u
  	uvwt(1,*)= -uvwt(1,*)	;in sonstcal definiert v=-v
  	mat99	= 1
  	getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/no_eff_path
	uvwt	=sonstcal(uvwt(0:3,*),mat99=mat99)
	uvwt(0,*)= -uvwt(0,*)	;back to lefthanded
	uvwt(1,*)= -uvwt(1,*) 	;back to lefthanded
	print, 'message from <get_rohdata.pro>: mat99 calibration for sonic '+cson
  endif else print, 'message from <get_rohdata.pro>: pure calibration mode: ',cson
endif else if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('METEK USA-1 ')) then uvwt=-3131.

endif else begin;cson -1
	print,"error message from <get_rohdata.pro>: can't calibrate, no value for cson"
	stop
endelse
endif;n_elements


end
