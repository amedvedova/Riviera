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
ELSE: print, 'error in <get_rohdata.pro>: unknown sensor type'+filename
ENDCASE

if n_elements(uvwt) gt 1 OR n_elements(tc) gt 1 then begin

;kalfilename
cson=make_cson(info.serial_no,info.sensor_type)
if cson ne '' then begin


;kalibrieren
if strupcase(strtrim(info.sensor_type)) eq strupcase(strtrim('Gill R2')) AND info.no_of_errors eq 0 AND info.anz_records gt 36000. then begin
 case kal(0) of
	2: begin
		gill=1
		if kal(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak
	    if kal(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,gill=gill,tpfak=tpfak,/vito
		uvwt=r2cal(tc,cson,tpfak,gill=gill)
		print, 'message from <get_rohdata.pro>: GILL calibration for sonic '+cson
	   end
	3: begin
		mat99=1
		if kal(1) eq 1 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak
		if kal(1) eq 2 then getsoncal_map,cson,kallw,kalpfad,mat99=mat99,tpfak=tpfak,/vito
		uvwt=r2cal(tc,cson,tpfak,mat99=mat99)
		print, 'message from <get_rohdata.pro>: mat99 calibration for sonic '+cson
	   end
 else:print,'error message from <get_rohdata.pro>: unknown calibration mode'
 endcase
 if info.ai(0) gt 0 then  uvwt=[uvwt(0,*),uvwt(1,*),uvwt(2,*),uvwt(6,*),q(0,*)] else uvwt=[uvwt(0,*),uvwt(1,*),uvwt(2,*),uvwt(6,*)]
endif



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


endif;n_elements


end
