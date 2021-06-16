;+
; NAME:
;       so_nix_stat
;
; PURPOSE:
;       is supposed to make the major calculations for sonic anemometry and a test of independence as well as a
;		test on trends according to Bendat and Piersol
;		(more or less the same as so_nix, only more options and only here stationary tests...)
;
; CATEGORY:
;       sonic
;
; INPUTS:
;		station_id		e.g.: 'RO', 'AG','MN','V1','V2' where V2 and V2 stand for comparison measurements
;		doy				doy which should be processed
;		options         : kalibrierung/behandlung als array der form [a,b,c,d]
;
;       	              a ist METHODE
;           	          1= pure (mit standardpfadlängen bei r2)
;               	      2= gillr2 herstellerklaibrierung (nur r2)
;                   	  3= matrixkalibrerierung windkanal
;
;   	                  b ist behandlung der PFADLÄNGEN (nur wirksam bei x=1, 2 und 3 und ausschliesslich bei r2's)
;       	              0= keine effektiven pfadlängen, standard 0.149 (default)
;           	          1= windkanal99 pfadlängen
;               	      2= sanvittore pfadlängen
;
;   	                  c ist behandlung des detrendings
;       	              0= kein detrending
;           	          1= lineares detrending
;						  2= boxcar average
;           	          3= fourier mittelwertentfernung
;
;   	                  d ist rotation/ausrichtung
;       	              0= keine drehung (original geräteintern)
;           	          1= u+ nach ggN, v+ nach ggE w senkrecht (GEOGRAPHIC)
;               	      2= u+ longitudinal, v+ lateral rechtsdrehend (STREAMLINE)
;           	          3= u+ slope upwards v+ in contour line w slope parallel
;
; KEYWORDS		   :	,/raus if data should be written to file
;
; CALLING SEQUENCE:
;		e.g. so_nix,'AG',237,[[1,0,0,0],[1,0,0,0]],/raus

pro so_nix_stat,station_id,doy,options,raus=raus

doy		= string(doy, format='(i3.3)')
datadir	= 'm:\data\'+strcompress(strlowcase(station_id),/remove_all)+'\rohdaten\fast\'
outdir	= 'm:\data\'+strcompress(strlowcase(station_id),/remove_all)+'\tagesfiles\'

if station_id eq 'MN' then sensor_id = 'N1' else sensor_id = 'N2'
get_was,station_id,sensor_id,doy,was,loginfo; get data pathes, valuable files and an example of a log file
anz_sen = size(loginfo) & anz_sen = anz_sen(1)

case strcompress(strupcase(station_id)) of
	'MN': samp_rate=[1/20.,1/20.,1/20.83,1/20.83,1/20.83,1/20.]
	'RO': samp_rate=[1/20.83,1/10.]
	'AG': samp_rate=[1/20.83,1/20.83]
else:print,'error message from <so_nix_stat.pro>: no such station'
endcase

z=halbstundenstring()

for sen=0,anz_sen-1 do begin

	tag = doy
	sensor_id	= STRMID(loginfo(sen).datafilename, 3, 2)
	level		= where(loginfo.sensor_id eq sensor_id) & level=level(0)
	if station_id eq 'MN' then level=sen ;com N1/N3 vertauscht
	anz_ai		= loginfo.ai(0)
	get_outputstructure,anz_ai(sen),final
	testarr		= intarr(4+anz_ai(sen),2,48)-9999

	 for i=-1, 46 do begin ;tagesschlaufe

		anz_rec_read=lonarr(3)+0 & data=0.d & uvwt=0.d

		for h=0,2 do begin ;3 files einlesen (moving average, fourier...)

			case 1 of
   			i+h eq -1:begin
   						zi=z(47,0) & tag=tag - 1 & dir	= datadir+string(tag, format='(i3.3)')+'\'
   						datafile= dir+strcompress(strupcase(station_id))+'_'+strcompress(strupcase(sensor_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						logfile = dir+strcompress(strupcase(station_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						exist=findfile(datafile) &	print,exist(0) & tag=tag+1
   					  end
   			i+h eq 48:begin
   						zi=z(0,0) & tag=tag + 1  & dir	= datadir+string(tag, format='(i3.3)')+'\'
   						datafile= dir+strcompress(strupcase(station_id))+'_'+strcompress(strupcase(sensor_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						logfile = dir+strcompress(strupcase(station_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						exist=findfile(datafile) &	print,exist(0)
   					end
   			i+h gt 48:begin
   						zi=z(0,0)  & dir	= datadir+string(tag, format='(i3.3)')+'\'
   						datafile= dir+strcompress(strupcase(station_id))+'_'+strcompress(strupcase(sensor_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						logfile = dir+strcompress(strupcase(station_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						exist=findfile(datafile) &	print,exist(0)
   					end
   			ELSE: begin
   						zi=z(i+h,0)  & dir	= datadir+string(tag, format='(i3.3)')+'\'
   						datafile= dir+strcompress(strupcase(station_id))+'_'+strcompress(strupcase(sensor_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						logfile = dir+strcompress(strupcase(station_id))+'_1999_'+string(tag, format='(i3.3)')+'_'+zi+'.raw'
						exist=findfile(datafile) &	print,exist(0)
   				  end
			ENDCASE

			;-------------------------------------------------
			; einlesen und kalibrieren/rotieren wenn gewünscht
			;-------------------------------------------------
			if exist(0) ne '' then begin

			 openr, lun,exist(0), /get_lun & fileinfo=fstat(lun) & free_lun,lun

			 if fileinfo.size gt 0. then begin

				;read rawdata and calibrates them (if wanted)
				get_rohdata,exist(0),loginfo(level),options(0:1,level),data  & nrec=n_elements(data(0,*))  & data=double(data)  ;& int=float(nrec)/n

				if data(0) ne -1 and nrec gt 10000. then begin

					;geographic north
					if options(3,level) ne 0 then begin
						;turn to sonic north and transform to righthanded coordinate system (if necessary)
						data(0:2,*)= get_uvwtSnorth(data(0:2,*),loginfo(level))
						;turn coordinate system such that u+ points slope upwards and v+ lies in the contour line
						if options(3,level) ge 3 then begin
							if options(3) eq 4 then begin
				  				;uv-ebene streamline
				  				REYNOLDS_AC, data, ens,eta, theta,/streamline			;ac schickt die rotierten daten zurück
				  				data(0:2,*)= koordinatenrotation(data(0:2,*),0.,0.,-eta)
				  			endif
							;turn coordinate system such that u+ points slope upwards and v+ lies in the contour line
							data(0:2,*)= koordinatenrotation(data(0:2,*),0.,0.,360.-((((loginfo(level).exposition-180)-loginfo(level).azimuth)+360) mod 360))
						endif else begin
							;turn coordinate system such that u+ points slope downwards and v+ lies in the contour line
							data(0:2,*)= koordinatenrotation(data(0:2,*),0.,0.,360.-((loginfo(level).exposition-loginfo(level).azimuth+360.) mod 360))
							;turn the uv-plane around v-axis into horizontal position
							data(0:2,*)=koordinatenrotation(data(0:2,*),0.,-loginfo(level).inklination,0.)
							;turn the coordinate system with u+ towards geographic north
							data(0:2,*)=koordinatenrotation(data(0:2,*),0.,0.,loginfo(level).exposition)
						endelse
					endif

					if h eq 0 then uvwt = data
					if h gt 0 AND  total(anz_rec_read) lt 1 then uvwt = data
					if h gt 0 AND  total(anz_rec_read) gt 1 then uvwt = [[uvwt],[data]]
					info = size(data)
					anz_rec_read(h)=info(2)

				endif;gt -1

			 endif;im file steht nix drin

			endif;if exist and gt -1

		endfor;h=0,2 3halbstundenruns

		IF anz_rec_read(1) gt ((1./samp_rate(level))*60*30.)-100. THEN BEGIN
			data= double(reform(uvwt(0:3+anz_ai(level),*))) & uvwt=0.
			a	= anz_rec_read(0) & b = anz_rec_read(0)+anz_rec_read(1)-1
			u	= 0  &   v	= 1 &    w	= 2
			alle_da=where(anz_rec_read gt 0)


			;calculate mean flow charcteristics according a.p.
			;-------------------------------------------------

			uvwt = data(*,a:b)
			info = SIZE(uvwt)  &   n_col = info(1)  &   n_row = info(2)
			raw_meandata	 = uvwt # REPLICATE(1.0 / n_row, n_row) & meandata=raw_meandata
			u_meandata	= meandata(u)
			v_meandata	= meandata(v)
			w_meandata	= meandata(w)

			uv_len	= SQRT(u_meandata ^ 2 + v_meandata ^ 2)
			uvw_len	= SQRT(u_meandata ^ 2 + v_meandata ^ 2 + w_meandata ^ 2)

			uv_cup_speed =  total(SQRT(data(0,a:b) ^ 2 + data(1,a:b) ^ 2))/float(n_row)
			uvw_cup_speed = total(SQRT(data(0,a:b) ^ 2 + data(1,a:b) ^ 2 + data(2,a:b) ^ 2))/float(n_row)

			IF v_meandata GE 0 THEN BEGIN

				IF u_meandata NE 0 THEN BEGIN
	   				eta = ATAN(v_meandata, u_meandata) * !RADEG
	   			ENDIF ELSE BEGIN
	   				eta = 90.0
				ENDELSE

			ENDIF ELSE BEGIN

				IF u_meandata NE 0 THEN BEGIN
	        		eta = ATAN(v_meandata, u_meandata) * !RADEG + 360
				ENDIF ELSE BEGIN
	        		eta = 270.0
	    		ENDELSE

			ENDELSE

			IF uv_len NE 0 THEN BEGIN
				theta = ATAN(w_meandata / uv_len) * !RADEG
			ENDIF ELSE theta = 90.0

			;----------
			;detrending
			;----------
			info = SIZE(data)  &   n_col = info(1)  &   n_row = info(2)

			case options(2,level) of
				0: print,'message of <det_stationarity.pro>: no detrending'
				1: begin
					IF anz_rec_read(1) gt ((1./samp_rate(level))*60*30.)-100. THEN BEGIN
						x = FINDGEN(anz_rec_read(1))
						FOR j = 0, n_col - 1 DO BEGIN
							coeff = POLY_FIT(x, data(j, a:b), 1)
							data(j, a:b) =  data(j, a:b) - POLY(x, coeff)
						ENDFOR
						print,'message of <det_stationarity.pro>: detrending linearly'
					ENDIF else print,'message of <det_stationarity.pro>: not enough data for detrending linearly'
				   end
				2: begin
					IF n_elements(alle_da) eq 3 THEN BEGIN
						FOR j = 0, n_col - 1 DO BEGIN
							t=reform(data(j,*))
							t =  t-smooth(t,(1/samp_rate(level))*60.*30.)
							data(j, a:b) = t(a:b)
							t=0.
						ENDFOR
						print,'message of <det_stationarity.pro>: detrending with boxcar average'
					ENDIF ELSE data(*,a:b)=-9999.
				   end
				3: begin
					IF b ge ((2./samp_rate(level))*60*30.) THEN BEGIN
						FOR j = 0, n_col - 1 DO BEGIN
							n=n_elements(data(0,0:((2./samp_rate(level))*30.*60.)-1))
							frequency 	= (lindgen(n/2)+1)/(n*samp_rate(level))
							cutoff		= 1.d/((1./samp_rate(level))*30.*60.*samp_rate(level))
							trans		= fft(data(j,0:(2.*(1d/samp_rate(level))*30.*60.)),-1)
							ix			= where(frequency lt cutoff,cnt)
							trans(0:max(ix))=0. & trans(n_elements(trans)-(max(ix)+1):*)=0.
							filtered	= double(FFT(trans , 1))
							data(j, a:b)= filtered(0,n_elements(filtered)-anz_rec_read(1):n_elements(filtered)-1)
						ENDFOR
						print,'message of <det_stationarity.pro>: detrending in frequency space'
					ENDIF ELSE data(*,a:b)=-9999.
				   end
			else:print,'error message of <det_stationarity.pro>: this detrending method is not implemented'
   			endcase

   			;------------------
			;stationarity tests
			;------------------

			if anz_rec_read(1) gt 0 and mean(data(a:b)) gt -9000. then begin
				;------------------------------------------------
				;run test(trend?) nach bendat/piersol,p94 ff
				;for 5% significance level of 0.05 values have to
				;be in the range [3<val<8]
				;------------------------------------------------
				runtest = intarr(n_col)
				runs	= 10
				anz_val=floor(anz_rec_read(1)/10)
				for c=0,n_col-1 do begin
					temp	= mean(data(c,a+0:a+anz_val-1))
					for j=1,8 do temp=[[temp],[mean(data(c,a+j*anz_val:a+(j+1)*anz_val-1))]]
					temp=reform(temp(0,*)) & temp	= [[temp],mean(data(c,a+(8+1)*anz_val:b))]
					med		= median(temp,/even)
					plus	= where(temp gt med,cntp)
						for j=0,cntp-2 do if plus(j)+1 eq plus(j+1) then runs=runs-1
					minu	= where(temp lt med,cntm)
						for j=0,cntm-2 do if minu(j)+1 eq minu(j+1) then runs=runs-1
					testarr(c,0,i+1)=runs
					runs=10
				endfor
				;-----------------------------------------------------------
				;reverse arrangement test(trend?) nach bendat/piersol,p97 ff
				;for 5% significance level of 0.05 values have to
				;be in the range [11<val<33]
				;-----------------------------------------------------------
				rev_arr = intarr(n_col)
				r_a		= intarr(9)
				for c=0,n_col-1 do begin
					temp	= mean(data(c,a+0:a+anz_val-1))
					for j=1,8 do temp=[[temp],[mean(data(c,a+j*anz_val:a+(j+1)*anz_val-1))]]
					temp=reform(temp(0,*)) & temp	= [[temp],mean(data(c,a+(8+1)*anz_val:b))]
					for j=0,n_elements(temp)-2 do begin
						ix=where(temp(j) gt temp(j+1:*),cnt)
						r_a(j)=cnt
					endfor
					testarr(c,1,i+1)=total(r_a)
				endfor


				;-----------------------------------------------------
				;half-hourly statistics (according to a.p.'s reynolds)
				;-----------------------------------------------------
				data=data(*,a:b) & info = SIZE(data)  &   n_col = info(1)  &   n_row = info(2)
				;streamline
				IF options(3,level) EQ 2 AND mean(data) gt -9000. THEN BEGIN
				;first rotation
					COSeta	= u_meandata / uv_len
					SINeta	= v_meandata / uv_len


					D1 =  [[   COSeta,  SINeta,     0    ], $
				    	   [  -SINeta,  COSeta,     0    ], $
						   [    0    ,    0   ,     1    ]]


				    ;second rotation
					COStheta = uv_len / uvw_len
					SINtheta = w_meandata / uvw_len

					D2 =   [[ COStheta,    0   ,  SINtheta], $
							[    0    ,    1   ,     0    ], $
							[-SINtheta,    0   ,  COStheta]]

					;combine rotation matrices
					rot_mat = D2 ## D1

					;rotate whole wind vector data array
					data(0:2,*) = data(0:2,*) ## TRANSPOSE(rot_mat)

				ENDIF

				IF options(3,level) EQ 2 OR options(2,level) GT 0 THEN $
					meandata	 = data # REPLICATE(1.0 / anz_rec_read(1),  anz_rec_read(1))

				;calculate veariance, std, skewness, kurtosis
				resid		= data - meandata # REPLICATE(1.0, anz_rec_read(1))
				variance	= resid ^ 2 # REPLICATE(1.0 / anz_rec_read(1), anz_rec_read(1))
				std_dev 	= SQRT(ABS(variance))
				skewness 	= resid ^ 3 # REPLICATE(1.0, anz_rec_read(1))
				skewness 	= skewness / (anz_rec_read(1) * std_dev ^ 3)
				kurtosis 	= resid ^ 4 # REPLICATE(1.0, anz_rec_read(1))
				kurtosis 	= kurtosis / (anz_rec_read(1) * std_dev ^ 4) - 3.0
				cov_mat 	= (resid # TRANSPOSE(resid)) / (anz_rec_read(1))

				sollzeit				=	halbstundenstring() & sollzeit = reform(sollzeit(*,0)) /100.
				final(i+1).time 		= float(doy+daytime(sollzeit(i+1)))
				final(i+1).ens			= anz_rec_read(1)
				if options(3,level)  eq 1 then begin
					final(i+1).eta		= loginfo(level).azimuth
					final(i+1).theta	= loginfo(level).inklination
				endif else begin
					final(i+1).eta		= double(eta)
					final(i+1).theta	= double(theta)
				endelse
				final(i+1).M_vec		= double(uv_len)
				final(i+1).M_cup		= double(uv_cup_speed)
				final(i+1).M_uvw		= double(uvw_cup_speed)
				if options(2,level) lt 3 then $
				final(i+1).mittel		= double(raw_meandata) $
				else $
				final(i+1).mittel		= double(meandata)
				final(i+1).std_dev		= double(std_dev)
				final(i+1).skewness		= double(skewness)
				final(i+1).kurtosis		= double(kurtosis)
				final(i+1).cov_mat		= double(cov_mat)

			endif;stationary test and mean values

		ENDIF ;anz_rec_read gt 1

	endfor;-1,46

	if keyword_set(raus)then write_sonixdata_to_u,final,strtrim(string(doy),1),loginfo(level),options(*,level),testarr

 endfor;sensor

end