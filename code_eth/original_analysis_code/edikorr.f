C=====================================================================
	program ediflux2000
C=====================================================================
C
C DESCRIPTION: ROUTINES TO WORK WITH SOLENT-BINARY-DATA-FILES
C              WHITCH WERE RECORDED DURING THE MAP-RIVIERA PROJECT.
C	       DATA ARE HANDLED CORRECTLY FOR ALL NUMBER OF ANALOG
C              INPUTS USED.
C              AN ASCII FILE WITH REFERENCE VALUES EXSISTS FOR EVERY
C              SONIC RAW DATA FILE.
C
C SUBROUTINES: dialog,location,makearray,filesexist,getdata,getarray,
C	       meanhum,conversions,period_average,krypton,lookupkry,
C              angles,turn_in_meteo,turn_in_stream,runtest,fluxes,
C	       outformat,outsimple,spectra,cospectra
C
C FUNCTIONS:   bar, pottemp, sfeich
C
C AUTHOR:      Andretta Marco
C VERSION:     0.0 (17.7.2000) version without spectra and cospectra.
C	       1.0 (18.8.2000) first fully working version, not tested.
C 	       2.0 (5.10.2000) v -> -v (Right hand coord. system) and
C			       new angels,turn_in_meteo,turn_in_stream.
C======================================================================
c23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	implicit none

	integer j
	integer irow,jrow
	integer fday,lday,nday,day
	integer fhour,lhour,tothour,nhour,hour,min
	integer filter,kry,analog,nord,nsonic,nkry
	integer av_period,av_intervalls,avint,turned
	integer arow,av_start,av_end
	integer runind(5)

	real height,lheight,rheight
	real mean(5),sigma(5),skew(5)	
	real marray(4500000,5), darray(4500000,5)
	real eps,teta,tetaout
	real tm,pm,rhm,rm,nm
        real t(100),p(100),rh(100),rain(100),net(100)
	real ustar,tstar,zoverl,qt,M,Hs,E
	real uiui(3),uiuj(3),uit(3),uiq(3),wusqr(3)

	character*1 loc
	character*16 out_file
	real freq
	freq =1000./48.
	! 75'000/hour, 1'800'000/day, 5'400'000/3days
C-----------------------------------------------------------------------
C Main   
C-----------------------------------------------------------------------
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	! Title
1	write(*,*)'--------------------------------------------------',
     &  '-----------------------'
	do j=1,4
	   write(*,*)
	enddo
	write(*,*)'             %%%%%%    %%%%%%    %%%%%%    %%%%%%'
 	write(*,*)'            %%    %%  %%    %%  %%    %%  %%    %%'
	write(*,*)'                %%    %%    %%  %%   EdiFlux    %%'
        write(*,*)'              %%      %%    %%  %%    %%  %%    %%'
	write(*,*)'            %%%%%%%%   %%%%%%    %%%%%%    %%%%%%'
	do j=1,4
	   write(*,*)
	enddo

	! User inputs
10	call dialog (loc,fday,fhour,lday,lhour,av_period,
     &  filter)
	call location (loc,lday,height,lheight,rheight,
     &                nord,nsonic,nkry,kry,analog)
	write(*,*)'---------------------------------------------------',
     &  '-----------------------'
	write(*,*)'Loc   Height   Sonic  Krypton  Analogin  Nord'
	write(*,'(2X,A,3X,F7.2,5X,I2,5X,I4,6X,I1,7X,I3)')loc,
     &  height,nsonic,nkry,analog,nord
	write(*,*)'---------------------------------------------------',
     &  '-----------------------'

	! Number of days and hours to compute
	nday = lday-fday
	nhour = lhour-fhour
	if(nhour.ge.24)then
	  nday = nday+1
	  nhour = nhour-24
	endif
	if(nhour.lt.0)then
	  nday = nday-1
	  nhour = nhour+24
	endif
	nhour = nhour+1
	tothour = 24*nday+nhour

	if(tothour.gt.36) then ! array(4500000,5)
	  write(*,*)'--------------------------------------------------'
	  write(*,*)'No more than 36 subsequent hours can be computed!'
	  goto 10
	endif

	! Name output files
        write(out_file,'(A1,I3.3,I2.2,A1,I2.2,A1,I2.2,A1,I3.3)')loc,
     &         fday,fhour,'n',tothour,'a',av_period,'f',filter

	open(20,
     &  file='../data_output/'//loc//'/'//out_file//
     &  '.frm',status='unknown',form='formatted')
	open(21,
     &  file='../data_output/'//loc//'/'//out_file//
     &  '.si1',status='unknown',form='formatted')
	open(22,
     &  file='../data_output/'//loc//'/'//out_file//
     &  '.si2',status='unknown',form='formatted')
	open(25,
     &  file='../data_output/'//loc//'/'//out_file//
     &  '.spc',status='unknown',form='formatted')
	open(26,
     &  file='../data_output/'//loc//'/'//out_file//
     &  '.csp',status='unknown',form='formatted')
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12


	! Read data,convert,apply recursive filter and store it
	jrow = 4 + kry            ! number of columns in arrays
	call makearray (out_file,loc,fday,lday,fhour,lhour,
     &                 rheight,filter,nkry,kry,nord,
     &                 jrow,irow,marray,darray,t,p,rh,rain,net)
	write(*,*)'---------------------------------------------------',
     &  '-----------------------'
	write(*,*)
C----------------------------------------------------------------------
C averaging intervals loop (2*30min,3*20,4*15,5*12,6*10...)
C----------------------------------------------------------------------
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	av_intervalls = (nday*24+nhour)*60/av_period
	arow = nint(float(av_period)*60.*1000./48.)

	do avint=1,av_intervalls
	  av_start = (avint-1)*arow+1
	  av_end   = av_start+arow-1

	  ! Check if file is finishing
	  if(av_start.ge.irow) goto 1000	
	  if(av_end.gt.irow) then
	    if((av_end-irow).le.6300) then !5min fehlen
		av_end = irow
	    else
	      goto 1000
	    endif
	  endif

	  ! Information on day,hour and min
	  write(*,'(1X,A12,A8,I4,A10,I7,A4,I7)')'-> Prepare ',
     &  'interval',avint,' from row ',av_start,' to ',av_end
	  day  = fday+(avint-1+fhour*60/av_period)/(24*60/av_period)
	  hour = fhour + (avint-1)/(60/av_period)
	  hour = mod(hour,24)
	  min  = av_period/2
	  min  = min+av_period*mod(avint-1,60/av_period)

	write(*,*) day,hour,min
	write(*,*) fday,(avint-1+fhour*60/av_period)/(24*60/av_period)
	write(*,*) avint,av_period,1+(avint-1)*av_period/60

	  tm = t(1+(avint-1)*av_period/60)
	  pm = p(1+(avint-1)*av_period/60)
	  rhm = rh(1+(avint-1)*av_period/60) 
	  rm = rain(1+(avint-1)*av_period/60) 
	  nm = net(1+(avint-1)*av_period/60) 

	  ! Means and angels and turn in mean wind direction
	  write(*,*)'-> Compute mean values and mean wind direction'
	  call period_average (av_start,av_end,jrow,marray,mean)
 	  call angles(mean,teta,eps)
	  tetaout = teta - 90.
	  ! wind direction is tetaout= teta -180 + 90
	  ! tetaout=angle(North=y-axis,where the wind comes from)
	  ! +90 because teta= angle(x-axis,wind vector)
	  ! -180 because wind dir(goes to)/wind vector(comes from)

	  do turned=1,2
	    if(turned.eq.2)then
	      write(*,*)'-> Turn in mean wind direction'
	      call turn_in_stream (av_start,av_end,marray,darray,
     &                         teta,eps)
	      call period_average (av_start,av_end,jrow,marray,mean)
	    endif


	    ! Apply runtest and compute turbulent fluxes
	    write(*,*)'-> Runtest'
	    call runtest (av_start,av_end,jrow,darray,runind)		
	    write(*,*)'-> Computing fluxes'
	    call fluxes (av_start,av_end,jrow,darray,mean,height,
     &                 tm,pm,sigma,skew,uiuj,uiui,uit,uiq,wusqr,
     &                 M,Hs,E,ustar,tstar,qt,zoverl)

	    ! Write output files, the second format easier to read-in
	    write(*,*)'-> Writing output files'
	    call outformat (turned,loc,day,hour,min,height,nsonic,
     &                   nkry,tetaout,eps,tm,pm,rhm,rm,nm,av_period,
     &	 	         filter,runind,mean,sigma,skew,lheight,
     &  	         uiuj,uiui,uit,uiq,wusqr,M,Hs,E,zoverl,
     &                   ustar,tstar,qt)
	    call outsimple (turned,loc,day,hour,min,
     &                   tetaout,eps,tm,pm,rhm,rm,nm,
     &	 	         runind,mean,sigma,skew,
     &  	         uiuj,uiui,uit,uiq,wusqr,M,Hs,E,zoverl,
     &                   ustar,tstar,qt)

	    ! Spectra and co-spectra
	    write(*,*)'-> Computing spectra'
	    call calcspectra (av_start,av_end,jrow,
     &                     darray,freq,mean,sigma,
     &                     loc,day,hour,min,height,pm,ustar,tstar,
     &                     zoverl,filter)
	    write(*,*)'-> Computing co-spectra'
	    call calccospectra (av_start,av_end,
     &                       darray,freq,mean,sigma,
     &                       loc,day,hour,min,height,pm,ustar,tstar,
     &                       zoverl,filter,uiuj,uit)
	enddo
900      enddo
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C----------------------------------------------------------------------
C end loop
C----------------------------------------------------------------------
1000	continue
	close(20)
	close(21)
	close(22)
	close(25)
	close(26)
	write(*,*)
	write(*,*)
	write(*,*)'				  E N D'
	write(*,*)'---------------------------------------------------',
     &  '-----------------------'
        end  



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC						
CCC  Subroutines
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C dialog
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine dialog (loc,fday,fhour,lday,lhour,
     &  av_period,filter)

	implicit none
	integer fday,fhour,lday,lhour,av_period  !output
	integer filter				 !outpuy
	character*1 loc 			 !outpuy

	! Local variables
	character*1 floc,lloc

	! Screen   
9	write(*,*) 'Type names of Edisol binary files you'
        write(*,*) 'want to read (capital,without .SLT !):'
	write(*,*)
        write(*,'(A55,$)')'First file ?   '
        read (*,'(A1,I3,I2,I2)') floc,fday,fhour
	write(*,'(A55,$)')'Last file ?   '
        read  (*,'(A1,I3,I2)') lloc,lday,lhour
19	write(*,'(A55,$)')'Averaging period (min) ?   '
	read (*,'(I2)') av_period
29	write(*,'(A55,$)')'Filter lenght (sec) ?   '
	read (*,'(I4)') filter
	write(*,*)

	! Check inputs
	if(lge(floc,'A').and.lle(floc,'H') .or.
     &     lge(floc,'a').and.lle(floc,'h')) then
  	 if(floc.ne.lloc) then
	  write(*,*)'-------------------------------------------------'
	  write(*,*)'First file and last do not have the same location!'
	   write(*,*)
	   go to 9
	  endif

	  loc = floc

	  if (lday.le.197) then
	   if (loc.eq.'C' .or. loc.eq.'D' .or. loc.eq.'c'
     &         .or. loc.eq.'d') then
	    write(*,*)'-------------------------------------------'
	    write(*,*) 'File recorded with Fastcom! Can`t read it.' 
	    write(*,*)
	    go to 9
	   endif
	  endif
	  if(lday.lt.fday) then
	   write(*,*)'-------------------------------'
	   write(*,*)'Last day comes before the first!'
	   write(*,*)
	   go to 9
	  endif
	  if(lday.lt.211 .and. fday.gt.197) then
	   write(*,*)'-------------------------------'
	   write(*,*)'This days are not available!'
	   write(*,*)
	   go to 9
	  endif
	  if (fhour.lt.0 .or. fhour.ge.24) then
	   write(*,*)'-------------------'
	   write(*,*)'Check starting time!'
	   write(*,*)
	   go to 9 
	  endif
	  if (lhour.lt.0 .or. lhour.ge.24) then
	   write(*,*)'----------------'
	   write(*,*) 'Check end time!'
	   write(*,*)
	   go to 9 
	  endif
	  if (lhour.lt.fhour .and. lday.eq.fday) then
	   write(*,*)'--------------------------------'
	   write(*,*)'Check starting time and end time!'
	   write(*,*)
	   go to 9 
	  endif
	  if (mod(60,av_period).ne.0 .or. av_period.gt.60) then
	   write(*,*)'-----------------------------------------'
	   write(*,*)'Check averaging period (5,6,10,12,15,..,60)!'
	   write(*,*)
	   go to 19 
	  endif
	  if (mod(filter,10).ne.0 .and. filter.gt.0) then
	   write(*,*)'--------------------------------------------'
	   write(*,*)'Check filter lenght, must be multiple of 10!'
	   write(*,*)
	   go to 29 
	  endif

	  ! Filter lenght
	  if (filter.gt.3600 .or. filter.lt.0) then
	   write(*,*)'------------------------------'
	   write(*,*)'Filter lenght must be between 0-3600!'
	   write(*,*)
	   go to 29 
	  endif
	else
	   write(*,*)'--------------------------------'
	   write(*,*)'Location incorrect (A-H or a-h)!'
	   write(*,*)
	   go to 9
	endif

	return
	end 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C location
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Further informations retrieved from the filename:
! Kry = 1 means krypton is present, nxxx are serial numbers needed for 
! calibrations, analog is the number of analogue inputs of this 
! sonic. The measurement height with respect to the surroundings and 
! the absolute height (lheight) are also given.
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine location(loc,lday,height,lheight,rheight,
     &                      nord,nsonic,nkry,kry,analog)

	implicit none
	integer lday  				!input
	integer nord,nsonic,nkry,kry,analog	!output
	real height,lheight,rheight		!output
	character*1 loc  			!input

	! San Vittore (sonic calibration experiment)
	if (lday.le.197) then
	  height = 1.80
	  lheight = 300. 
	  rheight = 0.1		! reference P measured in SVittore
	  if (lday.le.195) then 	!  1st layout
	    if     (loc.eq.'A' .or. loc.eq.'a') then
	      nord = 260
	      nsonic = 30
	      nkry = 1300
	      kry = 1
	      analog = 1
	    elseif (loc.eq.'B' .or. loc.eq.'b') then
	      nord = 258 
	      nsonic = 36
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'E' .or. loc.eq.'e') then
	      nord = 305 
	      nsonic = 69
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'F' .or. loc.eq.'f') then
	      nord = 260 
	      nsonic = 68
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'G' .or. loc.eq.'g') then
	      nord = 260 
	      nsonic = 47
	      nkry = 1370
	      kry = 1
	      analog = 1
	    elseif (loc.eq.'H' .or. loc.eq.'h') then
	      nord = 275 
	      nsonic = 35
	      nkry = 1299
	      kry = 1
	      analog = 1
	    endif
	  elseif (lday.lt.196) then	!  2nd layout
	    if     (loc.eq.'A' .or. loc.eq.'a') then
	      nord = 260
	      nsonic = 30
	      nkry = 1300
	      kry = 1
	      analog = 1
	    elseif (loc.eq.'B' .or. loc.eq.'b') then
	      nord = 78 
	      nsonic = 36
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'E' .or. loc.eq.'e') then
	      nord = 305 
	      nsonic = 69
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'F' .or. loc.eq.'f') then
	      nord = 80 
	      nsonic = 68
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'G' .or. loc.eq.'g') then
	      nord = 260 
	      nsonic = 47
	      nkry = 1370
	      kry = 1
	      analog = 1
	    elseif (loc.eq.'H' .or. loc.eq.'h') then
	      nord = 95 
	      nsonic = 35
	      nkry = 1299
	      kry = 1
	      analog = 1
	    endif
	  else				!  3rd layout
	    if     (loc.eq.'A' .or. loc.eq.'a') then
	      nord = 260
	      nsonic = 30
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'B' .or. loc.eq.'b') then
	      nord = 276 
	      nsonic = 36
	      nkry = 1299
	      kry = 1
	      analog = 1 
	    elseif (loc.eq.'E' .or. loc.eq.'e') then
	      nord = 250 
	      nsonic = 69
	      nkry = 1370
	      kry = 1
	      analog = 1
	    elseif (loc.eq.'F' .or. loc.eq.'f') then
	      nord = 260 
	      nsonic = 68
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'G' .or. loc.eq.'g') then
	      nord = 260 
	      nsonic = 47
	      nkry = 0
	      kry = 0
	      analog = 0
	    elseif (loc.eq.'H' .or. loc.eq.'h') then
	      nord = 260 
	      nsonic = 35
	      nkry = 0
	      kry = 0
	      analog = 0
	    endif
	  endif
	endif

	! Claro
	if (lday.gt.210) then	  
	 if     (loc.eq.'A' .or. loc.eq.'a') then
	   height = 3.56
	   lheight = 250. 
	   rheight = 3.
	   nord = 165
	   nsonic = 68
	   nkry = 1299
	   kry = 1
	   analog = 1
	 elseif (loc.eq.'B' .or. loc.eq.'b') then
	   height = 15.58
	   lheight = 250.
	   rheight = 15.
	   nord = 115 
	   nsonic = 69
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'C' .or. loc.eq.'c') then
	   height = 27.63
	   lheight = 250.
	   rheight = 27.
	   nord = 170
	   nsonic = 47
	   nkry = 1300
	   kry = 1
	   analog = 1
	 elseif (loc.eq.'D' .or. loc.eq.'d') then
	   height = 6.5
	   lheight = 340. 
	   rheight = height + lheight-250.
	   nord = 115
	   nsonic = 54
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'E' .or. loc.eq.'e') then
	   height = 15.34
	   lheight = 760.
	   rheight = height + lheight-250.
	   nord = 180
	   nsonic = 35
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'F' .or. loc.eq.'f') then
	   height = 23.78
	   lheight = 760.
	   rheight = height + lheight-250.
	   nord = 180
	   nsonic = 30
	   nkry = 1370
	   kry = 1
	   analog = 3
	 elseif (loc.eq.'G' .or. loc.eq.'g') then
	   height = 29.75
	   lheight = 760.
	   rheight = height + lheight-250.
	   nord = 180
	   nsonic = 36
	   nkry = 0
	   kry = 0
	   analog = 0
	 endif
	endif

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C make files with raw, averaged and variation data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      subroutine makearray (out_file,loc,fday,lday,fhour,lhour,
     &                     rheight,filter,nkry,kry,nord,
     &                     jrow,irowsum,marray,darray,t,p,rh,rain,net)

      implicit none
      integer i,j,count,nascii
      integer irow,jrow,irowsum
      integer aday,ihour,ahour,amin
      integer fday,lday,fhour,lhour ! input    
      integer filter ! input    
      integer start,end,nkry,kry,nord 
      real array(4500000,5),marray(4500000,5),darray(4500000,5)
      real arr(75100,7)
      real mean(5),rom,em,qm,dd,rheight
      real tm,pm,rhm,rm,nm
      real t(100),p(100),rh(100),rain(100),net(100)
      character*1 loc,rloc
      character*16 out_file !input
      character*12 actual_file,actual_ref
      character*42 path_file,path_ref
      logical existence

      ihour = fhour ! first day starts from hour 'fhour'
      amin = 0 ! first file must have min=0
      irowsum = 0 ! tot. lenght of output arrays

      ! Workfile open
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      open(10,file='../data_output/'//out_file//'.raw',
     &        status='unknown',form='formatted')
 
      count = 0
C----------------------------------------------------------------------
C loop1: day loop
C----------------------------------------------------------------------
	do aday = fday,lday
C----------------------------------------------------------------------
C loop2: hour loop, flexible, not always 24h (0-23)
C----------------------------------------------------------------------
	   irow = 0
2	   do ahour=ihour,23		
	      count = count+1
	      if (ahour.gt.lhour .and. aday.eq.lday) go to 2000

	      ! Input file name and path
	      write(actual_file,'(A,I3.3,I2.2,I2.2,A4)')loc,
     &        aday,ahour,amin,'.slt'

	      nascii = ichar(loc)-32
	      rloc = achar(nascii) !rloc is uppercase of loc 
	      actual_ref  = rloc//actual_file(2:8)//'.ref'

	path_file='/cdrom/cdrom0/'//loc//'/'//actual_file
	path_ref='../data_ref/'//rloc//'/'//actual_ref

	      ! Get binary raw data and reference data
	      call filesexist(path_file,existence)		
	      if(.not.existence) then
	write(*,*)'Raw data file ',actual_file,' does not exist!'
		go to 3000
	      endif	
	      call filesexist(path_ref,existence)		
	      if(.not.existence) then
	write(*,*)'Reference data file ',actual_ref,' does not exist!'
		go to 3000
	      endif

	      write(*,*)
	      write(*,*)'-> Getting raw data from ',actual_file,' ..'
	      call getdata(path_file,path_ref,irow,rheight,arr,
     &                     tm,pm,rhm,rm,nm)
	      write(*,*)'   .. loaded file has',irow,' rows'
	      t(count) = tm
	      p(count) = pm
	      rh(count) = rhm
	      rain(count) = rm
	      net(count) = nm

	      ! Check lenght of each file
	      if(irow.le.68750) then
		write(*,*)'Last file has less than 55min data!'
		go to 2000
	      endif

	      ! Computations on the raw data
	      call meanhum(tm,pm,rhm,rom,em,qm,dd)
	      call conversions(irow,em,pm,arr)
	      call turn_in_meteo(irow,nord,arr)
	      if(kry.eq.1) then
	        write(*,*)'-> Computing krypton data'	  
		start = 1
		end = irow/2
		call period_average1(start,end,jrow,arr,mean)
	        call krypton(start,end,mean,rom,dd,
     &                       arr,nkry,lday)
		start = irow/2+1
		end = irow
		call period_average1(start,end,jrow,arr,mean)
	        call krypton(start,end,mean,rom,dd,
     &                       arr,nkry,lday)
	      endif

	      ! Write array file
	      write(*,*)'-> Writing data in ',out_file,'.raw file'
	      do i=1,irow
		write(10,*) (arr(i,j),j=1,jrow)
	      enddo

	      irowsum = irowsum + irow
	      amin = 0 ! starting from the second hour all have min = 0
	   enddo
C----------------------------------------------------------------------
C end loop2
C----------------------------------------------------------------------
     	   ihour = 0 ! starting from the second day all have ihour = 0
	enddo
C----------------------------------------------------------------------
C end loop1
C----------------------------------------------------------------------
2000	continue

	close(10)
	write(*,*)
	write(*,*)'-------------------------------------------------',
     &  '-----------------------'

	call getarray (irowsum,jrow,out_file,array)

	! Apply recursive filter
	write(*,*)	  
	if(filter.ne.0) then
	  write(*,*)'-> Applying recursive filter'	  
	endif
	call recfilter(irowsum,jrow,filter,array,marray,darray)

3000    continue
	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C filesexist: inquires if the needed files exist 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine filesexist(path,existence)	

	implicit none
	character*42 path		!input
	logical existence		!output

	! Inquires if rawdata and ascii files exist
	inquire (file=path, exist=existence)

	return
	end 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C getdata: gets the raw data and the references.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine getdata(path_file,path_ref,irow,h,arr,tm,pm,rhm,
     &  rm,nm)

	implicit none
	integer irow			!output
	real pm,tm,rhm,rm,nm	!output
	real arr(75100,7)		!output
	real h				!input
	character*42 path_file, path_ref!input

	! Local variables
	integer*2 i,size,x, y, ifb
	integer*2 iibits
	integer irec,k
	real p1,t11,t1,rh1,p2,t22,t2,rh2
	real rain1,rain2,net1,net2
	real bar

	! Opens rawdata and reads 
        open (13,file=path_file,status='old', 
     &      form='unformatted',access='direct',recl=2,err=999)   

	! Reads the header of one .SLT data file (1h)
        irec=0
	irec=irec+1
        read (13,rec=irec) size
        ifb = iibits(size,8,15)

	! Reads the .SLT data file (1h) and puts it in the arr(k,i)
	k = 0 
        irec=irec+ifb/2-1
899       k=k+1
          do i=1,ifb/2
              irec=irec+1
              read (13,rec=irec,end=999) x
              y = iibits(x,8,8)+2**8*iibits(x,0,8)
	      arr(k,i) = float(y)
           end do
        go to 899
999     continue
 	irow = k-1
	close (13)

	! Opens reference data(values every 1/2hour)file and reads 
        open (14,file=path_ref,status='old',form='formatted')
	  read (14,*) t1,rh1,p1,t11,rain1,net1
	  read (14,*) t2,rh2,p2,t22,rain2,net2
	close (14)

	t1 = 273.16 + t1
	t2 = 273.16 + t2
	tm = (t1+t2)/2.		! [K]

	t11 = 273.16 + t11
	t22 = 273.16 + t22
	p1 = bar(h,t11,p1)	! use T at lowest level for pottemp
	p2 = bar(h,t22,p2)
	pm = (p1+p2)/2.	 	! [hPa]

	rhm = (rh1+rh2)/2.	! [%]
	rm = (rain1+rain2)/2.! [mm]
	nm = (net1+net2)/2.	! [W/m2]

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  meanhum (flux98I) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      subroutine meanhum(tm,pm,rhm,rom,em,qm,dd)

	implicit none
	real rom,em,qm,dd				!output
	real tm,pm,rhm				!input

	! Local variables
	real par1,rlogew,a,b,c,c1,c2,d,d1,d2,e,ew
	real ewOpm,rrw,rr

	! Compute the saturation vapour pressure ew[hPa] from temp.tm 
        par1=373.16/tm
        a=-7.90298*(par1-1)
        b=5.02808*(alog10(par1))
        c1=(11.344*(1.-(1./par1)))
        c2=-7.
        c=-1.3816*(10.**c2)*((10.**c1)-1.)
        d1=(-3.49149*(par1-1.))
        d2=-3.
        d=8.1328*(10.**d2)*((10.**d1)-1.)
        e=alog10(1013.246)
        rlogew=a+b+c+d+e
        ew=10.**rlogew

	! berechne das saettigungs-mischungsverhaeltnis rrw
	ewOpm = ew/pm
	rrw=0.62197*ewOpm/(1-ewOpm)

	! mischungsverhaeltnis rr 
        rr=rhm/100.*rrw
        em=rhm/100.*ew

	! Density of dry air dd[g/m^2]
	dd=pm*100000./(287.04*tm*0.9995)
C	dd=348.5577795*pm/tm

	! Density of H2O vapour rom[g/m^2]
	rom=dd*rr

	! Mean specific moisture qm 
	qm=rr/(1+rr)

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C conversions: retrieves the temperature [K] from the sound speed, 
C computes the potentialttemperature and could convert the analog 
C Inputs voltage.  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine conversions(irow,em,pm,arr)

	implicit none
	integer i,irow			!input
	real arr(75100,7),pottemp	!in/output
	real pm,em			!input

	! Local variable
	real t

	do i=1,irow
	   arr(i,1) = arr(i,1)/100.	! u,v,w converted to m/s
	   arr(i,2) = - arr(i,2)/100. ! right hand coord.system
	   arr(i,3) = arr(i,3)/100.
	   arr(i,4) = arr(i,4)/50.	! Edisol saves 1/50 * m/s
	   arr(i,4) = ((arr(i,4)/20.067)**2.)/(1.+0.3192*(em/pm))
	   t = arr(i,4)
	   arr(i,4) = pottemp(pm,t)
	enddo
	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C period_average1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine period_average1(av_start,av_end,jrow,arr,mean)

	implicit none
	integer av_start,av_end,jrow		!input
	real arr(75100,7)			!input
	real mean(5)	 			!output

	! Local variables
	integer i,j 
	real sum(5)

	do j=1,jrow
	  sum(j) = 0.
	  mean(j) = 0.
	enddo

	do j=1,jrow
	  do i=av_start,av_end
	    sum(j) = sum(j) + arr(i,j)
	  enddo
	  mean(j) = sum(j)/float(av_end-av_start+1)
	enddo

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C turn_in_meteo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	subroutine turn_in_meteo(irow,nord,arr)

	implicit none
	integer irow,nord	 !input
	real arr(75100,7)	 !input/output

	! Local variables
	integer ii 
	real alfa,wind(2),pi
	pi= 4.*atan(1.)
	alfa = float(nord+60) ! angle(y-axis,North)
	if(alfa .lt. 0.) alfa = alfa + 360.
 	if(alfa .ge. 400.) alfa = alfa - 360.
	alfa =  pi*alfa/180.

	do ii=1,irow      
	  wind(1)=arr(ii,1)
	  wind(2)=arr(ii,2)
	  arr(ii,1)=cos(alfa)*wind(1)+sin(alfa)*wind(2)
	  arr(ii,2)=-sin(alfa)*wind(1)+cos(alfa)*wind(2)
	enddo
	! u,v,w will allways refer to the Meteorological coord.system
	! that is x-axis toward East,y-axis t. North,z-axis t. Azimuth.
	return
	end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C recursive filter (McMillen 1988)  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      subroutine recfilter(irowsum,jrow,filter,array,marray,darray)

      implicit none
      integer i,j
      integer irowsum,jrow	! input
      integer filter ! input 
      real array(4500000,5) ! input
      real marray(4500000,5),darray(4500000,5) ! output
      real alfa,beta,sum  

C     alfa = exp(-0.04807692/filter) ! dt=1/20.8=0.04807692s
	if(filter.eq.0)then
	  alfa = 0
	  beta = 1
	else
          alfa = 1 - 0.0487692/filter
          beta = 1 - alfa
	endif	
	write(*,*) 'Alfa,beta',alfa,beta

      ! Average of first 10 minutes to initialise filter
      do j=1,jrow
        sum = 0.
        do i=1,12480  
          sum = sum + array(i,j) 
        enddo
        marray(1,j) = sum/12480. 
	darray(1,j) = array(1,j) - marray(1,j)
      enddo

      do j=1,jrow
        do i=2,irowsum
          marray(i,j) = alfa*marray(i-1,j) + beta*array(i,j) 
          darray(i,j) = array(i,j) - marray(i,j)
        enddo
      enddo


      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Getarray: reads in data for array 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine getarray (irowsum,jrow,out_file,array)

	implicit none
	integer irowsum,jrow !input
	real array(4500000,5) !output
	character*16 out_file !input

	! Local variables
	integer i,j

	do i=1,irowsum
	  do j=1,jrow
	    array(i,j) = 0.
	  enddo
	enddo

	! Read array files
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      open(15,file='../data_output/'//out_file//'.raw',
     &         status='unknown',form='formatted')

	! read(15,*)fday,fhour,jrow,filter,height,nsonic,nkry,nord
	do i=1,irowsum
	  read(15,*) (array(i,j),j=1,jrow)
	enddo
	close(15)

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C period_average
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine period_average(av_start,av_end,jrow,marray,mean)

	implicit none
	integer av_start,av_end,jrow		!input
	real marray(4500000,5)			!input
	real mean(5)	 			!output

	! Local variables
	integer i,j 
	real sum(5)

	do j=1,jrow
	  sum(j) = 0.
	  mean(j) = 0.
	enddo

	do j=1,jrow
	  do i=av_start,av_end
	    sum(j) = sum(j) + marray(i,j)
	  enddo
	  mean(j) = sum(j)/float(av_end-av_start+1)
	enddo

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Krypton
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine krypton(start,end,mean,rom,dd,
     &  arr,nkry,lday)   

	implicit none
	integer nkry,start,end !input
	real mean(5),rom,sfeich,dd !input
	real arr(75100,7) !output
	real b0,b1,b2,rkwd,rkww,rlnv0d,rlnv0w,xpk
	integer lday

	! Local variables
	integer i
	real rlnvm
	real ro(5000)

	! Clean window calibration
	if(lday.le.197) then
	  if(nkry.eq.1299)then
	    b0 = 0.121197
	    b1 = 0.97900448
	    b2 = 0.0006720408
	    rkwd = 0.152
	    rkww = 0.137
	    rlnv0d = 8.479
	    rlnv0w = 8.301
	    xpk = 1.393 
	  elseif (nkry.eq.1300)then
	    b0 = 0.08746693
	    b1 = 0.9840659
	    b2 = 0.0005359969
	    rkwd = 0.149
	    rkww = 0.135
	    rlnv0d = 8.456
	    rlnv0w = 8.285
	    xpk = 1.438
C	  elseif (nkry.eq.1370) then 
C	    b0 = 
C	    b1 = 
C	    b2 = 
C	    rkwd = 
C	    rkww = 
C	    rlnv0d = 
C	    rlnv0w = 
C	    xpk = 
	  endif
	else

	! Scaled window calibration
	if(nkry.eq.1299)then
	  b0 = 0.7582
	  b1 = 0.8636
	  b2 = 0.0044
	  rkwd = 0.152
	  rkww = 0.136
	  rlnv0d = 8.111
	  rlnv0w = 7.919
	  xpk = 1.393 
	elseif (nkry.eq.1300)then
	  b0 = 0.6748 
	  b1 = 0.8851
	  b2 = 0.0039
	  rkwd = 0.149
	  rkww = 0.135
	  rlnv0d = 8.043
	  rlnv0w = 7.866
	  xpk = 1.438
	elseif (nkry.eq.1370) then 
	  b0 = 0.6768
	  b1 = 0.8617
	  b2 = 0.0051
	  rkwd = 0.152
	  rkww = 0.137
	  rlnv0d = 8.222
	  rlnv0w = 8.054
	  xpk = 1.361
	endif
	endif

	! Convert krypton data from U[mV] to q[kg/kg]
	rlnvm = alog(mean(5))
	call lookupkry(rlnvm,rom,b0,b1,b2,rkwd,rkww,
     &                 rlnv0d,rlnv0w,xpk,ro) 	! output is ro(5000)
	if(mean(5).lt.1.) then
	  mean(5) = sfeich(1.,dd,ro)
	elseif(mean(5).gt.5000.)then
	  mean(5) = sfeich(5000.,dd,ro)
	else
	  mean(5) = sfeich(mean(5),dd,ro)
	endif

	do i=start,end
	  if(arr(i,5).lt.1.) then
            arr(i,5) = sfeich(1.,dd,ro) 
	  elseif(arr(i,5).gt.5000.) then
            arr(i,5) = sfeich(5000.,dd,ro) 
	  else
	    arr(i,5) = sfeich(arr(i,5),dd,ro)
	  endif
	enddo

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C lookupkry (NK flux98I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Die subroutine LOOKUPKRY berechnet fuer jedes moegliche signal
! (0...5000mV)des Krypton Hygrometers die aktuelle Dichte mit der
! mittleren Dichte rom. Berechne aus dem mittleren lnV (rlnvm) zuerst
! das mittlere V'' (rlnvmm), entspr. manual - dann d(ro)/d(V'') 
! (=drodlnv) und daraus ro. mit fallunterscheidung rlnvm dry/wet:
! dry(8.52 bis 6.600), wet(6.599 bis 0.0).

	subroutine lookupkry(rlnvm,rom,b0,b1,b2,rkwd,rkww,
     &           rlnv0d,rlnv0w,xpk,ro)

	implicit none
	real rom,rlnvm !input
	real b0,b1,b2,rkwd,rkww,rlnv0d,rlnv0w,xpk !input
	real ro(5000) !output

	! Local variables
	integer ii
	real RLNVMM,RLNV,DLNV,DET,D2DET
	real D3DET,DRODLNV,DRO,X

      if(RLNVM .LT. 6.600) THEN
        RLNVMM=(RLNVM-RLNV0W)/(-1.*RKWW)
      else
        RLNVMM=(RLNVM-RLNV0D)/(-1.*RKWD)
      endif

      do ii=1,5000
        X=float(ii)
        if(RLNVM .LT. 6.600)then
          RLNV=(ALOG(X)-RLNV0W)/(-1.*RKWW)
        else
          RLNV=(ALOG(X)-RLNV0D)/(-1.*RKWD)
        endif
        DLNV=RLNV-RLNVMM

	  ! RO*X = B0 + B1*(V*-V*O) + B2*(V*-V*O)^2
        DET=B1**2.-4.*B0*B2
        if((DET+4.*B2*ROM*XPK).GT.0)then

	  ! Taylor-Entwicklung um ro=rom bis 1.Ordnung
          D2DET=SQRT(DET+4.*B2*ROM*XPK)
          D3DET=D2DET-2.*B2*DLNV
          if(D3DET.GT.0.0)then
            DRODLNV=D2DET**2./D3DET
          else 
            DRODLNV=0.
          endif
	else
            DRODLNV=0.
        endif
	  dro=drodlnv*dlnv/xpk
	  ro(ii)=dro+rom
	  if(ro(ii).lt.0.) ro(ii)=0.
	enddo

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C angles
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        subroutine angles(mean,teta,eps)

	implicit none
	real mean(5)			!input
	real eps,teta 			!output

	! Local variables
	real rm12,pi
	pi=4.*atan(1.)
	eps = 0.
	teta = 0.


	! Angle from sonic x-axis to mean wind direction,<v>=0.
	IF (ABS(mean(1)) .LT. 0.000001) THEN
	  IF (mean(2).LT. 0.) teta= 90.
	  IF (mean(2).EQ. 0.) teta= 0.
	  IF (mean(2).GT. 0.) teta= 270.
	ELSE
	  IF (mean(2).GT. 0.0) THEN
	    teta = 270. + atan(mean(1)/mean(2))/pi*180.
	  ELSE
	    IF (ABS(mean(2)) .GT. 0.000001) THEN
	      teta = 90. + atan(mean(1)/mean(2))/pi*180.
	    ELSE 
	      IF (mean(1) .GT. 0) THEN 
		teta = 0.00 
	      ELSE 
		teta = 180.00
              ENDIF
            ENDIF
          ENDIF
        ENDIF

	if (teta .lt.   0.) teta = 360.0 + teta
 	if(teta .ge. 360.) teta = teta - 360.0

	! Angle into <w>=0 direction
	rm12 = sqrt(mean(1)**2.+ mean(2)**2.)
        if (rm12.ne.0.) then
          eps = atan(mean(3)/rm12)/pi*180.
        else 
          eps = 0.
        end if     

	return
	end     

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C turn_in_stream
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! dreht den Windvektor in die Windrichtung

	subroutine turn_in_stream (av_start,av_end,
     &                      marray,darray,teta,eps)

	implicit none
	integer av_start,av_end			 !input
	real marray(4500000,5),darray(4500000,5) !in/out
	real teta,eps,a,b		 	 !input

	! Local variables
	integer ii 
	real u,v,w,pi
	pi=4.*atan(1.)

	a = pi*teta/180.
	b = pi*eps/180.

C
C       1. Rotation v=0
C	[cos(a)   -sin(a)      0 ]
C	[sin(a)    cos(a)      0 ]
C	[  0         0         1 ]
C
C	2. Rotation w=0
C	[ cos(b)     0     sin(b)]
C	[  0         1         1 ]
C	[-sin(b)     0     cos(b)]
C

	do ii=av_start,av_end      
	  u =marray(ii,1)
	  v =marray(ii,2)
	  w= marray(ii,3)
	  marray(ii,1)= cos(b)*cos(a)*u -cos(b)*sin(a)*v +sin(b)*w
	  marray(ii,2)=        sin(a)*u +       cos(a)*v
	  marray(ii,3)=-sin(b)*cos(a)*u +sin(b)*sin(a)*v +cos(b)*w
	enddo

	do ii=av_start,av_end      
	  u =darray(ii,1)
	  v =darray(ii,2)
	  w= darray(ii,3)
	  darray(ii,1)= cos(b)*cos(a)*u -cos(b)*sin(a)*v +sin(b)*w
	  darray(ii,2)=        sin(a)*u +       cos(a)*v
	  darray(ii,3)=-sin(b)*cos(a)*u +sin(b)*sin(a)*v +cos(b)*w
	enddo

	return
	end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Runtest: Es werden die Standard-Abweichungen verglichen
C  mit der Median-Standard-Abweichung  (Versuch Jann)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine runtest(av_start,av_end,jrow,darray,kol)   

	implicit none
	integer av_start,av_end,jrow	!input
	real darray(4500000,5) !input
	integer kol(5) !output

	! Local variables
	integer i,ii,j,k,ite,itest,intcou
	real rm(5),rmq(5),a,xxx,sigtot,interv
	real sig(40,5),dummy(40)

	! Setzero
	interv=0.
	intcou=0
	ite=0
	do j=1,5
	  kol(j)=0
	  rm(j)= 0.
	  rmq(j)=0.
	  do i=1,40
	    dummy(i)= 0.
	    sig(i,j)= 0.
	  enddo
	enddo

	! Prepare runtest 	
	interv = float(av_end-av_start+1)/40. !divide in 40 sub-intervalls 

	do i=av_start,av_end
	  do j=1,jrow
	    rm(j) =rm(j)+darray(i,j)
	    rmq(j)=rmq(j)+(darray(i,j)**2.)
	  enddo

	  if(mod(i-av_start,nint(interv)).eq.0.)then
	    intcou=intcou+1
	    if(intcou.gt.40) goto 88
	    do j=1,jrow
	      XXX=(rmq(j)/interv)-(rm(j)/interv)**2.
	      if(XXX .gt. 0)then
		 sig(intcou,j)=sqrt(XXX)
	      else
		 sig(intcou,j)=0.
	      endif
	      rm(j) =0.
	      rmq(j)=0.
	    enddo
88	  endif
	enddo

	! Runtest
	do j=1,jrow
	  do i=1,40
	    dummy(i)=sig(i,j)
	  enddo

 	  ! Sort dummy
	  do i=2,40
            a=dummy(i)
            do ii=i-1,1,-1
              if (dummy(ii).le.a) goto 89
              dummy(ii+1)=dummy(ii)
	    enddo
	    ii=0
89	    dummy(ii+1)=a
	  enddo
          sigtot=.5*(dummy(20)+dummy(21)) ! Median
          k=0
          itest=-99

          do i=1,40
            if (sig(i,j) .gt. sigtot) then
              ite=1
            else
              ite=0
            endif
            if (ite.ne.itest) then
              k=k+1
              itest=ite
            end if 
	  enddo
        kol(j)=k
C	print*,kol(j),j
      enddo
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C fluxes
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine fluxes (av_start,av_end,jrow,darray,mean,height,
     &        tm,pm,sigma,skew,uiuj,usqr,uit,uiq,wusqr,
     &        M,Hs,E,ustar,tstar,qt,zoverl)

	implicit none
	integer av_start,av_end,arow,jrow !input
	real darray(4500000,5),mean(5),height !input
	real sigma(5),skew(5)			!output
	real uiuj(3),uit(3),uiq(3),wusqr(3),usqr(3)	!output
	real ustar,qt,tstar,L,zoverl		!output
	real M,Hs,E,d,cp,R,Lv            	!output
	real tm,pm

	! Local variables
	integer i,j,ii,jj
	real ustsq
        real errcode
        data errcode /-999.99/

	! Setzero
	j=0
	ustsq=0.
	ustar =0.
	qt=0.
	L = 0.
	zoverl = 0.
	tstar = 0.
	M = 0.
	Hs = 0. 
	E = 0.
	do i=1,5
	  sigma(i)=0.
        skew(i)=0.
	enddo
	do i=1,3
	  uiuj(i)=0.
	  uit(i)=0.
	  uiq(i)=0.
	  wusqr(i)=0.
	  usqr(i)=0.
	enddo

	! Sum up different covariance terms
	arow = av_end-av_start+1
	do i=av_start,av_end
	  do j=1,jrow
            sigma(j)=sigma(j)+darray(i,j)**2.
            skew(j)=skew(j)+darray(i,j)**3.
            if(j.le.3) then
              uit(j)=uit(j)+darray(i,j)*darray(i,4)	! u'T',v'T'...
              if(jrow.eq.5)then
		uiq(j)=uiq(j)+darray(i,5)*darray(i,j)
	      endif
	    endif
	  enddo
	  uiuj(1)=uiuj(1)+darray(i,1)*darray(i,2)	! u'v'
	  uiuj(2)=uiuj(2)+darray(i,2)*darray(i,3)	! v'w'
	  uiuj(3)=uiuj(3)+darray(i,1)*darray(i,3)	! u'w'
	  usqr(1)=usqr(1)+darray(i,1)**2.   ! (u')^2
	  usqr(2)=usqr(2)+darray(i,2)**2.   ! (v')^2   
	  usqr(3)=usqr(3)+darray(i,3)**2.   ! (w')^2    
	  wusqr(1)=wusqr(1)+(darray(i,1)*darray(i,3))**2.   ! w'u'^2    
	  wusqr(2)=wusqr(2)+(darray(i,4)*darray(i,3))**2.   ! w'T'^2
          if(jrow.eq.5) then
	    qt=qt+darray(i,4)*darray(i,5)		! q'T'
            wusqr(3)=wusqr(3)+(darray(i,5)*darray(i,3))**2. ! w'q'^2
	  endif
	enddo 

	! Sigma from 'varianzen', Skewness norm.,averages of cov. 
      	do jj=1,jrow
          sigma(jj)= sqrt(sigma(jj)/float(arow))
          skew(jj)=skew(jj)/float(arow)/((sigma(jj)**2.)*sigma(jj))
	  if(jj.le.3) then
            uiuj(jj)=uiuj(jj)/float(arow)
            usqr(jj)=usqr(jj)/float(arow)
            uit(jj)=uit(jj)/float(arow)
            uiq(jj)=uiq(jj)/float(arow)
            wusqr(jj)=wusqr(jj)/float(arow)
	  endif   
	enddo
      if(jrow.eq.5) then
        qt=qt/float(arow)
	endif
	ustsq=SQRT(uiuj(2)**2.+uiuj(3)**2.)	! u*^2
	wusqr(1)=wusqr(1)/ustsq**2.
	wusqr(2)=wusqr(2)/(uit(3)**2.)     
	if(jrow.eq.5)then
          wusqr(3)=wusqr(3)/(uiq(3)**2.)
	endif
	do ii=1,3
          if(wusqr(ii).gt.10000.) wusqr(ii)= errcode
	enddo

	! u* and z/L
	ustar = sqrt(ustsq)
	if(uit(3).lt. 1.0E-8 .and. uit(3).gt.-1.0E-8)then
	   L = 1.0E8
	   zoverl = 0.
	else
	   tstar = -uit(3)/ustar
	   L = - mean(4)*(ustar**3.)/(0.4*9.81*uit(3))

	   ! measured mean potential Temp.
	   zoverl = height/L
C	   print*,mean(4),ustar**3.,uit(3)
C	   print*,height,L,zoverl
	endif

	! M,H and E fluxes
	cp = 1005.
	R = 2.8704 
	d = pm/(R*tm)	! in kg/m^3
C	print*,d
	Lv = 1000000*(2.501-0.00237*(tm-273.16))
	M = - d * ustsq
C	print*,M
	Hs = d*cp*uit(3)
	E = d *Lv*uiq(3)

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C outformat
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine outformat (turned,loc,day,hour,min,height,nsonic,
     &                   nkry,teta,eps,tm,pm,rhm,rm,nm,av_period,
     &	 	         filter,runind,mean,sigma,skew,lheight,
     &  	         uiuj,uiui,uit,uiq,wusqr,M,Hs,E,zoverl,
     &                   ustar,tstar,qt)

	! All input variables
	implicit none
	integer day,hour,min
	integer nsonic,nkry,av_period,filter,turned
	integer runind(5)
	real height,lheight
	real eps,teta,tm,pm,rhm,rm,nm
	real mean(5) ,sigma(5), skew(5)
	real uiuj(3),uiui(3),uit(3),uiq(3),wusqr(3)
	real M,Hs,E,zoverl
	real ustar,tstar,qt	
	character*1 loc


	! Local variables
	integer k

	! Write output to file
	if(turned.eq.1) then
	write(20,'(A40,A38)')'******* MAP-Riviera project 1999 *******',
     &  '***************************************'
	write(20,198)'Serial numbers   :',nsonic,nkry
	write(20,199)'z and elevation  :',height,lheight
	write(20,200)'Date and location:',loc,day,hour,min
	write(20,201)'Meteo            :',tm,pm,rhm,rm,nm
	write(20,202)'Av. time + Filter:',av_period,'min',filter,'sec'
	write(20,204)'Wind direction   :',teta,eps
	endif

	write(20,'(A40,A38)')'----------------------------------------',
     &  '---------------------------------------'
	write(20,205)'                  ','u','v','w','T','q'
	write(20,206)'Run test index   :',(runind(k),k=1,5)
	write(20,207)'Averages         :',(mean(k),k=1,5)
	write(20,207)'Sigma            :',(sigma(k),k=1,5)
	write(20,207)'Skew             :',(skew(k),k=1,5)
	write(20,'(A40,A38)')'----------------------------------------',
     &  '---------------------------------------'
	write(20,208)'u`v`, v`w`, w`u` :',(uiuj(k),k=1,3)
	write(20,208)'u`u`, v`v`, w`w` :',(uiui(k),k=1,3)
	write(20,208)'u`T`, v`T`, w`T` :',(uit(k),k=1,3)
	write(20,208)'u`q`, v`q`, w`q` :',(uiq(k), k=1,3)    
	write(20,208)'wu^2, wt^2, wq^2 :',(wusqr(k),k=1,3)
	write(20,'(A40,A38)')'----------------------------------------',
     &  '---------------------------------------'
	write(20,210)'M[N/m2] H,E[W/m2]:',M,Hs,E    
	write(20,209)'u*, T*, z/L, q`T`:',ustar,tstar,zoverl,qt

	if(turned.eq.2)then
	write(20,'(A40,A38)')'****************************************',
     &  '***************************************'
	write(20,*)
	endif

198 	FORMAT (A20,2X,'Sonic ',I4.4,3X,'Kry ',I4.4)
199 	FORMAT (A20,2X,F5.2,' m.a.g.l',3X,F6.2,' m.a.s.l')
200 	FORMAT (A20,2X,'Location ',A1,3X,'Day ',I3,3X,'Time ',I2,':',I2)
201 	FORMAT (A20,2X,'T=',F6.1,2X,'p=',F6.1,2X,'RH=',F5.1,2X,'Rain=',
     &          F3.0,2X,'Net=',F5.0)	
202 	FORMAT (A20,2X,I3.3,1X,A3,2X,I3.3,1X,A3)	
204 	FORMAT (A20,2X,'Teta= ',F6.2,3X,'Eps=',F6.2)	
205 	FORMAT (A20,6X,5(A1,10X))
206 	FORMAT (A20,5X,5(I2,9X))
207 	FORMAT (A20,3(F10.6,1X),F7.2,1X,F10.6)
208 	FORMAT (A20,3(E12.3,8X))
209 	FORMAT (A20,4(F10.5,3X))
210 	FORMAT (A20,3(F11.6,9X))

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C simpler unformatted output for further use
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	subroutine outsimple (turned,loc,day,hour,min,
     &                   teta,eps,tm,pm,rhm,rm,nm,
     &	 	         runind,mean,sigma,skew,
     &  	         uiuj,uiui,uit,uiq,wusqr,M,Hs,E,zoverl,
     &                   ustar,tstar,qt)

	! All input variables
	implicit none
	integer day,hour,min,turned
	integer runind(5)
	real eps,teta,tm,pm,rhm,rm,nm
	real mean(5) ,sigma(5),skew(5)
	real uiuj(3),uiui(3),uit(3),uiq(3),wusqr(3)
	real M,Hs,E,zoverl
	real ustar,tstar,qt	
	character*1 loc

	! Local variables
	integer k

	! Write simple output format
	if(turned.eq.1)then
	  write(21,301)day,hour,min,teta,eps,tm,pm,rhm,rm,nm,
     &               (runind(k),k=1,5),(mean(k),k=1,5),
     &               (sigma(k),k=1,5),(skew(k),k=1,5),
     &               (uiuj(k),k=1,3),(uiui(k),k=1,3),(uit(k),k=1,3),
     &               (uiq(k),k=1,3),(wusqr(k),k=1,3),qt,
     &                M,Hs,E,zoverl,ustar,tstar
	else
	  write(22,301)day,hour,min,teta,eps,tm,pm,rhm,rm,nm,
     &               (runind(k),k=1,5),(mean(k),k=1,5),
     &               (sigma(k),k=1,5),(skew(k),k=1,5),
     &               (uiuj(k),k=1,3),(uiui(k),k=1,3),(uit(k),k=1,3),
     &               (uiq(k),k=1,3),(wusqr(k),k=1,3),qt,
     &                M,Hs,E,zoverl,ustar,tstar
	endif

301	FORMAT (I3.3,1X,2(I2.2,1X),2(F6.2,1X),5(F7.2,1X),5(I2.2,1X),
     & 4(5(F10.5,1X)),5(3(E12.3,1X)),7(F10.5,1X))

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C calcspec computes spectra
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      subroutine calcspectra (av_start,av_end,jrow,
     &                       darray,freq,mean,sigma,
     &                       loc,day,hour,min,height,pm,ustar,tstar,
     &                       zoverl,filter)

      implicit none
      integer av_start,av_end,jrow			! input
      integer day,hour,min,filter 			! input 
      real darray(4500000,5),mean(5),sigma(5)		 	! input
      real freq,height,pm,ustar,tstar,zoverl 		! input
      character*1 loc
	!Internal variables
	integer i,j,var,arow
	integer NRHIGH,NRLOW,NRLOOP,NumOfInt,NforLow
	integer IHELP,ISLOW,ISHIGH,IIKK,IINT,II3,II4,IMURX
	real RDATAH(4500000),RDATAL(512)
	real FDATAH(1024),FDATAL(512)
	real TAPHIGH(1024),TAPLOW(512)
	real FHIGH(513),FLOW(257)
	real SUH(513),SUL(257),SUHIGH(513)
	real DTHIGH,DTLOW,RHELP,SSUM,FCTAP,FCTAP2,SSUMTOT

      !Set parameters
      NRHIGH = 1024
      NRLOW = 512
      arow = av_end-av_start+1
      DTHIGH   = 1./freq
      NforLow  = 74          ! Anzahl werte fuer LOW
      NumOfInt = 37          ! Anzahl Intervalle/halbe Stunde

c-----------------------------------------------------------------------------
      DO j=1,jrow              ! fuer jeden vorhandenen Kanal
c-----------------------------------------------------------------------------
	var = j
	NRLOOP   = 1013             ! gueltige array pro Block
        CALL InitTAP(NRHIGH,SUHIGH,RDATAL,FDATAL)
        RHELP = 0.               ! Hilfsvar. zum Aufaddieren der array
	IHELP = 1                ! Hilfsvar. fuer den mom. Index (LOW)

        !Umspulen des darray Array's fuer jeden Kanal und bilden der
        ! LOW-freq. Daten
        ! Literatur: Kaimal, J.C.: The Atmosperic Boundary Layer, its
	!            Sturcture and Measurements, Lecture Notes, 
	!            15. Jan. - 28. Feb 88, 
        !            Indian Institute of Tropical Meterololgy, 
        !            P.O. Box 913, SHIVANJINAGAR, PUNE 411005
        ! Zuweisung der array fuer die Berechnung des niederfrequenten
	! Anteils (LOW). Es wird jeweils ueber 74 array gemittelt und
        ! es werden 506 array zugewiesen (vgl. Kaimal, S. 99)     

        IF (arow .EQ. 17460) THEN
          IMURX = 17408
        ELSE
          IMURX = arow
        END IF

	DO i=1,IMURX
          RDATAH(i) = darray(i+av_start-1,j)
	  IF (MOD(i,NforLow) .EQ. 0) THEN
	    RHELP = RHELP + RDATAH(i)
	    RDATAL(IHELP) = RHELP / (NforLow*1.)
	    RHELP = 0.
	    IHELP = IHELP + 1
	  ELSE
	    RHELP=RHELP + RDATAH(i)
	  ENDIF 
	ENDDO

        CALL DEFTAP (NRLOOP,TAPHIGH,FCTAP,FCTAP2)
        SSUMTOT = 0.0

        DO IINT=1,NumOfInt    ! fuer alle 37 Intervalle
          DO II3=1,NRHIGH     ! FDATAH vor jedem Aufruf von DEFDAT 
            FDATAH(II3) = 0.  ! initialisieren, damit nicht hinten mit
          ENDDO               ! Nullen aufgefuellt werden muss.
          CALL DEFDAT (NRLOOP,TAPHIGH,FCTAP,FCTAP2,FDATAH,
     &                 IINT,RDATAH)
          CALL REALFT(FDATAH,NRHIGH/2,+1)
          CALL SPECTR(NRHIGH,DTHIGH,FDATAH,SUH,SSUM)
	  SSUMTOT = SSUMTOT + SSUM
          DO II4 = 1,NRHIGH/2+1 
            SUHIGH (II4) = SUHIGH (II4) + SUH(II4) 
          ENDDO
        ENDDO    ! LOOP UEBER NumOfInt         

        DO IIKK=1,NRHIGH/2+1       ! Mitteln der spektr. Dichten
          SUH (IIKK) = SUHIGH (IIKK) / NumOfInt
        ENDDO

	SSUMTOT = SSUMTOT / NumOfInt
        CALL NORMPL(NRHIGH,DTHIGH,FHIGH,SUH,ISHIGH)

	! Low frequency
        NRLOOP = 506           ! Anzahl gefuellter Array's  
        DTLOW  = NforLow * DTHIGH
        CALL DEFTAP (NRLOOP,TAPLOW,FCTAP,FCTAP2)
        CALL DEFDAT (NRLOOP,TAPLOW,FCTAP,FCTAP2,FDATAL,IINT,RDATAL)
        CALL REALFT (FDATAL,NRLOW/2,+1)
        CALL SPECTR (NRLOW,DTLOW,FDATAL,SUL,SSUM)
        CALL NORMPL (NRLOW,DTLOW,FLOW,SUL,ISLOW)
        CALL WRITESPE (var,loc,day,hour,min,height,pm,filter,
     &                 ISHIGH,ISLOW,mean,ustar,tstar,zoverl,
     &                 FHIGH,SUH,FLOW,SUL,DTHIGH,DTLOW,sigma,
     &                 SSUMTOT,SSUM)
c-----------------------------------------------------------------------------
      ENDDO                     ! LOOP UEBER jrow
c-----------------------------------------------------------------------------

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  InitTAP:initialiesieren der ARRAYS fuer jedes Zeitintervall
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      SUBROUTINE InitTAP (NR,SUHIGH,RDATAL,FDATAL)

      IMPLICIT NONE
      INTEGER IIKK,NR !NRLOOP
      REAL    SUHIGH(*),RDATAL(*),FDATAL(*)

      DO IIKK=1,NR/2
         SUHIGH (IIKK) = 0.0
         RDATAL (IIKK) = 0.0
         FDATAL (IIKK) = 0.0
      ENDDO

      SUHIGH(NR/2+1) = 0.0

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE DEFTAP (NR,TAP,FCTAP,FCTAP2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I,NR !NRLOOP
	REAL TAP(*),pi,sumt,sumt2,ri,rn ! TAP IS OF LENGTH NR
	real fctap,fctap2
	PI=4.*ATAN(1.)
	SUMT=0.
	SUMT2=0.

	DO I=1,NR
           RI=FLOAT(I)
           RN=FLOAT(NR)
           IF (I.LE.(NR/10)) THEN
              TAP(I)=0.5*(1.-COS(10.*PI*RI/RN))
           ELSE IF (I.GE.(9*NR/10)) THEN
              TAP(I)=0.5*(1.+COS(10.*PI*(RI-9.*RN/10.)/RN))
           ELSE
              TAP(I)=1.
           END IF
           SUMT=SUMT+TAP(I)
           SUMT2=SUMT2+(TAP(I)**2.)
 	ENDDO

	FCTAP=SUMT/FLOAT(NR)
	FCTAP2=SQRT(SUMT2/FLOAT(NR))

	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DEFDAT(NR,TAP,FCTAP,FCTAP2,FDATA,NumOfInt,
     &                  DATAIN)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      IMPLICIT NONE
      INTEGER Start,I,NR,NumOfInt
      REAL    AVE,VAR,FCTAP,FCTAP2,FDATA(*),DATAIN(*)
      REAL    TAP(*)       ! TAP IS OF LENGTH NR. TAP im Hauptprogramm
		           ! Deklariert, damit ARRAY hier variabel bleibt

      IF (NR .GT. 512) THEN       ! darray muessen noch umgespult werden
         Start = (NumOfInt-1)*NR  ! erster Wert des aktuellen Intervalles
         DO I=1,NR
            FDATA(I) = DATAIN(Start+I)            
         ENDDO
      ELSE                      ! IF (NR .GT. 512), LOW freqUENCIES
         DO I=1,NR
	    FDATA(I) = DATAIN(I)
         ENDDO
      ENDIF
      DO I=1,NR
         FDATA(I)=FDATA(I)*TAP(I)     ! Cosine-Tapering
      ENDDO
      CALL MOMENTS(FDATA,NR,AVE,VAR)       

      ! Taparing-Effekt auf die Daten wieder entfernen	
      DO I=1,NR
         FDATA(I)=(FDATA(I)-TAP(I)*AVE/FCTAP)/FCTAP2
      ENDDO

C     CALL MOMENTS(FDATA,NR,AVE,VAR)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE MOMENTS(DATA,N,AVE,VAR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	implicit none
	integer j,n
        REAL DATA(*)
	real s,p,ave,var

       	S=0.
	DO J=1,N
           S=S+DATA(J)
	ENDDO
	AVE=S/N

	VAR=0.
	DO J=1,N
           S=DATA(J)-AVE
           P=S*S
           VAR=VAR+P
        ENDDO
	VAR=VAR/(N-1)

	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE REALFT(DATA,N,ISIGN)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	implicit none
	integer n,i,i1,i2,i3,i4,isign,n2p3
        REAL*8 WR, WI, WPR, WPI, WTEMP, THETA, POT
        REAL WRS, WIS, H1R,H2R,H1I,H2I
	real c1,c2
	REAL DATA
	DIMENSION DATA(2*N)

	THETA=3.141592653589793D0/DBLE(N)
        POT = DBLE(2.0)
	C1=0.5

	IF (ISIGN.EQ.1) THEN
           C2=-0.5
           CALL FOUR1(DATA,N,+1)
	ELSE
           C2=0.5
           THETA=-THETA
	END IF

	WPR=-2.0D0*DSIN(0.5D0*THETA)**POT
	WPI=DSIN(THETA)
	WR=1.0D0+WPR
	WI=WPI
	N2P3=2*N+3

	DO 10 I=2,N/2
           I1=2*I-1
           I2=I1+1
           I3=N2P3-I2
           I4=I3+1
           WRS=SNGL(WR)
           WIS=SNGL(WI)
           H1R=C1*(DATA(I1)+DATA(I3))
           H1I=C1*(DATA(I2)-DATA(I4))
           H2R=-C2*(DATA(I2)+DATA(I4))
           H2I=C2*(DATA(I1)-DATA(I3))
           DATA(I1)=H1R+WRS*H2R-WIS*H2I
           DATA(I2)=H1I+WRS*H2I+WIS*H2R
           DATA(I3)=H1R-WRS*H2R+WIS*H2I
           DATA(I4)=-H1I+WRS*H2I+WIS*H2R
           WTEMP=WR
           WR=WR*WPR-WI*WPI+WR
           WI=WI*WPR+WTEMP*WPI+WI
 10	CONTINUE

	IF (ISIGN.EQ.1) THEN
           H1R=DATA(1)
           DATA(1)=H1R+DATA(2)
           DATA(2)=H1R-DATA(2)
	ELSE
           H1R=DATA(1)
           DATA(1)=C1*(H1R+DATA(2))
           DATA(2)=C1*(H1R-DATA(2))
           CALL FOUR1(DATA,N,-1)
	END IF

	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE FOUR1(DATA,NN,ISIGN)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	implicit none
	integer n,nn,m,mmax,i,j,isign,istep
   	REAL*8 WR, WI, WPR, WPI, WTEMP, THETA, POT
	real tempr,tempi
	REAL DATA
	DIMENSION DATA(2*NN)        
        POT = DBLE(2.0)
	N=2*NN
	J=1

	DO I=1,N,2
           IF (J.GT.I) THEN
              TEMPR=DATA(J)
              TEMPI=DATA(J+1)
              DATA(J)=DATA(I)
              DATA(J+1)=DATA(I+1)
              DATA(I)=TEMPR
              DATA(I+1)=TEMPI
           END IF
           M=N/2
 1         IF ((M.GE.2).AND.(J.GT.M)) THEN
              J=J-M
              M=M/2
              GO TO 1
           END IF
           J=J+M
	ENDDO

	MMAX=2
 2	IF (N.GT.MMAX) THEN
           ISTEP=2*MMAX
           THETA=6.28318530717959D0/(ISIGN*MMAX)
           WPR=-2.D0*DSIN(0.5D0*THETA)**POT
           WPI=DSIN(THETA)
           WR=1.D0
           WI=0.D0

           DO M=1,MMAX,2
              DO I=M,N,ISTEP
                 J=I+MMAX
                 TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
                 TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
                 DATA(J)=DATA(I)-TEMPR
                 DATA(J+1)=DATA(I+1)-TEMPI
                 DATA(I)=DATA(I)+TEMPR
                 DATA(I+1)=DATA(I+1)+TEMPI
	      ENDDO
              WTEMP=WR
              WR=WR*WPR-WI*WPI+WR
              WI=WI*WPR+WTEMP*WPI+WI
	   ENDDO
           MMAX=ISTEP
           GO TO 2
	END IF

	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE SPECTR(NR,DT,FDATA,S,SSUM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C INPUT : DT(Messfrequenz), NR(Laenge der Messung), 
C         FDATA(fourriertransformierte Daten)
C OUTPUT: S(Spektrale Dichte),SSUM(Summe der spektralen Dichten)

	IMPLICIT NONE
	INTEGER NR,N2,I,J1,J2
	REAL    DT,FC,SSUM     !Messfreq, Messint,Summe der spektDichte
	REAL    FDATA(*), S(*) ! FDATA: Fourriertrans. Daten
                               ! S: Spektrale Enegiedichte
	FC=FLOAT(NR) * DT
	N2=NR**2
	S(1)=(FDATA(1)**2.)/FLOAT(N2)*FC 
 	S(NR/2+1)=2*(FDATA(2)**2.)/FLOAT(N2)*FC
C	S(NR/2+1)=  (FDATA(2)**2.)/FLOAT(N2)*FC

   	DO I=2,NR/2
 	  J1=(I-1)*2+1
	  J2=J1+1
	  S(I)=2.*(FDATA(J1)**2.+FDATA(J2)**2.)/FLOAT(N2)*FC
 	ENDDO

	SSUM=0.
	DO I=1,NR/2+1
	  SSUM=SSUM+S(I)
 	ENDDO
	SSUM=SSUM/(FLOAT(NR)*DT)

  	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        SUBROUTINE NORMPL(N,DT,F,SU,IS)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
	IMPLICIT NONE
        INTEGER IS,N,IK1,IK2,ILOOP
        REAL DT,SSUU
        REAL F(*),SU(*)

	IF (N .EQ. 1024) THEN
           DO IS=1,5
              F(IS)  = FLOAT(IS)/(DT*FLOAT(N))
C changed, because we want't to write out only F and SU, so that a scaling
C of the values can be done in the plotting-routines
C              FSU(IS)= F(IS)*SU(IS)/ustar2
C              F(IS)  = (F(IS)*height)/UMEAN
           ENDDO
           IS  = 5
           IK1 = 5
           IK2 = 5
        ELSE
           DO IS=1,3
              F(IS)=FLOAT(IS)/(DT*FLOAT(N))
C changed, because we want't to write out only F and SU, so that a scaling
C of the values can be done in the plotting-routines
C              FSU(IS)=F(IS)*SU(IS)/ustar2
C              F(IS)  = (F(IS)*height)/UMEAN
           ENDDO
           IS  = 3
           IK1 = 3
           IK2 = 3
	ENDIF

	DO WHILE (IK2 .LT. N/2.)
           IS  = IS  + 1
           IK1 = IK2 + 1
           IK2 = NINT(1.3*IK1)
           IK2 = IK2 - 1
           IF (IK2 .GT. N/2) IK2 = N/2
           SSUU = 0.
           DO ILOOP = IK1,IK2
              SSUU = SSUU+SU(ILOOP)
           END DO
           SSUU = SSUU/(FLOAT(IK2-IK1+1)) 
           F(IS)   = (FLOAT(IK1)+FLOAT(IK2))/(2.*FLOAT(N)*DT)
           SU(IS) = SSUU
C changed, because we want't to write out only F and SU, so that a scaling
C of the values can be done in the plotting-routines
C           FSU(IS) = F(IS)*SSUU/ustar2
C           F(IS)   = (F(IS)*height)/UMEAN
	END DO	

        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AUSSCHREIBEN DER RESULTATE aus der Berechnung der Spektren
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      SUBROUTINE WRITESPE (var,loc,day,hour,min,height,pm,filter,
     &                     ISHIGH,ISLOW,mean,ustar,tstar,zoverl,
     &                     FHIGH,SUH,FLOW,SUL,DTHIGH,DTLOW,sigma,
     &                     SSUMTOT,SSUM)

      IMPLICIT NONE
      INTEGER var,day,hour,min,ISHIGH,ISLOW,i,filter
      REAL	   height,pm,mean(5),ustar,tstar,zoverl,sigma(5),
     &             FHIGH(*),SUH(*),FLOW(*),SUL(*),DTHIGH,DTLOW,SSUMTOT,
     &             SSUM,SIGMA2
      CHARACTER*1 variable,loc

      SIGMA2= sigma(var)**2. 
      if(var.eq.1) variable = 'u'
      if(var.eq.2) variable = 'v'
      if(var.eq.3) variable = 'w'
      if(var.eq.4) variable = 't'
      if(var.eq.5) variable = 'q'

      WRITE (25,2001) loc,day,hour,min,height,filter,pm
      WRITE (25,2002) ISHIGH,ISLOW,DTHIGH,DTLOW
      WRITE (25,2003) variable,mean(var),ustar,tstar,zoverl
      WRITE (25,*)    SIGMA2,SSUMTOT,SSUM, ' sigma**2 FL, High, Low'

      DO i=1,ISHIGH
         IF (i .LE. ISLOW) THEN
            WRITE (25,*) FHIGH(i),SUH(i),FLOW(i),SUL(i)
         ELSE
            WRITE (25,*) FHIGH(i),SUH(i)
         ENDIF	   	 
      ENDDO

CS:36 K:U R:14 T:12:44 M:DBA Z:12.45 p: 940.23 IH:21 IL:19 
 2001 FORMAT (' Loc:',A1,' Day:',I3.3,' Hour: ',I2.2,':',I2.2,
     &        ' Z:',F5.2,' Filter:',I3,' p:',F7.2)
 2002 FORMAT (' IH:',I2,' IL:',I2,2(1X,E10.3))
 2003 FORMAT (' Var=',A1' U:',E16.8E2,' U*:',E16.8E2,' T*:',E16.8E2,
     &         ' z/L:',E16.8E2)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C calccospec computes cospectra (flux98I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      subroutine calccospectra (av_start,av_end,
     &                       darray,freq,mean,sigma,
     &                       loc,day,hour,min,height,pm,ustar,tstar,
     &                       zoverl,filter,uiuj,uit)

      implicit none
      integer av_start,av_end,arow			! input
      integer day,hour,min,filter 			! input 
      real darray(4500000,5),mean(5),sigma(5)	 	! input
      real freq,height,pm,ustar,tstar,zoverl 		! input
      real uiuj(3),uit(3)
      character*1 loc

      !Internal variables
      INTEGER ITOT,NRHIGH,NRLOW,i1,i2,LL
      parameter(itot=75000,NRHIGH=1024,NRLOW=512)

      REAL    RDATAH1(ITOT),RDATAH2(ITOT),RDATAL1(NRLOW),
     &        RDATAL2(NRLOW),SUH1(NRHIGH/2+1),SUL1(NRLOW/2+1),
     &        SUH2(NRHIGH/2+1),SUL2(NRLOW/2+1),
     &        FDATAH1(NRHIGH),FDATAH2(NRHIGH),FDATAL1(NRLOW),
     &        FDATAL2(NRLOW),
     &        FHIGH(NRHIGH/2+1),FLOW(NRLOW/2+1),
     &        CSPH12(NRHIGH/2+1),CSPL12(NRLOW/2+1),
     &        COSPH12(1024),
     &        TAPHIGH(NRHIGH),TAPLOW(NRLOW),
     &        QSPH12(NRHIGH),QSPL12(NRLOW),
     &        PHH12(NRHIGH),PHL12(NRLOW),AMH12(NRHIGH),AML12(NRLOW),
     &	      COHH12(NRHIGH),COHL12(NRLOW)

      INTEGER NRLOOP,NumOfInt,IINT,IHELP,NforLow
      INTEGER ISLOW,ISHIGH
      REAL    DTHIGH,DTLOW,RHELP1,RHELP2,SSUM1,SSUM2,COSUM12,
     &        FCTAP,FCTAP2,AVE1,AVE2,VAR1,VAR2,COVAR12,CSPSUM12

      arow = av_end-av_start+1
      DTHIGH   = 1./Freq
      NforLow  = 74	          ! Anzahl Werte fuer LOW
      NumOfInt = 37               ! Anzahl Intervalle/halbe Stunde   

C-----------------------------------------------------------------------
      do i1=1,3			  ! Cospektren fuer uw,vw und wT
C-----------------------------------------------------------------------
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	! Initialize
	If (i1.le.2) LL = i1
        If (i1.eq.3) LL = 4
	Nrloop = 1013
	CALL InitTAP(NRHIGH,COSPH12,RDATAL1,FDATAL1)
        CALL InitTAP(NRHIGH,COSPH12,RDATAL2,FDATAL2)
	RHELP2 = 0.
        RHELP1 = 0.
        IHELP  = 1  

	! Umspulen des Werte Array's fuer jeden Kanal und bilden der 
        ! LOW-Freq. Daten
	do i2=1,arow
	   RDATAH1(i2) = darray(i2+av_start-1,LL)	!u,v,T-Werte
           RDATAH2(i2) = darray(i2+av_start-1,3)            !w-Werte
	   If(mod(i2,NforLow).eq.0) then
	     RHELP1=RHELP1+RDATAH1(i2)
             RHELP2=RHELP2+RDATAH2(i2)
	     RDATAL1(IHELP) = RHELP1 / (NforLow*1.)
             RDATAL2(IHELP) = RHELP2 / (NforLow*1.)
             RHELP1 = 0.
             RHELP2 = 0.
             IHELP = IHELP + 1
	   Else
             RHELP1=RHELP1 + RDATAH1(i2)
             RHELP2=RHELP2 + RDATAH2(i2)
           ENDIF 
        Enddo

        CALL DEFTAP (NRLOOP,TAPHIGH,FCTAP,FCTAP2)
        COSUM12 = 0.0

C------------------------------------------------------------------------------
C Umspulen des Werte Array's fuer jeden Kanal und bilden der LOW-Freq. Daten
C------------------------------------------------------------------------------

	! High Frequency
	Do IINT=1,NumOfInt
	  Do i2=1,Nrhigh
	    FDATAH1(i2) = 0.
            FDATAH2(i2) = 0.
	  Enddo
	  CALL DEFDATCOSP(NRLOOP,TAPHIGH,FCTAP,FCTAP2,IINT,
     &			  FDATAH1,FDATAH2,RDATAH1,RDATAH2,
     &			  AVE1,AVE2,VAR1,VAR2,COVAR12)
	  CALL REALFT(FDATAH1,NRHIGH/2,+1)
          CALL REALFT(FDATAH2,NRHIGH/2,+1)
	  CALL SPECTR(NRHIGH,DTHIGH,FDATAH1,SUH1,SSUM1)
          CALL SPECTR(NRHIGH,DTHIGH,FDATAH2,SUH2,SSUM2)
	  CALL COSPCT(NRHIGH,DTHIGH,FDATAH1,FDATAH2,SUH1,SUH2,
     &		     CSPH12,QSPH12,AMH12,PHH12,COHH12,CSPSUM12)
	  COSUM12 = COSUM12 + CSPSUM12	    
	  DO I2=1,NRHIGH/2+1
	     COSPH12(I2) = COSPH12(I2) + CSPH12(I2)
	  ENDDO
	ENDDO	

	!Mitteln der spektralen Dichte
	DO I2=1,NRHIGH/2+1
	   CSPH12(I2) = COSPH12(I2)/NumOfInt
	ENDDO	   
	COSUM12 = COSUM12/NumOfInt
	 IF (i1.eq.1) write(*,'(2F10.5)') COSUM12,uiuj(1)
         IF (i1.eq.2) write(*,'(2F10.5)') COSUM12,uiuj(2)
         IF (i1.eq.3) write(*,'(2F10.5)') COSUM12,uit(3)
	CALL NORMPL(NRHIGH,DTHIGH,FHIGH,CSPH12,ISHIGH)

	! Low Frequency
        NRLOOP = 506           ! Anzahl gefuellter Array's  
        DTLOW  = NforLow * DTHIGH
        CALL DEFTAP (NRLOOP,TAPLOW,FCTAP,FCTAP2)
	CALL DEFDATCOSP(NRLOOP,TAPLOW,FCTAP,FCTAP2,IINT,
     &                    FDATAL1,FDATAL2,RDATAL1,RDATAL2,
     &                    AVE1,AVE2,VAR1,VAR2,COVAR12)
        CALL REALFT(FDATAL1,NRLOW/2,+1)
        CALL REALFT(FDATAL2,NRLOW/2,+1)
        CALL SPECTR(NRLOW,DTLOW,FDATAL1,SUL1,SSUM1)
        CALL SPECTR(NRLOW,DTLOW,FDATAL2,SUL2,SSUM2)
c        write(*,'(2F12.4)') VAR1,SSUM1
c        write(*,'(2F12.4)') VAR2,SSUM2
        CALL COSPCT(NRLOW,DTLOW,FDATAL1,FDATAL2,SUL1,SUL2,
     &               CSPL12,QSPL12,AML12,PHL12,COHL12,CSPSUM12)
        CALL NORMPL(NRLOW,DTLOW,FLOW,CSPL12,ISLOW)

C---- END Low Frequency ----------------------
	CALL WRITECOSPE(loc,day,hour,min,height,pm,filter,LL,
     &                  ISHIGH,ISLOW,mean,ustar,tstar,zoverl,FHIGH,
     &                  CSPH12,FLOW,CSPL12,DTHIGH,DTLOW,sigma,COSUM12,
     &                  uiuj)
C-----------------------------------------------------------------------
      ENDDO       ! LOOP UEBER jrow
C-----------------------------------------------------------------------

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DEFDATCOSP(NR,TAP,FCTAP,FCTAP2,NumOfInt,FDATA1,
     &			    FDATA2,RDATA1,RDATA2,AVE1,AVE2,VAR1,
     &			    VAR2,COVAR12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      IMPLICIT NONE
      INTEGER Start,I,NR,NumOfInt
      REAL    AVE1,AVE2,VAR1,VAR2,COVAR12,FCTAP,FCTAP2
      REAL    FDATA1(*),FDATA2(*),RDATA1(*),RDATA2(*),TAP(*)

      IF (NR .GT. 512) THEN       ! Werte muessen noch umgespult werden
         Start = (NumOfInt-1)*NR  ! 37 Intervalle (numofint) 1013-Werte
         DO I=1,NR
            FDATA1(I) = RDATA1(Start+I)
            FDATA2(I) = RDATA2(Start+I)
        ENDDO
      ELSE                      ! IF (NR .GT. 512), LOW FREQUENCIES
         DO I=1,NR
            FDATA1(I) = RDATA1(I)
            FDATA2(I) = RDATA2(I)
          ENDDO
      ENDIF

      DO I=1,NR
         FDATA1(I)=FDATA1(I)*TAP(I)     ! Cosine-Tapering
         FDATA2(I)=FDATA2(I)*TAP(I)
      ENDDO
      CALL MOMENTS(FDATA1,NR,AVE1,VAR1)       
      CALL MOMENTS(FDATA2,NR,AVE2,VAR2)
      DO I=1,NR
         FDATA1(I)=(FDATA1(I)-TAP(I)*AVE1/FCTAP)/FCTAP2
         FDATA2(I)=(FDATA2(I)-TAP(I)*AVE2/FCTAP)/FCTAP2
      ENDDO
      CALL MOMENTS(FDATA1,NR,AVE1,VAR1)       
      CALL MOMENTS(FDATA2,NR,AVE2,VAR2)
      CALL CROSSMOM(NR,FDATA1,FDATA2,AVE1,AVE2,COVAR12)

      RETURN
      END	

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE CROSSMOM(N,DATA1,DATA2,AVE1,AVE2,COVAR12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

	implicit none
	integer n,j
      REAL DATA1(N), DATA2(N)
      REAL AVE1, AVE2, COVAR12
	real s1,s2

      DO J=1,N
         S1=DATA1(J)-AVE1
         S2=DATA2(J)-AVE2
         COVAR12=COVAR12+S1*S2
      enddo

      COVAR12=COVAR12/(N-1)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE COSPCT(NR,DT,FDATA1,FDATA2,S1,S2,CSP12,QSP12,AM12,
     &		 PH12,COH12,CSPSUM12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      IMPLICIT NONE
        REAL DT,PI
        REAL CSPSUM12,FC
        REAL FDATA1(*), FDATA2(*)
        REAL S1(*),S2(*)
        REAL CSP12(*), QSP12(*)
        REAL AM12(*), PH12(*), COH12(*)
	INTEGER NR,N2,I,J1,J2

        PI=4.*ATAN(1.)
        FC=(DT/FLOAT(NR))
        N2=1
        CSP12(1)=(FDATA1(1)*FDATA2(1))/FLOAT(N2)*FC
        QSP12(1)=0.
        CSP12(NR/2+1)=2.*(FDATA1(2)*FDATA2(2))/FLOAT(N2)*FC
        QSP12(NR/2+1)=0.
        DO I=2,NR/2
                J1=(I-1)*2+1
                J2=J1+1
                CSP12(I)=2.*(FDATA1(J1)*FDATA2(J1)+
     &                       FDATA1(J2)*FDATA2(J2))
     &                     /FLOAT(N2)*FC
                QSP12(I)=2.*(FDATA1(J2)*FDATA2(J1)-
     &                       FDATA1(J1)*FDATA2(J2))
     &                     /FLOAT(N2)*FC
	enddo

        DO I=1,NR/2+1
          AM12(I)=SQRT(CSP12(I)**2+QSP12(I)**2)
          IF (CSP12(I).EQ.0.) THEN
            IF (QSP12(I).GT.0.) THEN
              PH12(I)=PI/2.
            ELSE IF (QSP12(I).LT.0.) THEN
              PH12(I)=-PI/2.
            ELSE
              PH12(I)=0.
            END IF
          ELSE
            IF (CSP12(I).GT.0.) THEN
              PH12(I)=ATAN(QSP12(I)/CSP12(I))
            ELSE 
              IF (QSP12(I).GE.0.) THEN
                PH12(I)=ATAN(QSP12(I)/CSP12(I))+PI
              ELSE
                PH12(I)=ATAN(QSP12(I)/CSP12(I))-PI
              ENDIF
            END IF 
          END IF
          IF ((S1(I) .gt. 1.E-15).AND.(S2(I) .gt. 1.E-15)) THEN
            COH12(I)=AM12(I)/SQRT(S1(I)*S2(I))
          ELSE
            COH12(I)=0.
          END IF
	enddo
        CSPSUM12=0.

        DO I=1,NR/2+1
          CSPSUM12=CSPSUM12+CSP12(I)
	enddo
        CSPSUM12=CSPSUM12/(FLOAT(NR)*DT)

        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AUSSCHREIBEN DER RESULTATE aus der Berechnung der Cospektren
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

      SUBROUTINE WRITECOSPE(loc,day,hour,min,height,pm,filter,LL,
     &                  ISHIGH,ISLOW,mean,ustar,tstar,zoverl,FHIGH,
     &                  CSPH12,FLOW,CSPL12,DTHIGH,DTLOW,sigma,COSUM12,
     &                  uiuj)

      IMPLICIT NONE
      INTEGER      day,hour,min,ISHIGH,ISLOW,LL,II,filter
      REAL         height,pm,mean(5),ustar,tstar,zoverl,sigma(5),
     &             FHIGH(*),CSPH12(*),FLOW(*),CSPL12(*),DTHIGH,DTLOW,
     &		   COSUM12,sigmax2,sigmaw2,uiuj(*)
      character*1 variable,loc

      if(LL.eq.1) variable='u'
      if(LL.eq.2) variable='v'
      if(LL.eq.4) variable='t'
      sigmax2= sigma(LL)**2.
      sigmaw2= sigma(3)**2.

      WRITE (26,*) variable,'w'
      WRITE (26,2001) loc,day,hour,min,height,filter,pm
      WRITE (26,2002) ISHIGH,ISLOW,DTHIGH,DTLOW
      WRITE (26,2003) mean(LL),ustar,tstar,zoverl
      WRITE (26,*)    sigmax2,sigmaw2,' sigma**2 u,(v,T)  w'

      DO II=1,ISHIGH
         IF (II .LE. ISLOW) THEN
            WRITE (26,*) FHIGH(II),CSPH12(II),FLOW(II),CSPL12(II)
         ELSE
            WRITE (26,*) FHIGH(II),CSPH12(II)
         ENDIF           
      ENDDO

CS:36 K:U R:14 T:12:44 M:DBA Z:12.45 p: 940.23 IH:21 IL:19 
 2001 FORMAT (' Loc:',A1,' Day:',I3,' Hour:',I2.2,':',I2.2,
     &        ' Z:',F5.2,' Filter:',I3,' p:',F7.2)
 2002 FORMAT (' IH:',I2,' IL:',I2,2(' ',E10.3))
 2003 FORMAT ('AV:',E16.8E2,' U*:',E16.8E2,' T*:',E16.8E2,' z/L:',
     &         E16.8E2)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC						
CCC  Functions
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  function sfeich (NK flux98I)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Berechnet die H2O dichte aus dem Ly a oder dem Krypton
! Hygrometer output signal und daraus die spezifische feuchte.

 	real function sfeich(x,dd,roro)

	implicit none
	real x,dd,roro(5000)	!input 
	real ro,rr
	integer ii

	! berechne spezifische feuchte q aus der wasserdampfdichte ro
	ii = nint(x)
	ro = roro(ii)
	rr=ro/dd
	sfeich=rr/(1.+rr)

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     POT. TEMPERATUR BERECHNEN (flux98I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	real function pottemp(pres,tepr)

	implicit none
	real*4    tepr,pres,kapa
	parameter (kapa=0.286)

	pottemp  = tepr *((1000./pres)**kapa)

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  FUNKTION BAR BERECHNET DEN DRUCK AUF HOEHE DZ (TURMNIVEAUS) AUFGRUND
C  DER TEMP UND DEM DRUCK (P0) AUF HOEHE Z0, MIT DER BAROMETERFORMEL
C  (Z0 = m a.s.l des Camps) (flux98I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	real function bar(dz,temp,p0)

	implicit none
	real dz,temp,p0,expon      

	expon =-1.*dz/(8000.*temp/273.16)
	bar = p0*exp(expon)

	return
	end
