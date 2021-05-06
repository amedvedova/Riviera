C==============================================================================
	program ReadF
C==============================================================================

	implicit none

	integer fday,lday,aday
	integer fhour,lhour,ahour,ihour,fmin,amin 
	integer kry,analog,nord,nsonic,nkry
	integer av_period,av_intervalls,avint
	integer irow,arow,av_start,av_end
	integer filter,fillen
	real height,h,a1,p01,p02,t01,t02,rh1,rh2
        real rain01,rain02,net01,net02,t1,t2
	
	real mean(7)	
	real array(75600,7)
	real x,y
	

	character*1 loc
	character*7 out_file
	character*50 actual_file
	character*50 actual_ref
	
	character*66 path_file
	character*57 path_ref
	character*12 output_file
	character*21 path_ref_new

	parameter(x=4500,y=1000)

	logical existence
	


C----------------------------------------------------------------------------


C Main   
C----------------------------------------------------------------------------


	! User inputs
10	call dialog(loc,fday,fhour,fmin,lday,lhour,av_period)
	call location(loc,lday,height,h,nsonic,nkry,kry,analog)

	write(*,*)'---------------------------------------------------',
     &  '-----------------------'
	write(*,*)'Loc   Height   Sonic  Krypton  Analogin'
	write(*,'(2X,A,3X,F7.2,5X,I2,5X,I4,6X,I1,7X,I3)')loc,
     &  height,nsonic,nkry,analog
	write(*,*)'---------------------------------------------------',
     &  '-----------------------'

	
 	ihour = fhour	! first day starts from hour 'fhour'
	amin = fmin	! first file can have min not equal 0

C-----------------------------------------------------------------------------
C loop1: day loop
C-----------------------------------------------------------------------------
	do aday = fday,lday

C-----------------------------------------------------------------------------
C loop2: hour loop, flexible, not always 24h (0-23)
C-----------------------------------------------------------------------------
	   do ahour=ihour,23		
	      if (ahour.gt.lhour .and. aday.eq.lday) goto 1000


	      ! Input file name and path
	      write(actual_file,'(A,I3.3,I2.2,I2.2,A4)')loc,
     &        aday,ahour,amin,'.slt'
	      actual_ref  = actual_file(1:8)//'.ref'
	      path_file = '/cdrom/week42e_g/f/f'//actual_file(2:12)


c	      path_ref  = 'ref/'//loc//'0/'//actual_ref

      

	      ! Get binary raw data and reference data
	      call filesexist(path_file,existence)		
	      if(.not.existence) go to 1000	
	      write(*,*)
	      write(*,*)'Getting raw data from ',actual_file,' ..'
	      write(*,*)
	      call getdata(path_file,array,irow)
	      write(*,*)'-> Loaded file has',irow,' rows'	  

	open(50,file='test291298.dat',status='unknown')
c	open(40,file=path_ref,status='old')
c 	read(40,*)a1,a1,p01,t01,rain01,net01 
c	read(40,*)a1,a1,p02,t02,rain02,net02
c	write(*,*)p01,t01,rain01,net01
c	write(*,*)p02,t02,rain02,net02

	close(40)

c       Abwechslungsweise e,f oder g aktiv machen, die anderen kommentieren
	
c	open(45,file='ref/E/E'//actual_ref(2:12),status='unknown')
c	open(46,file='ref/F/F'//actual_ref(2:12),status='unknown')
c	open(47,file='ref/G/G'//actual_ref(2:12),status='unknown')

	      call conversions(irow,array)
c	      call check(irow,array)
C-----------------------------------------------------------------------------
C loop3: averaging intervals loop (2*30min,3*20,4*15,5*12,6*10...)
C-----------------------------------------------------------------------------
	      av_intervalls = 60/av_period
	      arow = nint(float(av_period)*60.*1000./48.)

	      do avint=1,av_intervalls
		av_start = (avint-1)*arow+1
		av_end   = av_start+arow-1


		! Check if file is finishing 
		if(av_start.ge.irow) goto 100	
		if(av_end.gt.irow) then
		   if((av_end-irow).le.1000) then
		     av_end = irow
		   else
		     goto 100
		   endif
		endif 
		

		! Computations on Averaging interval data
	        write(*,'(1X,A12,A8,I2,A10,I5,A4,I5)')'-> Prepare ',
     &          'interval',avint,' from row ',av_start,' to ',av_end

	        call period_average(mean,av_start,av_end,array)
	print*,mean(6),mean(7)
C	mean(6)=T new , mean(7)=RH new

	if(avint.eq.1) then

	   t1=mean(6)
	   rh1=mean(7)

c	   write(45,200) t1,rh1,p01,t01,rain01,net01
c	   write(46,200) t1,rh1,p01,t01,rain01,net01
c	   write(47,200) t1,rh1,p01,t01,rain01,net01
	   write(50,'(i4,1x,i4,f7.3,1x,f7.3)')
     &         aday,ahour,t1,rh1

	elseif(avint.eq.2) then

	   t2=mean(6)
	   rh2=mean(7)
c          write(45,200) t2,rh2,p02,t02,rain02,net02
c	   write(46,200) t2,rh2,p02,t02,rain02,net02
c	   write(47,200) t2,rh2,p02,t02,rain02,net02
	   write(50,'(i4,1x,i4,f7.3,1x,f7.3)')
     &         aday,ahour,t2,rh2

	   else

	      write(*,*)'STOP'

	endif

	      Enddo
		
	      close(45)
	      close(46)
	      close(47)
	    
 200	      Format(f7.3,1x,f7.3,1x,f8.3,1x,f8.3,1x,
     &         f5.2,1x,f9.4)

C-----------------------------------------------------------------------------
C end loop3
C-----------------------------------------------------------------------------
100           continue
	      amin = 0  ! starting from the second hour all have min = 0
	   enddo
C-----------------------------------------------------------------------------
C end loop2
C-----------------------------------------------------------------------------
     	   ihour = 0	! starting from the second day all have ihour = 0
	enddo
C-----------------------------------------------------------------------------
C end loop1
C-----------------------------------------------------------------------------
1000	continue
	close(12)
	close(50)

	end  










CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC						
CCC  Subroutines
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C dialog
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine dialog(loc,fday,fhour,fmin,lday,lhour,
     &  av_period)

	implicit none
	integer fday,fhour,lday,lhour,av_period,fillen  !---Output
	integer fmin
	character*1 loc 			 	!---Output

	! Local variables
	character*1 floc,lloc
	integer izeil


	! Screen   
9	write(*,*) 'Type names of Edisol binary files you'
        write(*,*) 'want to read (capital,without .SLT !):'
	write(*,*)
        write(*,'(A55,$)')'First file ?   '
        read (*,'(A1,I3,I2,I2)') floc,fday,fhour,fmin
	write(*,'(A55,$)')'Last file ?   '
        read  (*,'(A1,I3,I2)') lloc,lday,lhour
19	write(*,'(A55,$)')'Averaging period (min) ?   '
	read (*,'(I2)') av_period



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
	   write(*,*)'Check averaging period (5,6,12,15,...,60)!'
	   write(*,*)
	   go to 19 
	  endif
	else
	   write(*,*)'--------------------------------'
	   write(*,*)'Location incorrect (A-H or a-h)!'
	   write(*,*)
	   go to 9
	endif

	return
	end 





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C location
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Further informations retrieved from the filename:
! Kry = 1 means krypton is present, nxxx are serial numbers needed for 
! calibrations, analog is the number of analogue inputs of this 
! sonic. The measurement height with respect to the surroundings and 
! the absolute height (lheight) are also given.

	subroutine location(loc,lday,height,h,nsonic,nkry,kry,analog)

	implicit none
	integer lday  				!---Input
	integer nord,nsonic,nkry,kry,analog	!---Output
	real h					!---Output
	character*1 loc  			!---Input

	! Local variables
	integer lheight
	real height


	! San Vittore (sonic calibration experiment)
	if (lday.le.197) then
	  height = 1.80
C	  lheight = 300 
	  h = 0.1		! reference P measured in SVittore
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
	   lheight = 250 
	   h = height
	   nord = 165
	   nsonic = 68
	   nkry = 1299
	   kry = 1
	   analog = 1
	 elseif (loc.eq.'B' .or. loc.eq.'b') then
	   height = 15.58
	   lheight = 250
	   h = height
	   nord = 115 
	   nsonic = 69
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'C' .or. loc.eq.'c') then
	   height = 27.63
	   lheight = 250
	   h = height
	   nord = 170
	   nsonic = 47
	   nkry = 1300
	   kry = 1
	   analog = 1
	 elseif (loc.eq.'D' .or. loc.eq.'d') then
	   height = 6.5
	   lheight = 340 
	   h = height + float(lheight-250)
	   nord = 115
	   nsonic = 54
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'E' .or. loc.eq.'e') then
	   height = 15.34
	   lheight = 760
	   h = height + float(lheight-250)
	   nord = 180
	   nsonic = 35
	   nkry = 0
	   kry = 0
	   analog = 0
	 elseif (loc.eq.'F' .or. loc.eq.'f') then
	   height = 23.78
	   lheight = 760
	   h = height + float(lheight-250)
	   nord = 180
	   nsonic = 30
	   nkry = 1370
	   kry = 1
	   analog = 3
	 elseif (loc.eq.'G' .or. loc.eq.'g') then
	   height = 29.75
	   lheight = 760
	   h = height + float(lheight-250)
	   nord = 180
	   nsonic = 36
	   nkry = 0
	   kry = 0
	   analog = 0
	 endif
	endif


	return
	end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C filesexist 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Inquires if files needed exist

	subroutine filesexist(path_file,existence)	
	
	implicit none
	character*66 path_file		!---Input
	logical existence		!---Output

	! Inquires if rawdata and ascii files exist
	inquire (file=path_file, exist=existence)
	if (.not.existence) then
	  write(*,*)
	  write(*,*)'%%%'
	  write(*,*)'%!% File ',path_file,
     &    ' does not exist!  End of execution.'	   
	  write(*,*)'%%%'
	  write(*,*)
	endif

	return
	end 




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C getdata
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Gets the raw data and the references.

	subroutine getdata(path_file,array,irow)

	implicit none
	integer irow			!---Output
	real array(75600,7)		!---Output
	character*66 path_file		!---Input

	! Local variables
	integer*2 i,size,x, y, ifb
	integer*2 iibits

	integer irec,k

	! Opens rawdata and reads 
        open (10,file=path_file,status='old',form='unformatted',
     &      access='direct',recl=2,err=999)   
        
	! Reads the header of one .SLT data file (1h)
        irec=0
	irec=irec+1
        read (10,rec=irec) size
        ifb = iibits(size,8,15)


	! Reads the .SLT data file (1h) and puts it in the array(k,i)
	k = 0 
        irec=irec+ifb/2-1
899       k=k+1
          do i=1,ifb/2
              irec=irec+1
              read (10,rec=irec,end=999) x
              y = iibits(x,8,8)+2**8*iibits(x,0,8)
	      array(k,i) = float(y)
           end do
        go to 899
999     continue
 	irow = k-1
	close (10)

	return
	end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C conversions  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Retrieves the temperature [K] from the sound speed, computes the potential
! temperature and could convert the analog Inputs voltage.

	subroutine conversions(irow,array)

	implicit none
	integer i,irow			!---Input
	real array(75600,7)	!---In-Output

	do i=1,irow
	  array(i,6) = (array(i,6)-400.)/10.
	  array(i,7) = array(i,7)/10.	!aus RTVORB in Flux98I
	enddo

	return
	end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C check data  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine check(irow,array)

	implicit none
	integer i,irow,x,y		!---Input
	real array(75600,7)	!---Input
	parameter (x=4500,y=1000)

	do i=1,irow
	  if (array(i,6).lt.0.) array(i,6)=0.
	  if (array(i,6).gt.40.) array(i,6)=40.
	  if (array(i,7).lt.0.) array(i,7)=0.
	  if (array(i,7).gt.100.) array(i,7)=100.

	enddo



	return
	end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C period_average
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine period_average(mean,av_start,av_end,array)

	implicit none
	integer av_start,av_end		!---Input
	real array(75600,7)		!---Input
	real mean(7)	 		!---Output

	! Local variables
	integer i,j 
	real sum(7)

	
	do j=1,7
	  sum(j) = 0.
	  mean(j) = 0.
	enddo

	do j=1,7
	  do i=av_start,av_end
	    sum(j) = sum(j) + array(i,j)
	  enddo
	  mean(j) = sum(j)/float(av_end-av_start+1)
	enddo

	return
	end
	
C flux98I
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     POT. TEMPERATUR BERECHNEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	real function pottemp(pres,tepr)

	implicit none
	real*4    tepr,pres,kapa
	parameter (kapa=0.286)

	pottemp  = tepr *((1000./pres)**kapa)

	return
	end



C flux98I
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  FUNKTION BAR BERECHNET DEN DRUCK AUF HOEHE DZ (TURMNIVEAUS) AUFGRUND
C  DER TEMP UND DEM DRUCK (P0) AUF HOEHE Z0, MIT DER BAROMETERFORMEL
C  (Z0 = m a.s.l des Camps)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	real function bar(dz,temp,p0)

	implicit none
	real dz,temp,p0,expon      

	expon =-1.*dz/(8000.*temp/273.16)
	bar = p0*exp(expon)

	return
	end











