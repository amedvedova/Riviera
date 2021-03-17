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




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC						
CCC  Subroutines
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C location
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Further informations retrieved from the filename:
! Kry = 1 means krypton is present, nxxx are serial numbers needed for 
! calibrations, analog is the number of analogue inputs of this 
! sonic. The measurement height with respect to the surroundings and 
! the absolute height (lheight) are also given.


	subroutine location(loc,lday,height,lheight,rheight,
     &                      nord,nsonic,nkry,kry,analog)

	implicit none
	integer lday  				!input
	integer nord,nsonic,nkry,kry,analog	!output
	real height,lheight,rheight		!output
	character*1 loc  			!input

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
	 elseif (loc.eq.'C' .or. loc.eq.'c') then
	   height = 27.63
	   lheight = 250.
	   rheight = 27.
	   nord = 170
	   nsonic = 47
	   nkry = 1300
	   kry = 1
	   analog = 1
	 elseif (loc.eq.'F' .or. loc.eq.'f') then
	   height = 23.78
	   lheight = 760.
	   rheight = height + lheight-250.
	   nord = 180
	   nsonic = 30
	   nkry = 1370
	   kry = 1
	   analog = 3
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

	do aday = fday,lday
	   irow = 0
 	   do ahour=ihour,23		

	      write(*,*)'-> Getting raw data from ',actual_file,' ..'
	      call getdata(path_file,path_ref,irow,rheight,arr,
     &                     tm,pm,rhm,rm,nm)
	      write(*,*)'   .. loaded file has',irow,' rows'
	      t(count) = tm
	      p(count) = pm
	      rh(count) = rhm
	      rain(count) = rm
	      net(count) = nm

	      ! Computations on the raw data
	      call meanhum(tm,pm,rhm,rom,em,qm,dd)
	      call conversions(irow,em,pm,arr)
	      call turn_in_meteo(irow,nord,arr)
	      if(kry.eq.1) then
	        write(*,*)'-> Computing krypton data'	  
		start = 1
		end = irow/2
		call period_average1(start,end,jrow,arr,mean)
	        call krypton(start,end,mean,rom,dd,arr,nkry,lday)
		start = irow/2+1
		end = irow
		call period_average1(start,end,jrow,arr,mean)
	        call krypton(start,end,mean,rom,dd,arr,nkry,lday)
	      endif





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Krypton
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine krypton(start,end,mean,rom,dd,arr,nkry,lday)   

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
	call lookupkry(rlnvm,rom,b0,b1,b2,rkwd,rkww,rlnv0d,rlnv0w,xpk,ro) 	! output is ro(5000)
	if(mean(5).ge.1.) then
	  mean(5) = sfeich(mean(5),dd,ro)
	else
	  mean(5) = sfeich(1.,dd,ro)
	endif
	do i=start,end
	  if(arr(i,5).ge.1.) then
          arr(i,5) = sfeich(arr(i,5),dd,ro) 
	  else
	    arr(i,5) = sfeich(1.,dd,ro)
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

