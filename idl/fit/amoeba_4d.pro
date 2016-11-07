;************************************************************************
;
;  4dlib/FITTING/amoeba_4d.pro
;
;  created:
;
;  modified:
;
;  20130418 tbt - Changed Error message.... better printing
;  11/24/98 TBT - Tim Luce Change:
;	 I recommend the following changes to the amoebainit routine in the
;	amoeba_4d.pro file.  The line which now reads:
;
;	  p(i,i-1)=p(0,i-1) + .1
;  
;	should be modified to:
;
;	  p(i,i-1)=p(0,i-1) - .5
;  
;	The following blurb can be used to explain the difference to the masses:
;
;	The fitting routine in the 4D profile fitting suite has been modified to
;	enhance the fitting speed and quality.  The original incarnation had an
;	initial condition which sampled a fairly small region in radius and also
;	initiated searches with knot values at smaller radii that the initial
;	guess.  The change which has been made starts the searches with a larger
;	variation in the initial guesses and biases the guesses to larger radii.
;	It is hoped that the larger initial steps and change in direction will 
;	enable the fitter to find the optimum fit more robustly and quickly.  The
;	major potential problem is that the same initial guesses will not likely
;	have the same optimization with the old fitter and the new.  Therefore,
;	reproducing old fits using the autoknot feature will likely be impossible.
;	The fitter will reproduce the exact fit if the exact knot locations are
;	used.  Questions can be directed to Tim Luce.;
;
;  12/04/97     Ted Terpstra - Changed name from amoeba.pro to amoeba_4d.pro
;               because IDL 5.0 has a built-in function amoeba.
;   6-17-96	K. Greene -- removed function amotry and put in
;		separate source file.
;   5/08/96 	TBT Added more print to y Gt maxy.
;   2/01/96 	tbt Added test for y GT maxy
;  12/05/95 	tbt Added model as argument to famoeba.
;  10/13/95 	tbt Renamed all Amoebas to famoebas.
;               Added common splinecom_dens to function amoeba
;  10/05/95 	tbt Looking at bad subsrcipt (-1) to y.
;               If input Y is all 0, return with error
;
;
;***********************************************************************

pro amoebainit, pguess, function_name, p, y

  nparam = n_elements(pguess)

  y = fltarr(nparam + 1)
  p = fltarr(nparam+1,nparam)
 
  y(0)   = call_function(function_name, pguess)

  p(0,*) = pguess

  for i=1,nparam do begin
    p(i,*) = pguess

;   p(i,i-1) = p(0,i-1) + .1   ; Pre 11/24/98;;; tbt See comment above
    p(i,i-1) = p(0,i-1) - .5

    y(i) = call_function( function_name, p(i,*))
  endfor

  return
end

;
;**********************************************************************
;

function amoeba_4d, pguess, function_name, ftol, iter, ierr, itmax

;; 20110223 tbt Added icnt
;; 19951205 tbt added model as an argument  commented out @density_common
;;; @density_common

;;itmax=200
;itmax=model.maxit

;Message, /Continue, '<--------------------------------'
;Help, pguess
;help, function_name

  Common T_local, icnt


  ierr = 0
  psolution = pguess

  amoebainit, pguess, function_name, p, y

  ndim=   n_elements(pguess)        ;numknot-2
  iter=0

jump1: 

  psum=total(p,1)

jump2:

  miny = min(y,ilo)
  maxy = max(y,ihi)

  IF (miny EQ maxy) AND (miny EQ 0) THEN BEGIN
     PRINT, 'AMOEBA_4d:  ERROR - y is all 0 - return with error'
     ierr=1
     return, p(0,*)    ; tbt
  ENDIF     ; miny

  ;nextmax=max(y(where(y lt maxy)))

  ytemp = where(y lt maxy)
  yt = Min([ytemp,0])   ; Make an array in case ytemp =-1  tbt

  If (yt Lt 0) Then Begin
    ny = N_Elements(y)

;    PRINT, 'AMOEBA_4D:  ERROR - y GE maxy - return with error'
;    Print, 'AMOEBA_4D:        maxy, min(y), max(y), Ny, y =',   $
;                      maxy, Min(y), Max(y), ny, y(0:ny-1<5) 

    print
    Message, /Continue, 'ERROR - all y GE maxy - return with error.'
    Print, '   maxy, min(y), max(y) =',  maxy, Min(y), Max(y) 
    print, '   Ny, y =',   ny, y(0:ny-1<5)

;    help, pguess, function_name, p, y
;    Print, '   Traceback:'
;    Help, /TRACEBACK
;    print
 
    ierr=1
    return, p(0,*)    ; tbt
  ENDIF     ; ytemp

  nextmax = max(y(ytemp))
  inhia   = where(y eq nextmax)
  inhi    = inhia(0)
  rtol    = 2.*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo)))

  if (rtol lt ftol) then begin
    swap     = y(0)
    y(0)     = y(ilo)
    y(ilo)   = swap
    swapa    = p(0,*)
    p(0,*)   = p(ilo,*)
    p(ilo,*) = swapa
    return, transpose(p(0,*))
  endif

  iter=iter+2

  if (iter gt itmax) then begin
    if N_Elements(icnt) Lt 1 Then icnt = 0
    icnt = icnt+1
    print,'AMOEBA_4D: Error:  ITMAX =', itmax, $
	  	   ', was exceeded by ',       $
                   iter, icnt
    ierr = 1
    return, p(0,*)
  endif

  ytry=amotry(p,y,function_name,psum,ihi,-1.0)

  if (ytry le y(ilo)) then begin
    ytry=amotry(p,y,function_name,psum,ihi,2.0)
  endif else begin
    if (ytry ge y(inhi)) then begin
      ysave = y(ihi)
      ytry  = amotry(p,y,function_name,psum,ihi,0.5)
      if (ytry ge ysave) then begin
        for i=0,ndim do begin
          if (i ne ilo) then begin
            psum = 0.5*(p(i,*)+p(ilo,*))
            y(i) = call_function(function_name,psum)
          endif
        endfor
        iter=iter+ndim
        goto, jump1
      endif
    endif else iter=iter-1
  endelse

  goto, jump2

;-------------------------------

  Message, /Continue, 'Amoeba_4d: returning at 1 _______ But should never get here!!!!!'

  psolution = transpose(p(0,*))

  return, psolution
end

