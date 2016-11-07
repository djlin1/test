;************************************************************************
;
;  4dlib/FITTING/sigrho.pro
;
;  created:
;
;  modified:
;    8/26/96     Ted Terpstra - Corrected h=p*h equation.
;                               Went back to old calculation.

;    8-09-96     K. Greene -- Add use of function TEMPORARY
;
;
;***********************************************************************

function sigrho, sigma, ndim

h = fltarr(ndim)
p = exp(sigma)

knotloc = fltarr(ndim+2)

z = 1. + p(ndim-1)

for i=ndim-2,0,-1 do z=1.+p(i)*z

h(0)=1./z

for i=1,ndim-1 do   h(i)=p(i-1)*h(i-1)

;------------------------------------------------------------------
;
; more efficient way to do above 2 lines		8-7-96  klg
;
;h = p * temporary(h)            ; h is zero  ! 
;h(0)=1./z
;-------------------------------------------------------------------

knotloc(0)      = 0.
knotloc(ndim+1) = 1.

for i=1,ndim do knotloc(i) = h(i-1)+knotloc(i-1)

return, knotloc
end