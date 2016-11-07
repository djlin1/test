;**********************************************************************
;
;  4dlib/FITTING/rhosig.pro
;
;  created:
;
;  modified:
;
;
;**********************************************************************

function rhosig,rho,numknot

sigma=fltarr(numknot-2)

for i=0,numknot-3 do sigma(i)=alog((rho(i+2)-rho(i+1))/(rho(i+1)-rho(i)))

return,sigma
end