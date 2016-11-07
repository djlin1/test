;*************************************************************************
;
;  4dlib/FITTING/amotry.pro
;
;  created:
;
;  modified:
;    6-17-96	removed AMOTRY from AMOEBA.PRO and from FAMOEBA.PRO and
;		put in separate procedure
;
;
;*************************************************************************

function amotry,p,y,function_name,psum,ihi,fac

ndim= n_elements(y) - 1
ptry=fltarr(ndim)
fac1=(1.-fac)/ndim
fac2=fac1-fac

for j=0,ndim-1 do ptry(j)=psum(j)*fac1-p(ihi,j)*fac2

ytry=call_function(function_name,ptry)

if (ytry lt y(ihi)) then begin
  y(ihi)=ytry
  for j=0,ndim-1 do begin
    psum(j)=psum(j)-p(ihi,j)+ptry(j)
    p(ihi,j)=ptry(j)
  endfor
endif

amotry=ytry

return,amotry
end
