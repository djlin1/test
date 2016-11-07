FUNCTION getrho,shot,time,Rvals,runid=runid
;.compile /u/collinscs/FIDA/bagfida/ccedit/CGRAD/getrho.pro

;shot=159249
;time=600. ;ms
;coreview=[1.709,1.807,1.752,1.856,1.908,2.19]
;edgeview=[2.013,2.066,2.116,2.169,2.221,2.19]
;fullview=[1.709,1.807,2.116,2.169,2.221,2.19]
;Rvals=coreview

IF ~KEYWORD_SET(runid) THEN runid='efit02'

Zvals=0.*Rvals

g=readg(shot,time,runid=runid)

mult=fltarr(N_ELEMENTS(Rvals))+1.0
FOR j=0,N_ELEMENTS(Rvals)-1 DO BEGIN
IF g.rmaxis GT Rvals[j] THEN mult[j]=-1.0
ENDFOR
a=rho_rz(g,Rvals,Zvals,/norm)
a*=mult
print,'Shot= ',shot
print,'Time [ms]= ', time
print,'Major Radius [m]= ',Rvals
print,'Rho = ',a

RETURN,a
END
