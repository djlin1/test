;***************************************************************************
;
;  4dlib/FITTING/design_matrix.pro
;
;  created:
;
;  modified:
;  20040302 tbt Looked for troidal_rotation edge fit problem 
;               (see toroidal_rotation_new.pro)
;  20031210 tbt Changed -1.E29 t0 -.99999E30 = flag_value
;  20031210 tbt Go back to pre 20030916 with lu=transpose(gee) as per Tim
;  20031009 tbt now doing geeinv #  H. Before design_matrix was doing
;                         geeinv ## H
;               added ident, geeinv
;  20031007 tbt found major bug giving all spline values bad from
;               knot n-1 to n.
;               Changed definitions of geeinvH and geeinvC.
;  20030929 tbt changed -1.E30 to -1.E29
;  20030916 TBT Looking for why gaprofiles fit is different from
;  spline fit with default BC for fit between knot(nmax-1) and knot(nmax)
;
;
;**************************************************************************

function design_matrix, xdata, knotloc, geeinvc, fx, b, bcs=bcs, der=der


;Input:
;  knotloc[numknots]  real array of knot locations,
;  xdata[nx]          real array of x values to be fit.
;  bcs[0:3]           real array of boundary conditions. (-1.E30 is a flag)

;output
;   design_matrix[nx,numknots]
;   geeinvc[numknots]
;   fx[numknots] 
;   der = derivative

;-----------------------------------------------------------

; Save off interation counter, for local use.
Common local_common3, iter


If N_Elements(iter) Lt 1 Then iter = 0

iter = iter+1

numknot = n_elements(knotloc)

a       = fltarr(n_elements(xdata),numknot)
b       = fltarr(n_elements(xdata),numknot)

gee     = fltarr(numknot,numknot)
h       = fltarr(numknot,numknot)
geeinvh = fltarr(numknot,numknot)
geeinv  = fltarr(numknot,numknot)
ident   = Fltarr(numknot)  ;  Identity array.
geeinvc = fltarr(numknot)
rowa    = fltarr(numknot)
rowb    = fltarr(numknot)
d       = fltarr(n_elements(xdata),numknot)
c       = fltarr(numknot)
fx      = fltarr(numknot)

for i=0,n_elements(xdata)-1 do begin
  rowa(*)=0.
  rowb(*)=0.

  bt=where(knotloc ge xdata(i) and knotloc gt 0.)
  j = bt[0]

;  rowa(bt(0)-1) = (knotloc(bt(0))-xdata(i))/(knotloc(bt(0))-knotloc(bt(0)-1))
;  rowa(bt(0))   = 1.-rowa(bt(0)-1)
;
;  rowb(bt(0)-1)=(rowa(bt(0)-1)^3.-rowa(bt(0)-1))* $
;                (knotloc(bt(0))-knotloc(bt(0)-1))^2./6.
;  rowb(bt(0))  =(rowa(bt(0))^3.-rowa(bt(0)))* $
;                (knotloc(bt(0))-knotloc(bt(0)-1))^2./6.

  rowa(j-1) = (knotloc(j)-xdata(i))/(knotloc(j)-knotloc(j-1))
  rowa(j)   = 1.-rowa(j-1)

  rowb(j-1)=(rowa(j-1)^3.- rowa(j-1)) * (knotloc(j)-knotloc(j-1))^2./6.
  rowb(j)  =(rowa(j)^3.  - rowa(j))   * (knotloc(j)-knotloc(j-1))^2./6.
  a(i,*)=rowa
  b(i,*)=rowb
endfor

;pre 20030916
;if (bcs(0) gt -1.E30) then fx(0)=bcs(0)

;    20030916
;if (bcs(0) gt -1.E29) then fx(0)=bcs(0)

;    20031210
flag_value = -.99999E30

if (bcs(0) gt flag_value) then fx(0)=bcs(0)

if (bcs(1) gt flag_value) then begin
  gee(0,0) = (knotloc(1)-knotloc(0))/3.
  gee(0,1) = gee(0,0)/2.
  h(0,0)   = -1./(knotloc(1)-knotloc(0))
  h(0,1)   = -1.*h(0,0)
  c(0)     = -1.*bcs(1)
endif else begin
  gee(0,0)=1.
endelse


; if (bcs(2) gt -1.E29) then fx(numknot-2)=bcs(2)   	--old
; if (bcs(2) gt -1.E29) then fx(numknot-1)=bcs(2)        -- 20031210

if (bcs(2) gt flag_value) then fx(numknot-1)=bcs(2)


;if (bcs(3) gt -1.E29) then begin  -- 20031210

if (bcs(3) gt flag_value) then begin
  gee(numknot-1,numknot-1) = -1.*(knotloc(numknot-1)-knotloc(numknot-2))/3.
  gee(numknot-1,numknot-2) = gee(numknot-1,numknot-1)/2.
  h(numknot-1,numknot-1)   =  1./(knotloc(numknot-1)-knotloc(numknot-2))
  h(numknot-1,numknot-2)   = -1.*h(numknot-1,numknot-1)
  c(numknot-1)             = -1.*bcs(3)
endif else begin
  gee(numknot-1,numknot-1) =  1.
endelse


for i=1,numknot-2 do begin
  gee(i,i-1) =         (knotloc(i)  -knotloc(i-1))/6.
  gee(i,i)   =         (knotloc(i+1)-knotloc(i-1))/3.  
  gee(i,i+1) =         (knotloc(i+1)-knotloc(i))  /6.
  h(i,i-1)   =      1./(knotloc(i)  -knotloc(i-1))
  h(i,i)     = -1.*(1./(knotloc(i+1)-knotloc(i))  $
                   +1./(knotloc(i)  -knotloc(i-1)))
  h(i,i+1)   =      1./(knotloc(i+1)-knotloc(i))
endfor


;---------------------------------------------
; Original computation that actually gets geeinv ## H
;lu=gee
;ludc,lu,idum
;
;for i=0,numknot-1 do begin
;  geeinvh(*,i)=lusol(lu,idum,h(*,i))
;endfor
;
;geeinvc=lusol(lu,idum,c)

; 20031007 Commented above - added below tbt


;====  ---------------------------------------------
; 20031210 tbt Original computation but with lu=transpose(gee)
;              as per Tim Luce

;lu=gee

lu=Transpose(gee)
ludc,lu,idum

for i=0,numknot-1 do begin
  geeinvh(*,i)=lusol(lu,idum,h(*,i))
endfor

geeinvc=lusol(lu,idum,c)

;====


;------------------
; Calculate geeinv # H using LUDC- not geeinv ## H
;
;lu = gee
;
;ludc, lu, idum
;
;for i=0,numknot-1 do begin
;  ident(*) = 0.
;  ident(i) = 1.
;  geeinv(i,*) = lusol(lu, idum, ident)
;endfor
;
;
;------------------
; Calculate geeinv # H - not geeinv ## H
;
;// 20031210 tbt commented out below as per Tim Luce.
;sv   = gee
;maxW = 0.0
;
;Svdc, sv, W, U, V
;
;for i=0,numknot-1 do begin
;  ident(*) = 0.
;  ident(i) = 1.
;  geeinv(i,*) = SVsol(U, W, V, ident)
;  maxW = Max([maxW, Max(Abs(W))/Min(Abs(W))])
;endfor
;
;// ----------------------------------------------------
;
;If (N_elements(xdata) Eq 101 Or N_elements(xdata) Eq 121 ) Then begin
; Print info if this a the final call for fit or fit with edge points.
;  Message, /Continue, 'Using SVsol, condtion number = ' +  $
;                       String(maxW)  +                     $
;                       ' Identity Matrix = ' +             $
;                       String(Max([Abs(geeinv #  gee),     $
;                                   Abs(geeinv ## gee)])) + $
;                       '  '  + String(n_elements(xdata)) + $
;                       ' Iterations =' + String(iter)
;Endif
;
;
;geeinvh = geeinv # h
;geeinvc = geeinv # c
;
;
;
; Failsafe method that works but Invert is not stable.
;geeinv  = Invert(gee)
;
;geeinvh = geeinv # h
;geeinvc = geeinv # c
;---------------------------------------------


d = a + b#geeinvh
;-----------------


if (keyword_set(der)) then begin
  ap=fltarr(n_elements(xdata),numknot)
  bp=fltarr(n_elements(xdata),numknot)
  rowap=fltarr(numknot)
  rowbp=fltarr(numknot)

  for i=0,n_elements(xdata)-1 do begin
    rowap(*)=0.
    rowbp(*)=0.
    bt=where(knotloc ge xdata(i) and knotloc gt 0.)
    j = bt[0]


;    substituting j for bt[0]......tbt
;    rowap(bt(0)-1)=-1./(knotloc(bt(0))-knotloc(bt(0)-1))
;    rowap(bt(0))=-1.*rowap(bt(0)-1)
;    rowbp(bt(0)-1)=(knotloc(bt(0))-knotloc(bt(0)-1))* $
;                   (1.-3.*a(i,bt(0)-1)^2.)/6.
;    rowbp(bt(0))=(knotloc(bt(0))-knotloc(bt(0)-1))* $
;                 (3.*a(i,bt(0))^2.-1.)/6.


    rowap(j-1) = -1./(knotloc(j)-knotloc(j-1))
    rowap(j)   = -1.*rowap(j-1)

    rowbp(j-1) = (knotloc(j)-knotloc(j-1))* $
                   (1.-3.*a(i,j-1)^2.)/6.
    rowbp(j)   = (knotloc(j)-knotloc(j-1))* $
                      (3.*a(i,j)^2.-1.)/6.

    ap(i,*)=rowap
    bp(i,*)=rowbp
  endfor

  dp  = ap + bp#geeinvh
  dpp = a # geeinvh
  der = {d1:dp, d2:dpp}
  
endif


return, d
end
