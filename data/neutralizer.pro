pro neutralizer,smixin,Zkev,Abeam,Zfrac,nl=nl
;+
; NAME:
;	NEUTRALIZER
;
; PURPOSE:
;	This program calculates the fractions of all the possible species of ions
;	and neutrals as a funsction of gas cell line density at any energy between
;	10 keV and 200 keV per amu.
;	Calculates the fractions at varied energies and at a fixed line density.
;
; CALLING SEQUENCE:
;	NEUTRALIZER,Smixin,Zfrac,Zkev,Abeam,Nl
;
; INPUTS:
;	Smixin - ion species mix at accelerator
;	Zkev   - neutral beam voltage [keV] (scalar or vector)
;	Abeam  - mass of beam neutral [amu]
;
; OUTPUTS:
;	Zfrac - neutral current fractions for full, half, and third energy
;
; KEYWORDS:
;	Nl     - line density (units? default=1.0)
;
; MODIFICATION HISTORY:
;	Written by: REB, PPPL, adapted from neutralizer.pro in TRANSP package
;		               from code originally written by J. Kim, GA
;-

 if n_elements(nl) eq 0 then nl=1.0
 nn=n_elements(Zkev)
 zfrac=fltarr(nn,3) 
 XS=fltarr(nn,26)
 
 p1=smixin[*,0]  ; fraction of atomic ions
 p2=smixin[*,1]  ; fraction of diatomic ions
 p3=smixin[*,2]  ; fractioin of triatomic ions
 eb=zkev/abeam  ; Energy/amu [keV]

 EN=[10.,20.,30.,40.,60.,80.,100.,120.,150.,200.]
 S=[[8.50,6.0,4.0,2.6,1.1,.56,.28,.15,.07,.018] , 	$
    [9.0,8.5,7.3,6.0,4.0,2.6,1.7,1.1,.65,.28],		$
    [9.0,8.95,8.5,7.5,6.0,4.2,3.6,2.6,1.7,0.9],	$
    [0.92,1.3,1.6,1.6,1.5,1.2,1.1,0.95,0.87,0.7],	$
    [0.82,0.92,1.15,1.3,1.6,1.6,1.5,1.4,1.25,1.1],	$
    [0.77,0.85,0.92,1.1,1.3,1.55,1.6,1.6,1.5,1.45],	$
    [4.6,3.6,2.8,2.1,1.2,0.7,0.45,0.3,0.15,0.08],	$
    [5.0,4.2,3.6,2.9,2.1,1.5,1.0,0.7,0.44,0.22],	$
    [0.67,1.2,1.5,1.7,1.8,2.0,2.0,2.0,2.0,2.1],	$
    [0.55,0.95,1.2,1.4,1.7,1.8,1.9,2.0,2.0,2.0],	$
    [0.38,0.43,0.60,0.67,0.67,0.49,0.30,0.3,0.25,0.25],$
    [0.25,0.4,0.43,0.55,0.67,0.69,0.65,0.49,0.3,0.3],	$
    [2.4,1.98,1.63,1.28,1.0,0.85,0.75,0.67,0.6,0.51],	$
    [2.6,2.3,1.98,1.67,1.28,1.1,0.9,0.85,0.75,0.82],	$
    [2.4,2.1,1.9,1.9,2.2,2.4,2.3,2.25,2.1,1.8],	$
    [2.4,2.3,2.1,2.0,1.9,2.15,2.3,2.4,2.3,2.2],	$
    [7.7,8.5,8.2,7.7,6.1,4.6,3.6,2.5,2.0,1.2],		$
    [6.7,8.1,8.5,8.2,7.7,6.6,5.6,4.6,3.6,2.2],		$
    [1.1,1.25,1.3,1.25,1.15,1.1,0.99,0.92,0.87,0.79],	$
    [3.4,4.1,4.3,4.1,3.15,2.4,1.9,1.6,1.2,0.7],	$
    [0.86,1.65,1.8,1.95,2.15,2.3,2.3,2.3,2.4,2.4],	$
    [6.2,8.8,9.6,9.6,9.0,7.9,6.7,5.2,4.5,3.6],		$
    [0.047,0.1,0.062,0.025,2.2E-3,4.6E-4,1.0E-4,3.0E-5,1.0E-5,1.5E-6],	$
    [.22,.19,.12,.095,.053,0.029,0.021,8.E-3,4.5E-3,1.4E-3],		$
    [.4,.44,.44,.43,.4,.34,.3,0.25,0.22,0.17],		$
    [10.0,8.7,7.4,6.7,5.4,4.7,4.0,3.6,3.2,2.6]]
 
 for j=0,25 do begin
   XS[*,j]=interpol(S[*,j],EN,EB)
 endfor 
 
 S1=XS[*,0]
 S2=XS[*,1]
 S3=XS[*,2]
 S4=XS[*,3]
 S5=XS[*,4]
 S6=XS[*,5]
 S7=XS[*,6]
 S8=XS[*,7]
 S9=XS[*,8]
 S10=XS[*,9]
 S11=XS[*,10]
 S12=XS[*,11]
 S13=XS[*,12]
 S14=XS[*,13]
 S15=XS[*,14]
 S16=XS[*,15]
 S17=XS[*,16]
 S18=XS[*,17]
 S19=XS[*,18]
 S20=XS[*,19]
 S21=XS[*,20]
 S22=XS[*,21]
 E1=XS[*,22]
 E2=XS[*,23]
 E3=XS[*,24]
 E4=XS[*,25]
 T1=0.333*(S21+S22)+0.667*(S19+S20)
 T2=S8+0.5*(S16+S18)
 T3=S10+0.5*(S12+S14)
 T4=S7+0.5*(S15+S17)
 T5=S9+0.5*(S11+S13)
 T6=S4+E2+E4
 T7=S1+E1+E3
 R1=0.5*(T2+T3)+0.5*SQRT((T2-T3)^2+4*S8*S10)
 R2=0.5*(T2+T3)-0.5*SQRT((T2-T3)^2+4*S8*S10)
 Q1=0.5*(T4+T5)+0.5*SQRT((T4-T5)^2+4*S7*S9)
 Q2=0.5*(T4+T5)-0.5*SQRT((T4-T5)^2+4*S7*S9)
 U1=.5*(T6+T7)+.5*SQRT((T6-T7)^2+4*(S1-E4)*(S4-E3))
 U2=.5*(T6+T7)-.5*SQRT((T6-T7)^2+4*(S1-E4)*(S4-E3))
 RR=1./(R1-R2)
 TR1=1./(T1-R1)
 TR2=1./(T1-R2)
 SS1=S1+S4
 SS2=S2+S5
 SS3=S3+S6
 H3=(T6*E3+E4*(S4-E3))/(U1*U2)
 H1=(U2*H3-U2+S1+E1)/(U1-U2)
 H2=(-U1*H3+U1-S1-E1)/(U1-U2)
 W3=(T7*E4+E3*(S1-E4))/(U1*U2)
 W1=(U2*W3-S1)/(U1-U2)
 W2=(-U1*W3+S1)/(U1-U2)
 QQ=(S2+S5-Q1)*(Q2-Q1)
 YY=(S2+S5-Q2)*(Q2-Q1)
 QQQ=S7*(S13-2*S2)
 G=(Q2-T4)*(S17-2*S2)/QQ+QQQ/QQ
 GG=(T4-Q1)*(S17-2*S2)/YY-QQQ/YY
 TA=S14-2*S3
 TB=S18-2*S3
 A=RR*TR1*((R1-T2)*S20-S19*S8)
 AA=RR*TR2*(S19*S8-(R2-T2)*S20)
 AAA=TR1*TR2*(S19*S8-(T1-T2)*S20)
 B=RR*TR1*((R1-T3)*S19-S20*S10)
 BB=RR*TR2*(S20*S10-(R2-T3)*S19)
 BBB=TR1*TR2*(S20*S10-S19*(T1-T3))
 D=(TA*A+TB*B)/(SS3-R1)
 DD=(TA*AA+TB*BB)/(SS3-R2)
 DDD=(TA*AAA+TB*BBB+S22-3*S3)/(SS3-T1)


 X=NL
 EF1=EXP(-SS1*X)
 EF2=EXP(-SS2*X)
 EF3=EXP(-SS3*X)
 FQ1=EXP(-Q1*X)
 FQ2=EXP(-Q2*X)
 FT1=EXP(-T1*X)
 FR1=EXP(-R1*X)
 FR2=EXP(-R2*X)
 AF1N=W1*EXP(-U1*X)+W2*EXP(-U2*X)+W3
; AF1C=H1*EXP(-U1*X)+H2*EXP(-U2*X)+H3
; AFNC=1-AF1N-AF1C
 DF2N=(FQ1-FQ2)*S7/(Q2-Q1)
; DF2C=(T4-Q2)*FQ1/(Q1-Q2)+(Q1-T4)*FQ2/(Q1-Q2)
 DF1N=(2*S2/SS2-EF2*(G+GG+2*S2/SS2)+G*FQ1+GG*FQ2)*.5
; DF1C=1-DF1N-DF2N-DF2C
; TF3C=FT1
 TF2N=(A*FR1+AA*FR2+AAA*FT1)*.667
; TF2C=(B*FR1+BB*FR2+BBB*FT1)*.667
 TF1N=(3*S3/SS3-(3*S3/SS3+D+DD+DDD)*EF3+D*FR1+DD*FR2+DDD*FT1)*0.333
; TF1C=1-TF1N-TF2N-TF2C-TF3C
 PA1N=P1*AF1N
; PA1C=P1*AF1C
; PANC=P1*AFNC
 PD1N=P2*DF1N
; PD1C=P2*DF1C
 PD2N=P2*DF2N
; PD2C=P2*DF2C
 PT1N=P3*TF1N
; PT1C=P3*TF1C
 PT2N=P3*TF2N
; PT2C=P3*TF2C
; PT3C=P3*TF3C
 PTN=PA1N+PD1N+PD2N+PT1N+PT2N
; AF1N=100*AF1N
; AF1C=100*AF1C
; ANNC=100*AFNC
; DF1N=100*DF1N
; DF1C=100*DF1C
; DF2N=100*DF2N
; DF2C=100*DF2C
; TF1N=100*TF1N
; TF1C=100*TF1C
; TF2N=100*TF2N
; TFC2=100*TF2C
; TFC3=100*TF3C

; PA1N=100*PA1N
; PA1C=100*PA1C
; PANC=100*PANC
; PD1N=100*PD1N
; PD1C=100*PD1C
 ;PD2N=100*PD2N
; PD2C=100*PD2C
; PT1N=100*PT1N
; PT1C=100*PT1C
; PT2N=100*PT2N
; PTC2=100*PT2C
; PTC3=100*PT3C
 PTNHALF=PD1N + PD2N
 PTNTHIRD=PT1N + PT2N
; PTN=100*PTN

 full=PA1N
 half=2*(PD1N+PD2N)
 third=3*(PT1N+PT2N)
 total=full+half+third
 zfrac[*,0]=full/total
 zfrac[*,1]=half/total
 zfrac[*,2]=third/total
 if nn eq 1 then zfrac=reform(zfrac)
 
end
