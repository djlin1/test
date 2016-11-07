pro d3d_beams,Zkev,Abeam,Zfrac,Zbeam=Zbeam,smixin=smixin
;+
; NAME: D3D_BEAMS
;
; PURPOSE: 
;	The purpose of this procedure is to compute the neutral current 
;	fraction for the DIII-D neutral beams
;
; CALLING SEQUENCE:
;	D3D_BEAM,Zkev,Abeam,Zfrac
;
; INPUTS:
;	Zkev - Neutral beam voltage in keV (scalar or vector)
;	Abeam - mass of beam ion in amu
;
; OUTPUTS:
;	Zfrac - neutral current fractions for full, half, third energy
;
; KEYWORDS:
;	Smixin - ion species mix at accelerator
;	Zbeam - charge of ion (1 - hydrogen,deuterium, 2 - helium)
;		(default = 1)
;
; MODIFICATION HISTORY:
;	6 Jan 2004, REB, PPPL - adapted from d3d_beams.for in TRANSP package
;-


nn=n_elements(Zkev)
smixin=fltarr(nn,3)
if n_elements(Zbeam) eq 0 then Zbeam=1.0 


; treat neutralizer density as 1.0
  nl=1.0
 
; Use fit for species mix at accelerator from Steve Riggs' memo 3/3/93
   smixin[*,0]=(-7.737629E-5*ZKEV+1.374258E-2)*ZKEV+0.1854158
   smixin[*,1]=( 4.049498E-6*ZKEV-3.088367E-4)*ZKEV+0.1544813
   smixin[*,2]=1.0-smixin[*,0]-smixin[*,1]
   
; Check for helium
   if (abs(Zbeam-1.0) gt 0.05) then zfrac=[1.,0.,0.]
 
; Use Jinchoon Kim's neutralizer model to get species mix into tokamak
   neutralizer,smixin,zkev,abeam,Zfrac,nl=nl

end
