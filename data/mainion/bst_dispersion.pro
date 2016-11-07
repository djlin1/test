

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2008-09-05 14:30
;
; VERSION:
;   2.0.0
;
; PURPOSE:
;
;   Here we have a collection of routines to calculate the dispersion
;   for the B-Stark system using the Spex-3/4m spectrometer.
;
;   These routines are to be used to convert from wavelength to
;   pixel number.
;
;   The calculations are done using an off axis calculation but assuming
;   ideal optics.
;
;   These routines have the ability to use a cubic correction to
;   take into account distortions due to the spectrometer/camera optical
;   abberations.
;
;   The correction used will be of the form:
;   x = a0 + a1*TAN(theta_f) + a2*TAN(theta_f)^2 + a3*TAN(theta_f)^3
; 
;   For ideal optics a0, a2 & a3 will be 0.
;   a1 is the effictive focal length (f1*M).
;
;
;   These routines need as input the spectrometer parameter phi.
;
; 
;   In all cases, unless the /FROM_EDGE keyword is set, the location 
;   is assumed to be given in channels from the 'central' location.
;
;   How the central location is defined depends on how the 
;   dispersion correction coefficents were found.  For the procedure 
;   used for the B-Stark/CER calibration in
;   [BST_DISPERSION_CALIBRATION], the central location is the 
;   the center of the spectrum.
;
;   If the /FROM_EDGE keyword is set, the location is assumed to be in
;   channels from the edge of the spectrum.
;
;
;   The wavelength should be given as ABS(order)*lambda and should be in 
;   angstroms.
;
;   Notes for this stuff are in the PROGRAMING CALCULATIONS 2007
;   notebook.
;
; TO DO:
;   Fix handling of array data.
;
;-======================================================================


     ;+=================================================================
     ; Extract the spectrometer parameters from the BST_CHORD_PARAM
     ; common block.
     ;
     ; This assumes that the chord parameters have already been loaded
     ; with BST_CHORD_PARAM.
     ;
     ; The structure returned will be of the form:
     ;   spec_param = { $
     ;                  phi:0D $
     ;                 ,gpmm:0 $
     ;                 ,num_channels:0 $
     ;                 ,order:0D $
     ;                 ,coeff:DBLARR(4) $
     ;                }
     ;
     ;-=================================================================
     FUNCTION BST_DISP_GET_SPEC_PARAM
       COMMON BST_CHORD_PARAM, chord_param

       IF N_ELEMENTS(chord_param) EQ 0 THEN BEGIN
         MESSAGE, 'Chord parameters are not loaded.'
       ENDIF

       spec_param = chord_param.detector

       RETURN, spec_param
     END ; FUNCTION BST_DISP_GET_SPEC_PARAM


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Calculate the central wavelength given the location and 
     ;   wavelength of a line.
     ;
     ;   See the file header,  for more information about this 
     ;   calculation.
     ;
     ; KEYWORDS:
     ;   /FROM_EDGE
     ;     If set, the the given location is assumed to be in channels
     ;     from the edge of the spectrum.  Otherwise the location is
     ;     assumed to be in channels from the 'central' location
     ;     (generally the center of the spectrum).
     ;
     ;-=================================================================
     FUNCTION BST_DISP_GET_LAMBDA0, LOCATION=location_in $
                                    ,WAVELENGTH=wavelength $
                                    ,SPEC_PARAM=spec_param $
                                    ,FROM_EDGE=from_edge

       IF N_ELEMENTS(spec_param) EQ 0 THEN BEGIN
         spec_param = BST_DISP_GET_SPEC_PARAM()
       ENDIF

       ; If /FROM_EDGE is set then the location given is measured
       ; from the chip edge instead of the chip center.
       ; We need to convert.
       IF KEYWORD_SET(from_edge) THEN BEGIN
         location = location_in - spec_param.num_channels/2 + 0.5D
       ENDIF ELSE BEGIN
         location = location_in
       ENDELSE

       grating_spacing = 1D / spec_param.gpmm / 1D-7

       num_lines = N_ELEMENTS(location)
       theta_f = DBLARR(num_lines)

       FOR line=0,num_lines-1 DO BEGIN
         ; Create the cubic coefficient array.
         coeff = [spec_param.coeff[0] - location[line], spec_param.coeff[1:*]]

         ; Find the solutions for TAN(theta_f), only take the real ones.
         tan_theta_f = CUBIC_ROOT(coeff, /DOUBLE, /REAL)

         ; If there is more than one root take the smallest one.
         IF N_ELEMENTS(tan_theta_f) GT 1 THEN BEGIN
           tan_theta_f = MIN(tan_theta_f, /ABSOLUTE)
         ENDIF

         
         theta_f[line] = ATAN(tan_theta_f)
       ENDFOR
           
       ; Now solve for lambda0

       a =  COS(spec_param.phi) + COS(theta_f + spec_param.phi)
       b =  SIN(spec_param.phi) - SIN(theta_f + spec_param.phi)
       c = wavelength / grating_spacing * spec_param.order
       a2_b2 = a^2 + b^2

       ;lambda0 = ((-2D)/spec_param.order * COS(spec_param.phi)* grating_spacing) $
       ;  * ( (-1D)*a*c + b*SQRT( a2_b2 - c^2) ) / a2_b2
       
       lambda0 = ((-2D)/spec_param.order * COS(spec_param.phi)* grating_spacing) $
         * ( (-1D)*a*c - b*SQRT( a2_b2 - c^2) ) / a2_b2


       IF num_lines EQ 1 THEN BEGIN
         RETURN, lambda0[0]
       ENDIF ELSE BEGIN
         RETURN, lambda0
       ENDELSE

     END ;BST_DISP_GET_LAMBDA0


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Calculate the wavelength of a line given the channel location 
     ;   of the line and the central wavelength.
     ;
     ;   See the file header,  for more information about this 
     ;   calculation.
     ;
     ; KEYWORDS:
     ;   /FROM_EDGE
     ;     If set, the the given location is assumed to be in channels
     ;     from the edge of the spectrum.  Otherwise the location is
     ;     assumed to be in channels from the 'central' location
     ;     (generally the center of the spectrum).
     ;
     ;-=================================================================
     FUNCTION BST_DISP_GET_WAVELENGTH, LOCATION=location_in $
                                       ,LAMBDA0=lambda0 $
                                       ,SPEC_PARAM=spec_param $
                                       ,FROM_EDGE=from_edge

       IF  N_ELEMENTS(spec_param) EQ 0 THEN BEGIN
         spec_param = BST_DISP_GET_SPEC_PARAM()
       ENDIF

       ; If /FROM_EDGE is set then the location given is measured
       ; from the chip edge instead of the chip center.
       ; We need to convert.
       IF KEYWORD_SET(from_edge) THEN BEGIN
         location = location_in - spec_param.num_channels/2 + 0.5D
       ENDIF ELSE BEGIN
         location = location_in
       ENDELSE

       grating_spacing = 1D / spec_param.gpmm / 1D-7

       num_lines = N_ELEMENTS(location)
       theta_f = DBLARR(num_lines)

       FOR line=0,num_lines-1 DO BEGIN
         ; Create the cubic coefficient array.
         coeff = [spec_param.coeff[0] - location[line], spec_param.coeff[1:*]]

         ; Find the solutions for TAN(theta_f), only take the real ones.
         tan_theta_f = CUBIC_ROOT(coeff, /DOUBLE, /REAL)

         ; If there is more than one root take the smallest one.
         IF N_ELEMENTS(tan_theta_f) GT 1 THEN BEGIN
           tan_theta_f = MIN(tan_theta_f, /ABSOLUTE)
         ENDIF

         
         theta_f[line] = ATAN(tan_theta_f)
       ENDFOR

       grating_tilt = ASIN(-1D * spec_param.order * lambda0 $
                           / ( 2D*COS(spec_param.phi) * grating_spacing ))

       wavelength = ( SIN(spec_param.phi - grating_tilt + theta_f) $
                      - SIN(spec_param.phi +grating_tilt) )*grating_spacing/spec_param.order

       RETURN, wavelength
     END ;PRO BST_DISP_GET_WAVELENGTH


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Calculate the location of a line given the wavelength of the
     ;   line and the central wavelength of the spectrum.
     ;
     ;   See the file header,  for more information about this 
     ;   calculation.
     ;
     ; KEYWORDS:
     ;  /FROM_EDGE
     ;     If set, the the given location is assumed to be in channels
     ;     from the edge of the spectrum.  Otherwise the location is
     ;     assumed to be in channels from the 'central' location
     ;     (generally the center of the spectrum).
     ;
     ;-=================================================================
     FUNCTION BST_DISP_GET_LOCATION, WAVELENGTH=wavelength $
                                     ,LAMBDA0=lambda0_in $
                                     ,SPEC_PARAM=spec_param $
                                     ,FROM_EDGE=from_edge

       IF  N_ELEMENTS(spec_param) EQ 0 THEN BEGIN
         spec_param = BST_DISP_GET_SPEC_PARAM()
       ENDIF

       IF N_ELEMENTS(lambda0_in) NE 1 THEN BEGIN
         MESSAGE, STRING('Only 1 value of LAMBDA0 expected. ', N_ELEMENTS(lambda0_in) $
                         ,' given.', FORMAT='(a,i0,a)')
       ENDIF ELSE BEGIN
         lambda0 = lambda0_in[0]
       ENDELSE

       grating_spacing = 1D / spec_param.gpmm / 1D-7

       grating_tilt = ASIN(-1D * spec_param.order * lambda0 $
                           / ( 2D*COS(spec_param.phi) * grating_spacing ))
       
       theta_m = ASIN(wavelength * spec_param.order / grating_spacing $
                      + SIN(spec_param.phi + grating_tilt))

       beta = grating_tilt + theta_m

       theta_f = beta - spec_param.phi

       tan_theta_f = TAN(theta_f)

       num_lines = N_ELEMENTS(wavelength)
       location = DBLARR(num_lines)

       FOR line=0L,num_lines-1 DO BEGIN
         location[line] = POLY(tan_theta_f[line], spec_param.coeff)
       ENDFOR

       ; If /FROM_EDGE is set then the location returned should
       ; be measured from the edge of the chip, not from the center
       IF KEYWORD_SET(from_edge) THEN BEGIN
         location = location + spec_param.num_channels/2 - 0.5D
       ENDIF

       RETURN, location

     END ;PRO BST_DISP_GET_LOCATION


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Calculate the dispersion given a wavelength and the central
     ;   wavelength.  The dispersion, defined as angstroms/channel,
     ;   will only be correct at the given wavelength, not over the
     ;   entire spectrum.
     ;  
     ;   See the file header,  for more information about this 
     ;   calculation.
     ;
     ;-=================================================================
     FUNCTION BST_DISP_GET_DISPERSION, WAVELENGTH=wavelength $
                                       ,LAMBDA0=lambda0_in $
                                       ,SPEC_PARAM=spec_param
                                       
       IF  N_ELEMENTS(spec_param) EQ 0 THEN BEGIN
         spec_param = BST_DISP_GET_SPEC_PARAM()
       ENDIF

       IF N_ELEMENTS(lambda0_in) NE 1 THEN BEGIN
         MESSAGE, STRING('Only 1 value of LAMBDA0 expected. ', N_ELEMENTS(lambda0_in) $
                         ,' given.', FORMAT='(a,i0,a)')
       ENDIF ELSE BEGIN
         lambda0 = lambda0_in[0]
       ENDELSE

       grating_spacing = 1D / spec_param.gpmm / 1D-7

       grating_tilt = ASIN(-1D * spec_param.order * lambda0 $
                           / ( 2D*COS(spec_param.phi) * grating_spacing ))
       
       sin_theta_m = wavelength * spec_param.order / grating_spacing $
                      + SIN(spec_param.phi + grating_tilt)

       theta_m = ASIN(sin_theta_m)

       beta = grating_tilt + theta_m

       theta_f = beta - spec_param.phi

       tan_theta_f = TAN(theta_f)

       dtanthetaf_dlambda = 1D/(cos(theta_f))^2/sqrt(1D - sin_theta_m^2) $
         * spec_param.order / grating_spacing

       num_lines = N_ELEMENTS(wavelength)
       dx_dlambda = DBLARR(num_lines)

       FOR line=0,num_lines-1 DO BEGIN
         dx_dlambda[line] = POLY(tan_theta_f[line], spec_param.coeff[1:*]*[1D, 2D, 3D])
       ENDFOR

       dx_dlambda = dx_dlambda * dtanthetaf_dlambda


       RETURN, 1/dx_dlambda
     END ; FUNCTION BST_DISP_GET_DISPERSION


     ;+=================================================================
     ; Nothing here, just for IDL auto compilation.
     ;
     ; TAGS:
     ;   EXCLUDE
     ;-=================================================================
     PRO BST_DISPERSION

     END ;PRO BST_DISPERSION
