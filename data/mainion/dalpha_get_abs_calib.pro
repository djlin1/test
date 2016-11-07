;; This function returns the absolute calibration number for a given
;; shots number and chord.
;; These numbers are in the same format as the CER system 
;;[Calibration factors are in unit (count rate)/(photons/cm**2-sr-sec)]

FUNCTION DALPHA_GET_ABS_CALIB,shot,chord,WAVELENGTH=wavelength

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error in DALPHA_GET_ABS_CALIB'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

  ;; Need to access CERVIEW_COMMON for MU3 Patch Panel
  COMMON CERVIEW_COMMON
  
  calib=-1

  IF ~KEYWORD_SET(wavelength) THEN BEGIN
      MESSAGE,'Returning calib at D-alpha',/CON
      wavelength = 6561.0
  ENDIF

  ;; We installed T25 and went to first order operation only for shots
  ;; 141897 through the end of the spring 2010 experiments and into LTO-II
  IF shot LT 141897 THEN BEGIN
      PRINT,'No Absolute Calibration'
      GOTO,GET_OUT
  ENDIF
  ;; Re-focused the main ion system weekend before 141997
  IF shot GE 141898 AND shot LT 142912 THEN BEGIN
      PRINT,'Calibration taken after re-focusing of main ion views'
      ;; Go ahead and return the calibration
      IF STRUPCASE(chord) EQ 'B01' THEN calib=8.3217198d-07
      IF STRUPCASE(chord) EQ 'B02' THEN calib=3.4324716d-07
      GOTO,GET_OUT
  ENDIF

  IF shot GT 142912 AND shot LT 143000 THEN BEGIN
      PRINT,'Calibration invalid...'
      PRINT,'After end of plasma ops 2010'
      GOTO,GET_OUT
  ENDIF

  IF shot GT 143000 AND shot LT 144222 THEN BEGIN
      PRINT,'Calibration Invalid: before proper focus of main-ion system'
      GOTO,GET_OUT
  ENDIF

  IF shot GE 144222 AND shot LT 144652 THEN BEGIN
      
      ;; Main-ion chords from b.i.g. cross-calibration 144289
      IF STRUPCASE(chord) EQ 'M01' THEN calib=0.877022d-07
      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.24063d-07
      IF STRUPCASE(chord) EQ 'M03' THEN calib=1.19750d-07
      IF STRUPCASE(chord) EQ 'M04' THEN calib=1.15580d-07
      IF STRUPCASE(chord) EQ 'M05' THEN calib=1.39219d-07
      IF STRUPCASE(chord) EQ 'M06' THEN calib=1.35973d-07
      IF STRUPCASE(chord) EQ 'M07' THEN calib=1.38217d-07
      IF STRUPCASE(chord) EQ 'M08' THEN calib=1.35723d-07

      ;; Interpolated calibration
;      IF STRUPCASE(chord) EQ 'M09' THEN calib=1.86068d-07
;      IF STRUPCASE(chord) EQ 'M10' THEN calib=3.29819d-07
;      IF STRUPCASE(chord) EQ 'M11' THEN calib=3.28596d-07
;      IF STRUPCASE(chord) EQ 'M12' THEN calib=3.11832d-07
;      IF STRUPCASE(chord) EQ 'M13' THEN calib=1.79863d-07
;      IF STRUPCASE(chord) EQ 'M14' THEN calib=2.42870d-07
;      IF STRUPCASE(chord) EQ 'M15' THEN calib=2.06519d-07
;      IF STRUPCASE(chord) EQ 'M16' THEN calib=0.303499d-07

      ;; Head-to-head calibration
      IF STRUPCASE(chord) EQ 'M09' THEN calib=1.86068d-07
      IF STRUPCASE(chord) EQ 'M10' THEN calib=3.48809d-07
      IF STRUPCASE(chord) EQ 'M11' THEN calib=2.76663d-07
      IF STRUPCASE(chord) EQ 'M12' THEN calib=3.93862d-07
      IF STRUPCASE(chord) EQ 'M13' THEN calib=1.92214d-07
      IF STRUPCASE(chord) EQ 'M14' THEN calib=2.52242d-07
      IF STRUPCASE(chord) EQ 'M15' THEN calib=1.64710d-07
      IF STRUPCASE(chord) EQ 'M16' THEN calib=0.345846d-07

      ;; Mean of both methods
      IF STRUPCASE(chord) EQ 'M09' THEN calib=(1.86068d-07+1.86068d-07)/2d
      IF STRUPCASE(chord) EQ 'M10' THEN calib=(3.48809d-07+3.29819d-07)/2d
      IF STRUPCASE(chord) EQ 'M11' THEN calib=(2.76663d-07+3.28596d-07)/2d
      IF STRUPCASE(chord) EQ 'M12' THEN calib=(3.93862d-07+3.11832d-07)/2d
      IF STRUPCASE(chord) EQ 'M13' THEN calib=(1.92214d-07+1.79863d-07)/2d
      IF STRUPCASE(chord) EQ 'M14' THEN calib=(2.52242d-07+2.42870d-07)/2d
      IF STRUPCASE(chord) EQ 'M15' THEN calib=(1.64710d-07+2.06519d-07)/2d
      IF STRUPCASE(chord) EQ 'M16' THEN calib=(0.345846d-07+0.303499d-07)/2d
      
      GOTO,GET_OUT
  ENDIF ELSE IF shot GE 144652 AND shot LE 147778  THEN BEGIN
      ;; From big cross-calibration 144860
      IF STRUPCASE(chord) EQ 'M01' THEN calib=1.01942d-07*(0.92)
      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.42386d-07
      IF STRUPCASE(chord) EQ 'M03' THEN calib=1.38872d-07
      IF STRUPCASE(chord) EQ 'M04' THEN calib=1.36920d-07
      IF STRUPCASE(chord) EQ 'M05' THEN calib=1.08088d-07
      IF STRUPCASE(chord) EQ 'M06' THEN calib=1.71709d-07
      IF STRUPCASE(chord) EQ 'M07' THEN calib=1.36177d-07
      IF STRUPCASE(chord) EQ 'M08' THEN calib=1.97140d-07

      IF STRUPCASE(chord) EQ 'M09' THEN calib=0.918423d-07
      IF STRUPCASE(chord) EQ 'M10' THEN calib=1.32365d-07
      IF STRUPCASE(chord) EQ 'M11' THEN calib=1.53510d-07
      IF STRUPCASE(chord) EQ 'M12' THEN calib=1.40447d-07
      IF STRUPCASE(chord) EQ 'M13' THEN calib=1.33759d-07
      IF STRUPCASE(chord) EQ 'M14' THEN calib=1.25921d-07
      IF STRUPCASE(chord) EQ 'M15' THEN calib=1.02908d-07
      IF STRUPCASE(chord) EQ 'M16' THEN calib=0.161329d-07

      ;; Re-done with dalpha_big_intensity_crosscal_144860_bst_spectral
;;      0.813006      1.19825      1.39947      1.28395      1.24961      1.17269     0.954539     0.141738

      IF STRUPCASE(chord) EQ 'M09' THEN calib=0.813006d-07
      IF STRUPCASE(chord) EQ 'M10' THEN calib=1.19825d-07
      IF STRUPCASE(chord) EQ 'M11' THEN calib=1.39947d-07
      IF STRUPCASE(chord) EQ 'M12' THEN calib=1.28395d-07
      IF STRUPCASE(chord) EQ 'M13' THEN calib=1.24961d-07
      IF STRUPCASE(chord) EQ 'M14' THEN calib=1.17269d-07
      IF STRUPCASE(chord) EQ 'M15' THEN calib=0.954539d-07
      IF STRUPCASE(chord) EQ 'M16' THEN calib=0.141738d-07

      ;; Re-done with back intensity calibration with the labsphere
;      IF STRUPCASE(chord) EQ 'M01' THEN calib=
      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.3238796e-07
      IF STRUPCASE(chord) EQ 'M03' THEN calib=1.3053633e-07
      IF STRUPCASE(chord) EQ 'M04' THEN calib=1.3295287e-07
      IF STRUPCASE(chord) EQ 'M05' THEN calib=1.0501449e-07
;      IF STRUPCASE(chord) EQ 'M06' THEN calib=1.8312230e-07
      IF STRUPCASE(chord) EQ 'M07' THEN calib=1.4185104e-07
      IF STRUPCASE(chord) EQ 'M08' THEN calib=2.0128539e-07

      IF STRUPCASE(chord) EQ 'M09' THEN calib=0.6371712e-07
      IF STRUPCASE(chord) EQ 'M10' THEN calib=1.0138861e-07
      IF STRUPCASE(chord) EQ 'M11' THEN calib=1.2571059e-07
      IF STRUPCASE(chord) EQ 'M12' THEN calib=1.1990957e-07
      IF STRUPCASE(chord) EQ 'M13' THEN calib=1.2014367e-07
      IF STRUPCASE(chord) EQ 'M14' THEN calib=1.2123490e-07
      IF STRUPCASE(chord) EQ 'M15' THEN calib=1.0776032e-07
      ;; M16 doesn't make sense.  Use relative calib asjusted in line.
      IF STRUPCASE(chord) EQ 'M16' THEN calib=(.60d0/.5d0)*0.141738d-07
;      IF STRUPCASE(chord) EQ 'M16' THEN calib=0.1137965e-07      
  ENDIF ELSE IF shot GE 147808 AND shot LE 150958 THEN BEGIN
      ;; Here are the D-alpha calibration values and the values for
      ;; carbon and helium as well commented.  Need to implement a
      ;; wavelength input for this routine.
      ;; These values from dalpha_abs_calib_20120307.pro
      ;; Now MU1 has the grating blazed for 500nm.
      ;; UPDATE: Re-did calibration on 20120316 with gratings swapped again
      IF STRUPCASE(chord) EQ 'M01' THEN calib=5.7581386e-09 * (1./1.58) ;; See lab book 20120213
      ;; IF STRUPCASE(chord) EQ 'M01' AND wl EQ 5340. THEN calib=4.5272950e-09
      ;; IF STRUPCASE(chord) EQ 'M01' AND wl EQ 4686. THEN calib=2.7332097e-09

;      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.9392574e-07 ;; wrong grating
      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.1136100e-07
      ;; IF STRUPCASE(chord) EQ 'M02' AND wl EQ 5340. THEN calib=1.9275291e-07
      ;; IF STRUPCASE(chord) EQ 'M02' AND wl EQ 4686. THEN calib=1.2253544e-07

;      IF STRUPCASE(chord) EQ 'M03' THEN calib=2.0936140e-07
      IF STRUPCASE(chord) EQ 'M03' THEN calib=1.161538e-07
      ;; IF STRUPCASE(chord) EQ 'M03' AND wl EQ 5340. THEN calib=2.0021587e-07
      ;; IF STRUPCASE(chord) EQ 'M03' AND wl EQ 4686. THEN calib=1.2391816e-07

;      IF STRUPCASE(chord) EQ 'M04' THEN calib=2.0434191e-07
      IF STRUPCASE(chord) EQ 'M04' THEN calib=1.2050922e-07
      ;; IF STRUPCASE(chord) EQ 'M04' AND wl EQ 5340. THEN calib=1.9442605e-07
      ;; IF STRUPCASE(chord) EQ 'M04' AND wl EQ 4686. THEN calib=1.1984507e-07

;      IF STRUPCASE(chord) EQ 'M05' THEN calib=2.4625842e-08
      IF STRUPCASE(chord) EQ 'M05' THEN calib=3.8971795e-08
      ;; IF STRUPCASE(chord) EQ 'M05' AND wl EQ 5340. THEN calib=1.4414957e-08
      ;; IF STRUPCASE(chord) EQ 'M05' AND wl EQ 4686. THEN calib=6.2623583e-09

;      IF STRUPCASE(chord) EQ 'M06' THEN calib=4.1337389e-08
      IF STRUPCASE(chord) EQ 'M06' THEN calib=6.2891752e-08
      ;; IF STRUPCASE(chord) EQ 'M06' AND wl EQ 5340. THEN calib=2.5593618e-08
      ;; IF STRUPCASE(chord) EQ 'M06' AND wl EQ 4686. THEN calib=1.1337468e-08

;      IF STRUPCASE(chord) EQ 'M07' THEN calib=3.3902294e-08
      IF STRUPCASE(chord) EQ 'M07' THEN calib=2.2828928e-08 / (0.426) ;; calib error
      ;; IF STRUPCASE(chord) EQ 'M07' AND wl EQ 5340. THEN calib=2.0166590e-08
      ;; IF STRUPCASE(chord) EQ 'M07' AND wl EQ 4686. THEN calib=9.3054785e-09

;      IF STRUPCASE(chord) EQ 'M08' THEN calib=4.8300076e-08
      IF STRUPCASE(chord) EQ 'M08' THEN calib=7.6147315e-08
      ;; IF STRUPCASE(chord) EQ 'M08' AND wl EQ 5340. THEN calib=2.9984326e-08
      ;; IF STRUPCASE(chord) EQ 'M08' AND wl EQ 4686. THEN calib=1.3993067e-08

      IF STRUPCASE(chord) EQ 'M09' THEN calib=8.5706161e-08
      ;; IF STRUPCASE(chord) EQ 'M09' AND wl EQ 5340. THEN calib=5.3943604e-08
      ;; IF STRUPCASE(chord) EQ 'M09' AND wl EQ 4686. THEN calib=2.2146584e-08

      IF STRUPCASE(chord) EQ 'M10' THEN calib=8.9340902e-08
      ;; IF STRUPCASE(chord) EQ 'M10' AND wl EQ 5340. THEN calib=5.6111094e-08
      ;; IF STRUPCASE(chord) EQ 'M10' AND wl EQ 4686. THEN calib=2.3166961e-08

      IF STRUPCASE(chord) EQ 'M11' THEN calib=1.0611107e-07
      ;; IF STRUPCASE(chord) EQ 'M11' AND wl EQ 5340. THEN calib=6.7809333e-08
      ;; IF STRUPCASE(chord) EQ 'M11' AND wl EQ 4686. THEN calib=2.7708640e-08

      IF STRUPCASE(chord) EQ 'M12' THEN calib=9.7206955e-08
      ;; IF STRUPCASE(chord) EQ 'M12' AND wl EQ 5340. THEN calib=6.2018238e-08
      ;; IF STRUPCASE(chord) EQ 'M12' AND wl EQ 4686. THEN calib=2.5188446e-08

      IF STRUPCASE(chord) EQ 'M13' THEN calib=9.7224912e-08
      ;; IF STRUPCASE(chord) EQ 'M13' AND wl EQ 5340. THEN calib=5.8187238e-08
      ;; IF STRUPCASE(chord) EQ 'M13' AND wl EQ 4686. THEN calib=2.2944853e-08

      IF STRUPCASE(chord) EQ 'M14' THEN calib=9.8286370e-08
      ;; IF STRUPCASE(chord) EQ 'M14' AND wl EQ 5340. THEN calib=5.9676630e-08
      ;; IF STRUPCASE(chord) EQ 'M14' AND wl EQ 4686. THEN calib=2.3899299e-08

      IF STRUPCASE(chord) EQ 'M15' THEN calib=8.7919710e-08
      ;; IF STRUPCASE(chord) EQ 'M15' AND wl EQ 5340. THEN calib=5.3812729e-08
      ;; IF STRUPCASE(chord) EQ 'M15' AND wl EQ 4686. THEN calib=2.1998561e-08

      IF STRUPCASE(chord) EQ 'M16' THEN calib=9.3009127e-09
      ;; IF STRUPCASE(chord) EQ 'M16' AND wl EQ 5340. THEN calib=6.1331066e-09
      ;; IF STRUPCASE(chord) EQ 'M16' AND wl EQ 4686. THEN calib=2.6530837e-09
  ENDIF ELSE IF shot GT 151189 AND shot LE 155696 THEN BEGIN ;; Update this shot when the first shot happens

      IF STRUPCASE(chord) EQ 'M01' THEN calib=-1

      IF STRUPCASE(chord) EQ 'M02' THEN calib=1.2286097e-07

      IF STRUPCASE(chord) EQ 'M03' THEN calib=1.2175431e-07 

      IF STRUPCASE(chord) EQ 'M04' THEN calib=1.3601760e-07

      IF STRUPCASE(chord) EQ 'M05' THEN calib=3.8340373e-08

      IF STRUPCASE(chord) EQ 'M06' THEN calib=7.3667997e-08

      IF STRUPCASE(chord) EQ 'M07' THEN calib=4.4940631e-08

      IF STRUPCASE(chord) EQ 'M08' THEN calib=5.9834159e-08

      IF STRUPCASE(chord) EQ 'M09' THEN calib=8.4106788e-08

      IF STRUPCASE(chord) EQ 'M10' THEN calib=1.2010675e-07

      IF STRUPCASE(chord) EQ 'M11' THEN calib=1.3878086e-07

      IF STRUPCASE(chord) EQ 'M12' THEN calib=1.3207693e-07

      IF STRUPCASE(chord) EQ 'M13' THEN calib=9.8561319e-08

      IF STRUPCASE(chord) EQ 'M14' THEN calib=1.0928136e-07

      IF STRUPCASE(chord) EQ 'M15' THEN calib=8.9489830e-08

      IF STRUPCASE(chord) EQ 'M16' THEN calib=1.1004619e-08

  ENDIF ELSE IF shot GE 156399 AND shot LT 161800 THEN BEGIN
      ;; FY14

      IF STRUPCASE(chord) EQ 'M01' AND wavelength EQ 6561.0 THEN calib= 1.0210566e-07
      ;; Back calibration 20150302 calib = 8.5403035e-08 (83% of original)
      IF STRUPCASE(chord) EQ 'M01' AND wavelength EQ 5340.0 THEN calib= 9.3161987e-08
      ;; Back calibration 20150302 calib = 8.2210122e-08 (88% of original)

      IF STRUPCASE(chord) EQ 'M02' AND wavelength EQ 6561.0 THEN calib= 1.1200486e-07      
      ;; Back calibration 20150302 calib = 9.0698346e-08 (81% of original)
      IF STRUPCASE(chord) EQ 'M02' AND wavelength EQ 5340.0 THEN calib= 9.9167899e-08

      IF STRUPCASE(chord) EQ 'M03' AND wavelength EQ 6561.0 THEN calib= 1.0098530e-07
      ;; Back calibration 20150302 calib = 8.3708918e-08 (83% of original)
      IF STRUPCASE(chord) EQ 'M03' AND wavelength EQ 5340.0 THEN calib= 8.8478061e-08

      IF STRUPCASE(chord) EQ 'M04' AND wavelength EQ 6561.0 THEN calib= 1.1418335e-07
      ;; Back calibration 20150302 calib = 9.2977725e-08 (81% of original)
      IF STRUPCASE(chord) EQ 'M04' AND wavelength EQ 5340.0 THEN calib= 9.8170725e-08

      IF STRUPCASE(chord) EQ 'M05' AND wavelength EQ 6561.0 THEN calib= 4.1575498e-08
      ;; Back calibration 20150302 calib = 3.1996680e-08 (77% or original)
      IF STRUPCASE(chord) EQ 'M05' AND wavelength EQ 5340.0 THEN calib= 3.8656014e-08

      IF STRUPCASE(chord) EQ 'M06' AND wavelength EQ 6561.0 THEN BEGIN
          ;; Back calibration 20150302 calib = 2.6598773e-08 (32% or original)
          MESSAGE,'Warning!  M06 Intensity back-calibration 32% of forward!',/CONT
          calib= 8.2871217e-08
      ENDIF
      IF STRUPCASE(chord) EQ 'M06' AND wavelength EQ 5340.0 THEN calib= 7.6382973e-08

      IF STRUPCASE(chord) EQ 'M07' AND wavelength EQ 6561.0 THEN calib= 5.4444919e-08
      ;; Back calibration 20150302 calib = 4.4796318e-08 (82% or orignial)
      IF STRUPCASE(chord) EQ 'M07' AND wavelength EQ 5340.0 THEN calib= 5.3730324e-08

      IF STRUPCASE(chord) EQ 'M08' AND wavelength EQ 6561.0 THEN calib= 6.7468629e-08
      ;; Back calibration 20150302 calib = 5.8085717e-08 (86% of original)
      IF STRUPCASE(chord) EQ 'M08' AND wavelength EQ 5340.0 THEN calib = 6.5600629e-08

      ;; For FY14 we have to get the pseudo-chord for /mu3 and parse the
      ;; string chord_data.chord to see which calibration to use.

      ;; Spot M09 on CCD M12
      IF STRUPCASE(chord) EQ 'M09' THEN calib= 2.3438919e-08

      ;; Spot M10 on CCD M09
      IF STRUPCASE(chord) EQ 'M10' THEN calib= 1.0272773e-07
      ;; Back calibration 20150302 calib = 5.0644754e-08 (50% of original)

      ;; Spot M11 on CCD M10
      IF STRUPCASE(chord) EQ 'M11' THEN calib= 1.2939063e-07
      ;; Back calibration 20150302 calib = 1.0437824e-07 (80% of original)

      ;; Spot M12 on CCD M11
      IF STRUPCASE(chord) EQ 'M12' THEN calib= 1.2506452e-07
      ;; Back calibration 20150302 calib = 3.3843366e-08 (27% of original)

      ;; Spot M13
      IF STRUPCASE(chord) EQ 'M13' THEN BEGIN
          GET_PSEUDOCHORD,shot,/MU3
          IF STRCMP(chord_data.chord,'MainIon-3 ( M10 M11 M12 M13 )') THEN BEGIN
              ;; Patch Standard
              MESSAGE,"MU3 on Patch 'Standard'",/CONT
              ;; On CCD M12
              calib= 1.2506452e-07
              ;; Back calibration 20150302 calib = 7.1347706e-08 (57%
              ;; of original)
          ENDIF
          IF STRCMP(chord_data.chord,'MainIon-3 ( M12 M13 M14 M15 )') THEN BEGIN
              ;; Patch edge
              MESSAGE,"MU3 on Patch 'Edge'",/CONT
              ;; On CCD M09
              calib= 1.0807047e-07
          ENDIF 
          GET_CHORD,shot,chord,/WHITE
      ENDIF

      ;; Spot M14 on CCD M12
      IF STRUPCASE(chord) EQ 'M14' THEN calib= 1.1822660e-07

      ;; Spot M15 on CCD M10
      IF STRUPCASE(chord) EQ 'M15' THEN calib= 1.0653118e-07

      ;; Spot M16 on CCD xx
      IF STRUPCASE(chord) EQ 'M16' THEN BEGIN
          MESSAGE,'M16 not operational this year',/CONT
          calib=-1
      ENDIF

      IF STRUPCASE(chord) EQ 'M17' AND wavelength EQ 6561.0 THEN calib= 1.9302749e-08
      ;; Back calibration 20150302 calib = 1.1905925e-08 (62% of original)

      IF STRUPCASE(chord) EQ 'M18' AND wavelength EQ 6561.0 THEN calib= 2.8026393e-08
      ;; Back calibration 20150302 calib = 2.1104035e-08 (75% or original)

      IF STRUPCASE(chord) EQ 'M19' AND wavelength EQ 6561.0 THEN calib= 2.7067529e-08            
      ;; Back calibration 20150302 calib = 2.3053389e-08 (85% or original)

      IF STRUPCASE(chord) EQ 'M20' AND wavelength EQ 6561.0 THEN calib= 2.9481899e-08
      ;; Back calibration 20150302 calib = 2.5030324e-08 (85% or original)

  ENDIF ELSE IF shot GE 161800 AND shot LE 163899 THEN BEGIN
      ;; FY15
      IF STRUPCASE(chord) EQ 'M01' AND wavelength EQ 6561.0 THEN calib= 1.8692373e-07
      IF STRUPCASE(chord) EQ 'M02' AND wavelength EQ 6561.0 THEN calib= 1.7100633e-07
      IF STRUPCASE(chord) EQ 'M03' AND wavelength EQ 6561.0 THEN calib= 1.8940157e-07
      IF STRUPCASE(chord) EQ 'M04' AND wavelength EQ 6561.0 THEN calib= 1.9437245e-07
      IF STRUPCASE(chord) EQ 'M05' AND wavelength EQ 6561.0 THEN calib= 1.0878577e-07
      IF STRUPCASE(chord) EQ 'M06' AND wavelength EQ 6561.0 THEN calib= 1.0632513e-07
      IF STRUPCASE(chord) EQ 'M07' AND wavelength EQ 6561.0 THEN calib= 1.0329770e-07
      IF STRUPCASE(chord) EQ 'M08' AND wavelength EQ 6561.0 THEN calib= 1.0591083e-07
;      IF STRUPCASE(chord) EQ 'M10' AND wavelength EQ 6561.0 THEN calib= 
;      IF STRUPCASE(chord) EQ 'M11' AND wavelength EQ 6561.0 THEN calib= 
;      IF STRUPCASE(chord) EQ 'M12' AND wavelength EQ 6561.0 THEN calib= 
;      IF STRUPCASE(chord) EQ 'M13' AND wavelength EQ 6561.0 THEN calib= 
      IF STRUPCASE(chord) EQ 'M17' AND wavelength EQ 6561.0 THEN calib= 2.9319131e-08
      IF STRUPCASE(chord) EQ 'M24' AND wavelength EQ 6561.0 THEN calib= 3.6489985e-08
      IF STRUPCASE(chord) EQ 'M25' AND wavelength EQ 6561.0 THEN calib= 3.4756596e-08
      IF STRUPCASE(chord) EQ 'M26' AND wavelength EQ 6561.0 THEN calib= 4.1855050e-08
      IF STRUPCASE(chord) EQ 'M27' AND wavelength EQ 6561.0 THEN calib= 3.6014230e-08
      IF STRUPCASE(chord) EQ 'M28' AND wavelength EQ 6561.0 THEN calib= 3.6364319e-08
      IF STRUPCASE(chord) EQ 'M29' AND wavelength EQ 6561.0 THEN calib= 3.7738204e-08
      IF STRUPCASE(chord) EQ 'M31' AND wavelength EQ 6561.0 THEN calib= 3.3841367e-08
  ENDIF
  IF shot GE 163900 THEN BEGIN
      ;; Temporary
      IF STRUPCASE(chord) EQ 'M02' AND wavelength EQ 6561.0 THEN calib = 1.2e-07
      IF STRUPCASE(chord) EQ 'M03' AND wavelength EQ 6561.0 THEN calib = 1.4e-07
      IF STRUPCASE(chord) EQ 'M04' AND wavelength EQ 6561.0 THEN calib = 1.3e-07
      IF STRUPCASE(chord) EQ 'M05' AND wavelength EQ 6561.0 THEN calib = 1.2e-07
      IF STRUPCASE(chord) EQ 'M06' AND wavelength EQ 6561.0 THEN calib = 1.1e-07
      IF STRUPCASE(chord) EQ 'M07' AND wavelength EQ 6561.0 THEN calib = 1.4e-07
      IF STRUPCASE(chord) EQ 'M08' AND wavelength EQ 6561.0 THEN calib = 1.0e-07

      ;; Temporary
      IF STRUPCASE(chord) EQ 'M17' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08*0.72
      IF STRUPCASE(chord) EQ 'M18' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M19' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M20' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08*0.82
      IF STRUPCASE(chord) EQ 'M21' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M22' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M23' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M24' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M25' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08*0.8
      IF STRUPCASE(chord) EQ 'M26' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M27' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M28' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M29' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M30' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M31' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
      IF STRUPCASE(chord) EQ 'M32' AND wavelength EQ 6561.0 THEN calib= 3.67228e-08
  ENDIF


  GET_OUT:

  RETURN,calib

END
