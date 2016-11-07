;; This file contains the fiducial offsets between beam-into-gas and
;; wavecal.
;; The number reported here is the b.i.g. centroid - the wavecal
;;                                                   centroid.
FUNCTION DALPHA_GET_FIDUCIAL_OFFSET,shot,chord

  result={ierr:1}
  IF shot GE 144677  AND shot LE 147778 THEN BEGIN
      IF chord EQ 'm01' THEN offset=  0.022d
      IF chord EQ 'm02' THEN offset=  0.016d
      IF chord EQ 'm03' THEN offset=  0.013d
      IF chord EQ 'm04' THEN offset= -0.301d
      IF chord EQ 'm05' THEN offset=  0.024d
      IF chord EQ 'm06' THEN offset= -0.118d
      IF chord EQ 'm07' THEN offset= -0.126d
      IF chord EQ 'm08' THEN offset= -0.051d
      IF chord EQ 'm09' THEN offset=  0.028d
      IF chord EQ 'm10' THEN offset= -0.239d
      IF chord EQ 'm11' THEN offset=  0.419d
      IF chord EQ 'm12' THEN offset=  0.381d
      IF chord EQ 'm13' THEN offset= -0.563d
      IF chord EQ 'm14' THEN offset= -0.057d
      IF chord EQ 'm15' THEN offset=  0.326d
      IF chord EQ 'm16' THEN offset= -0.050d
  ENDIF ELSE IF shot GE 148158 AND shot LE 150958 THEN BEGIN
      IF chord EQ 'm01' THEN offset=  0.0d
      IF chord EQ 'm02' THEN offset=  0.0d
      IF chord EQ 'm03' THEN offset=  0.0d
      IF chord EQ 'm04' THEN offset=  0.0d
      IF chord EQ 'm05' THEN offset=  0.0d
      IF chord EQ 'm06' THEN offset=  0.0d
      IF chord EQ 'm07' THEN offset=  0.0d
      IF chord EQ 'm08' THEN offset=  0.0d
      IF chord EQ 'm09' THEN offset=  0.0d
      IF chord EQ 'm10' THEN offset=  0.0d
      IF chord EQ 'm11' THEN offset=  0.0d
      IF chord EQ 'm12' THEN offset=  0.0d
      IF chord EQ 'm13' THEN offset=  0.0d
      IF chord EQ 'm14' THEN offset=  0.0d
      IF chord EQ 'm15' THEN offset=  0.0d
      IF chord EQ 'm16' THEN offset=  0.0d
  ENDIF ELSE IF shot GE 151500 AND shot LE 155696 THEN BEGIN
      IF chord EQ 'm01' THEN offset=  0.0d
      IF chord EQ 'm02' THEN offset=  (699.37d0 - 699.51d0)
      IF chord EQ 'm03' THEN offset=  (697.11d0 - 697.33d0)
      IF chord EQ 'm04' THEN offset=  (696.51d0 - 696.68d0)
      IF chord EQ 'm05' THEN offset=  (709.53d0 - 709.34d0)
      IF chord EQ 'm06' THEN offset=  (710.27d0 - 710.39d0)
      IF chord EQ 'm07' THEN offset=  (704.71d0 - 704.64d0)
      IF chord EQ 'm08' THEN offset=  (703.18d0 - 703.32d0)
      IF chord EQ 'm09' THEN offset=  (700.93d0 - 701.12d0)
      IF chord EQ 'm10' THEN offset=  (558.15d0 - 558.45d0)
      IF chord EQ 'm11' THEN offset=  (552.59d0 - 552.37d0)
      IF chord EQ 'm12' THEN offset=  (552.57d0 - 552.45d0)
      IF chord EQ 'm13' THEN offset=  (706.04d0 - 706.38d0)
      IF chord EQ 'm14' THEN offset=  (707.65d0 - 707.69d0)
      IF chord EQ 'm15' THEN offset=  (690.39d0 - 690.06d0)
      IF chord EQ 'm16' THEN offset=  (689.14d0 - 689.18d0)
  ENDIF ELSE IF shot GE 156339 AND shot LE 161800 THEN BEGIN
      ;;                              cold       WC fiducial
      IF chord EQ 'm01' THEN offset= (406.417d - 406.19392d)
      IF chord EQ 'm02' THEN offset= (407.236d - 407.10732d)
      IF chord EQ 'm03' THEN offset= (404.366d - 404.16568d)
      IF chord EQ 'm04' THEN offset= (403.951d - 403.61740d)
      IF chord EQ 'm05' THEN offset= (411.570d - 411.48884d)
      IF chord EQ 'm06' THEN offset= (412.763d - 412.74331d)
      IF chord EQ 'm07' THEN offset= (406.867d - 406.58587d)
      IF chord EQ 'm08' THEN offset= (405.184d - 405.23852d)
      IF chord EQ 'm09' THEN offset= 0.0;(d - d)
      IF chord EQ 'm10' THEN offset= (407.583d - 407.50348d)
      IF chord EQ 'm11' THEN offset= (409.096d - 408.63400d)
      IF chord EQ 'm12' THEN offset= (403.070d - 403.01170d)
      IF chord EQ 'm13' THEN offset= (403.498d - 402.94229d)
      IF chord EQ 'm14' THEN offset= 0.0;(d - d)
      IF chord EQ 'm15' THEN offset= 0.0;(d - d)
      IF chord EQ 'm16' THEN offset= 0.0;(d - d)
      IF chord EQ 'm17' THEN offset= (395.866d - 395.91447d)
      IF chord EQ 'm18' THEN offset= (397.666d - 397.63957d)
      IF chord EQ 'm19' THEN offset= (395.147d - 394.93264d)
      IF chord EQ 'm20' THEN offset= (394.191d - 393.90039d)
  ENDIF ELSE IF shot GE 161800 AND shot LE 163899 THEN BEGIN
      ;;                              cold       WC fiducial
      IF chord EQ 'm01' THEN offset= (670.39 - 670.19)
      IF chord EQ 'm02' THEN offset= (671.77 - 671.91)
      IF chord EQ 'm03' THEN offset= (674.71 - 674.67)
      IF chord EQ 'm04' THEN offset= (673.42 - 673.46)
      IF chord EQ 'm05' THEN offset= (674.32 - 674.34)
      IF chord EQ 'm06' THEN offset= (674.94 - 675.04)
      IF chord EQ 'm07' THEN offset= (672.08 - 672.09)
      IF chord EQ 'm08' THEN offset= (671.65 - 671.33)
      IF chord EQ 'm09' THEN offset= 0.0
      IF chord EQ 'm10' THEN offset= (546.14 - 546.13)
      IF chord EQ 'm11' THEN offset= (546.78 - 546.78)
      IF chord EQ 'm12' THEN offset= (537.06 - 536.97)
      IF chord EQ 'm13' THEN offset= (535.88 - 535.90)
      IF chord EQ 'm14' THEN offset= 0.0
      IF chord EQ 'm15' THEN offset= 0.0
      IF chord EQ 'm16' THEN offset= 0.0
      IF chord EQ 'm17' THEN offset= (534.39 - 534.40)
      IF chord EQ 'm24' THEN offset= (535.18 - 534.93)
      IF chord EQ 'm25' THEN offset= (534.58 - 534.73)
      IF chord EQ 'm26' THEN offset= (535.13 - 535.10)
      IF chord EQ 'm27' THEN offset= (535.64 - 535.79)
      IF chord EQ 'm28' THEN offset= (534.28 - 534.21)
      IF chord EQ 'm29' THEN offset= (533.89 - 533.92)
      IF chord EQ 'm31' THEN offset= (533.31 - 533.48)
  ENDIF ELSE IF shot GE 163900 THEN BEGIN
      IF chord EQ 'm01' THEN offset = (673.769 - 673.790)
      IF chord EQ 'm02' THEN offset = (674.309 - 674.533)
      IF chord EQ 'm03' THEN offset = (675.275 - 675.373)
      IF chord EQ 'm04' THEN offset = (675.875 - 675.989)
      IF chord EQ 'm05' THEN offset = (669.312 - 669.324)
      IF chord EQ 'm06' THEN offset = (668.453 - 668.539)
      IF chord EQ 'm07' THEN offset = (667.992 - 668.197)
      IF chord EQ 'm08' THEN offset = (666.701 - 666.760)
      IF chord EQ 'm09' THEN offset = (0.0); (683.141
      IF chord EQ 'm10' THEN offset = (682.531 - 682.369)
      IF chord EQ 'm11' THEN offset = (682.492 - 682.481)
      IF chord EQ 'm12' THEN offset = (684.755 - 684.721)
      IF chord EQ 'm13' THEN offset = (678.884 - 678.843)
      IF chord EQ 'm14' THEN offset = (678.969 - 678.985)
      IF chord EQ 'm15' THEN offset = (678.560 - 678.546)
      IF chord EQ 'm16' THEN offset = (677.324 - 677.344)
      IF chord EQ 'm17' THEN offset = (534.854 - 535.356)
      IF chord EQ 'm18' THEN offset = (534.992 - 535.394)
      IF chord EQ 'm19' THEN offset = (535.587 - 535.826)
      IF chord EQ 'm20' THEN offset = (535.518 - 535.941)
      IF chord EQ 'm21' THEN offset = (536.558 - 536.539)
      IF chord EQ 'm22' THEN offset = (536.136 - 536.143)
      IF chord EQ 'm23' THEN offset = (535.857 - 536.002)
      IF chord EQ 'm24' THEN offset = (534.336 - 534.788)
      IF chord EQ 'm25' THEN offset = (523.634 - 523.628)
      IF chord EQ 'm26' THEN offset = (525.562 - 525.629)
      IF chord EQ 'm27' THEN offset = (524.156 - 524.375)
      IF chord EQ 'm28' THEN offset = (525.336 - 525.410)
      IF chord EQ 'm29' THEN offset = (519.401 - 519.447)
      IF chord EQ 'm30' THEN offset = (517.855 - 518.048)
      IF chord EQ 'm31' THEN offset = (517.590 - 517.658)
      IF chord EQ 'm32' THEN offset = (516.708 - 516.789)
  ENDIF ELSE BEGIN
      MESSAGE,'Shot out of range!!!',/CONT
      MESSAGE,'Do **NOT** use this fiducial for velocity',/CONT
      offset=0.
  ENDELSE

  result={ierr:0,value:offset}

  RETURN,result


END
