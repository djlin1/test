;; This is the geometry factor formed by integrating
;; 1/(pi*wy*wz)exp(-(y/wy)^2-(z/wz)^2) dl
;; giving a factor 1/m for the path length integral.
;; Here, we can take a pencil density in particles m**-1 and multiply
;; by the geomfac to get line-integrated density 
;; \int dl[m] nb(x,y,z)[m**-3] -> particles [m**-2]
;; geomfac[m**-1] * nbp[m**-1] -> particles [m**-2]
;; 
;; Or, this can be used to remove any geometrical factors from a
;; line-integrated measurement (such as beam-into-gas BES) by taking
;; ph/s-m**-2-sR and dividing by geomfac to get ph/s-m-sR which should
;; have no horizontal or vertical shape or divergence assocaited with
;; it, purely attenutation (if there is any).
FUNCTION DALPHA_GET_GEOMFAC,shot,chord,beam

  ;; 30LT
  IF shot GE 144101 AND $
    STRCMP(chord,'m01',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.05510

  IF shot GE 144101 AND $
    STRCMP(chord,'m02',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.14588

  IF shot GE 144101 AND $
    STRCMP(chord,'m03',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.19367

  IF shot GE 144101 AND $
    STRCMP(chord,'m04',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.25732

  IF shot GE 144101 AND $
    STRCMP(chord,'m05',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.33219

  IF shot GE 144101 AND $
    STRCMP(chord,'m06',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.43129

  IF shot GE 144101 AND $
    STRCMP(chord,'m07',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.50907

  IF shot GE 144101 AND $
    STRCMP(chord,'m08',/FOLD) AND $
    STRCMP(beam,'30L',3,/FOLD) THEN $
    geomfac = 3.57571

  ;; 30RT
  IF shot GE 144101 AND $
    STRCMP(chord,'m01',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.68303

  IF shot GE 144101 AND $
    STRCMP(chord,'m02',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.73414

  IF shot GE 144101 AND $
    STRCMP(chord,'m03',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.77440

  IF shot GE 144101 AND $
    STRCMP(chord,'m04',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.81244

  IF shot GE 144101 AND $
    STRCMP(chord,'m05',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.85637

  IF shot GE 144101 AND $
    STRCMP(chord,'m06',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.90790

  IF shot GE 144101 AND $
    STRCMP(chord,'m07',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.95486

  IF shot GE 144101 AND $
    STRCMP(chord,'m08',/FOLD) AND $
    STRCMP(beam,'30R',3,/FOLD) THEN $
    geomfac = 2.99279



  ;; 210RT
  IF shot GE 144101 AND $
    STRCMP(chord,'m09',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.44806

  IF shot GE 144101 AND $
    STRCMP(chord,'m10',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.41634

  IF shot GE 144101 AND $
    STRCMP(chord,'m11',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.38893

  IF shot GE 144101 AND $
    STRCMP(chord,'m12',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.35595

  IF shot GE 144101 AND $
    STRCMP(chord,'m13',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.33119

  IF shot GE 144101 AND $
    STRCMP(chord,'m14',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.31674

  IF shot GE 144101 AND $
    STRCMP(chord,'m15',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.28770

  IF shot GE 144101 AND $
    STRCMP(chord,'m16',/FOLD) AND $
    STRCMP(beam,'210R',4,/FOLD) THEN $
    geomfac = 2.25790

  ;; 330LT
  IF shot GE 144101 AND $
    STRCMP(chord,'m09',/FOLD) AND $
    STRCMP(beam,'330L',4,/FOLD) THEN $
    geomfac = 2.79709

  ;; 330LT
  IF shot GE 144101 AND $
    STRCMP(chord,'m07',/FOLD) AND $
    STRCMP(beam,'330R',4,/FOLD) THEN $
    geomfac = 2.35130
  
  IF shot GE 156399 THEN BEGIN
      ;; These all use 156477
      IF STRCMP(chord,'m01',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 2.71549
      
      IF STRCMP(chord,'m02',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.13492
      
      IF STRCMP(chord,'m03',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.19931

      IF STRCMP(chord,'m04',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.26800

      IF STRCMP(chord,'m05',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.32971

      IF STRCMP(chord,'m06',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.41963

      IF STRCMP(chord,'m07',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.51285

      IF STRCMP(chord,'m08',/FOLD_CASE) AND $
        STRCMP(beam,'30l',3,/FOLD_CASE) THEN $
        geomfac = 3.57092

      IF STRCMP(chord,'m01',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.50003

      IF STRCMP(chord,'m02',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.74553

      IF STRCMP(chord,'m03',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.77828

      IF STRCMP(chord,'m04',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.81586

      IF STRCMP(chord,'m05',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.85637

      IF STRCMP(chord,'m06',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.90790

      IF STRCMP(chord,'m07',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.95486

      IF STRCMP(chord,'m08',/FOLD_CASE) AND $
        STRCMP(beam,'30r',3,/FOLD_CASE) THEN $
        geomfac = 2.99279

      IF STRCMP(chord,'m09',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.30293

      IF STRCMP(chord,'m10',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.28268

      IF STRCMP(chord,'m11',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.27056

      IF STRCMP(chord,'m12',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.24100

      IF STRCMP(chord,'m13',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.23923

      IF STRCMP(chord,'m14',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.23186

      IF STRCMP(chord,'m15',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.22456

      IF STRCMP(chord,'m16',/FOLD_CASE) AND $
        STRCMP(beam,'210l',4,/FOLD_CASE) THEN $
        geomfac = 2.21052

      IF STRCMP(chord,'m09',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.42726

      IF STRCMP(chord,'m10',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.38847

      IF STRCMP(chord,'m11',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.36025

      IF STRCMP(chord,'m12',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.32279

      IF STRCMP(chord,'m13',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.31219

      IF STRCMP(chord,'m14',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.29437

      IF STRCMP(chord,'m15',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.26507

      IF STRCMP(chord,'m16',/FOLD_CASE) AND $
        STRCMP(beam,'210r',4,/FOLD_CASE) THEN $
        geomfac = 2.24061

      IF STRCMP(chord,'m17',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.79309

      IF STRCMP(chord,'m18',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.65003

      IF STRCMP(chord,'m19',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.62657

      IF STRCMP(chord,'m20',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.59269

      IF STRCMP(chord,'m21',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.72705

      IF STRCMP(chord,'m22',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.71446

      IF STRCMP(chord,'m23',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.70165

      IF STRCMP(chord,'m24',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.68671

      IF STRCMP(chord,'m25',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.67518

      IF STRCMP(chord,'m26',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.66304

      IF STRCMP(chord,'m27',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.64960

      IF STRCMP(chord,'m28',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.63750

      IF STRCMP(chord,'m29',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.62647

      IF STRCMP(chord,'m30',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.61391

      IF STRCMP(chord,'m31',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.60348

      IF STRCMP(chord,'m32',/FOLD_CASE) AND $
        STRCMP(beam,'330l',4,/FOLD_CASE) THEN $
        geomfac = 2.59072
      

      IF STRCMP(chord,'m17',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.55000

      IF STRCMP(chord,'m18',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.45804

      IF STRCMP(chord,'m19',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.44329

      IF STRCMP(chord,'m20',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.42208       
      
      IF STRCMP(chord,'m21',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.50710
      
      IF STRCMP(chord,'m22',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.49900
      
      IF STRCMP(chord,'m23',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.49079
      
      IF STRCMP(chord,'m24',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.48123
      
      IF STRCMP(chord,'m25',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.47393
      
      IF STRCMP(chord,'m26',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.46623
      
      IF STRCMP(chord,'m27',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.45771
      
      IF STRCMP(chord,'m28',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.45008
      
      IF STRCMP(chord,'m29',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.44318
      
      IF STRCMP(chord,'m30',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.43528
      
      IF STRCMP(chord,'m31',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.42876

      IF STRCMP(chord,'m32',/FOLD_CASE) AND $
        STRCMP(beam,'330r',4,/FOLD_CASE) THEN $
        geomfac = 2.42080
      
  ENDIF
  

  IF N_ELEMENTS(geomfac) EQ 0 THEN geomfac=-1.

  RETURN,geomfac


END
