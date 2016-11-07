;;CC 6/0/2015
;;Added 3G system and renamed the file to shorter name
;;CC 5/9/2014
;;This is a cleaned up version of Brian G.'s original code
;;It does NOT require tssub file to run

;;data=get_oblique_data(shot,/FUDGE)

;;INPUT
;;shot
;;keyword FUDGE to subtract crosstalk between channels
 
;;OUTPUT
;;Structure containing the calibrated oblique FIDA data (Radiance
;;vs. wavelength) and spatial data for each chord. Contains all the
;;data (no timeslice subtraction applied in this code)


;;It does not use the dreaded keyword white when accessing data from
;;cerview. I added extra spatial calibration data (like for each of
;;the three fibers that make up each chord). 

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; This should be done with a text file.
;; Similar to CER or CER Calibration.
;; In a central repository.
;; A text file with columns [ disp, fid, calib, pr0, pr1, radius]
;; that occurs after each shot that things change.
;; Some of this information I've found in muscatello's area
;; I think that the jackpot is
;; /u/muscatel/idl/fianalysis/2GFIDA/CALIB/
;; apply_intens_calib.pro has the details for applying the calib
;; write_intens_calib.pro writes the calibration arrays
;; get_abs_intensity.pro get the labsphere stuff
;;
;; Muscatello has calibration files for shots
;; pre 143160 
;;  (2010 back calib)
;; 143161 - 143700 (2010 forward calib)
;; 143701 - on (2011 back calib)
;; Have 2011 back as 2012 forward.
;; Have 2012 back as well (not processed yet).
;; The calibration spectrum Chris has looks different than the one Xi
;; has given me.  Chris's obviously correct for the blocking bar, Xi's
;; do not.
;;
;; Discussion with Xi Chen. 20130130.
;; Here's the mapping we will use.
;; CERVIEW chords 'f03' and f04' have three "channels" each, labeled
;; 2,4,6 and 1,3,5 repspectively.  This has never changed.
;; When a full calibration occurs, we get the dispersion and fiducial
;; that is specific to a give channel.
;; Then, for a series of pre-determined patch panels, we get intensity
;; calibration.  This means that each channel has it's own intensity
;; calibration when it's connected to a given patch.
;; Regarding files, the dispersion and fiducials are simple to store,
;; as they are single number.  Each patch has a radius, which is also
;; a single number.
;; Therefore, a text file such as
;; /e/alfven/fida/calib/fid/f04_c1.txt
;; with a series of shots and fiducials is sufficient.
;; i.e.
;; 143701 148701 647.704
;; [shot] [shot] [fid]
;; ...
;; Same for dispersion.
;; 
;; Patch panel changes the radius of the measurement and the
;; calibration file to use.
;;
;; Radius shold be set by a file
;; /e/alfven/fida/calib/radius/f04_p7_c1.txt
;; This will have the spatial calibration.
;; 
;; For inetensity each claibration needs to have its own file.
;; Thus we have to have the file name with shot numbers in it.
;; /e/alfven/fida/calib/inten/f04_p7_c1_[shot]_[shot].txt
;;
;; disp and fid data from
;; /u/muscatel/idl/fianalysis/2GFIDA/CALIB/wl_cal.pro
;; intensity from /u/muscatel/idl/fianalysis/2GFIDA/CALIB/[forward/back]/[PATCH]/...
;;
;; Example shots to check out the calibration are
;; 145183 from 2011 run campaign.
;; 144226 from may 2011.
;+
;; 
;; This has never changed.
;; chan 1 is ALWAYS f04 LHS of spectrum in CERVIEW.
;;
;; FUNCTION BAG_FIDA_GET_OBLIQUE_CHANS,shot,chord
;;
;-
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 

FUNCTION BAG_FIDA_GET_OBLIQUE_CHANS,shot,chord
  result = {ierr:1}

  IF STRCMP(chord,'f03',/FOLD) THEN chan = [2,4,6] 
  IF STRCMP(chord,'f04',/FOLD) THEN chan = [1,3,5]
  IF STRCMP(chord,'f05',/FOLD) THEN chan = 7
  IF STRCMP(chord,'f06',/FOLD) THEN chan = 5
  IF STRCMP(chord,'f07',/FOLD) THEN chan = 3
  IF STRCMP(chord,'f08',/FOLD) THEN chan = 1
  IF STRCMP(chord,'f09',/FOLD) THEN chan = 8
  IF STRCMP(chord,'f10',/FOLD) THEN chan = 6
  IF STRCMP(chord,'f11',/FOLD) THEN chan = 4
  IF STRCMP(chord,'f12',/FOLD) THEN chan = 2
  
  ;; If our conditions were met then we have a valid shot with a known
  ;; FIDA configuration.
  IF N_ELEMENTS(chan) GT 0 THEN BEGIN
      result = {shot:shot,$
                chord:chord,$
                chan:chan,$
                ierr:0}
  ENDIF

  RETURN,result
END

;+
;; 
;;FUNCTION BAG_FIDA_GET_OBLIQUE_DISPERSION,shot,chord,chan
;;
;-
FUNCTION BAG_FIDA_GET_OBLIQUE_DISPERSION,shot,chord,chan

  result = {ierr:1}
  ;;dir = '/e/alfven/fida/calib/disp/'
  dir='/fusion/projects/diagnostics/fida/calib/disp/'
IF STRCMP(chord,'f03',/FOLD) OR  STRCMP(chord,'f04',/FOLD) THEN BEGIN 
  file = FILE_SEARCH(dir,chord+'_c'+STRTRIM(chan,2)+'.txt')
ENDIF ELSE BEGIN
  file = FILE_SEARCH(dir,chord+'.txt')
ENDELSE
  test = FILE_TEST(file)
  IF test THEN BEGIN
      s = ' '      
      OPENR,lun,file,/GET_LUN
      count=0
      WHILE ~EOF(lun) DO BEGIN
          READF,lun,s
          IF ~STRCMP(s,';',1) THEN BEGIN
              IF N_ELEMENTS(strs) EQ 0 THEN BEGIN
                  strs = [s]
              ENDIF ELSE BEGIN
                  strs = [strs,s]
              ENDELSE
              count++
          ENDIF
      ENDWHILE
      CLOSE,lun
      FREE_LUN,lun

      ;; Extract shot range and dispersion.
      shot_start = LONARR(count)
      shot_end = LONARR(count)
      disps = DBLARR(count)
      FOR i=0,count-1 DO BEGIN
          spl = STRSPLIT(strs[i],' ',/EXT)
          shot_start[i] = LONG(spl[0])
          shot_end[i] = LONG(spl[1])
          disps[i] = DOUBLE(spl[2])
      ENDFOR
      ;; Get dispersion for this shot/chord/chan/
      wh = WHERE(shot_start LE LONG(shot) AND shot_end GE LONG(shot),nwh)
      IF nwh EQ 1 THEN BEGIN
          disp = (disps[wh])[0]
      ENDIF ELSE BEGIN

          MESSAGE,'Cannot find valid shot',/CONT
          GOTO,GET_OUT
      ENDELSE
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid dispersion file',/CONT
      GOTO,GET_OUT
  ENDELSE

  IF N_ELEMENTS(disp) GT 0 THEN BEGIN
      result = {shot:shot,$
                chord:chord,$
                chan:chan,$
                disp:disp,$
                ierr:0}
  ENDIF

  GET_OUT:

  RETURN,result
END

;+
;; 
;-
FUNCTION BAG_FIDA_GET_OBLIQUE_FIDUCIAL,shot,chord,chan

  result = {ierr:1}
  
  ;;dir = '/e/alfven/fida/calib/fid/'
  dir='/fusion/projects/diagnostics/fida/calib/fid/'
IF STRCMP(chord,'f03',/FOLD) OR  STRCMP(chord,'f04',/FOLD) THEN BEGIN
  file = FILE_SEARCH(dir,chord+'_c'+STRTRIM(chan,2)+'.txt')
ENDIF ELSE BEGIN
  file = FILE_SEARCH(dir,chord+'.txt')
ENDELSE
  test = FILE_TEST(file)
  IF test THEN BEGIN
      s = ' '      
      OPENR,lun,file,/GET_LUN
      count=0
      WHILE ~EOF(lun) DO BEGIN
          READF,lun,s
          IF ~STRCMP(s,';',1) THEN BEGIN
              IF N_ELEMENTS(strs) EQ 0 THEN BEGIN
                  strs = [s]
              ENDIF ELSE BEGIN
                  strs = [strs,s]
              ENDELSE
              count++
          ENDIF
      ENDWHILE
      CLOSE,lun
      FREE_LUN,lun

      ;; Extract shot range and fiducial.
      shot_start = LONARR(count)
      shot_end = LONARR(count)
      fids = DBLARR(count)
      FOR i=0,count-1 DO BEGIN
          spl = STRSPLIT(strs[i],' ',/EXT)
          shot_start[i] = LONG(spl[0])
          shot_end[i] = LONG(spl[1])
          fids[i] = DOUBLE(spl[2])
      ENDFOR
      ;; Get fiducial for this shot/chord/chan/
      wh = WHERE(shot_start LE LONG(shot) AND shot_end GE LONG(shot),nwh)
      IF nwh EQ 1 THEN BEGIN
          fid = (fids[wh])[0]
      ENDIF ELSE BEGIN
          MESSAGE,'Cannot find valid shot',/CONT
          GOTO,GET_OUT
      ENDELSE
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid fiducial file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:
      
  IF N_ELEMENTS(fid) GT 0 THEN BEGIN
      result = {shot:shot,$
                chord:chord,$
                chan:chan,$
                fid:fid,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+

;; Get pixel range for each spectrum.
;; pixel range is documented starting with 1, but
;; array indices are [pixmin, pixmax]-1 for new file system.
;-
;; Get pixel range
;; Here Xi has given me IDL array subscripts, but pixels start with 1
;; by convention, so I'll have to see what was done to create the
;; dispersion and, especially, fiducial.
FUNCTION BAG_FIDA_GET_OBLIQUE_PIXEL_RANGE,shot,chord,chan

  result = {ierr:1}

  ;;dir = '/e/alfven/fida/calib/pixrange/'
  dir='/fusion/projects/diagnostics/fida/calib/pixrange/'
IF STRCMP(chord,'f03',/FOLD) OR  STRCMP(chord,'f04',/FOLD) THEN BEGIN
  file = FILE_SEARCH(dir,chord+'_c'+STRTRIM(chan,2)+'.txt')
  test = FILE_TEST(file)
ENDIF ELSE BEGIN
pixrange=[1,384]
GOTO,GET_OUT
ENDELSE
  
  IF test THEN BEGIN
      s = ' '      
      OPENR,lun,file,/GET_LUN
      count=0
      WHILE ~EOF(lun) DO BEGIN
          READF,lun,s
          IF ~STRCMP(s,';',1) THEN BEGIN
              IF N_ELEMENTS(strs) EQ 0 THEN BEGIN
                  strs = [s]
              ENDIF ELSE BEGIN
                  strs = [strs,s]
              ENDELSE
              count++
          ENDIF
      ENDWHILE
      CLOSE,lun
      FREE_LUN,lun

      ;; Extract shot range and pixel ranges
      shot_start = LONARR(count)
      shot_end = LONARR(count)
      pixranges = INTARR(2,count)
      FOR i=0,count-1 DO BEGIN
          spl = STRSPLIT(strs[i],' ',/EXT)
          shot_start[i] = LONG(spl[0])
          shot_end[i] = LONG(spl[1])
          pixranges[*,i] = [FIX(spl[2]),FIX(spl[3])]
      ENDFOR
      ;; Get pixel range for this shot/chord/chan/
      wh = WHERE(shot_start LE LONG(shot) AND shot_end GE LONG(shot),nwh)
      IF nwh EQ 1 THEN BEGIN
          pixrange = pixranges[*,wh]
      ENDIF ELSE BEGIN
          MESSAGE,'Cannot find valid shot',/CONT
          GOTO,GET_OUT
      ENDELSE
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid pixel range file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:
     
  IF N_ELEMENTS(pixrange) GT 0 THEN BEGIN
      result = {shot:shot,$
                chord:chord,$
                chan:chan,$
                pixrange:pixrange,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+
;; 
;-
;; Get hot pixels

FUNCTION BAG_FIDA_GET_OBLIQUE_BAD_PIXELS,shot,chord

  result = {ierr:1}

IF shot LT 162177 THEN BEGIN 
  ;; F03
  IF STRCMP(chord,'f03',/FOLD) THEN BEGIN
      pixrange = [77]
  ENDIF
  
  ;; F04
  IF STRCMP(chord,'f04',/FOLD) THEN BEGIN
      pixrange = [-1]
  ENDIF

ENDIF

  ;; F07
  IF STRCMP(chord,'f07',/FOLD) THEN BEGIN
      pixrange = [329]
  ENDIF
  
  IF N_ELEMENTS(pixrange) GT 0 THEN BEGIN
      result = {shot:shot,$
                chord:chord,$
                pixrange:pixrange,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+
;; Get the patch panel
;-
FUNCTION BAG_FIDA_GET_OBLIQUE_PATCH,shot

  result = {ierr:1}
  
  ;;dir = '/e/alfven/fida/calib/patch/'
  dir='/fusion/projects/diagnostics/fida/calib/patch/'
  file = FILE_SEARCH(dir+'patch.txt')
  test = FILE_TEST(file)
  IF test THEN BEGIN
      s = ' ' 
      OPENR,lun,file,/GET_LUN
      count=0
      WHILE ~EOF(lun) DO BEGIN
          READF,lun,s
          IF ~STRCMP(s,';',1) THEN BEGIN
              IF N_ELEMENTS(strs) EQ 0 THEN BEGIN
                  strs = [s]
              ENDIF ELSE BEGIN
                  strs = [strs,s]
              ENDELSE
              count++
          ENDIF
      ENDWHILE
      CLOSE,lun
      FREE_LUN,lun

      ;; Extract shot range and fiducial.
      shot_start = LONARR(count)
      shot_end = LONARR(count)
      patches = INTARR(count)
      FOR i=0,count-1 DO BEGIN
          spl = STRSPLIT(strs[i],' ',/EXT)
          shot_start[i] = LONG(spl[0])
          shot_end[i] = LONG(spl[1])
          patches[i] = FIX(spl[2])
      ENDFOR
      ;; Get patch panel for this shot
      wh = WHERE(shot_start LE LONG(shot) AND shot_end GE LONG(shot),nwh)
      IF nwh EQ 1 THEN BEGIN
          patch = (patches[wh])[0]
      ENDIF ELSE BEGIN
          MESSAGE,'Cannot find valid shot',/CONT
          GOTO,GET_OUT
      ENDELSE
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid patch file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:
      
  IF N_ELEMENTS(patch) GT 0 THEN BEGIN
      result = {shot:shot,$
                patch:patch,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+
;;  This changes with patch for a select subset of radii that are
;;  considered calibrated (PATCH7, PATCH 8, PATCH9).
;-
;; Get calibration factors
;; Oblique FIDA is done differently than CER.
;; According to muscatello's thesis, the specral data is acquired and
;; the conversion from counts/s to radiance is saved as an array,
;; not a single number.
;; The Oblique FIDA is set to run at 1.0 ms all the time, but that
;; could change...so the spectra is divided by the integration time
;; Therefore, the count rate is just the CCD counts (as counts/ms)
;;
;; The calibration spectrum is C(lambda) in (ph/ms-cm**2-nm-sR) / (counts / ms)
;; The spectral data is counts/ms.
;; Therefore, the spectrum is 
;; S(lambda)[counts/ms] * C(lambda) [ph/ms-cm**2-nm-sR]/[counts/ms]
;; Obtaining [ph/ms-cm**2-nm-sR]
;; 
;; Muscatell's code for applying intensity calibration indicates that 
;; for shots LT 143161 use back2010b
;;           143161-143700 use forward2010
;;           143701-999999 use back2011
FUNCTION BAG_FIDA_GET_OBLIQUE_CALIB,shot,patch,chord,chan

  result = {ierr:1}

  ;;dir = '/e/alfven/fida/calib/intens'
  dir='/fusion/projects/diagnostics/fida/calib/intens/'

IF 0 THEN BEGIN 
 dir='/u/collinscs/FIDA/2GFIDA/calib/intens/' 
  print,'USING /u/collinscs/FIDA/2GFIDA/calib/intens'
ENDIF

  ;; Directory of year-to-year calibrations.
  IF shot LT 143161 THEN dir_ext = 'back2010b'
  IF shot GE 143161 AND shot LT 143700 THEN dir_ext = 'forward2010'
  IF shot GE 143701 AND shot LT 156346 THEN dir_ext = 'back2011'
  IF shot GE 156347 AND shot LT 162176 THEN dir_ext = 'forward2014'
  IF shot GE 162177 AND shot LT 999999 THEN dir_ext = 'forward2015'
  ;; Next calibration
;  IF shot GE 

  ;; Patch panels
  dir_patch = 'PATCH'+STRTRIM(patch,2)
  
  ;; IDL .sav file
IF STRCMP(chord,'f03',/FOLD) OR  STRCMP(chord,'f04',/FOLD) THEN BEGIN
  fname = 'intensity_cal_2Gc'+STRTRIM(chan,2)+'.dat'
ENDIF ELSE BEGIN
  fname = 'intensity_cal_3G'+chord+'.dat'
ENDELSE

  file = STRJOIN([dir,dir_ext,dir_patch,fname],PATH_SEP())
print,'calfile: ',file
  test = FILE_TEST(file)
  IF test THEN BEGIN
      RESTORE,file
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid patch file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:  

  IF N_ELEMENTS(cal) GT 0 THEN BEGIN
      result = {shot:shot,$
                chan:chan,$
                calib:cal,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+
;;  This changes with PATCH.
;-
;; Get radius
FUNCTION BAG_FIDA_GET_OBLIQUE_RADIUS,shot,patch,chan,chord

  result = {ierr:1}

  ;;dir = '/e/alfven/fida/calib/patch/'
  dir='/fusion/projects/diagnostics/fida/calib/patch/'
  file = FILE_SEARCH(dir+'patch'+STRTRIM(patch,2)+'.txt')
  test = FILE_TEST(file)
  IF test THEN BEGIN
      s = ' ' 
      OPENR,lun,file,/GET_LUN
      count=0
      max_nr = 0
      WHILE ~EOF(lun) DO BEGIN
          READF,lun,s
          IF ~STRCMP(s,';',1) THEN BEGIN
              IF N_ELEMENTS(strs) EQ 0 THEN BEGIN
                  strs = [s]
              ENDIF ELSE BEGIN
                  strs = [strs,s]
              ENDELSE
              spl = STRSPLIT(s,' ',/EXT)
              max_nr = max_nr>FIX(spl[2])
              count++
          ENDIF
      ENDWHILE
      CLOSE,lun
      FREE_LUN,lun

      ;; Extract shot range and radii
      shot_start = LONARR(count)
      shot_end = LONARR(count)
      nradii = INTARR(count)
      radii = DBLARR(max_nr,count)
      FOR i=0,count-1 DO BEGIN
          spl = STRSPLIT(strs[i],' ',/EXT)
          shot_start[i] = LONG(spl[0])
          shot_end[i] = LONG(spl[1])    
          nradii[i] = FIX(spl[2])
          FOR j=3,nradii[i]+3-1 DO radii[j-3,i] = DOUBLE(spl[j])
      ENDFOR
      ;; Get patch panel for this shot
      wh = WHERE(shot_start LE LONG(shot) AND shot_end GE LONG(shot),nwh)
      IF nwh EQ 1 THEN BEGIN
          nr = (nradii[wh])[0]
          IF STRCMP(chord,'f03',/FOLD) OR  STRCMP(chord,'f04',/FOLD) THEN BEGIN
          radius = (radii[chan-1,wh])[0]
          ENDIF ELSE BEGIN
          radius = (radii[chan-1+6,wh])[0]
          ENDELSE
      ENDIF ELSE BEGIN
          MESSAGE,'Cannot find valid shot',/CONT
          GOTO,GET_OUT
      ENDELSE
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid patch panel radii file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:
      
  IF N_ELEMENTS(radius) GT 0 THEN BEGIN
      result = {shot:shot,$
                chan:chan,$
                radius:radius,$
                ierr:0}
  ENDIF
  
  RETURN,result
END

;+
;; 
;-
;Get detailed spatial calibration data, this is not always available
;for older shots but hopefully will be available from now on
FUNCTION CC_GET_OBLIQUE_SPATIAL,shot,patch

  result = {ierr:1}

  ;;dir = '/e/alfven/fida/calib/patch'
  dir='/fusion/projects/diagnostics/fida/calib/patch/'

  ;; Directory of year-to-year spatial calibrations.
  IF shot LT 148702 THEN dir_ext = '' ;don't have that data
  IF shot GE 148702 AND shot LT 162176 THEN dir_ext = 'forward2012'
  IF shot GE 162177 AND shot LT 999999 THEN dir_ext = 'forward2015'

 ;; IDL .sav file for patch panel
  fname = 'PATCH'+STRTRIM(patch,2)+'.dat'

  file = STRJOIN([dir,dir_ext,fname],PATH_SEP())

  test = FILE_TEST(file)
  IF test THEN BEGIN
      RESTORE,file
      result = {shot:shot,$
                patchdat:patchdat,$
                ierr:0}
  ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid spatial calibration file',/CONT
      GOTO,GET_OUT
  ENDELSE

  GET_OUT:  

;spatial calibration returns structure named patchdat
  
  RETURN,result

END ;cc_get_oblique_spatial



;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; Get Olbique spectroscopic data.
;; This is different than standard CER data because of the way the
;; dispersion and absolute calibraion are applied.
;; For example, shot 150413 for demo.

FUNCTION GET_OBLIQUE_DATA,shot,BEAM=beam,USER=user,FUDGE=fudge,BALANCE=balance

  result={ierr:1}
  COMMON CERVIEW_COMMON

  ;; Set defaults
chords=['f03','f04']
views=[REPLICATE('o',3),REPLICATE('o',3)] ;oblique views
IF shot GE 162177 THEN  chords = ['f03','f04','f05','f06','f07','f08','f09','f10','f11','f12']
IF shot GE 162177 THEN   views = [REPLICATE('o',3),REPLICATE('o',3),  'v',  'v',  'o',  'v',  'o',  'o',  'o',  'o'] 
pchord=UINTARR(N_ELEMENTS(views))
IF shot GE 162177 THEN BEGIN
;hardwire vertical chords
pchord[6]=1
pchord[7]=3
pchord[9]=2
ENDIF
;@@@@@@@@@@@@@@@@@@ 



IF KEYWORD_SET(fudge) THEN BEGIN
   MESSAGE,'Loading fudge factor',/CONT
   ;;restore IDL .sav file
   ;dir = '/u/collinscs/FIDA/2GFIDA/calib/intens'
   ;;dir = '/e/alfven/fida/calib/intens'
   dir='/fusion/projects/diagnostics/fida/calib/intens/'
 ;++++++++++++++++++++++++++++++++++++++++++++++++++
 ;2GFIDA 

   IF shot LT 156347 THEN dir_ext = '' ;don't have that data
   IF shot GE 156347 AND shot LT 162176 THEN BEGIN 
   dir_ext = 'forward2014' & fname = 'fudge2014.dat'
   ENDIF
   IF shot GE 162177 AND shot LT 999999 THEN BEGIN
   dir_ext = 'forward2015' & fname = 'fudge2015_2G.dat'
   ENDIF
   file = STRJOIN([dir,dir_ext,fname],PATH_SEP())
   test = FILE_TEST(file)

 IF test THEN BEGIN
    RESTORE,file ;restore structure called crosstalk
   ;first account for the scatter from c4
   GET_CHORD,shot,'f03' 
    c4pixels=crosstalk.c4.ch_pixels
    c4intstart=crosstalk.c4.pix_int_start
    c4intend=crosstalk.c4.pix_int_end
    ch4counts=chord_data.data[c4pixels[0]:c4pixels[1],*] ;ch4 ccd counts at all timeslices  
    ;find total ccd counts in c4 at pixels that were used to compute fudge factor
    c4brightness=fltarr((SIZE(ch4counts,/DIM))[1])
    FOR j=0,(SIZE(ch4counts,/DIM))[1]-1 DO BEGIN
        c4brightness[j]=total(ch4counts[c4intstart:c4intend,j])
    ENDFOR
   ;then account for the scatter from c3   
   GET_CHORD,shot,'f04' 
    c3pixels=crosstalk.c3.ch_pixels
    c3intstart=crosstalk.c3.pix_int_start
    c3intend=crosstalk.c3.pix_int_end
    ch3counts=chord_data.data[c3pixels[0]:c3pixels[1],*] ;ch3 ccd counts at all timeslices 
    ;find total ccd counts in c3 at pixels that were used to compute fudge factor
    c3brightness=fltarr((SIZE(ch3counts,/DIM))[1])
    FOR j=0,(SIZE(ch3counts,/DIM))[1]-1 DO BEGIN
        c3brightness[j]=total(ch3counts[c3intstart:c3intend,j])
    ENDFOR 
    ;then account for the scatter from c5  
    IF shot GE 162177 THEN BEGIN
    c5pixels=crosstalk.c5.ch_pixels
    c5intstart=crosstalk.c5.pix_int_start
    c5intend=crosstalk.c5.pix_int_end
    ch5counts=chord_data.data[c5pixels[0]:c5pixels[1],*] ;ch5 ccd counts at all timeslices 
    ;find total ccd counts in c5 at pixels that were used to compute fudge factor
    c5brightness=fltarr((SIZE(ch5counts,/DIM))[1])
    FOR j=0,(SIZE(ch5counts,/DIM))[1]-1 DO BEGIN
        c5brightness[j]=total(ch5counts[c5intstart:c5intend,j])
    ENDFOR
    ENDIF   
   ;restore fudge factors
    c3c4fudge=crosstalk.c3.c4fudge
    c3c5fudge=crosstalk.c3.c5fudge
    c3c6fudge=crosstalk.c3.c6fudge
    c4c3fudge=crosstalk.c4.c3fudge
    c4c5fudge=crosstalk.c4.c5fudge
    c4c6fudge=crosstalk.c4.c6fudge
    IF shot GE 162177 THEN c5c4fudge=crosstalk.c5.c4fudge
   ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid fudge factor data for 2GFIDA',/CONT
   ENDELSE
 ;++++++++++++++++++++++++++++++++++++++++++++++++++
 ;3GFIDA 
  IF shot GE 162177 AND shot LT 999999 THEN dir_ext = 'forward2015' & fname = 'fudge2015_3G.dat'
    file = STRJOIN([dir,dir_ext,fname],PATH_SEP())
    test = FILE_TEST(file)
    IF test THEN BEGIN
    RESTORE,file ;restore structure called crosstalk3G
    ENDIF ELSE BEGIN
      MESSAGE,'Cannot find valid fudge factor data for 3GFIDA',/CONT
   ENDELSE
 
ENDIF ;fudge
;@@@@@@@@@@@@@@@@@@

  ;; Set the beam we're looking at
  IF ~KEYWORD_SET(beam) THEN $
    beam = '210rt'  ;; for FIDA

  ;; Get the patch panel for this shot
  patch_str = BAG_FIDA_GET_OBLIQUE_PATCH(shot)
  IF patch_str.ierr THEN GOTO,GET_OUT

  ;; Get the detailed spatial calibration for this shot
  spatial= CC_GET_OBLIQUE_SPATIAL(shot,patch_str.patch)

  ;; Get the chord and channel strings
  FOR i=0,N_ELEMENTS(chords)-1 DO BEGIN 
      chan_str = BAG_FIDA_GET_OBLIQUE_CHANS(shot,chords[i])
   IF STRCMP(chords[i],'f03',/FOLD) OR  STRCMP(chords[i],'f04',/FOLD) THEN BEGIN   
      FOR j=0,N_ELEMENTS(chan_str.chan)-1 DO BEGIN
          IF N_ELEMENTS(chords_chans) EQ 0 THEN $
            chords_chans = chords[i]+'_c'+STRTRIM(chan_str.chan[j],2) $
          ELSE $
            chords_chans = [chords_chans,chords[i]+'_c'+STRTRIM(chan_str.chan[j],2)]
      ENDFOR
  ENDIF ELSE BEGIN                         ;2GFIDA
  chords_chans = [chords_chans,STRTRIM(chan_str.chord,2)]
  ENDELSE
  
  ENDFOR
            
  result = {shot:shot,$
            chords:chords_chans,$
            views:views,$
            beam:beam,$
            ierr:1}        

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;we need to see if there is a tssub file for this shot
;if so, we will get NBI histories, store active and bg spectra
 cerfit_dir=STRJOIN([GETENV('HOME'),'cerfitfida'],PATH_SEP())
 IF KEYWORD_SET(user) THEN cerfit_dir=STRJOIN(['/home',user,'cerfitfida'],PATH_SEP())
 dir=STRJOIN([cerfit_dir,STRTRIM(shot,2)],PATH_SEP())
 test_tssub_shot = FILE_TEST(dir,/DIR)

beam_str=-1
IF test_tssub_shot THEN BEGIN
 MESSAGE,'TSSUB file found for shot'+STRTRIM(shot,2),/CONT
  ;; Get the NBI histories so we can divide out the fraction of time
  ;; the CX beam is on.
  ;; Set the beam name so we can do a STRPOS with beam_str.names
cx_beam = BAG_TSSUB2_BEAM_NAME(beam)
;cx_beam='21rt'
MESSAGE,'Getting NBI',/CONT
  beam_str = GET_NBI(shot)
  wh_cx_beam = WHERE(STRPOS(beam_str.names,cx_beam) GT 0,nwh_cx_beam)
ENDIF
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;+++++++++++++++
  FOR i=0,N_ELEMENTS(chords)-1 DO BEGIN
      ;; Get data for this FIDA chord
      MESSAGE,'Getting chord '+chords[i],/CONT
      ;; Do not use /WHITE because the intesity calibration is a
      ;; spectrum as a function of pixel, which is the whitelight
      ;; response.  Setting /WHITE here will double-count the
      ;; witelight response.  Also, this is based on an old whitelight
      ;; response file give to Dave Kaplan (according to Muscatello)
      ;; that is old and invalid.
;      GET_CHORD,shot,chords[i],/WHITE ;; BAG 20140416
      GET_CHORD,shot,chords[i] 
      IF chord_data.shot EQ -1 THEN GOTO,SKIP_CHORD

;+++++++
;Get data and the dataerr
      data = chord_data.data ;this includes dark background subtraction and is an array [1024,8000]

      ;; Get the channels (i.e. 1,3,5) for this chord
      chan_str = BAG_FIDA_GET_OBLIQUE_CHANS(shot,chords[i])
      ;; If this fails then do no more work.
      IF chan_str.ierr THEN GOTO,SKIP_CHORD

      ;; Run through each bad pixel and remove the anomaly
       badpix_str = BAG_FIDA_GET_OBLIQUE_BAD_PIXELS(shot,chords[i])
      IF ~badpix_str.ierr THEN BEGIN
          nbad = N_ELEMENTS(badpix_str.pixrange)
          FOR j=0,nbad-1 DO BEGIN
              ;; Badpix for raw data
              FOR k=0L,(SIZE(data,/DIM))[1]-1 DO BEGIN
                  data[badpix_str.pixrange[0],k] = $
                    (data[badpix_str.pixrange[0]-1,k]+$
                     data[badpix_str.pixrange[0]+1,k])/2.0
              ENDFOR
          ENDFOR
      ENDIF

      ;; Divide by integration time to turn the spectrum into counts/ms rather than doing it
      ;; channel-by-channel
      FOR j=0L,(SIZE(data,/DIM))[1]-1 DO data[*,j] /= (chord_data.t_integ[j])
      
      ;; Error of raw data
      dataerr = SQRT(ABS(data))>1.0

;check if tssub is available for this chord
 dir=STRJOIN([cerfit_dir,STRTRIM(shot,2),chords[i]],PATH_SEP())
 test_tssub_chord = FILE_TEST(dir,/DIR)
 IF test_tssub_chord THEN  MESSAGE,'Processing TSSUB for chord '+chords[i],/CONT


;++++++++++++++++++ 
;; Now channel-by-channel calculations
      FOR j=0,N_ELEMENTS(chan_str.chan)-1 DO BEGIN
print,chords[i]
print,chan_str.chan[j]

          ;; For each chan on this chord, we need to get the pixel
          ;; range, dispersion, fiducial, and calibration to turn
          ;; spectrum[pixel,time] to intensity[lambda,time]
          disp_str = BAG_FIDA_GET_OBLIQUE_DISPERSION(shot,chords[i],chan_str.chan[j])
          IF disp_str.ierr THEN BEGIN
              MESSAGE,'No Dispersion',/CONT
              GOTO,SKIP_CHAN
          ENDIF
          fid_str = BAG_FIDA_GET_OBLIQUE_FIDUCIAL(shot,chords[i],chan_str.chan[j])
          IF fid_str.ierr THEN GOTO,SKIP_CHAN
          pixrange_str = BAG_FIDA_GET_OBLIQUE_PIXEL_RANGE(shot,chords[i],chan_str.chan[j])
          IF pixrange_str.ierr THEN GOTO,SKIP_CHAN
          calib_str = BAG_FIDA_GET_OBLIQUE_CALIB(shot,patch_str.patch,chords[i],chan_str.chan[j])
          IF calib_str.ierr THEN GOTO,SKIP_CHAN

          ;; FIDA does dispersion different than CER
          pixel = DINDGEN(pixrange_str.pixrange[1]-pixrange_str.pixrange[0]+1)
          pixel += pixrange_str.pixrange[0]
 

;; All data for this channel (no tssub)
          chan_data = data[pixrange_str.pixrange[0]-1:pixrange_str.pixrange[1]-1,*]
          chan_dataerr = dataerr[pixrange_str.pixrange[0]-1:pixrange_str.pixrange[1]-1,*]
;; Get processed spectra and background spectra for this channel
          ;chan_background =  spec_str.background[pixrange_str.pixrange[0]-1:pixrange_str.pixrange[1]-1,*]
         ; chan_active = spec_str.spectra[pixrange_str.pixrange[0]-1:pixrange_str.pixrange[1]-1,*]

          rawdata=chan_data

;before calibration, subtract contaminating CCD counts
;do this by applying fudge factor: light from c3 scatters to c4,c5,c6
;contaminating CCD counts are linearly proportional to the cold light in c3 
;subtract contaminating counts at each timeslice, so that c4=c6-c3c4fudge*c3brightness  

IF KEYWORD_SET(fudge) THEN BEGIN
 
   IF STRCMP(chords[i],'f03',/FOLD) OR  STRCMP(chords[i],'f04',/FOLD) THEN BEGIN  
       IF shot LT 162742 THEN BEGIN
       IF chan_str.chan[j] EQ 4 THEN BEGIN
       MESSAGE,'Applying scattered light correction to c4',/CONT       
           IF shot GE 162177 THEN BEGIN
             FOR tt=0,(SIZE(chan_data,/DIM))[1]-1 DO chan_data[*,tt]-= c3c4fudge*c3brightness[tt] +c5c4fudge*c5brightness[tt] 
           ENDIF ELSE BEGIN
            FOR tt=0,(SIZE(chan_data,/DIM))[1]-1 DO chan_data[*,tt]-= c3c4fudge*c3brightness[tt]
           ENDELSE
       ENDIF  
       IF chan_str.chan[j] EQ 5 THEN BEGIN  
       MESSAGE,'Applying scattered light correction to c5',/CONT
           FOR tt=0,(SIZE(chan_data,/DIM))[1]-1 DO chan_data[*,tt]-= c3c5fudge*c3brightness[tt] + c4c5fudge*c4brightness[tt]
       ENDIF
       IF chan_str.chan[j] EQ 6 THEN BEGIN 
       MESSAGE,'Applying scattered light correction to c6',/CONT
           FOR tt=0,(SIZE(chan_data,/DIM))[1]-1 DO chan_data[*,tt]-= c3c6fudge*c3brightness[tt] + c4c6fudge*c4brightness[tt] 
       ENDIF
       ENDIF
   ENDIF ELSE BEGIN;3Gfida
      IF chords[i] EQ 'f05' THEN fudgefrom=['f12','f11','f10'] &$
      IF chords[i] EQ 'f06' THEN fudgefrom=['f11','f10','f07'] &$
      IF chords[i] EQ 'f07' THEN fudgefrom=['f11','f10','f09','f08'] &$;f11f07 is small but hits fida region
      IF chords[i] EQ 'f08' THEN fudgefrom=['f12','f10','f09','f07'] &$;f12f08 is small but hits fida region
      IF chords[i] EQ 'f09' THEN fudgefrom=['f10','f08','f07'] &$
      IF chords[i] EQ 'f10' THEN fudgefrom=['f12','f11','f09','f07'] &$
      IF chords[i] EQ 'f11' THEN fudgefrom=['f05','f10'] &$
      IF chords[i] EQ 'f12' THEN fudgefrom=['f11','f10','f05']
      nt=(SIZE(chan_data,/DIM))[1] &$
      npix=(SIZE(chan_data,/DIM))[0]
      noise=FLTARR(npix,nt) &$
       FOR ff=0,N_ELEMENTS(fudgefrom)-1 DO BEGIN &$
        location=WHERE(STRCMP(crosstalk3G.desc,fudgefrom[ff]+chords[i],/FOLD)) &$
        dims=SIZE(crosstalk3G.desc,/DIMENSIONS) &$
        ind=ARRAY_INDICES(dims,location,/DIMENSIONS) &$
        get_chord,shot,fudgefrom[ff] &$
        bright=fltarr(nt) &$
        FOR tt=0,nt-1 DO BEGIN &$
        bright[tt]=TOTAL(chord_data.data[*,tt]) &$
        noise[*,tt]+=bright[tt]*crosstalk3G.fudge[ind[0],ind[1],*] &$
        ENDFOR  &$; times
       ENDFOR
       MESSAGE,'Applying scattered light correction to 3GFIDA, '+chords[i],/CONT
       FOR tt=0,nt-1 DO chan_data[*,tt]-= noise[*,tt]     
   ENDELSE
ENDIF  ;fudge          
 

        
;wavelength calibration by fitting oxygen line
          lambda_guess = disp_str.disp * pixel + fid_str.fid ;; nm
          lambda_guess*= 10.0 ;; Angstroms.          
;          PRINT,'O-V Pixel: '+chords[i]+'.'+STRTRIM(chan_str.chan[j],2),INTERPOL(pixel,lambda_guess,6500.24)
         
          ;; Time-average raw data for wavecal
          ntimes=(size(chord_data.data,/DIM))[1]-1   
          subntimes=min([5000,ntimes]) ;take the first 5 seconds of the shot is probably good enough   
          subset=chan_data[*,0:subntimes]
          data_avg = TOTAL(subset,2)/N_ELEMENTS(subset[1,*])
          ;; Fit the Oxygen line for true wavecal
          guess_range = 20.0 ;; A
          guess_line = 6500.24 ;; A
          wh_guess = WHERE(lambda_guess GE guess_line - guess_range/2.0 AND $
                           lambda_guess LE guess_line + guess_range/2.0,nwh_guess)
          toFitX = lambda_guess[wh_guess]
          toFitY = data_avg[wh_guess]
          gParams = [100.0, guess_line,3.0,10.0,0.1]
          gFit = GAUSSFIT(toFitX,toFitY,gParams,NTERMS=5)
          fiducial = gParams[1] ;; Apparent O-V line location
          MESSAGE,'Guess/Fit Fiducial Difference:'+STRTRIM(ABS(fiducial-guess_line),2)+' A',/CONT
          toFitX -= (fiducial-guess_line) ;; For overplotting
        
          ;; Calibrate the spectrum
          ;; Use the active count rate (counts/sec)
          ;; Apply calibration
          cal_lambda = calib_str.calib.lambda*10.0 ;; Angstroms
          cal_conversion = calib_str.calib.rad_conversion ;; ph/ms-cm**2-nm-sR
 ;+++++++++         
; cal_conversion = calib_str.calib.new_rad_conversion ;; ph/ms-cm**2-nm-sR
;---------         
          cal_conversion *= 1.e3 ;; ms/s
          cal_conversion *= 0.1 ;; nm/A
          cal_conversion *= 1.e4 ;; (cm/m)^2
          ;; cal conversion is now standard in mks ph/s-m**2-A-sR
          ;;see how far off the wavelength calibration is
          ;;...if it's way off don't use it

          IF ABS(fiducial-guess_line) LT 2. AND max(tofity) GT 10. THEN BEGIN          
          lambda = lambda_guess - (fiducial-guess_line) ;; Correct our guess
          ;; Put the calibration onto our wavelength axis.
          cal_interp = INTERPOL(cal_conversion,cal_lambda,lambda)
          ENDIF ELSE BEGIN
          lambda = lambda_guess; 
          cal_interp = cal_conversion;
          MESSAGE,'Did not apply OV wavelength calibration',/CONT
          ENDELSE 
       
  
          ;at each point in time, apply calibration
          FOR k=0,(SIZE(chan_data,/DIM))[1]-1 DO BEGIN                      
              chan_data[*,k]*=FLOAT(cal_interp) ;; ph/s-m**2-A-sR
              chan_dataerr[*,k]*=FLOAT(cal_interp) ;; ph/s-m**2-A-sR
          ENDFOR
          
; help,cal_conversion,cal_lambda,lambda   
;          help,data,chan_data
     

;Apply tssub if available 
      spec=-1
      err=-1
      time=-1
      active=-1
      bg=-1
      bg_all=-1 
      tssub_str=-1  
      bgbalance=0

      activeonly=-1
      errao=-1
      activetime=-1

      IF test_tssub_chord THEN BEGIN
          spec_str=get_spectrum(shot,chords[i],beam,/NEUTRONS,USER=user,CALIBRATED_DATA=chan_data,T_START=chord_data.t_start,BALANCE=balance)     
          IF ~spec_str.ierr THEN BEGIN
              tssub_str = spec_str.tssub_str
              
              IF WHERE(TAG_NAMES(tssub_str) EQ 'ts') EQ -1 && $
                (N_ELEMENTS(tssub_str.ts) GT 1 || tssub_str.ts NE 0) THEN BEGIN
                  err=0.*spec_str.spectra 
                  FOR jj=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
                      ;; Run through each slice and divide by the fraction of time the
                      ;; beam is on 
;                       whts=WHERE(tssub_str.ts[jj] EQ chord_data.t_start)
                      cer_t_start = chord_data.t_start[tssub_str.ts[jj]-1]
                      cer_t_integ = chord_data.t_integ[tssub_str.ts[jj]-1]
;                       cer_t_start = chord_data.t_start[whts]
;                       cer_t_integ = chord_data.t_integ[whts]
                      wh_t_nbi = WHERE(beam_str.time GE cer_t_start*1.e-3 AND $
                                       beam_str.time LE (cer_t_start+cer_t_integ)*1.e-3,nwh_t_nbi)
                      frac = MEAN(FLOAT(beam_str.ibeam[wh_t_nbi,wh_cx_beam[0]]))
                      spec_str.spectra[*,jj]/=frac
                      
                      ;; Form the intrinsic error of each pixel's counts by adding
                      ;; the error on the active and background counts in quadrature.
                      FOR k=0,(SIZE(spec_str.spectra,/DIM))[0]-1 DO BEGIN
                          erra = SQRT(ABS(spec_str.active[k,jj]))>1.0
                          errb = SQRT(ABS(spec_str.background[k,jj]))>1.0
                          err[k,jj] = SQRT(erra^2 + errb^2)
                      ENDFOR ;;k 
                  ENDFOR ;;jj
                  spec=spec_str.spectra
                  active=spec_str.active
                  bg=spec_str.background
                  bg_all=spec_str.all_background
                  bgbalance=spec_str.balance
                  time=spec_str.time
              ENDIF ELSE BEGIN
                  spec=-1
                  active=-1
                  bg=-1
                  bg_all=-1
                  bgbalance=-1
                  time=-1
              ENDELSE

;               IF WHERE(TAG_NAMES(tssub_str) EQ 'ACT_TIME') NE -1 && $
;                 (N_ELEMENTS(tssub_str.act_only) GT 1 || tssub_str.act_only NE 0) THEN BEGIN
              IF WHERE(TAG_NAMES(tssub_str) EQ 'ACT_TIME') NE -1 && $
                (N_ELEMENTS(tssub_str.act_time) GT 1 || tssub_str.act_time NE 0) THEN BEGIN
                  errao=0.*spec_str.active_only
;                   FOR jj=0,N_ELEMENTS(tssub_str.act_only)-1 DO BEGIN
                  FOR jj=0,N_ELEMENTS(tssub_str.act_time)-1 DO BEGIN
                      ;; Run through each slice and divide by the fraction of time the
                      ;; beam is on 
;                       whact=WHERE(tssub_str.act_time[jj] EQ chord_data.t_start)
; ;                       cer_t_start = chord_data.t_start[tssub_str.act_only[jj]-1]
; ;                       cer_t_integ = chord_data.t_integ[tssub_str.act_only[jj]-1]
;                       cer_t_start = chord_data.t_start[whact]
;                       cer_t_integ = chord_data.t_integ[whact]
;                       wh_t_nbi = WHERE(beam_str.time GE cer_t_start*1.e-3 AND $
;                                        beam_str.time LE (cer_t_start+cer_t_integ)*1.e-3,nwh_t_nbi)
;                       frac = MEAN(FLOAT(beam_str.ibeam[wh_t_nbi,wh_cx_beam[0]]))
; ;                       spec_str.active_only[*,jj]/=frac
                      
                      ;; Form the intrinsic error of each pixel's counts by adding
                      ;; the error on the active and background counts in quadrature.
                      FOR k=0,(SIZE(spec_str.active_only,/DIM))[0]-1 DO BEGIN
                          erra = SQRT(ABS(spec_str.active_only[k,jj]))>1.0
                          errao[k,jj] = SQRT(erra^2)
                      ENDFOR ;;k 
                  ENDFOR ;;jj
                  activeonly=spec_str.active_only
                  activetime=spec_str.active_time
              ENDIF ELSE BEGIN
                  activeonly=-1
                  activetime=-1
              ENDELSE
          ENDIF ;;spec_str.ierr
      ENDIF ;test_tssub_chord


      

radius_str = BAG_FIDA_GET_OBLIQUE_RADIUS(shot,patch_str.patch,chan_str.chan[j],chords[i])

;;GET detailed spatial data if it is available
        
         IF spatial.ierr THEN BEGIN
         spatial_cal = {ierr:spatial.ierr}
         ENDIF ELSE BEGIN
         tags = TAG_NAMES(spatial.patchdat)
         IF STRCMP(chords[i],'f03',/FOLD) OR  STRCMP(chords[i],'f04',/FOLD) THEN BEGIN
         currentchord=chords[i]+'_c'+STRTRIM(chan_str.chan[j],2)
         ENDIF ELSE BEGIN
         currentchord=chords[i]
         ENDELSE
         whc = WHERE(STRCMP(tags,currentchord,/FOLD))
         spatial_cal = {X:spatial.patchdat.(whc).X,$
                        Y:spatial.patchdat.(whc).Y,$
                        R:spatial.patchdat.(whc).R,$
                        PHI:spatial.patchdat.(whc).PHI,$
                        FIBERS:spatial.patchdat.(whc).fibers,$
                        UNITS:spatial.patchdat.(whc).units,$
                        ierr:spatial.ierr}
         ENDELSE

;naming convention
wcc= WHERE(STRCMP(chords_chans,currentchord,/FOLD))
IF views[wcc] EQ 'o' THEN pchord[wcc]=FIX((min(spatial.patchdat.(whc).fibers.fibers)-1)/3)+1
          chord_str = {wavelength:lambda,$
                       radius:radius_str.radius,$
                       data:chan_data,$
                       dataerr:chan_dataerr,$
                       datatime:chord_data.t_start[0:ntimes],$
                       integtime:chord_data.t_integ[0:ntimes],$
                       spec:spec,$
                       err:err,$
                       time:time,$
                       active:active,$
                       bg:bg,$
                       bg_all:bg_all,$
                       balance:bgbalance,$
                       activeonly:activeonly,$
                       errao:errao,$
                       activetime:activetime,$
                       tssub_str:tssub_str,$
                       spatial:spatial_cal,$
                       geomfac:1.0,$
                       calib:1.0,$
                       cal:cal_interp,$
                       disp:disp_str.disp,$
                       gain:1.0,$
                       wc_wl:toFitX,$
                       wc_fit:gFit,$
                       pixel:pixel,$
                       rawdata:rawdata,$
                       ierr:0}
                       
                       
          result = CREATE_STRUCT(result,currentchord,chord_str)
          GOTO,CHORD_GOOD          
          ;; We go here if the chan data is in error
          SKIP_CHAN:
          result = CREATE_STRUCT(result,currentchord,{ierr:1})
          CHORD_GOOD:
      ENDFOR
      SKIP_CHORD:
  ENDFOR
  ;; No error if we've made it thus far
  result.ierr=0
 ;; Add the NBI infomation for later use in the attenuation data.
  result = CREATE_STRUCT(result,'beam_str',beam_str)
  GET_OUT:

  ;; ADD mds channel 
  result = CREATE_STRUCT(result,{mdsch:'ch'+STRTRIM(STRING(pchord,FORMAT='(I02)'),2)})
  RETURN,result

END
