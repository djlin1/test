;; Get spred data.
;; This and bag_get_spred_active_data are like
;; /e/4d/4dbrian/4dlib/FITTING/get_spred_cx_signal.pro
;; Required inputs are shot
;; NBI structure can be passed as well, or it will be gotten
;; Here are cross-sections that get used in imp_dens.pro
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_c6.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_he2.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_c6.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_n7.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_o8.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef93#h_ne10.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef97#h_en2_kvi#c6.dat
;; get_cxe_rates.F:             1  CXE 65 file = qef97#h_en2_kvi#he2.dat
;; 
;; It says on the website that the units are ph/cm**2/s/sr
FUNCTION BAG_SPRED_GET_DATA,shot

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN,{ierr:1}
  ENDIF
  
  ;; Get SPRED time histories for a few lines
  ;; The wavelength is after the _ in Angstroms
  tag = ['HeII_304',$
          'LiIII_114',$
          'BV_262',$
          'CVI_182',$
          'NVII_134',$
          'OVIII_102',$
          'NeX_187']

  ;; Line IDs for cross-sections
  line = ['He II 2-1',$
          'Li III 3-1',$  ;; Making this right makes the code bomg
          'B V 3-2',$
          'C VI 3-2',$
          'N VII 3-2',$
          'O VIII 3-2',$
          'Ne X 4-3']

  ;; Wavelength in Angstroms
  wavelength = FLTARR(N_ELEMENTS(tag))
  FOR i=0,N_ELEMENTS(tag)-1 DO wavelength[i] = FLOAT( (STRSPLIT(tag[i],'_',/EXTRACT))[1] )

  ;; Charge of ion that recieves beam neutral electron
  Zi = [2.0, 3.0, 5.0, 6.0, 7.0, 8.0, 10.0]

  ;; This has been copied from get_spred_data2
  ;; Use realistic values for SPRED geometry.
  ;; Spred sits right above the 315T0 port.
  ;; I have chnaged this to the TANG1 geomfac because it
  ;; was so close for 30LT.  BAG 20130608
  ;; R = [1.7702, 1.80559]
  ;; PHI = [13.56, 20.15]
  lens_r   = 2.7351
  lens_z   = 0.0343
  lens_phi = 318.110
  beam = ['30lt','30rt','15lt','15rt','21lt','21rt','33lt','33rt']
  geomfac  = [3.129, 2.848, 0., 0., 0., 0., 0., 0.]
  view_r   = [1.77, 1.80]
  view_z   = [0.0530, 0.0530]
  view_phi = [13.51, 20.15]

  geom = {lens_r:lens_r,$
          lens_z:lens_z,$
          lens_phi:lens_phi,$
          beam:beam,$
          geomfac:geomfac,$
          view_r:view_r,$
          view_z:view_z,$
          view_phi:view_phi}

  ;; Make a blank CER-type structure based on the total history of
  ;; SPRED data (passive+active)
  GADAT,time,tmpy,tag[0],shot,/ALLDATA,XMIN=0.0
  stime = time - SHIFT(time,1) ;; ~ 2.0 ms 
  stime[0] = MEAN(stime[1:*])
  ;; Time is actually reporetd at the end of the integration time, so
  ;; it has to be shifted backwards by stime
  time-=stime
  tmp_chan = {gain:1.0,$
              lens_phi:lens_phi,$
              lens_r:lens_r,$
              lens_z:lens_z,$
              line:'',$
              wavelength:0.,$
              zi:0.,$
              amp:time*0.0,$
              amperr:time*0.0,$
              time:time,$
              stime:stime,$
              ierr:1}
  chan = REPLICATE(tmp_chan,N_ELEMENTS(line))

  MESSAGE,'Setting SPRED Intensity Error to 10%',/CONT
  FOR i=0,N_ELEMENTS(tag)-1 DO BEGIN
      chan[i].line = line[i]
      chan[i].wavelength = wavelength[i]
      chan[i].Zi=Zi[i]
      GADAT,time,signal,tag[i],shot,/ALLDATA,ERR=gadat_err,$
        XMIN=0.0
      IF gadat_err EQ 0 THEN BEGIN
          chan[i].amp = signal
          chan[i].amperr = 0.10*signal
          chan[i].ierr = 0
      ENDIF
  ENDFOR
  
  result = {shot:shot,$
            tag:tag,$
            line:line,$
            wavelength_unit:'Angstroms',$
            amp_unit:'ph/s/cm**2/sr',$
            geom:geom,$
            chan:chan,$
            code:'SPRED',$
            ierr:0}

  RETURN,result

END
