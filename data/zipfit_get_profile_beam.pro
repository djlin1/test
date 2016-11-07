

;; 20140604 - Corrected error where initial pdensity was returned.
FUNCTION ZIPFIT_GET_PROFILE_BEAM,shot,time_in,RUNID=runid,$
                                 BEAM_STR=beam_str,$
                                 BEAM_NAME=beam_name

  result={ierr:1}

  status = MDSPLUS_SETUP()
  MDSOPEN,'IONS',shot,STATUS=status

  IF ~(status MOD 2) THEN BEGIN ;; Failure
      MESSAGE,'Failure to open IONS Tree',/CONT
      GOTO,GET_OUT
  ENDIF

  rmesh=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:RMESH')
  beam_att=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:BEAM_ATT') ;; [rmesh, frac, beam_name, time]
  beam_att_err=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:BEAM_ATT_ERR')
  n2frac=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:N2FRAC')
  n3frac=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:N3FRAC')
  n4frac=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:N4FRAC')
  ;; The EFIT times
  bmatt_time=MDSVALUE('\IONS::TOP.IMPDENS.BEAM:BMATT_TIME')
  foo = MIN((bmatt_time-time_in)^2,wht)
  time = bmatt_time[wht]
  MESSAGE,'Returning ZIPFIT Beam Deposition at t='+STRTRIM(time,2)+' ms',/CONT

  ;; If we were passed an NBI strcutrue from GET_NBI( )
  ;; then use it, otherwise we'll need to get it to compute the beam
  ;; neutral densities.
  IF N_ELEMENTS(beam_str) EQ 0 THEN BEGIN
      MESSAGE,'Getting NBI',/CONT
      beam_str = GET_NBI(shot)
  ENDIF ELSE BEGIN
      
  ENDELSE

  ;; For each time we need to add on a 500 ms (+/-)250 ms NBI history.
  whb = WHERE(beam_str.time GE (time-250.0)*1.e-3 AND $
              beam_str.time GE (time-250.0)*1.e-3,nwhb)

  beam = {ierr:beam_str.ierr,$
          shot:beam_str.shot,$
          volts:beam_str.volts*1.e-3,$ ;; kV (from "old_units")
          power:beam_str.power,$
          names:beam_str.names,$
          time:beam_str.time[whb],$
          ibeam:beam_str.ibeam[whb,*],$
          beamangleh:beam_str.beamangleh}


  IF ~KEYWORD_SET(beam_name) THEN BEGIN
      ;; Compute for each beam
      pc=NRL_FORMULARY(/MKS)
      ab = 2d0 ;; deuterium
      pdensity=FLTARR(N_ELEMENTS(rmesh),3,N_ELEMENTS(beam.names)) ;; pencil density
      pdensity_err = pdensity
      source_cfracs=FLTARR(3,N_ELEMENTS(beam.names))
      source_pfracs=source_cfracs
      source_nfracs=source_cfracs

      FOR ib=0,N_ELEMENTS(beam.names)-1 DO BEGIN
          IF beam.volts[ib] NE 0 THEN BEGIN

              beam_fractions = BEAM_GET_FRACTIONS(beam.volts[ib],/CHUCK)
              source_cfracs[*,ib] = beam_fractions.cfracs
              pff = beam_fractions.pfracs[0]
              pfh = beam_fractions.pfracs[1]
              pft = beam_fractions.pfracs[2]
              
              source_pfracs[*,ib]=[pff,pfh,pft]
              ;; This is the "power" attenuation as a function of radius
              fpower=beam.power[ib]*pff*REFORM(beam_att[*,0,ib,wht])
              fpower_err = beam.power[ib]*pff*REFORM(beam_att_err[*,0,ib,wht])
              hpower=beam.power[ib]*pfh*REFORM(beam_att[*,1,ib,wht])
              hpower_err=beam.power[ib]*pfh*REFORM(beam_att_err[*,1,ib,wht])
              tpower=beam.power[ib]*pft*REFORM(beam_att[*,2,ib,wht])
              tpower_err=beam.power[ib]*pft*REFORM(beam_att_err[*,2,ib,wht])
              ;; For density as a function of radius, need velocity.
              ;; Full, half and third velocities.
              fvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(ab*pc.mp.value) ;; m/s
              hvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(2d0*ab*pc.mp.value)
              tvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(3d0*ab*pc.mp.value)
              ;; Density attenuation is power attenuation/(e (Einj/ab) v_b)
              fdensity=fpower/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              fdensity_err = fpower_err/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              hdensity=hpower/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              hdensity_err=hpower_err/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              tdensity=tpower/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)
              tdensity_err=tpower_err/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)

              ;; This is the pencil density in particles / m
              pdensity[*,0,ib] = fdensity
              pdensity_err[*,0,ib] = fdensity_err
              pdensity[*,1,ib] = hdensity
              pdensity_err[*,1,ib] = hdensity_err
              pdensity[*,2,ib] = tdensity
              pdensity_err[*,2,ib] = tdensity_err

          ENDIF
      ENDFOR
  ENDIF ELSE BEGIN
      ;; Compute for each beam
      pc=NRL_FORMULARY(/MKS)
      ab = 2d0 ;; deuterium
      pdensity=FLTARR(N_ELEMENTS(rmesh),3,N_ELEMENTS(bmatt_time)) ;; pencil density
      pdensity_err = pdensity
      source_cfracs=FLTARR(3,N_ELEMENTS(bmatt_time))
      source_pfracs=source_cfracs
      source_nfracs=source_cfracs
      
      ib=WHERE(beam_name EQ beam.names)
      ib=ib[0]
      FOR bt=0,N_ELEMENTS(bmatt_time)-1 DO BEGIN
          IF beam.volts[ib] NE 0 THEN BEGIN

              beam_fractions = BEAM_GET_FRACTIONS(beam.volts[ib],/CHUCK)
              source_cfracs[*,bt] = beam_fractions.cfracs
              pff = beam_fractions.pfracs[0]
              pfh = beam_fractions.pfracs[1]
              pft = beam_fractions.pfracs[2]
              
              source_pfracs[*,bt]=[pff,pfh,pft]
              ;; This is the "power" attenuation as a function of radius
              fpower=beam.power[ib]*pff*REFORM(beam_att[*,0,ib,bt])
              fpower_err = beam.power[ib]*pff*REFORM(beam_att_err[*,0,ib,bt])
              hpower=beam.power[ib]*pfh*REFORM(beam_att[*,1,ib,bt])
              hpower_err=beam.power[ib]*pfh*REFORM(beam_att_err[*,1,ib,bt])
              tpower=beam.power[ib]*pft*REFORM(beam_att[*,2,ib,bt])
              tpower_err=beam.power[ib]*pft*REFORM(beam_att_err[*,2,ib,bt])
              ;; For density as a function of radius, need velocity.
              ;; Full, half and third velocities.
              fvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(ab*pc.mp.value) ;; m/s
              hvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(2d0*ab*pc.mp.value)
              tvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(3d0*ab*pc.mp.value)
              ;; Density attenuation is power attenuation/(e (Einj/ab) v_b)
              fdensity=fpower/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              fdensity_err = fpower_err/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              hdensity=hpower/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              hdensity_err=hpower_err/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              tdensity=tpower/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)
              tdensity_err=tpower_err/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)

              ;; This is the pencil density in particles / m
              pdensity[*,0,bt] = fdensity
              pdensity_err[*,0,bt] = fdensity_err
              pdensity[*,1,bt] = hdensity
              pdensity_err[*,1,bt] = hdensity_err
              pdensity[*,2,bt] = tdensity
              pdensity_err[*,2,bt] = tdensity_err
              
          ENDIF
      ENDFOR
  ENDELSE
  

  result = {beam:beam,$
            rmesh:rmesh,$
            beam_att:REFORM(beam_att[*,*,*,wht]),$
            beam_att_err:REFORM(beam_att_err[*,*,*,wht]),$
            n2frac:REFORM(n2frac[*,*,*,wht]),$
            n3frac:REFORM(n3frac[*,*,*,wht]),$
            n4frac:REFORM(n4frac[*,*,*,wht]),$
            pdensity:pdensity,$ ;; pencil denisty m**-1
            pdensity_err:pdensity_err,$
            bmatt_time:bmatt_time }
  
  GET_OUT:
  RETURN,result

END
