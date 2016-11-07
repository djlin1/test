FUNCTION read_abs_calib, chord, wave=wave, file=file, noheader=noheader

default, wave, 5290.5
if n_elements(file) eq 0 then BEGIN
   ;;file = '/cerbl/intensity/' + chord + '_abs.calib'
   file='/fusion/projects/results/cerbl/intensity/'+chord+'_abs.calib'
ENDIF

openr, unit, file, /get_lun
c=''
if ~keyword_set(noheader) then $
  while strmid(c, 0, 5) ne '*****' do readf, unit, c

j=-1
while ~ eof(unit) do begin
    readf, unit, c
    if strmid(c, 1, 1) ne ' ' then begin
        ;;;this is a new shot sequence
        j++
        shot1 = long(strmid(c, 0, 6))
        shot = concat(shot, shot1)
        cal = concat(cal, !values.f_nan)
    endif
    wave1 = float(strmid(c, 5, 7))
    if abs(wave1-wave) lt 1 then begin
        ;;;this is the wavelength we want
        cal1 = float((strmid(c, 13, 6)))
        cal[j] = cal1
    endif
endwhile
free_lun, unit
n = n_elements(shot)
if n eq 1 then ind=1 else ind = indgen(n)

chord1 = strsplit(file_basename(file), '_', /extract)
chord1 = chord1[0]

RETURN, {chord: chord1, wave: wave, ind: ind, shot: shot, cal: cal}
END
