FUNCTION rm_spikes,y,gap=gap
; WWH
; Remove spikes associated with neutron/gamma hits from a spectrum
; Ignore the end points because they aren't used

; INPUT    y; the spectrum
; OUTPUT   the spectrum with spikes removed

np=n_elements(y)
ave=total(y)/np
if not keyword_set(gap) then gap=1.0*ave

yy=y
for i=2,np-3 do $
  if y[i]-y[i-2] gt gap and y[i]-y[i+2] gt gap then yy[i]=.5*(y[i-2]+y[i+2])
for i=4,np-5 do $
  if y[i]-y[i-4] gt gap and y[i]-y[i+4] gt gap then yy[i]=.5*(y[i-4]+y[i+4])


return,yy
end

