pro defualt,var,default,verbose=verbose
message,/cont,'You spell it DEFAULT, not DEFUALT'
default, var,default,verbose=verbose
end

pro default,var,default,comment,verbose=verbose
; sets var to default, unless it already has a value (i.e. is defined) bdb, 91
; much clearer than if n_elements(foo) eq 0.....
if n_elements(comment) eq 0 then comment=''
if (n_elements(var) eq 0) then begin
  var=default 
  if keyword_set(verbose) then print, 'default [',default,'] taken, '+comment
endif else if keyword_set(verbose) then $
	print, 'default: variable was defined,  ', $
		strtrim(n_elements(var),2), ' elements'
return
end

