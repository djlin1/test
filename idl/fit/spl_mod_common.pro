
;     4dlib/FITTING/spl_mod_common.pro

;  980520  TBT  Added model_interp
;  960129  TBT  Created file spl_mod_common.pro so common could be included.

common reflcom, corerefldata, edgerefldata
common ldatacom, ldata
common fitcom,bc,knotval,ydatafit
common data_tofitcom,xdata,ydata,ydata_err, ydata_norm, ydata_in_time_block
common data_outcom,  xout, yout, yout_err,  yout_norm,   yout_in_time_block
common modelcom, m
common pointcom, pp
common normtimes, norm_ntimes     ; # of times for normalzation
;;;common fit_interpolationcom, model_interp   ; linear or spline
