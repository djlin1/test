;; Return the sign of a scalar or array
FUNCTION SIGN,a
  RETURN,FLOAT(a)/ABS(FLOAT(a))
END
