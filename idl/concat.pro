FUNCTION concat, a, b, dimension=dimension

if n_elements(a) eq 0 then RETURN, b

default, dimension, 1
case dimension of
    1: RETURN, [a, b]
    2: RETURN, [[a], [b]]
    3: RETURN, [[[a]], [[b]]]
    else: message, 'Cannot concatonate over higher than DIMENSION'
endcase
END

; FUNCTION concat, a, b

; if n_elements(a) gt 0 then RETURN, [a, b] $
; else RETURN, b

; END
