;performs tail recursive palindrome
;check_res contains the sum of all the result of "check" function


(define (palin str-input check-pos str-len check_res)	;result of each subproblems (smaller cases of the actual problem), result is stored in check_res
	(define res (check str-input check-pos str-len))	;performs the checking if corresponding end character is equal to the current pos of the character checker
	(if 
		(> check-pos (/ str-len 2))
		(palin str-input (- check-pos 1) str-len (+ check_res res))	;whatever the result is, just finish through the string until n/2
		(+ check_res res)											;return the final check_res 
	)
)

;performs case insensitive comparison of characters
;returns 0 if similar, 1 otherwise
(define (check str-input check-pos str-len)
	(if
		(char-ci=? (string-ref str-input (- check-pos 1)) (string-ref str-input (- str-len check-pos)))
		0
		1
	)
)

;performs the actual conversion of palin result to assessment of palindromicity
(define (palin-check str-input)
	(define check_result (palin str-input (string-length str-input) (string-length str-input) 0))
	(display "The string ")
	(display str-input)
	(if 
		(> check_result 0)
		(display " is not a palindrome")
		(display " is a palindrome")
	)
	(display "\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Author: Zenith Arnejo
;Program Description: performs tail-recursive palindrome checking
;Sample Guile call: (palin-check "racecar")