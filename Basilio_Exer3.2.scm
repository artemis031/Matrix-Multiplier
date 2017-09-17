
;performs dot product on 2 vectors with (0,m) and (m,0) dimension
(define (dot-product vec1 vec2 sum_res)

	(if (null? vec1)	;checks if either of the list is null
		sum_res			;stops call if there is nothing to add anymore	
		;else{}
		(dot-product (cdr vec1) (cdr vec2) (+ sum_res (* (car vec1) (car vec2))))
		;here we call the cdr of each vector, which returns a sublist(a list without the first element)
		;Then the accumulator would be the sum, of the sum of the product of the two first elements and the current accumulator.
	)
)

;Basically returns a converted specified columns of matrixB into rows for easier dot-product function calls
(define (col-to-row twoLevelList listToBeReturned index)
	;(define acc (list (append (caar twoLevelList) acc) ))
	(if (null? twoLevelList)
		listToBeReturned
		(col-to-row (cdr twoLevelList) (append listToBeReturned (list (list-ref (car twoLevelList) index ) ) ) index )		
	) 
)

;This function returns a single row of matrixC, to be appended on a higher-level function that will call it.
(define (rowPlacer rowOfMatrixA matrixB columnIndexOfB listAccumulator)
	(if (= (length listAccumulator) (length (car matrixB)))		;stops if the length of Accumulator is equal to the length of the column of matrixB
		listAccumulator
		(rowPlacer rowOfMatrixA matrixB (+ columnIndexOfB 1) (append listAccumulator (list (dot-product rowOfMatrixA (col-to-row matrixB '() columnIndexOfB) 0 ))))
	)
)

;This function is big picture of the outputs of the previous ones.
;Get the result of each corresponding row, and append them one by one to matmul_res
(define (matrix-mul mat1 mat2 matmul_res)
	(if (null? mat1)		
		matmul_res
		(matrix-mul (cdr mat1) mat2 (append matmul_res (list (rowPlacer (car mat1) mat2 0 '()))))
	)
)


;sample runs
(define matrixA (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12 ) (list 13 14 15 16 )))
(define matrixB (list (list 1 2 3 4 5 6 7) (list 8 9 10 11 12 13 14) (list 15 16 17 18 19 20 21) (list 22 23 24 25 26 27 28) ))
(define answer2 (matrix-mul matrixA matrixB '() ))



;creates new matrix for the next row headers to be used as vector 2 of dot product
; (define (next_row curr_head new_head)

; )

;creates the result of multiplication in terms of column
; (define (mul-col vec_mat1 mat2 col_res)

; )


;Author: Jefferson Basilio
;Program Description: performs tail-recursive matrix multiplication