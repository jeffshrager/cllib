# Matrix &harr; List Conversions

As explained [earlier](https://github.com/martinkersner/cl-math/blob/master/README.md#matrices-and-vectors) matrices
are [structures](https://github.com/martinkersner/cl-math/blob/master/matrix.lisp#L119) with three slots

* rows
* columns
* data.

*data* are represented as list of lists, where list represents rows of *matrix*.

```common-lisp
; data of matrix (2 rows, 1 column) represented with lists
(setf data '((1 2 3)
             (4 5 6)))
```

*rows* and *columns* are used for dimensionality checks in some matrix operations, therefore those values
have to be kept updated.

## Matrices vs Vectors
Matrices and vectors are represented with the same *matrix* data structure. (Row) vectors can be expressed
with only one list, however *matrix* data structure expects that each list representation of matrix (and vector as well)
is wrapped in one main list which holds all rows.

```common-lisp
; OK matrix
'((1 2 3)
 (4 5 6)))
 
; BAD vector
'(1 2 3)

; OK vector
'((1 2 3))
```

## Matrices
Functions in this section can be applied even to well defined (row) vectors as described in previous section.
If you want to create *matrix* data structure from not well defined (row) vector or obtain (row) vector without
outer wrapping list, read section [*Vectors*](https://github.com/martinkersner/cl-math/master/matrix-list-conversions.md#vectors) below.

### List &rarr; Matrix
The simplest way of creating *matrix* from lists is using `matrix-from-data` function. `matrix-from-data` automatically
computes *rows* and *cols* and control consistency of matrix dimensions.

```common-lisp
(setf data '((1 2 3)
             (4 5 6)))
             
(matrix-from-data data)

;#S(MATRIX :ROWS 2 :COLS 3 :DATA ((1 2 3) (4 5 6)))
```

### Matrix &rarr; List
In some cases, when we want to obtain only *data* part of *matrix* structure, `matrix-data` function can be used.

```common-lisp
(setf mat
  (matrix-from-data '((1 2 3)
                      (4 5 6))))

(matrix-data mat)

;((1 2 3) (4 5 6))
```

## Vectors
Following functions should be used if you either create matrix from not well defined row vector or if you want to
obtain list vector without outer wrapping list.

### List &rarr; Vector
`matrix-from-data-peel` function add wrapping outer list so matrix can be correctly constructed.

```common-lisp
(setf row-vector '(1 2 3))

(matrix-from-data-peel row-vector)

;#S(MATRIX :ROWS 1 :COLS 3 :DATA ((1 2 3)))
```

### Vector &rarr; List
In cases, when you know your matrix has only one row (row vector), you will find `matrix-data-peel` function useful.
`matrix-data-peel` returns data of matrix without outer wrapping list.

```common-lisp
(setf row-matrix
  (matrix-from-data-peel '((1 2 3))))

(matrix-data-peel row-matrix)

;(1 2 3)
```
