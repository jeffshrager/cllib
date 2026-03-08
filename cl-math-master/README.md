# cl-math

Mathematical library for Common Lisp.

Martin Kersner, m.kersner@gmail.com

## Matrices and Vectors
Matrix is defined as a structure with three slots
* rows
* cols
* data.

*rows* and *cols* slots hold size of matrix. *data* is formed by list of lists, where each list represents row of matrix and individual values of lists represent cells of matrix. Matrix with only one row or one column is considered as vector, therefore any function for matrices can be also applied to vectors. Row and columns indexes starts at 0.

For the large part of functions there aren't any control checks on parameters that are provided.

### Matrix Creation
Matrix with 2 rows and 2 columns containing with two values (1 and 2) in the first row and two values (3 and 4) in the second row can be created with the following command.

```lisp
(make-matrix :rows 2
             :cols 2
             :data '((1 2)
                     (3 4)))
```

However, this approach of creating matrix with known data values is rather error prone. Recommended command for creating new matrix is following.

```lisp
(matrix-from-data '((1 2)
                    (3 4)))
```

If you need to create matrix of specific size (*rows* and *cols*) and fill it with specific *value*, you can use function called *initialize-matrix*.
```lisp
(initialize-matrix rows col val)
```
Because matrices with a single constant value (especially zeros and ones) are often required, functions *empty*, *zeros* and *ones* enable creating such matrices.
```lisp
(empty (rows cols))
(empty cols)

(zeros (rows cols)) ; create a matrix of size rows x cols
(zeros cols) ; create a matrix of size 1 x cols

(ones (rows cols))
(ones cols)
```

In order to generate matrix of the same size as some already existing matrix you can use some of functions like *empty-like* *zeros-like* or *ones-like*. 
```lisp
(empty-like mat) ; matrix will be filled with NIL
(zeros-like mat) ; matrix will be filled with zeros
(ones-like mat) ; matrix will be filled with ones
```

### Matrix Information
After matrix is created we can request information about it.

```lisp
(matrix-rows mat)
(matrix-cols mat)
(matrix-shape mat) ; return list with number of rows and cols
(matrix-data mat)
```

### Data Access
Any row or column of matrix can be accesed via
```lisp
(nth-row mat row-idx)
(nth-col mat col-idx)
```

For more complicated value accesses you should use *[]* function.
```lisp
([] mat :row 0)                  ; first row of matrix
([] mat :row '(0 2))             ; first, second and third row of matrix
([] mat :col 0)                  ; first column of matrix
([] mat :col '(0 2))             ; first, second and third column of matrix
([] mat :row 0 :col 0)           ; value in second row and second column
([] mat :row '(0 1) :col '(0 1)) ; submatrix of size 2x2 from the first two rows and columns
```

### Data Modification
Values of matrix can be replaced with *setf* function. To specify concrete rows and columns, *[]* function should be used.
```lisp
(setf mat (matrix-from-data '((1 2 3)(4 5 6))))
(setf ([] mat :row 1) '((9 9 9)))
```

Following functions let you add new column to matrix *mat*. All new column elements consist of the same value *val*.

```lisp
(prepend-col-val mat val) ; insert as zeroth column
(append-col-val mat val)  ; insert as last column
(insert-col-val mat val :idx col-idx) ; insert column at col-idx position
```

### Matrix Operations
cl-math provides various matrix operations, from simple ones like matrix transpose to more complicated ones (e.g. matrix inverse). Matrix operations are divided to several groups

* single matrix operations
* matrix to matrix operations
* matrix and single value operations
* matrix and row/column operations
* row/column operations

#### Single Matrix Operations
```lisp
(transpose mat)
(det mat) ; determinant
(inv mat) ; inverse of matrix
(sigmoid mat) ; element-wise sigmoid operation on matrix
```

```lisp
(remove-col mat col-idx)
(remove-row mat row-idx)
```

#### Matrix To Matrix Operations
```lisp
(dot mat1 mat2) ; matrix multiplication
(+mm mat1 mat2) ; element-wise matrix addition
(-mm mat1 mat2) ; element-wise matrix subtraction
(*mm mat1 mat2) ; element-wise matrix multiplication
(/mm mat1 mat2) ; element-wise matrix division
```

Matrix concatenation can be performed using *vstack* and *hstack* functions.
```lisp
(vstack mat-left mat-right)
(hstack mat-top mat-bottom)
```

#### Matrix And Single Value Operations
Value *v* represents single number or expression leading to single number.

```lisp
(+mv mat val) ; add value v to each element of matrix m
(-mv mat val) ; subtract value v from each element of matrix m
(*mv mat val) ; multiply value v with each element of matrix m
(/mv mat val) ; divide value v with each element of matrix m
```

#### Matrix And Row/Column Operations
##### Row Operations
```lisp
(+mr mat row) ; add row r to each row of matrix m
(-mr mat row) ; subtract row r from each row of matrix m
(*mr mat row) ; multiply row r with each row of matrix m
(/mr mat row) ; divide row r with each row of matrix m
```

##### Column Operations
```lisp
(+mc mat col) ; add column c to each column of matrix m
(-mc mat col) ; subtract column c from each column of matrix m
(*mc mat col) ; multiply column c with each column of matrix m
(/mc mat col) ; divide column c with each column of matrix m
```

#### Row/Column Operations
```lisp
(nth-col-min mat col-idx)
(nth-col-max mat col-idx)

(nth-row-min mat row-idx)
(nth-row-max mat row-idx)
```

```lisp
(sum-rows mat)
(sum-cols mat)
```
