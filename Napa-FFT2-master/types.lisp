(deftype index ()
  `(unsigned-byte #. (min (1- (integer-length most-positive-fixnum))
                          (integer-length (1- array-dimension-limit)))))

(deftype half-index ()
  `(unsigned-byte #.(truncate (integer-length most-positive-fixnum) 2)))

(deftype size ()
  `(and (integer 1) index))

(deftype half-size ()
  `(and (integer 1) half-index))

(deftype complex-sample ()
  `(complex double-float))

(deftype complex-sample-array (&optional size)
  `(simple-array complex-sample (,size)))
