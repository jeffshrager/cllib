;; in-order out-of-place transform
;; decimation order is pretty bad, cache wise,
;; but the constant factors are really nice.

(defun gen-fft/1 (&key (dst 'dst)
                    (src 'src)
                    (startd 'startd)
                    (strided 1)
                    (starts 'starts)
                    (strides 1)
                    (twiddle 'twiddle)
                    (scale   1d0)
                    tmp startt
                    cooley-tukey)
  (declare (ignore strided strides twiddle cooley-tukey
                   tmp startt))
  `(progn
     (setf (ref ,dst ,startd) (scale (ref ,src ,starts)
                                     ,scale))
     ,dst))

(defun gen-fft/2 (&key (dst 'dst)
                    (src 'src)
                    (startd 'startd)
                    (strided 1)
                    (starts 'starts)
                    (strides 1)
                    (twiddle 'twiddle)
                    (scale   1d0)
                    tmp startt
                    cooley-tukey)
  (declare (ignore twiddle cooley-tukey tmp startt))
  `(let ((s0 (scale (ref ,src ,starts) ,scale))
         (s1 (scale (ref ,src (+ ,starts ,strides)) ,scale)))
     (setf (ref ,dst              ,startd) (+ s0 s1)
           (ref ,dst (+ ,startd ,strided)) (- s0 s1))
     ,dst))

;; direction!

(defun gen-fft/4 (&key (dst 'dst)
                    (src 'src)
                    (startd 'startd)
                    (strided 1)
                    (starts 'starts)
                    (strides 1)
                    (twiddle 'twiddle)
                    (scale   1d0)
                    tmp startt
                    cooley-tukey)
  (declare (ignore twiddle cooley-tukey tmp startt))
  `(macrolet ((@src (&optional (index 0))
                `(scale (ref ,',src (+ ,',starts (* ,index ,',strides)))
                        ,',scale))
              (@dst (&optional (index 0))
                `(ref ,',dst (+ ,',startd (* ,index ,',strided)))))
     (let* ((s0 (@src))
            (s2 (@src 2))
            (s0+s2 (+ s0 s2))
            (s0-s2 (- s0 s2))
            (s1 (@src 1))
            (s3 (@src 3))
            (s1+s3 (+ s1 s3))
            (s1-s3 (mul+i (- s1 s3))))
       (setf (@dst 0) (+ s0+s2 s1+s3)
             (@dst 1) (- s0-s2 s1-s3)
             (@dst 2) (- s0+s2 s1+s3)
             (@dst 3) (+ s0-s2 s1-s3))
       ,dst)))

(defun gen-fft/8 (&key (dst 'dst)
                    (src 'src)
                    (startd 'startd)
                    (strided 1)
                    (starts 'starts)
                    (strides 1)
                    (twiddle 'twiddle)
                    (scale  1d0)
                    tmp startt
                    cooley-tukey)
  (declare (ignore twiddle cooley-tukey tmp startt))
  `(macrolet ((@src (&optional (index 0))
                `(scale (ref ,',src (+ ,',starts (* ,index ,',strides)))
                        ,',scale))
              (@dst (&optional (index 0))
                `(ref ,',dst (+ ,',startd (* ,index ,',strided)))))
     (let* ((s0 (@src))
            (s4 (@src 4))
            (s0+4 (+ s0 s4))
            (s0-4 (- s0 s4))
            
            (s1 (@src 1))
            (s5 (@src 5))
            (s1+5 (+ s1 s5))
            (s1-5 (- s1 s5))
            
            (s2 (@src 2))
            (s6 (@src 6))
            (s2+6 (+ s2 s6))
            (s2-6 (- s2 s6))
            
            (s3 (@src 3))
            (s7 (@src 7))
            (s3+7 (+ s3 s7))
            (s3-7 (- s3 s7)))
       (let ((a (+ s0+4 s2+6))
             (b (+ s1+5 s3+7)))
         (setf (@dst 0) (+ a b)
               (@dst 4) (- a b)))
       (let ((a (+ s0-4 ,(mul-root 's2-6 -2/8)))
             (b ,(mul-root `(+ s1-5 ,(mul-root 's3-7 -2/8))
                          -1/8)))
         (setf (@dst 1) (+ a b)
               (@dst 5) (- a b)))
       (let ((a (- s0+4 s2+6))
             (b ,(mul-root '(- s1+5 s3+7)
                           -2/8)))
         (setf (@dst 2) (+ a b)
               (@dst 6) (- a b)))
       (let ((a (+ s0-4 ,(mul-root 's2-6 -6/8)))
             (b ,(mul-root `(+ ,(mul-root 's1-5 -2/8)
                               s3-7)
                           -1/8)))
         (setf (@dst 3) (+ a b)
               (@dst 7) (- a b)))
       ,dst)))

(defun bit-reverse-values (bitwidth values)
  (flet ((rev (x)
           (let ((acc 0))
             (loop repeat bitwidth
                   for bit = (logand x 1)
                   do (setf x (ash x -1))
                      (setf acc (logior (ash acc 1) bit))
                   finally (return acc)))))
    (map '(simple-array index 1) #'rev values)))

;; doesn't work very well (:
(defun gen-unrolled-fft/small (size &key (dst 'dst)
                                         (src 'src)
                                      (startd 'startd)
                                      (strided 1)
                                      (starts 'starts)
                                      (strides 1)
                                      (twiddle 'twiddle)
                                      (scale   1d0))
  (assert (> size 8))
  (let* ((leaf-size  8)
         (base-cases (loop for i below size by leaf-size
                           collect i))
         (reversed   (bit-reverse-values (integer-length (1- size))
                                         base-cases)))
    `(flet ((rec (dst src startd starts)
              ,(gen-fft/8 :dst 'dst :src 'src
                          :startd 'startd :strided strided
                          :starts 'starts
                          :strides (* strides (the integer (/ size leaf-size)))
                          :scale scale)))
       (declare (notinline rec))
       (loop for i of-type index from ,startd by (* ,leaf-size ,strided)
             for reversed across ,reversed
             do (rec ,dst ,src i (+ ,starts (* reversed ,strides))))
       ,@(loop for current = (* 2 leaf-size) then (* 2 current)
               for size/2 = (truncate current 2)
               while (<= current size)
               for forms = (if (<= current 16)
                               `(,@(loop for idx below size/2
                                         for i from size/2
                                         for root = (/ (- idx) current)
                                         for place = `(ref ,dst (+ startd (* ,i ,strided)))
                                         unless (zerop idx)
                                           collect
                                         `(setf ,place
                                                ,(mul-root place root `(ref ,twiddle ,i))))
                                 ,@(loop repeat size/2
                                         for i from 0
                                         for j from size/2
                                         collect `(let ((a (ref ,dst (+ startd (* ,i ,strided))))
                                                        (b (ref ,dst (+ startd (* ,j ,strided)))))
                                                    (setf (ref ,dst (+ startd (* ,i ,strided))) (+ a b)
                                                          (ref ,dst (+ startd (* ,j ,strided))) (- a b)))))
                               `((loop for .src. of-type index from (+ startd ,size/2) by 2
                                       for .c. of-type index from ,size/2 below ,current by 2
                                       do (let ((i0 (ref ,dst .src.))
                                                (i1 (ref ,dst (+ 1 .src.)))
                                                (c0 (ref ,twiddle .c.))
                                                (c1 (ref ,twiddle (+ 1 .c.))))
                                            (setf (ref ,dst       .src.) (* i0 c0)
                                                  (ref ,dst (+ 1 .src.)) (* i1 c1))))
                                 (loop for i of-type index from startd by 2
                                       for j of-type index from (+ startd ,size/2) by 2
                                       for .count. of-type index
                                         from ,(truncate size/2 2) above 0
                                       do (let ((a0 (ref ,dst i))
                                                (b0 (ref ,dst j))
                                                (a1 (ref ,dst (+ i 1)))
                                                (b1 (ref ,dst (+ j 1))))
                                            (setf (ref ,dst i)       (+ a0 b0)
                                                  (ref ,dst j)       (- a0 b0)
                                                  (ref ,dst (+ 1 i)) (+ a1 b1)
                                                  (ref ,dst (+ 1 j)) (- a1 b1))))))
               collect
               (if (<= (/ size current) 4)
                   `(progn
                      ,@(loop for offset below size by current
                              collect `(let ((startd (+ ,startd ,offset)))
                                         ,@forms)))
                   `(loop for startd from ,startd by ,current below ,size do
                     (progn
                       ,@forms))))
       ,dst)))

(defun gen-fft/small (size &rest args
                      &key (dst 'dst)
                        (src 'src)
                        (startd 'startd)
                        (strided 1)
                        (starts 'starts)
                        (strides 1)
                        (twiddle 'twiddle)
                        (scale 1d0)
                        tmp startt
                        cooley-tukey)
  (declare (ignore cooley-tukey tmp startt))
  (check-type size (integer 1))
  (case size
    (1
     (apply 'gen-fft/1 args))
    (2
     (apply 'gen-fft/2 args))
    (4
     (apply 'gen-fft/4 args))
    (8
     (apply 'gen-fft/8 args))
    (t
     (let ((2*strides (* 2 strides))
           (size/2    (truncate size 2)))
       `(flet ((rec (startd starts)
                 ,(gen-fft/small size/2
                                 :dst dst
                                 :src src
                                 :startd 'startd
                                 :starts 'starts
                                 :strides 2*strides
                                 :strided strided
                                 :scale   scale)))
          (rec ,startd ,starts)
          (rec (+ ,startd ,size/2)
               (+ ,starts ,strides))
          ,@(if (<= size 16)
                `(,@(loop for idx below size/2
                          for i from size/2
                          for root = (/ (- idx) size)
                          for place = `(ref ,dst (+ ,startd (* ,i ,strided)))
                          unless (zerop idx)
                            collect
                          `(setf ,place
                                 ,(mul-root place root `(ref ,twiddle ,i))))
                  ,@(loop repeat size/2
                          for i from 0
                          for j from size/2
                          collect `(let ((a (ref ,dst (+ ,startd (* ,i ,strided))))
                                         (b (ref ,dst (+ ,startd (* ,j ,strided)))))
                                     (setf (ref ,dst (+ ,startd (* ,i ,strided))) (+ a b)
                                           (ref ,dst (+ ,startd (* ,j ,strided))) (- a b)))))
                `((loop for .src. of-type index from (+ ,startd ,size/2) by 2
                        for .c. of-type index from ,size/2 below ,size by 2
                        do (let ((i0 (ref ,dst .src.))
                                 (i1 (ref ,dst (+ 1 .src.)))
                                 (c0 (ref ,twiddle .c.))
                                 (c1 (ref ,twiddle (+ 1 .c.))))
                             (setf (ref ,dst       .src.) (* i0 c0)
                                   (ref ,dst (+ 1 .src.)) (* i1 c1))))
                  (loop for i of-type index from ,startd by 2
                        for j of-type index from (+ ,startd ,size/2) by 2
                        for .count. of-type index
                          from ,(truncate size/2 2) above 0
                        do (let ((a0 (ref ,dst i))
                                 (b0 (ref ,dst j))
                                 (a1 (ref ,dst (+ i 1)))
                                 (b1 (ref ,dst (+ j 1))))
                             (setf (ref ,dst i)       (+ a0 b0)
                                   (ref ,dst j)       (- a0 b0)
                                   (ref ,dst (+ 1 i)) (+ a1 b1)
                                   (ref ,dst (+ 1 j)) (- a1 b1))))))
          ,dst)))))
