;; large in-place transpose (with a scratch buffer for non-square
;; matrices)


(defconstant +transpose-base-size+ (ash 1 5))
(defconstant +transpose-blocking-factor+ 4)

(declaim (inline %transpose! %transpose-into))

(defun %transpose! (vec size start stride)
  (declare (type complex-sample-array vec)
           (type half-size size stride)
           (type index start)
           (optimize speed (safety 0)))
  (labels ((in-place (size start)
             (declare (type half-size size)
                      (type index start))
             (cond ((<= size +transpose-base-size+)
                    (loop
                      for i of-type half-index below size by 2
                      for start1 of-type index from start by (* 2 stride)
                      for start2 of-type index from start by 2
                      do (loop for start1 of-type index from start1
                               for start2 of-type index from start2 by stride
                               for j of-type half-index from i above 0
                               do (rotatef (ref vec start1)
                                           (ref vec start2))
                                  (rotatef (ref vec (+ start1 stride))
                                           (ref vec (+ start2 1)))
                               finally (rotatef (ref vec (+ start1 1))
                                                (ref vec (+ start2 stride)))))
                    vec)
                   (t
                    (let* ((size/2      (truncate size 2))
                           (long-stride (* size/2 stride)))
                      (in-place size/2
                                start)
                      (swap size/2
                            (+ start size/2)
                            (+ start long-stride))
                      (in-place size/2
                                (+ start long-stride size/2))))))
           (swap (size start1 start2)
             (declare (type half-size size)
                      (type index start1 start2))
             (cond ((<= size +transpose-base-size+)
                    (loop
                      for i from size above 0 by 4
                      for start1 of-type index from start1 by (* 4 stride)
                      for start2 of-type index from start2 by 4
                      do (loop for j from size above 0
                               for start1a of-type index from start1
                               for start1b of-type index from (+ start1 (* 2 stride))
                               for start2 of-type index from start2 by stride
                               do (rotatef (aref vec start1a)
                                           (aref vec start2))
                                  (rotatef (aref vec (+ start1a stride))
                                           (aref vec (+ start2 1)))
                               (rotatef (aref vec start1b)
                                        (aref vec (+ start2 2)))
                               (rotatef (aref vec (+ start1b stride))
                                        (aref vec (+ start2 3)))))
                    vec)
                   (t
                    (let* ((size/2 (truncate size 2))
                           (long-stride (* size/2 stride)))
                      (swap size/2
                            start1
                            start2)
                      (swap size/2
                            (+ start1 size/2)
                            (+ start2 long-stride))
                      (swap size/2
                            (+ start1 long-stride)
                            (+ start2 size/2))
                      (swap size/2
                            (+ start1 long-stride size/2)
                            (+ start1 long-stride size/2)))))))
    (in-place size start)))

(defun %transpose-into (dst src size startd starts strided strides)
  (declare (type complex-sample-array dst src)
           (type size size)
           (type index starts startd)
           (type half-size strided strides)
           (optimize speed (safety 0)))
  (labels ((rec (size startd starts)
             (declare (type half-size size)
                      (type index startd starts))
             (cond ((<= size +transpose-base-size+)
                    (loop
                      for i from size above 0 by 4
                      for startd of-type index from startd by (* 4 strided)
                      for starts of-type index from starts by 4
                      do (loop for j from size above 0
                               for startd1 of-type index from startd
                               for startd2 of-type index from (+ startd (* 2 strided))
                               for starts of-type index from starts by strides
                               do (setf (aref dst startd1) (aref src starts)
                                        (aref dst (+ startd1 strided)) (aref src (+ starts 1))
                                        (aref dst startd2)  (aref src (+ starts 2))
                                        (aref dst (+ startd2 strided)) (aref src (+ starts 3)))))
                    dst)
                   (t
                    (let* ((size/2 (truncate size 2))
                           (long-strided (* size/2 strided))
                           (long-strides (* size/2 strides)))
                      (rec size/2
                           startd
                           starts)
                      (rec size/2
                           (+ startd size/2)
                           (+ starts long-strides))
                      (rec size/2
                           (+ startd long-strided)
                           (+ starts size/2))
                      (rec size/2
                           (+ startd long-strided size/2)
                           (+ starts long-strides size/2)))))))
    (rec size startd starts)))

(defun transpose (vec tmp size1 size2 total vecs tmps)
  (declare (type complex-sample-array vec tmp)
           (type half-size size1 size2)
           (type size total)
           (type index vecs tmps)
           (optimize speed))
  (flet ((%transpose-into (dst src size startd starts strided strides)
           (%transpose-into dst src size startd starts strided strides)
           dst)
         (blit (dst startd src starts count)
           (declare (type complex-sample-array dst src)
                    (type index startd starts count)
                    (optimize speed (safety 0)))
           (loop for count of-type index from count above 0 by 4
                 for dsti of-type index from startd by 4
                 for srci of-type index from starts by 4
                 do (setf (ref dst (+ dsti 0)) (ref src (+ srci 0))
                          (ref dst (+ dsti 1)) (ref src (+ srci 1))
                          (ref dst (+ dsti 2)) (ref src (+ srci 2))
                          (ref dst (+ dsti 3)) (ref src (+ srci 3))))
           dst))
    (cond ((= size1 size2)
           (return-from transpose (%transpose! vec size1 vecs size1)))
          ((< size1 size2)
           (let* ((size  size1)
                  (block (truncate total 2)))
             (%transpose-into tmp vec size
                              0   0
                              size2 size1)
             (%transpose-into tmp vec size
                              size1 block
                              size2 size1))
           (blit vec vecs tmp tmps total))
          (t
           (let* ((size  size2)
                  (block (truncate total 2)))
             (%transpose-into tmp vec size
                              0   0
                              size2 size1)
             (%transpose-into tmp vec size
                              block size2
                              size2 size1))
           (blit vec vecs tmp tmps total)))))

(declaim (notinline %transpose! %transpose-into))

(defun generate-square-transpose (size &key (vec 'vec)
                                         (tmp 'tmp)
                                         (vecs 0)
                                         (tmps 0)
                                         (base-size +transpose-base-size+)
                                         (twiddle nil)
                                         (twiddle-start 0)
                                         (scale   1d0)
                                         (blocking-factor +transpose-blocking-factor+))
  (declare (ignore tmp tmps))
  (assert (<= blocking-factor size))
  (assert (zerop (mod size blocking-factor)))
  (flet ((emit-swap (x y offset)
           `(let ((x (scale (%twiddle ,x ,twiddle ,offset)
                            ,scale))
                  (y (scale (%twiddle ,y ,twiddle ,offset)
                            ,scale)))
              (setf ,x y
                    ,y x))))
    `(labels ((in-place (size start
                              ,@(and twiddle `(twiddle-start)))
                (declare (type half-size size)
                         ,@(and twiddle '((type index twiddle-start)))
                         (type index start))
                (cond ((<= size ,base-size)
                       (loop
                         for i of-type half-index below size
                         for start1 of-type index from start by ,size
                         for start2 of-type index from start
                         ,@(and twiddle `(for idx of-type index from twiddle-start
                                              by ,size))
                         do (loop for start1 of-type index from start1
                                  for start2 of-type index from start2 by ,size
                                  for j of-type half-index from i above 0
                                  ,@(and twiddle
                                         `(for idx of-type index from idx))
                                  do (progn
                                       ,(emit-swap `(ref ,vec start1)
                                                   `(ref ,vec start2)
                                                   'idx)))
                         ,@(and (or twiddle scale)
                                `(do (setf (ref ,vec (+ start1 i))
                                           (scale (%twiddle (ref ,vec (+ start1 i))
                                                            ,twiddle ,(and twiddle
                                                                           '(+ idx i)))
                                                  ,scale)))))
                       ,vec)
                      (t
                       (let* ((size/2      (truncate size 2))
                              (long-stride (* size/2 ,size)))
                         (in-place size/2
                                   start
                                   ,@(and twiddle '(twiddle-start)))
                         (swap size/2
                               (+ start size/2)
                               (+ start long-stride)
                               ,@(and twiddle `((+ twiddle-start size/2))))
                         (in-place size/2
                                   (+ start long-stride size/2)
                                   ,@(and twiddle `((+ twiddle-start
                                                      long-stride size/2))))))))
              (swap (size start1 start2
                          ,@(and twiddle `(twiddle-start)))
                (declare (type half-size size)
                         (type index start1 start2)
                         ,@(and twiddle '((type index twiddle-start))))
                (cond ((<= size ,base-size)
                       (loop
                         for i from size above 0 by ,blocking-factor
                         for start1 of-type index from start1 by ,(* blocking-factor size)
                         for start2 of-type index from start2 by ,blocking-factor
                         ,@(and twiddle
                                `(for idx of-type index from twiddle-start by
                                      ,(* blocking-factor size)))
                         do (loop for j from size above 0
                                  for start1 of-type index from start1
                                  for start2 of-type index from start2 by ,size
                                  ,@(and twiddle
                                         `(for idx of-type index from idx))
                                  do (progn
                                       ,@(loop
                                           for block below blocking-factor
                                           collect (emit-swap `(ref ,vec (+ start1 ,(* block size)))
                                                              `(ref ,vec (+ start2 ,block))
                                                              `(+ idx ,(* block size)))))))
                       ,vec)
                      (t
                       (let* ((size/2 (truncate size 2))
                              (long-stride (* size/2 ,size)))
                         (swap size/2
                               start1
                               start2
                               ,@(and twiddle '(twiddle-start)))
                         (swap size/2
                               (+ start1 size/2)
                               (+ start2 long-stride)
                               ,@(and twiddle '((+ twiddle-start size/2))))
                         (swap size/2
                               (+ start1 long-stride)
                               (+ start2 size/2)
                               ,@(and twiddle '((+ twiddle-start long-stride))))
                         (swap size/2
                               (+ start1 long-stride size/2)
                               (+ start2 long-stride size/2)
                               ,@(and twiddle `((+ twiddle-start
                                                   long-stride size/2)))))))))
       (in-place ,size ,vecs
                 ,@(and twiddle `(,twiddle-start))))))

(defun generate-transpose-copy (size strided strides
                                &key (dst 'dst)
                                     (src 'src)
                                     (startd 0)
                                     (starts 0)
                                     (base-size +transpose-base-size+)
                                     (blocking-factor +transpose-blocking-factor+))
  (assert (<= blocking-factor size))
  (assert (zerop (mod size blocking-factor)))
  `(labels ((rec (size startd starts)
              (declare (type half-size size)
                       (type index startd starts))
              (cond ((<= size ,base-size)
                     (loop
                       for i from size above 0 by ,blocking-factor
                       for startd of-type index from startd
                         by (* ,blocking-factor ,strided)
                       for starts of-type index from starts by ,blocking-factor
                       do (loop for j from size above 0
                                for startd of-type index from startd
                                for starts of-type index from starts by ,strides
                                do (setf ,@(loop
                                             for block below blocking-factor
                                             append `((ref ,dst (+ startd (* ,strided ,block)))
                                                      (ref ,src (+ starts ,block)))) )))
                     ,dst)
                    (t
                     (let* ((size/2 (truncate size 2))
                            (long-strided (* size/2 ,strided))
                            (long-strides (* size/2 ,strides)))
                       (rec size/2
                            startd
                            starts)
                       (rec size/2
                            (+ startd size/2)
                            (+ starts long-strides))
                       (rec size/2
                            (+ startd long-strided)
                            (+ starts size/2))
                       (rec size/2
                            (+ startd long-strided size/2)
                            (+ starts long-strides size/2)))))))
     (rec ,size ,startd ,starts)))

(defun generate-blit (size &key (dst 'dst)
                             (src 'src)
                             (startd 0)
                             (starts 0)
                             (twiddle nil)
                             (twiddle-start 0)
                             (scale   1d0)
                             (blocking-factor +transpose-blocking-factor+))
  (assert (<= blocking-factor size))
  (assert (zerop (mod size blocking-factor)))
  `(loop for i    of-type index below ,size by ,blocking-factor
         for dsti of-type index from ,startd by ,blocking-factor
         for srci of-type index from ,starts by ,blocking-factor
         do (setf ,@(loop for block below blocking-factor
                          append `((ref ,dst (+ dsti ,block))
                                   (scale (%twiddle (ref ,src (+ srci ,block))
                                                    ,twiddle
                                                    (+ ,twiddle-start
                                                       i
                                                       ,block))
                                          ,scale))))))

(defun onep (x)
  (and (numberp x)
       (= 1 x)))

(defun generate-transpose (size1 size2 copy
                           &rest args
                           &key
                             (vec 'vec)
                             (tmp 'tmp)
                             (vecs 0)
                             (tmps 0)
                             (base-size +transpose-base-size+)
                             (twiddle nil)
                             (twiddle-start 0)
                             (scale   1d0)
                             (blocking-factor +transpose-blocking-factor+)
                           &aux (total (* size1 size2)))
  (if (= size1 size2)
      (cond (copy
             (assert (eql scale 1d0))
             (assert (not twiddle))
             (generate-transpose-copy
              size1 size1 size2
              :dst tmp
              :src vec
              :startd tmps
              :starts vecs
              :base-size base-size
              :blocking-factor blocking-factor))
            (t
             (apply 'generate-square-transpose size1 args)))
      (let ((size  (min size1 size2))
            (block (truncate total 2)))
        `(flet ((rec (dst src startd starts)
                  (declare (type complex-sample-array dst src)
                           (type index startd starts))
                  ,(generate-transpose-copy size size2 size1
                                            :dst 'dst :src 'src
                                            :startd 'startd :starts 'starts
                                            :base-size base-size
                                            :blocking-factor blocking-factor))
                (copy (dst src startd starts
                           ,@(and twiddle '(twiddle twiddle-start))
                           ,@(and (not (constantp scale)) '(scale)))
                  (declare (type complex-sample-array dst src
                                 ,@(and twiddle '(twiddle)))
                           (type index startd starts
                                 ,@(and twiddle '(twiddle-start)))
                           ,@(and (not (constantp scale))
                                  '((type double-float scale))))
                  ,(generate-blit total
                                  :dst 'dst
                                  :src 'src
                                  :startd 'startd
                                  :starts 'starts
                                  :twiddle (and twiddle 'twiddle)
                                  :twiddle-start (and twiddle 'twiddle-start)
                                  :scale (if (constantp scale)
                                             scale
                                             'scale)
                                  :blocking-factor blocking-factor)
                  dst))
           (declare (notinline copy))
           ,(if (< size1 size2)
                `(progn
                   (rec ,tmp ,vec ,tmps ,vecs)
                   (rec ,tmp ,vec (+ ,tmps ,size1) (+ ,vecs ,block)))
                `(progn
                   (rec ,tmp ,vec ,tmps ,vecs)
                   (rec ,tmp ,vec (+ ,tmps ,block) (+ ,vecs ,size2))))
           ,(if copy
                tmp
                `(copy ,vec ,tmp ,vecs ,tmps
                       ,@(and twiddle `(,twiddle ,twiddle-start))
                       ,@(and (not (constantp scale)) `(,scale))))))))

