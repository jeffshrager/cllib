;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; Convert epi-named functional files into our format.
;;; (Slightly after the below perl procedure written by Mark Hahn)

;;; The EPI's come in in a separate dir for each trial, and are called
;;; as: OKRTFREE_02054_006_image0_227.im.  The only thing that's
;;; really relevant is the _xxx (as _227) number, which is a sequence.
;;; The images are just functionals and are in slice-major order, so,
;;; ..._001 is the first image of slice 1, _002 is the second, etc.
;;; As there's no other information given, you have to know either the
;;; number of slices or the number of samples, and then figure out
;;; which slice and sample you're on from that.  (In fact, you really
;;; need to know that number of samples because the slice number is
;;; (round (/ _xxx nsamples)) and the sample within the slice is (mod
;;; _xxx nsamples).

;;; What I do is to essentailly FAKE a reconstruction process for EPIs. 
;;; During that process, I link (-s) from the home EPI directories into
;;; the target functional directory (default: <segment>/func); this allows
;;; invert to do the right thing.  It's a bit messy and the interface
;;; has to make descisions all over the place about whether we're dealing
;;; with EPIs or spirals but... that's life.  Oh, and the WARNING about
;;; cleaning the EPI directories before doing this is crucial.  I ought
;;; to put in a test that ignores any EPI directory with a number of 
;;; files that doesn't match the predicted slices x samples; these are
;;; generally EPI map directories for tuning purposes.

#|
#!/usr/bin/perl
$slices=9;
$samples=32;

open(DIR,"ls *.im|");
while (<DIR>) {
    chop;
    # names look like OKRTFREE_02054_006_image0_227.im
    ($study,$session,$series,$junk,$foo) = split(/[_.]/,$_);
    $slice = int($foo / $samples);
    $sample = $foo % $samples;
    $new = sprintf("sl%02d.%03d",$slice+1,$sample+1);
    print "$_ $new\n";
    rename($_,$new);
}

(Turns out that this doesn't actually work because ls *.im fails if 
 there's too many files. Note also that slices is unused.  My code DOES
 work!)
|#

(defun link-epis-to-funcs (epi-source-dir files func-target-dir nsamples
			     &optional (sample-offset 0))
  (mapcar #'(lambda (f) (convert-epi-file epi-source-dir f 
					  func-target-dir nsamples 
					  sample-offset))
	  files)
  )

;;; This renames and links the images from an epi source directory
;;; into the function target directory.  If sample-offset is given
;;; this gets added to the sample number.  

(defun convert-epi-file (epi-source-dir file 
			 func-target-dir nsamples
			 &optional (sample-offset 0))
  (let* ((name (file-name file))
	 (p1 (parse-string name :break-char #\_))
	 (p2 (parse-string (fifth p1) :break-char #\.))
	 )
    (if (string-equal "im" (second p2))
	(let* ((epi-image (read-from-string (first p2)))
	       (slice (truncate (/ epi-image nsamples)))
	       (sample (+ sample-offset (mod epi-image nsamples)))
	       )
          (system! (format nil "ln -sf ~a/~a ~a/sl~2,,,'0@a.~3,,,'0@a" 
			   epi-source-dir name func-target-dir 
			   (1+ slice) (1+ sample)))
	  )
      )))

;;; This controls epi-to-functional "reconstruction" for multiple
;;; trials in a segment.  The idea is to link the epi images into
;;; the func directory appropriately renamed so that invert does
;;; the right thing.  Oh My Goodness....  The epi-source-upper-dir
;;; is the directory that contains the epi imageN dirs.  We count
;;; them up and then do each one in turn.

;;; ***HEURISTIC WARNING***
;;; This tries to be smart about ignoring EPI directories that are
;;; tuning mapes, which have a different number of images than
;;; predicted.  Note that in principle we could figure out the number
;;; of samples without asking the user by dividing the number of files
;;; in the directory by the number of slices, but the tuning maps will
;;; usually have the right number of slices, just not the right number
;;; of slices/samples, so unfortunately we need the number of samples,
;;; not just the number of slices.

(defun epi-recon (epi-source-upper-dir func-target-dir nsamples nslices
		  &aux (offset 0))
  (let* ((image-dirs (understand-directory epi-source-upper-dir))
	 (nimages (length image-dirs)))
    (dolist (image-dir image-dirs)
      ;; Before doing anything, check to see that we have the 
      ;; right number of images and, if not, ignore this directory.
      ;; (The 1- here accounts for the image_0.ah file, which is junk.)
      (let* ((epi-source-dir (pfn epi-source-upper-dir (file-name image-dir)))
	     (d (understand-directory epi-source-dir))
	     )
	(if (= (* nsamples nslices) (1- (length d)))
	    (progn
	      (format t "Reconstructing ~a...~%" (file-name image-dir))
	      (link-epis-to-funcs epi-source-dir d 
				  func-target-dir nsamples offset)
	      (incf offset nsamples))
	    (format t "Ignoring ~a (with ~a images)...~%" 
		    (file-name image-dir) (length d))
	    )
	))))

;;; Try to guess where the epi files are.  We assume that the top is a
;;; dir called <brain>/epi, but then after that we just keep chuging
;;; down the dir tree until we find directories full of subdirs called
;;; "imagex" where x is a number.  These get stored off the the side
;;; and returned at the end of the search.

(defvar *probably-epi-dirs* ()) 

(defun find-epis ()
  (setq *probably-epi-dirs* ())
  (scan-for-epis (understand-directory (pfn (study-brain-dir *s*)
					    "epi")))
  )

;;; ***************** NOT COMPLETE *****************
