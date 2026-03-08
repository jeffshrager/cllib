;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Main paths for data and various package stuff.  

(setq *xgrab-location* "/users/shrager/mri/fis/xgrab/xgrabsc-2.4beta/xgrabsc")

;;; The settings in here will generally be changed only by the site
;;; administrator.  

(setq *palette-path* "/users/shrager/mri/fis/defpals")

;;; FIS assumes that the brains are kept in a top level directory, and
;;; various commands, such as find and load default to this location.

(setq *topdata-path* "/data")

;;; These tell reconstruction where the param files are.

;;; The location of all param files.

(setq *param-file-dir* "/data")

;;; This is wrong and has been dyked out and replaced with a default.

;;; A table that associates the field of view with the name of
;;; the parm file.

;(setq *fov-to-param-file-alist*
;  '(
;    (18 . "parm.sf92.64")
;    (24 . "parm.sf92.128")
;    ))

(defvar *default-parm-file* "parm.sf92.128")
