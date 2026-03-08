;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Definitions of global vars and structs.  Note that there are
;;; other globals in various places, but those are mostly left overs
;;; and ought to be noted and moved here eventually.  (Some things are
;;; in prefs and site - those are okay.

(defvar *series-top-path* ())
(defvar *results-top-path* ())

;;; This is t or nil as whether debugging is on or not.  Eventually maybe
;;; I'll have levels.

(defvar *dbg* ())

;;; The mode tells us whether we ought to be giving "simple" printouts
;;; or super user printouts.  The undocumented 'su' command toggles the 
;;; mode.  Unless mode is eq 'superuser, it's in 'user mode by default!

(defvar *mode* 'user)

;;; The complete file path/name where this study was found on load.

(defvar *study-file-path* "")

;;; *z* represents the current series, if there is one.  Series are 
;;; just list result-sets which have a separate html log.

;;; NOTE: The only way to know what mode we're in, that is, whether
;;; we have a series loaded or not, is to test whether *z* is set.

(defvar *z* ())

(defstruct series
  name
  path ; gets reset each time the series is loaded -- just a convience
  result-sets)

(defstruct srs ; series-result-set
  result-set
  study
  segment)

;;; The *s* global is used all over to refer to the currently loaded
;;; study.  Some functions (it's inconsistent) require a study argument,
;;; and some other default to using *s*; some use *s* internally.

(defvar *s* ())

;;; This is set by the load command and is used in the use command to 
;;; see if we're loading up the same study as is already in core.

(defvar *loaded-study-name* ())

(defstruct study
  subject-name
  brain-id
  brain-dir
  mr-operator
  date
  segments
  pfile-dir
  structural-dir
  fisversion
  )

;;; The idea is that each segment is separately reconstructed.  So most
;;; of the important information really is in the segment structed, not
;;; the study structure.  *g* refers to the segment in context at the
;;; moment (see use.lsp)

(defvar *g* ())

(defstruct segment
  number
  name
  scan-type ; 'spiral or 'epi
  dir
  pfile-dir ; if epi, this points to the epi directory containing the epi
            ; imagesN directories
  inplanes-dir
  volume-dir
  results-dir ; this is replaced in this version by
              ; a separate *r* struct.  It's left here for backward
              ; compatibility
  mra-dir
  functional-slices-dir
  invert-output-dir

  prescription

  trials
  condition-graph
  )

;;; There are some confusing things near here that are worth
;;; discussion.  Although the spin info in the prescription is
;;; techincally associated with the pfiles, and should by right go
;;; with the trial, we assume that a segment has a fixed fov and
;;; therefore fixed prescription (ad spin info therein).

;;; Another confusing thing is that the map file generally describes
;;; all the pfiles in the segment.  I've put the name of the map with
;;; each pfile, which repeats info all over the place and has to be
;;; decoded and recoded all the time (yuk!) so that you can, in
;;; principle, reconstruct a single trial, although it's probably the
;;; case that this won't ever be done.

(defstruct trial
  number
  condition
  pfile
  mapfile
  )

(defstruct prescription
  slices
  samples
  size
  skip
  fov
  param-file
  period
  )

;;; A results set is owned by users, not a part of the study
;;; structure.  *r* points to the result set being used at the moment.
;;; The results set is defined by a unix directory.  That is, when you
;;; start up fis, you are assumed to be living in a directory of
;;; results.  This location is used as your results set.  It looks
;;; like what we normally called a brain directory, or, in FIS
;;; terminology, a segment directory.

(defvar *r* ())

(defstruct result-set
  name ; the name given by the user when this was loaded
  path ; where to find this result set (where it was found or created)
       ; this is reset whenever the result is used so it isn't depended upon
       ; by the loader.
  study-name ; the study that this goes with
  segment-name ; the name of the segment for this study that these results go with
  results ; a list of results, created by (understand-existing-results..)
  fisversion
)

;;; The reults referred to by this struct are the ones in the results
;;; set.  

(defstruct result
  name
  over ; that is, whether this is the average ('avg) or a single trial
       ; (indicated by trial number), etc.

  stat ; the rest of the name (as .f, .p, .f.mask.blah.blah.blah)
  
  allfiles ; The complete names of all the files that are described by
           ; this instance.
  labels 
  )

;;; AVOITOOLS operate on vois, which come from masked results.

(defstruct voi
  id ; a numerical reference created by the voi locator tool
  volume ; number of voxels
  mean
  peak
  icenter ; (x y z) in image space
  scenter ; (x y z) in scanner space
  tcenter ; (x y z) in talairach space, or '*undefined*
  tpldata ; signal data
  )

