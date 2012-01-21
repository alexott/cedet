;;; ede-auto.el --- Autoload features for EDE.
;;
;; Copyright (C) 2010, 2012 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-auto.el,v 1.3 2010-03-15 13:40:54 xscript Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; EDE Autoloads are a way to refer to different project types without
;; loading those projects into Emacs.
;;
;; These routines are used to detect a project in a filesystem before
;; handing over control to the usual EDE project system.

;;; Code:
;;;###autoload
(defclass ede-project-autoload ()
  ((name :initarg :name
	 :documentation "Name of this project type")
   (file :initarg :file
	 :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
   (proj-root-dirmatch :initarg :proj-root-dirmatch
		       :type string
		       :documentation
		       "To avoid loading a project, check if the directory matches this.
For projects that use directory name matches, a function would load that project.
Specifying this matcher will allow EDE to check without loading the project.")
   (proj-root :initarg :proj-root
	      :type function
	      :documentation "A function symbol to call for the project root.
This function takes no arguments, and returns the current directories
root, if available.  Leave blank to use the EDE directory walking
routine instead.")
   (initializers :initarg :initializers
		 :initform nil
		 :documentation
		 "Initializers passed to the project object.
These are used so there can be multiple types of projects
associated with a single object class, based on the initilizeres used.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use.")
   (generic-p :initform nil
	      :documentation
	      "Generic projects are added to the project list at the end.
The add routine will set this to non-nil so that future non-generic placement will
be successful.")
   (new-p :initarg :new-p
	  :initform t
	  :documentation
	  "Non-nil if this is an option when a user creates a project.")
   (safe-p :initarg :safe-p
	   :initform t
	   :documentation
	   "Non-nil if the project load files are \"safe\".
An unsafe project is one that loads project variables via Emacs
Lisp code.  A safe project is one that loads project variables by
scanning files without loading Lisp code from them.")
   )
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload "edeproject-makefile"
			 :name "Make" :file 'ede-proj
			 :proj-file "Project.ede"
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload "edeproject-automake"
			 :name "Automake" :file 'ede-proj
			 :proj-file "Project.ede"
			 :initializers '(:makefile-type Makefile.am)
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload "automake"
			 :name "automake" :file 'project-am
			 :proj-file "Makefile.am"
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile
			 :new-p nil
			 :safe-p t)
   )
  "List of vectors defining how to determine what type of projects exist.")

(put 'ede-project-class-files 'risky-local-variable t)

(defun ede-add-project-autoload (projauto &optional flag)
  "Add PROJAUTO, an EDE autoload definition to `ede-project-class-files'.
Optional argument FLAG indicates how this autoload should be
added.  Possible values are:
  'generic - A generic project type.  Keep this at the very end.
  'unique - A unique project type for a specific project.  Keep at the very
            front of the list so more generic projects don't get priority."
  ;; First, can we identify PROJAUTO as already in the list?  If so, replace.
  (let ((projlist ede-project-class-files)
	(projname (object-name-string projauto)))
    (while (and projlist (not (string= (object-name-string (car projlist)) projname)))
      (setq projlist (cdr projlist)))

    (if projlist
	;; Stick the new one into the old slot.
	(setcar projlist projauto)

      ;; Else, see where to insert it.
      (cond ((and flag (eq flag 'unique))
	     ;; Unique items get stuck right onto the front.
	     (setq ede-project-class-files
		   (cons projauto ede-project-class-files)))

	    ;; Generic Projects go at the very end of the list.
	    ((and flag (eq flag 'generic))
	     (oset projauto generic-p t)
	     (setq ede-project-class-files
		   (append ede-project-class-files
			   (list projauto))))

	    ;; Normal projects go at the end of the list, but
	    ;; before the generic projects.
	    (t
	     (let ((prev nil)
		   (next ede-project-class-files))
	       (while (and next (not (oref (car next) generic-p)))
		 (setq prev next
		       next (cdr next)))
	       (when (not prev)
		 (error "ede-project-class-files not initialized"))
	       ;; Splice into the list.
	       (setcdr prev (cons projauto next))))))))

;;; EDE project-autoload methods
;;
(defmethod ede-project-root ((this ede-project-autoload))
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

(defmethod ede-project-root-directory ((this ede-project-autoload)
				       &optional file)
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  If there is no FILE, use
the current buffer."
  (when (not file)
    (setq file default-directory))
  (when (slot-boundp this :proj-root)
    (let ((dirmatch (oref this proj-root-dirmatch))
	  (rootfcn (oref this proj-root))
	  (callfcn t))
      (when rootfcn
	(when (and (not (featurep (oref this file))) dirmatch)
	  (unless (string-match dirmatch file)
	    (setq callfcn nil)))
	(when callfcn
	  (condition-case nil
	      (funcall rootfcn file)
	    (error 
	     (funcall rootfcn))))
	))))

(defmethod ede-dir-to-projectfile ((this ede-project-autoload) dir)
  "Return a full file name of project THIS found in DIR.
Return nil if the project file does not exist."
  (let* ((d (file-name-as-directory dir))
	 (root (ede-project-root-directory this d))
	 (pf (oref this proj-file))
	 (f (cond ((stringp pf)
		   (expand-file-name pf (or root d)))
		  ((and (symbolp pf) (fboundp pf))
		   (funcall pf (or root d)))))
	 )
    (when (and f (file-exists-p f))
      f)))

(defmethod ede-auto-load-project ((this ede-project-autoload) dir)
  "Load in the project associated with THIS project autoload description.
THIS project description should be valid for DIR, where the project will
be loaded."
  ;; Last line of defense: don't load unsafe projects.
  (when (not (or (oref this :safe-p)
		 (ede-directory-safe-p dir)))
    (error "Attempt to load an unsafe project (bug elsewhere in EDE)"))
  ;; Things are good - so load the project.
  (let ((o (funcall (oref this load-type) dir)))
    (when (not o)
      (error "Project type error: :load-type failed to create a project"))
    (ede-add-project-to-global-list o)))

(provide 'ede-auto)

;;; ede-auto.el ends here
