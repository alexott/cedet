;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Alex Ott <alexott@gmail.com>

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
;;; jvm-base.el --- 

(require 'ede)
(require 'cedet-files)

;;;###autoload
(defclass ede-single-root-project (ede-project)
  ((file-mod-time :initform 0
		  :documentation "Modification time for project file")
   (supports-multiple-commands-p :initform t
				 :documentation "")
   (project-options :initform nil
		   :initarg :project-options
		   :type list
		   :documentation "")
   (current-targets :initform nil
		    :initarg :current-targets
		    :type list
		    :documentation "")
   (target-options :initform nil
		   :initarg :target-options
		   :type list
		   :documentation "")
   (existing-targets :initform nil
		     :initarg :existing-targets
		     :type list
		     :documentation "")
   (source-dirs :initform nil
		:initarg :source-dirs
		:type list
		:documentation "Alist that maps a major mode into list of directories with
source code.  In most cases, project itself will generate it automatically, based on the
content of project file.")
   )
  "Base project class for projects with project file in root directory."
  :method-invocation-order :depth-first)

;; DONE: flag, that specify that project supports multiple targets in one invocation
;; DONE: we should be able to specify several targets in command-line?
;; DONE: add slot for global options, like -o/-U for lein, -B for maven, etc. and
;; corresponding function to setup them
;; DONE: add method to change current target(s) in given project - it should use 
;; DONE: add method to change target options?
;; DONE: source directories should be a part of project? source directories are alists
;; that maps mode to list of directories
;; TODO: should we limit a choice of available global options?
;; TODO: how source directories could be dynamic? Like, don't include test source code
;; when we're editing the main source code, but include it when we edit test source code.
;; Should we make a separate slot for "main" source code, and for "test" source code...

(defmethod initialize-instance ((this ede-single-root-project)
                                &rest fields)
  "Make sure the :targets is setup."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil)))

;;; TARGET MANAGEMENT
;;

(defclass ede-single-root-target (ede-target)
  ((project :initform nil
	    :initarg :project)
   )
  "EDE Single Root Project target.
All directories need at least one target.")

(defun ede-single-root-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
                 (string= (oref T :path) dir))
        (setq match T)
	))
    match))

(defmethod ede-find-target ((proj ede-single-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
         (cls 'ede-single-root-target)
         (targets (oref proj targets))
         (dir default-directory)
         (ans (ede-single-root-find-matching-target cls dir targets))
         )
    (when (not ans)
      (setq ans (make-instance
                 cls
                 :name (file-name-nondirectory (directory-file-name dir))
                 :path dir
                 :source nil
		 :project proj))
      (object-add-to-list proj :targets ans))
    ans))

(defmethod project-compile-target ((obj ede-single-root-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (when (oref obj :project)
    (project-compile-project (oref obj :project) command)))

;;; File Stuff
;;
(defmethod ede-project-root-directory ((this ede-single-root-project)
                                       &optional file)
  "Return the root for THIS project with file."
  (oref this :directory))

(defmethod ede-project-root ((this ede-single-root-project))
  "Return my root."
  this)

(defmethod ede-find-subproject-for-directory ((proj ede-single-root-project)
                                              dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-source-paths ((proj ede-single-root-project) mode)
  "Get the base to all source trees in the current project for MODE."
  (let ((dir (ede-project-root-directory proj))
	(src-dirs (assoc mode (oref proj source-dirs))))
    (when src-dirs
      (remove nil
	      (mapcar (lambda (x)
			(let ((dir-name (concat dir x)))
			  (when (file-accessible-directory-p dir-name)
			    dir-name)))
		      (cdr src-dirs))))))

;;; Utility functions
(defun ede-single-root-get-mod-time (file)
  "Returns modification time for given file"
  (if (file-exists-p file)
      (float-time (nth 5 (file-attributes file)))
    0))

(defun ede-single-root-file-updated-p (proj)
  "Checks, was project file updated since last check or not."
  (when proj
    (> (ede-single-root-get-mod-time (oref proj file))
       (oref proj file-mod-time))))

(defun ede-single-root-read-string-with-compl (prompt &optional compls require-match?)
  "Reads string from minubufer using completions if they're specified"
  (if compls
      (completing-read prompt compls nil require-match?)
    (read-from-minibuffer prompt)))

(defun ede-single-root-read-list-from-minibufer (proj slot descr
							 &optional one-value? compl-slot-or-list)
  (if (and proj (symbolp slot) (slot-exists-p proj slot))
      (let* (lst
	     (compls (cond ((and (symbolp compl-slot-or-list)
				 (slot-exists-p proj compl-slot-or-list))
			    (eieio-oref proj compl-slot-or-list))
			   ((sequencep compl-slot-or-list) compl-slot-or-list)))
	    (str (ede-single-root-read-string-with-compl (format "Enter the first %s (ENTER to finish): " descr)
							    compls)))
	(while (and str (not (= (length str) 0)))
	  (setq lst (cons str lst))
	  (if one-value?
	      (setq str nil)
	    (setq str (ede-single-root-read-string-with-compl (format "Enter the next %s (ENTER to finish): " descr)
								 compls))))
	(eieio-oset proj slot (nreverse lst)))
    (message "There is no project, or slot '%s' doesn't exist in current project" slot)))

(defun ede-single-root-set-current-targets ()
  "Sets target(s) for current project."
  (interactive)
  (let ((proj (ede-current-project)))
    (ede-single-root-read-list-from-minibufer proj 'current-targets "target"
						 (and proj (slot-exists-p proj 'supports-multiple-commands-p)
						      (not (oref proj supports-multiple-commands-p)))
						 'existing-targets)))

(defun ede-single-root-set-current-target-options ()
  "Sets additional options for current target(s) in current project."
  (interactive)
  (let ((proj (ede-current-project)))
    (ede-single-root-read-list-from-minibufer proj 'target-options "target option")))

(defun ede-single-root-set-project-options ()
  "Sets a list of target-independent options for current project."
  (interactive)
  (let ((proj (ede-current-project)))
    (ede-single-root-read-list-from-minibufer proj 'project-options "project option")))

(provide 'ede/single-root)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/single-root"
;; End:

;;; jvm-base.el ends here
