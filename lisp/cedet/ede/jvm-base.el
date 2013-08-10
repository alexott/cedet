;; Copyright (C) 2012 Free Software Foundation, Inc.

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

(require 'ede/single-root)
(require 'cedet-files)

;;;###autoload
(defclass ede-jvm-base-project (ede-single-root-project)
  ((classpath :initform nil
	      :initarg :classpath ;; for projects, that can't detect classpath automatically
	      :type list
	      :documentation
	      "Classpath that is either detected automatically, or set by user, depending
on project's type."
	      )
   )
  "Base project class for JVM-base projects."
  :method-invocation-order :depth-first)

(defmethod ede-java-classpath ((proj ede-jvm-base-project))
  "Generic implementation for JVM-based projects"
  (oref proj :classpath))

;;; TARGET MANAGEMENT
;;

(defclass ede-jvm-base-target (ede-target)
  ((project :initform nil
	    :initarg :project)
   )
  "EDE JVM-based Project target.
All directories need at least one target.")

(defun ede-jvm-get-classpath-from-command (proj exec-flag outfile-name exec-command options)
  "Get classpath for given JVM-based project"
  (if (or (not exec-flag)
	  (and (oref proj classpath)
	       (not (ede-single-root-file-updated-p proj))))
      (oref proj classpath)
    (let* ((default-directory (ede-project-root-directory proj))
	   (err 0))
      (condition-case niloutfile
	  (setq err (apply 'call-process exec-command options))
	(error 1))
      (when (zerop err)
	(let ((files (cedet-files-list-recursively default-directory outfile-name))
	      cp)
	  (dolist (F files)
	    (save-excursion
	      (condition-case nil
		  (let* ((output (with-temp-buffer
				   (insert-file-contents F)
				   (buffer-string)))
			 (cp-list (if output (split-string output ":") nil)))
		    (when (file-exists-p F)
		      (delete-file F)
		      (when (and output cp-list)
			(dolist (C cp-list)
			  (add-to-list 'cp C)))))
		(error (when (file-exists-p F)
			 (delete-file F))
		       nil))))
	  (when cp
	    (oset proj classpath cp)
	    (oset proj file-mod-time (ede-single-root-get-mod-time (oref proj file))))
	  cp)))))

(provide 'ede/jvm-base)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/jvm-base"
;; End:

;;; jvm-base.el ends here
