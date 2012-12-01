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

;;; Commentary:
;;

(require 'ede/jvm-base)

(defgroup ede-ant nil
  "Emacs Development Environment. Ant options"
  :group 'ede
  :group 'tools
  :group 'extensions)

(defcustom ede-ant-ant-command "ant"
  "Executabe, that will be executed as Ant"
  :group 'ede-ant
  :require  'ede/proj-ant
  :type 'string)

;;;###autoload
(defconst ede-ant-project-file-name "build.xml"
  "name of project file for Ant projects")

;;;###autoload
(defun ede-ant-project-root (&optional dir)
  "Get the Ant root directory for DIR."
  (ede-find-project-root ede-ant-project-file-name dir))

(defvar ede-ant-project-list nil
  "List of projects created by option `ede-ant-project'.")

;;;###autoload
(defun ede-ant-load (dir &optional rootproj)
  "Return a Leiningen Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing dir ede-ant-project-list)
      ;; Doesn't already exist, so lets make one.
      (let ((this
             (ede-ant-project "Ant"
			      :name "Ant dir" ; make fancy name from dir here.
			      :directory dir
			      :file (expand-file-name ede-ant-project-file-name dir)
			      )))
	(ede-add-project-to-global-list this)
	this)))

;;;###autoload
(defclass ede-ant-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-ant-project-list)
   )
  "EDE Ant project class."
  :method-invocation-order :depth-first)

(defmethod project-compile-project ((proj ede-ant-project) &optional command)
  "Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (combine-and-quote-strings
	      (append (list ede-ant-ant-command "-noinput" "-e" (oref proj :current-target))
		      (oref proj :target-options))))))

;;; Classpath-related stuff
;; TODO: how to find include .jars? cedet-files-list-recursively is tooo slow for big file
;; trees. Maybe it's better to use find instead?
(defmethod ede-java-classpath ((proj ede-ant-project))
  "Get classpath for Ant project"
  (if (and (oref proj :classpath)
	   (not (ede-jvm-base-file-updated-p proj)))
      (oref proj :classpath)
    (let ((lst nil;(cedet-files-get-list-of-files (ede-project-root-directory proj) "*.jar")
	   ))
      lst)))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "ant"
		       :name "Ant"
		       :file 'ede/proj-ant
		       :proj-file ede-ant-project-file-name
		       :proj-root 'ede-ant-project-root
		       :load-type 'ede-ant-load
		       :class-sym 'ede-ant-project
		       :new-p nil
		       :safe-p t
		       ))

(provide 'ede/proj-ant)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/proj-ant"
;; End:

;;; proj-ant.el ends here
