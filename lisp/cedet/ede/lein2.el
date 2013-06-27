;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.

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

(defgroup ede-lein2 nil
  "Emacs Development Environment. Lein2 options"
  :group 'ede
  :group 'tools
  :group 'extensions)

(defcustom ede-lein2-execute-lein-to-get-classpath t
  "Defines, should we execute Lein to get classpath information or not."
  :group 'ede-lein2
  :require 'ede/lein2
  :type 'boolean)

(defcustom ede-lein2-lein-command "lein"
  "Executabe, that will be executed as lein"
  :group 'ede-lein2
  :require  'ede/lein2
  :type 'string)

(defcustom ede-lein2-lein-options nil
  "Lein's default command line options"
  :group 'ede-lein2
  :require  'ede/lein2
  :type 'list)

;;;###autoload
(defconst ede-lein2-project-file-name "project.clj"
  "name of project file for Lein2 projects")

;;;###autoload
(defun ede-lein2-project-root (&optional dir)
  "Get the Lein2 root directory for DIR."
  (ede-find-project-root ede-lein2-project-file-name dir))

(defvar ede-lein2-project-list nil
  "List of projects created by option `ede-lein2-project'.")

;;;###autoload
(defun ede-lein2-load (dir &optional rootproj)
  "Return a Leiningen Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing dir ede-lein2-project-list)
      ;; Doesn't already exist, so lets make one.
      (let ((this
             (ede-lein2-project "Leiningen2"
                                 :name "Leiningen dir" ; make fancy name from dir here.
                                 :directory dir
                                 :file (expand-file-name ede-lein2-project-file-name dir)
				 :current-target '("jar"))))
	(ede-add-project-to-global-list this)
	this)))

;;;###autoload
(defclass ede-lein2-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-lein2-project-list)
   )
  "EDE Leiningen2 project class."
  :method-invocation-order :depth-first)

(defmethod initialize-instance ((this ede-lein2-project)
                                &rest fields)
  "Make sure the all slots are setup."
  (call-next-method)
  (ede-normalize-file/directory this ede-lein2-project-file-name)
  (oset this supports-multiple-commands-p nil)
  ;; TODO: add analysis of project.clj by calling rescan?
  ;; TODO: move to rescan later, and it should be based on content of project.clj
  (oset this source-dirs '((java-mode "src-java" "java")
			   (clojure-mode "src" "test")
			   (clojurescript-mode "src-cljs")))
  )

(defmethod project-compile-project ((proj ede-lein2-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (combine-and-quote-strings
	      (append (list ede-lein2-lein-command)
		      ede-lein2-lein-options
		      (oref proj :project-options)
		      (when (oref proj :current-target)
			(list (car (oref proj :current-target))))
		      (oref proj :target-options))))))

;;; Classpath-related stuff
(defconst lein2-outfile-name "lein-classpath")

(defmethod ede-java-classpath ((proj ede-lein2-project))
  "Get classpath for Lein project"
  (ede-jvm-get-classpath-from-command proj ede-lein2-execute-lein-to-get-classpath
				      lein2-outfile-name ede-lein2-lein-command
				      `(,nil ,nil ,nil "classpath"
					     ,lein2-outfile-name)))

;; TODO: re-implement when project.clj parser will be available
(defmethod project-rescan ((proj ede-lein2-project))
  "Rescan the EDE proj project THIS."
  (when (ede-single-root-file-updated-p proj)
    ;; TODO: fill information
    ))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "lein2"
		       :name "Lein2"
		       :file 'ede/lein2
		       :proj-file ede-lein2-project-file-name
		       :proj-root 'ede-lein2-project-root
		       :load-type 'ede-lein2-load
		       :class-sym 'ede-lein2-project
		       :new-p nil
		       :safe-p t
		       )
 'generic)

(provide 'ede/lein2)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/lein2"
;; End:

;;; lein2.el ends here
