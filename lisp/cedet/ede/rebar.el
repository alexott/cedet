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

(require 'ede/single-root)

;; TODO: https://twitter.com/5HT/status/348670556838719489 https://twitter.com/5HT/status/348672457357541376

(defgroup ede-rebar nil
  "Emacs Development Environment. Rebar options"
  :group 'ede
  :group 'tools
  :group 'extensions)

(defcustom ede-rebar-rebar-command "rebar"
  "Executabe, that will be executed as rebar"
  :group 'ede-rebar
  :require  'ede/rebar
  :type 'string)

(defcustom ede-rebar-rebar-options nil
  "Rebar's default command line options"
  :group 'ede-rebar
  :require  'ede/rebar
  :type 'list)

;;;###autoload
(defconst ede-rebar-project-file-name "rebar.config"
  "name of project file for Rebar projects")

;;;###autoload
(defun ede-rebar-project-root (&optional dir)
  "Get the Rebar root directory for DIR."
  (ede-find-project-root ede-rebar-project-file-name dir))

(defvar ede-rebar-project-list nil
  "List of projects created by option `ede-rebar-project'.")

;;;###autoload
(defun ede-rebar-load (dir &optional rootproj)
  "Return a Rebar Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing dir ede-rebar-project-list)
      ;; Doesn't already exist, so lets make one.
      (let ((this
             (ede-rebar-project "Rebar"
				:name "Rebar dir" ; make fancy name from dir here.
				:directory dir
				:file (expand-file-name ede-rebar-project-file-name dir)
				:current-target '("compile")
				:existing-targets '("compile" "ct" "eunit" "generate"
						    "analyze" "check-deps" "get-deps"
						    "clean" "build_plt" "check_plt"
						    "create" "create-app" "create-node"
						    "list-templates" "doc" "xref"
						    "delete-deps")
				)))
	(ede-add-project-to-global-list this)
	this)))

;;;###autoload
(defclass ede-rebar-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-rebar-project-list)
   )
  "EDE Rebar project class."
  :method-invocation-order :depth-first)

(defmethod initialize-instance ((this ede-rebar-project)
                                &rest fields)
  "Make sure the all slots are setup."
  (call-next-method)
  (ede-normalize-file/directory this ede-rebar-project-file-name)
  ;; TODO: add analysis of rebar.config
  (oset this source-dirs '((erlang-mode "src" "include" "test")))
  )

(defmethod project-compile-project ((proj ede-rebar-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (combine-and-quote-strings
	      (append (list ede-rebar-rebar-command)
		      (oref proj :project-options)
		      (oref proj :target-options)
		      (oref proj :current-target))))))

;; TODO: re-implement when rebar.config parser will be available
(defmethod project-rescan ((proj ede-rebar-project))
  "Rescan the EDE proj project THIS."
  (when (ede-single-root-file-updated-p proj)
    ;; TODO: fill information
    ))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "rebar"
		       :name "Rebar"
		       :file 'ede/rebar
		       :proj-file ede-rebar-project-file-name
		       :proj-root 'ede-rebar-project-root
		       :load-type 'ede-rebar-load
		       :class-sym 'ede-rebar-project
		       :new-p nil
		       :safe-p t
		       )
 'generic)

(provide 'ede/rebar)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/rebar"
;; End:

;;; rebar.el ends here
