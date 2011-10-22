;;; cedet-butil.el --- Utilities for building CEDET via Makefile
;;
;; Copyright (C) 2011 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
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
;; Building CEDET is a challenge.  These utilities are needed to provide simple
;; functions for dealing with different environments for compilation of sources.

;;; Code:

(defun cedet-remove-builtin ()
  "Force any built-in CEDET to be removed from the load path.
Force all symbols that belong to CEDET to be unloaded.
This is a needed first step in getting CEDET installed from outside sources."
  (interactive)
  (when (featurep 'cedet)
    (error "Cannot unload builtin CEDET if CEDET is already loaded in."))
  (when (featurep 'eieio)
    (error "EIEIO is already loaded.  Removing CEDET now would be unwise."))
  (when (featurep 'semantic)
    (error "Semantic is already loaded.  Removing CEDET now would be unwise."))

  ;; Remove from load-path.
  (let* ((clp (locate-library "cedet"))
	 (root (when clp (directory-file-name (file-name-directory clp))))
	 )
    (setq load-path (delete root load-path)))

  ;; Find ALL autoloaded symbols related to CEDET, and delete them.
  (dolist (R '("^cedet" "^semantic" "^global-ede-" "^srecode-")) 
    (dolist (S (apropos R))
      (when (and (fboundp (car S))
		 (let ((sf (symbol-function (car S))))
		   (and (listp sf) (eq (car sf) 'autoload))))
	(fmakunbound (car S)))))
  )

(provide 'cedet-butil)

;;; cedet-butil.el ends here
