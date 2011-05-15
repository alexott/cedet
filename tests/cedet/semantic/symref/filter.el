;;;###autoload
(defun semantic-symref-test-count-hits-in-tag ()
  "Lookup in the current tag the symbol under point.
the count all the other references to the same symbol within the
tag that contains point, and return that."
  (interactive)
  (let* ((ctxt (semantic-analyze-current-context))
	 (target (car (reverse (oref ctxt prefix))))
	 (tag (semantic-current-tag))
	 (start (current-time))
	 (Lcount 0))
    (when (semantic-tag-p target)
      (semantic-symref-hits-in-region
       target (lambda (start end prefix) (setq Lcount (1+ Lcount)))
       (semantic-tag-start tag)
       (semantic-tag-end tag))
      (when (cedet-called-interactively-p)
	(message "Found %d occurances of %s in %.2f seconds"
		 Lcount (semantic-tag-name target)
		 (semantic-elapsed-time start (current-time))))
      Lcount)))
