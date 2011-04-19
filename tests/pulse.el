(defun pulse-test (&optional no-error)
  "Test the lightening function for pulsing a line.
When optional NO-ERROR Don't throw an error if we can't run tests."
  (interactive)
  (if (or (not pulse-flag) (not (pulse-available-p)))
      (if no-error
          nil
        (error (concat "Pulse test only works on versions of Emacs"
                       " that support pulsing")))
    ;; Run the tests
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse one line.")
      (read-char))
    (pulse-momentary-highlight-one-line (point))
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse a region.")
      (read-char))
    (pulse-momentary-highlight-region (point)
                                      (save-excursion
                                        (condition-case nil
                                            (forward-char 30)
                                          (error nil))
                                        (point)))
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse line a specific color.")
      (read-char))
    (pulse-momentary-highlight-one-line (point) 'modeline)
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse a pre-existing overlay.")
      (read-char))
    (let* ((start (point-at-bol))
           (end (save-excursion
                  (end-of-line)
                  (when (not (eobp))
                    (forward-char 1))
                  (point)))
           (o (pulse-make-overlay start end))
           )
      (pulse-momentary-highlight-overlay o)
      (if (pulse-overlay-live-p o)
          (pulse-overlay-delete o)
        (error "Non-temporary overlay was deleted!"))
      )
    (when (cedet-called-interactively-p)
      (message "Done!"))))
