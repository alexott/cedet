(defun semantic-test-data-cache ()
  "Test the data cache."
  (interactive)
  (let ((data '(a b c)))
    (save-excursion
      (set-buffer (get-buffer-create " *semantic-test-data-cache*"))
      (erase-buffer)
      (insert "The Moose is Loose")
      (goto-char (point-min))
      (semantic-cache-data-to-buffer (current-buffer) (point) (+ (point) 5)
                                     data 'moose 'exit-cache-zone)
      (if (equal (semantic-get-cache-data 'moose) data)
          (message "Successfully retrieved cached data.")
        (error "Failed to retrieve cached data"))
      )))

(defun semantic-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (semantic-throw-on-input 'done-die)
  (message "Exit Code: %s"
           (semantic-exit-on-input 'testing
             (let ((inhibit-quit nil)
                   (message-log-max nil))
               (while t
                 (message "Looping ... press a key to test")
                 (semantic-throw-on-input 'test-inner-loop))
               'exit)))
  (when (input-pending-p)
    (if (fboundp 'read-event)
        (read-event)
      (read-char)))
  )
