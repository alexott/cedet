(defun semantic-lex-spp-write-test ()
  "Test the semantic tag writer against the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*SPP Write Test*"
    (semantic-lex-spp-table-write-slot-value
     (semantic-lex-spp-save-table))))

(defun semantic-lex-spp-write-utest ()
  "Unit test using the test spp file to test the slot write fcn."
  (interactive)
  (let* ((sem (locate-library "semantic/lex-spp.el"))
	 (dir (file-name-directory sem)))
    (save-excursion
      (set-buffer (find-file-noselect
		   (expand-file-name "tests/testsppreplace.c"
				     dir)))
      (semantic-lex-spp-write-test))))
