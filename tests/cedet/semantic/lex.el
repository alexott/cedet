(defun semantic-lex-test-full-depth (arg)
  "Test the semantic lexer in the current buffer parsing through lists.
Usually the lexer parses
If universal argument ARG, then try the whole buffer."
  (interactive "P")
  (let* ((start (current-time))
         (result (semantic-lex
                  (if arg (point-min) (point))
                  (point-max)
                  100))
         (end (current-time)))
    (message "Elapsed Time: %.2f seconds."
             (semantic-elapsed-time start end))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

(defun semantic-lex-test-region (beg end)
  "Test the semantic lexer in the current buffer.
Analyze the area between BEG and END."
  (interactive "r")
  (let ((result (semantic-lex beg end)))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))
