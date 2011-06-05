(defvar cogre-periodic-node-name-list
  '( ( "cogre-node" cogre-node )
     ( "cogre-node (2)" cogre-node )
     ( "cogre-package" cogre-package )
     ( "cogre-class" cogre-class )
     ( "cogre-class (2)" cogre-class )
     ( "cogre-instance" cogre-instance )
     ( "cogre-instance (2)" cogre-instance )
     ( "Notes about COGRE" cogre-note )
    )
  "List of node names and classes in the graph.
Used for testing purposes in conversion routines.")

(defvar cogre-periodic-link-connectivity-list
  '(
    ( "cogre-node" "cogre-node2" cogre-link )
    ( "cogre-package" "cogre-class" 'cogre-aggregate )
    ( "cogre-class (2)" "cogre-class" cogre-inherit )
    ( "cogre-instance" "cogre-instance (2)" cogre-arrow)
    )
  "List of link connectivity from the periodic table graph.
Used for testing purpses in conversion routines.")

;;;###autoload
(defun cogre-periodic-utest ()
  "Run the cogre periodic table for unit testing.
Also test various output mechanisms from the periodic table."
  (interactive)
  ;; Create the table.
  (cogre-periodic)
  (sit-for 0)
  ;; ASCII output
  (cogre-export-ascii)
  (sit-for 0)
  )
