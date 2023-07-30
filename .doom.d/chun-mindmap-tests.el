(defun load-relative-elisp-file (relative-path)
  "Load an elisp file using a relative path."
  (let ((full-path (expand-file-name relative-path (file-name-directory buffer-file-name))))
    (load full-path)))

(load-relative-elisp-file "chun-mindmap.el")
;; (load "~/.doom.d/chun-mindmap.el")

(require 'test-simple)
(test-simple-start) ;; Zero counters and start the stop watch.

;; check graph strcture
(assert-t (chun-mind-map/create-graph))
(assert-t (plist-get (chun-mind-map/create-graph) :node-count) 0)
;; -- add node
(let* ((graph (chun-mind-map/create-graph))
       (node0 (chun-mind-map/graph-add-node graph "hello world"))
       (node1 (chun-mind-map/graph-add-node graph "hello world1"))
       (node2 (chun-mind-map/graph-add-node graph "hello world2"))
       dot-code
       )
  (assert-equal (plist-get graph :node-count) 3)
  (assert-equal (plist-get node0 :nodeid) "n0")
  (chun-mind-map/graph-add-edge graph "n0" "n1")
  (chun-mind-map/graph-add-edge graph "n0" "n2")
  (setq dot-code (chun-mind-map/graph-to-dot graph))
  (message "%s" dot-code)
  (princ dot-code)
  )
