;; mind-map
;; Convert a subtree in org-mode into a mind-map pdf.


(defvar chun-mind-map-graph nil "A dict holding a graph struture.
the dtype is Dict[str, List[str]], mapping a node to its neighbourhood nodes")

(defun chun-mind-map-visualize-this ()
  "Visualize the subtree under the current headline."
  (interactive))

(require 'ht)
(require 'cl)

(defun chun-mind-map-extract-graph-from-subtree (element)
  "Extract the graph structure from current subtree.
Input:
  - element: a org element
Returns a hashmap.
")
(defun level-wise-traversal ()
  "Perform level-wise traversal of headlines in the Org-mode buffer."
  (interactive)
  (let ((current-level 1))
    (org-map-entries
     (lambda ()
       (let ((level (org-outline-level)))
         (if (> level current-level)
             (progn
               (setq current-level level)
               (message "Entering Level %d" level))
           (when (< level current-level)
             (setq current-level level)
             (message "Returning to Level %d" level)))))
     t nil)))

;; Example usage:
;; Place your cursor anywhere in the Org-mode buffer and call the function `level-wise-traversal`.

(defun chun-mind-map/--get-title (element)
  (org-element-property :raw-value element))


(defun chun-mind-map/build-graph-on-this()
  (interactive)
  (let* ((element (org-element-at-point))
         (content (buffer-substring-no-properties (org-element-property :begin element)
                                                  (org-element-property :end element)))
         (headline-to-nodeid-map (ht-create))
         (graph (chun-mind-map/create-graph))
         (title (chun-mind-map/--get-title element))

         (org-file-path (file-truename buffer-file-name))
         (out-filename (concat (file-name-sans-extension org-file-path) "-" title ".dot"))
         dot-code
         )

    (save-excursion
      (with-temp-buffer
        (insert content)
        (setq element (org-element-parse-buffer))
        (org-element-map element 'headline (lambda (node) (chun-mind-map/--visit-headline graph node headline-to-nodeid-map)))
        ))

    (message "graph:\n%S" graph)
    (with-temp-file out-filename
      (insert (chun-mind-map/graph-to-dot graph)))
    (chun-mind-map/compile-dot-to-pdf out-filename)
    ))

(defun chun-mind-map/--visit-headline (graph element headline-to-nodeid-map)
  (defun --get-node-id (element)
    (format "%d:%d" (org-element-property :begin element)
            (org-element-property :end element)))

  (message "visit title: %S" (org-element-property :raw-value element))

  (let* ((parent (org-element-property :parent element))
         (title (org-element-property :raw-value element))
         (current-node (chun-mind-map/graph-add-node graph title))
         (current-key (--get-node-id element))
         parent-key
         parent-nodeid)
    ;; insert key into headline-to-node-id-map
    (ht-set headline-to-nodeid-map current-key (plist-get current-node :nodeid))

    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))

    (when parent
      (setq parent-key (--get-node-id parent))
      (when (ht-contains? headline-to-nodeid-map parent-key)
        (chun-mind-map/graph-add-edge graph
                                      (ht-get headline-to-nodeid-map parent-key)
                                      (plist-get current-node :nodeid))))))


(defun chun-mind-map/--traversal-within-org-element (graph element &optional current-level parent-node)
  "Build a graph using a tree level-traversal.

Input:
  - graph: a graph plist
  - element: an org element
  - current-level: the level of element's level
  - parent-node: a plist returned by `chun-mind-map/graph-add-node'

Returns: nil
"
  (let* ((level (or current-level 1)))
    (cond ((not element) nil)
          ((eq (org-element-type element) 'headline)
           (let* ((headline-level (org-element-property :level element))
                  (title (org-element-property :raw-value element))
                  (content (buffer-substring-no-properties (org-element-property :begin element)
                                                           (org-element-property :end element)))
                  (current-node (chun-mind-map/graph-add-node graph title)))
             ;; (message "testing title: %S" title)
             (when (eq headline-level level)
               (message "visit title: %S" title)
               ;; process the current node
               (when parent-node
                 (let* ((src-id (plist-get parent-node :nodeid))
                        (dst-id (plist-get current-node :nodeid)))
                   (chun-mind-map/graph-add-edge graph src-id dst-id)))

               ;; (message "content: %S" content)
               (save-excursion
                 (with-temp-buffer
                   (insert content)
                   (let* ((children (org-element-parse-buffer)))
                     ;; (message "children: %S" children)
                     (org-element-map children 'headline (lambda (child) (chun-mind-map/--traversal-within-org-element graph child (+ 1 current-level) current-node)))
                     ;; (chun-mind-map/--traversal-within-org-element children (+ 1 current-level))
                     )))


               ;; (chun-mind-map/--traversal-within-org-element (org-element-contents element) headline-level)))
           ))))))

;; ============================== utilities to generate DOT code ==============================
;;
(defun chun-mind-map/graph-to-dot (graph)
  "Generate DOT code to visualize the graph structure.
Input:
  - graph: The graph (plist) containing nodes and connections.
Returns: A string containing the DOT code."
  (let* ((dot-style (chun-mind-map/default-dot-style))
        (graph-kind (plist-get dot-style :graph-kind))
        (label (plist-get dot-style :label))
        (fontname (plist-get dot-style :fontname))
        (node-style (plist-get dot-style :node-style))
        (edge-style (plist-get dot-style :edge-style))
        (nodes (plist-get graph :nodes))
        (connections (plist-get graph :graph))
        (indent-level 0)
        (indent " ")
        dot-code)

    (defun -insert (fmt &rest args)
      "indented insert"
      ;; insert indent
      (let ((i 0))
        (while (< i indent-level)
          (insert indent)
          (incf i)))
      (insert (apply 'format fmt args)))

    (save-excursion
      (with-current-buffer (get-buffer-create "*chun-mind-map*")
        (erase-buffer)
        ;; Generate DOT code for the graph header
        (-insert "%s \"[stackcollapse]\" {\n" graph-kind)
        ;; Set global graph attributes
        (incf indent-level)
        (-insert "graph [fontname=\"%s\" overlap=false concentrate=true rank=sink rankdir=LR outputorder=edgesfirst splines=curved nodesep=0.1 ranksep=0.6];\n" fontname)
        ;; Set node attributes
        (-insert "node [style=\"%s\" penwidth=%d fillcolor=\"%s\" fontcolor=\"%s\"];\n"
                              (plist-get node-style :style)
                              (plist-get node-style :penwidth)
                              (plist-get node-style :fillcolor)
                              (plist-get node-style :fontcolor))
        (-insert "edge [penwidth=%d color=\"%s\"];\n"
                              (plist-get edge-style :penwidth)
                              (plist-get edge-style :color))
        ;; Insert node declarations
        (dolist (node-info (ht-items nodes))
          (let* ((nodeid (car node-info))
                 (label (car (cdr node-info))))
            (-insert "\"%s\" [label=\"%s\"];\n" nodeid label)))
        ;; Insert edges
        (dolist (source-info (ht-items connections))
          (let* ((source-node-id (car source-info))
                 (target-node-ids (cdr source-info)))
            (message "targets %S" (car target-node-ids))
            (dolist (target-node-id (car target-node-ids))
              (unless (null target-node-id) (-insert "\"%s\" -> \"%s\";\n" source-node-id target-node-id)))))

        ;; Insert end
        (decf indent-level)
        (-insert "}\n")

        (buffer-string)))))

(defun chun-mind-map/default-dot-style ()
  "Return a hashtable with dot related styles"
    (list
     :graph-kind "digraph"
     :label "mind map"
     :fontname "RW Chancery L, Apple Chancery, Comic Sans MS, cursive"

     :node-style (list
                :style "filled"
                :penwidth 0
                :fillcolor "#f0f0ffA0"
                :fontcolor "indigo")

     :edge-style (list
                  :penwidth 5
                  :color "#f0f0ff")))

(defun chun-mind-map/compile-dot-to-pdf (dot-file)
  "Compile the DOT-FILE to PDF and open the resulting PDF file."
  (interactive "fEnter the path to the DOT file: ")
  (let* ((output-pdf (concat (file-name-sans-extension dot-file) ".pdf"))
         (command (format "dot -Tpdf \"%s\" -o \"%s\"" dot-file output-pdf))
         (open-file-command (format "open \"%s\"" output-pdf))
         )
    (message "dot command: %s" command)
    (shell-command command)
    (if (file-exists-p output-pdf)
        (progn
          (message "DOT file compiled successfully to PDF: %s" output-pdf)
          (shell-command open-file-command)
          ;; (find-file-other-window output-pdf)
          )
      (message "Failed to compile DOT file to PDF."))))

;; some utilities to help construct a graph, like add node, add edge and so on
(defun chun-mind-map/create-graph ()
  (let* ((graph (list
                 :nodes (ht-create) ;; nodeid -> node properties
                 :node-count 0
                 :graph (ht-create) ;; nodeid
                 )))
    graph))

(defun chun-mind-map/graph-add-node (graph label)
  (let* ((nodeid (format "n%d" (plist-get graph :node-count)))
         (node (list :label label :nodeid nodeid))
         (node-count (plist-get graph :node-count))
         (nodes (plist-get graph :nodes)))

    (plist-put graph :node-count (+ node-count 1))
    (ht-set nodes nodeid label)

    (ht-set (plist-get graph :graph) nodeid '())
    node))

(defun chun-mind-map/graph-add-edge (graph source-node-id target-node-id)
  "Add an edge between two nodes in the graph.
Input:
  - graph: The graph (plist) containing nodes and connections.
  - source-node-id: The ID of the source node.
  - target-node-id: The ID of the target node.
Returns: Modified graph with the edge added."
  (message "edge: %S -> %S" source-node-id target-node-id)
  (let* ((connections (plist-get graph :graph))
         (existing-edges (ht-get (plist-get graph :graph) source-node-id)))

    (if existing-edges
        (ht-set connections source-node-id (append existing-edges (list target-node-id)))
      (ht-set connections source-node-id (list target-node-id)))

    graph))
