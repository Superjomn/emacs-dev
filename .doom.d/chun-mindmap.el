;; mind-map
;; Convert a subtree in org-mode into a mind-map pdf.

(require 'ht)
(require 'cl)
(require 'org-element)

(defun chun-mind-map/--get-title (element)
  (org-element-property :raw-value element))

(cl-defstruct chun-mind-map/graph
  "The graph holds all the stuff for visualization."

  (graph (ht-create))
  (node-count 0)
  (nodes (ht-create)))

(cl-defstruct chun-mind-map/node
  nodeid
  label
  ;; figure path, only a single figure is supported
  (image "")
  ;; a list
  (enum '())
  )

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
         (first-list (chun-mind-map/--get-first-list-or-enumeration element))
         (enum '())
         current-node
         (current-key (--get-node-id element))
         parent-key
         parent-nodeid)

    (if first-list
        (setq enum (chun-mind-map/--org-list-get-enum first-list)))
    (setq current-node (chun-mind-map/graph-add-node graph title enum))
    ;; insert key into headline-to-node-id-map
    (ht-set headline-to-nodeid-map current-key (chun-mind-map/node-nodeid current-node))

    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))

    (when parent
      (setq parent-key (--get-node-id parent))
      (when (ht-contains? headline-to-nodeid-map parent-key)
        (chun-mind-map/graph-add-edge graph
                                      (ht-get headline-to-nodeid-map parent-key)
                                      (chun-mind-map/node-nodeid current-node))))))

(defun chun-mind-map/--get-first-list-or-enumeration (element)
  "Get the first list or enumeration within an Org element.
Ignore nested lists inside sub-headlines."
  (let* ((found nil))
    (org-element-map element '(item plain-list headline)
      (lambda (el)
        (unless found
          (cond
           ((eq (org-element-type el) 'item) (setq found el))
           ((eq (org-element-type el) 'plain-list) (setq found el))
           ((eq (org-element-type el) 'headline) (setq found nil))))))
    found))

(defun chun-mind-map/--org-list-get-enum (list-element)
  (when (eq (org-element-type list-element) 'plain-list)
    (let* ((contents (org-element-contents list-element))
           (ret '())
           )
      (dolist (element contents)
        (let* ((content (buffer-substring (org-element-property :contents-begin element)
                                          (org-element-property :contents-end element)))
               (trimed (string-trim-right content)))
          (setq ret (append ret (list trimed)))))
      ret
      )))

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
        (nodes (chun-mind-map/graph-nodes graph))
        (connections (chun-mind-map/graph-graph graph))
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
          (let* ((node (car (cdr node-info))))
            (-insert "%s;\n"(chun-mind-map/dot-node node))))
        ;; Insert edges
        (dolist (source-info (ht-items connections))
          (let* ((source-node-id (car source-info))
                 (target-node-ids (cdr source-info)))
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

(defun chun-mind-map/dot-node (node)
  (if (not (chun-mind-map/node-enum node))
      (chun-mind-map/dot-plan-node node)
    (chun-mind-map/dot-list-node node)))

(defun chun-mind-map/dot-plan-node (node)
  "Add a node in DOT
Input:
  - node: `chun-mind-map/node'
Returns: str"
  (format "\"%s\" [label=\"%s\"]"
          (chun-mind-map/node-nodeid node)
          (chun-mind-map/node-label node)))

(cl-defun chun-mind-map/dot-list-node (node)
  "Convert a `chun-mind-map/node` to a DOT language representation of a node with a table."
  (let* ((label (chun-mind-map/node-label node))
         (enum (chun-mind-map/node-enum node))
         (table-rows (cons (format "<tr><td><b>%s</b></td></tr>" label)
                           (mapcar (lambda (item) (format "<tr><td>%s</td></tr>" item)) enum))))

    (message "draw enum %S" (chun-mind-map/node-enum node))
    (format "%s [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">%s</table>> style=none]"
            (chun-mind-map/node-nodeid node)
            (string-join table-rows "\n"))))

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
  (make-chun-mind-map/graph))

(defun chun-mind-map/graph-add-node (graph label enum)
  (message "enum: %S" enum)
  (let* ((nodeid (format "n%d" (chun-mind-map/graph-node-count graph)))
         (node (make-chun-mind-map/node :nodeid nodeid :label label :enum enum))
         (node-count (chun-mind-map/graph-node-count graph))
         (nodes (chun-mind-map/graph-nodes graph)))
    (message (format "nodeid: %s label: %s" nodeid label))

    (incf (chun-mind-map/graph-node-count graph))
    (ht-set nodes nodeid node)

    (ht-set (chun-mind-map/graph-graph graph) nodeid '())
    node))

(defun chun-mind-map/graph-add-edge (graph source-node-id target-node-id)
  "Add an edge between two nodes in the graph.
Input:
  - graph: The graph (plist) containing nodes and connections.
  - source-node-id: The ID of the source node.
  - target-node-id: The ID of the target node.
Returns: Modified graph with the edge added."
  ;; (message "edge: %S -> %S" source-node-id target-node-id)
  (let* ((connections (chun-mind-map/graph-graph graph))
         (existing-edges (ht-get (chun-mind-map/graph-graph graph) source-node-id)))

    (if existing-edges
        (ht-set connections source-node-id (append existing-edges (list target-node-id)))
      (ht-set connections source-node-id (list target-node-id)))

    graph))
