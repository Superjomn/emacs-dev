;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("quote" "#begin_quote\n  $1\n#end_quote" "quote" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/quote" nil nil)
                       ("pause" "#+latex: \\pause\n" "pause" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/pause" nil nil)
                       ("newanki" "* $1 :vocab:\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:ANKI_DECK: English-learn-org\n:END:\n\n** Front\n$2\n** Back\n$3\n" "newanki" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/newanki" nil nil)
                       ("link" "[[$2][$1]]\n" "link" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/link" nil nil)
                       ("ankiv" "* $1 :vocab:\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n\n** Front\n$2\n** Back\n$3\n" "ankiv" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/ankiv" nil nil)
                       ("ankis" "* $1 :sentence:\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:ANKI_DECK: English-learn-org\n:END:\n\n** Front\n$1\n** Back\n$2" "ankis" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/ankis" nil nil)
                       ("ankieasy" "* $1\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:ANKI_DECK: English-learn-org\n:END:\n\n** Front\n$1\n\n** Back\n" "ankieasy" nil nil nil "/Users/yanchunwei/.doom.d/snippets/org-mode/ankieasy" nil nil)))


;;; Do not edit! File generated at Sat Dec 18 08:22:03 2021
