;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'ox-pollen)
(require 'org-element)

(defun test-ox-pollen--org-element-parse (string parser &rest args)
  "Use PRASER to parse STRING into an element.

Pass ARGS to PARSER."
  (with-temp-buffer
    (save-excursion
      (insert string))
    (apply parser args)))

(ert-deftest test-ox-pollen--str-to-list ()
  (should
   (equal (ox-pollen--str-to-list "abc")
          '("abc")))
  (should
   (equal (ox-pollen--str-to-list
           "\"abc\" \"def ghi\"")
          '("abc" "def ghi"))))

(ert-deftest test-ox-pollen--escape-lozenge ()
  (should
   (equal (ox-pollen--escape-lozenge "abc")
          "abc"))
  (should
   (equal (ox-pollen--escape-lozenge "ab◊cd")
          "ab◊\"◊\"cd")))

(ert-deftest test-ox-pollen--block ()
  (should
   (equal (funcall (ox-pollen--block "test")
                   nil "content" nil)
          "◊test{content}")))

(ert-deftest test-ox-pollen--identity ()
  (should
   (equal (ox-pollen--identity nil "content" nil)
          "content")))

(ert-deftest test-ox-pollen--discard ()
  (should
   (equal (ox-pollen--discard "a" "b" "c")
          "")))

(ert-deftest test-ox-pollen--value-block ()
  (let ((element
         (test-ox-pollen--org-element-parse
          "=abc=" #'org-element-code-parser)))
    (should
     (equal (funcall (ox-pollen--value-block "tag")
                     element nil nil)
            "◊tag{abc}"))
    (should
     (equal (funcall (ox-pollen--value-block "code")
                     element nil nil)
            "◊code{abc}"))))

(ert-deftest test-ox-pollen-export-block ()
  (let ((element (test-ox-pollen--org-element-parse
                  "#+begin_export\nabcdef\n#+end_export"
                  #'org-element-export-block-parser
                  nil nil)))
    (should
     (equal (ox-pollen-export-block element)
            "abcdef\n"))))

;; (ert-deftest test-ox-pollen-property-drawer ())
;; (ert-deftest test-ox-pollen-node-property ())
;; (ert-deftest test-ox-pollen-target ())
;; (ert-deftest test-ox-pollen-template ())
;; (ert-deftest test-ox-pollen-timestamp ())
;; (ert-deftest test-ox-pollen-drawer ())
;; (ert-deftest test-ox-pollen-entity ())
;; (ert-deftest test-ox-pollen-plain-text ())
;; (ert-deftest test-ox-pollen-headline ())
;; (ert-deftest test-ox-pollen-src-block ())
;; (ert-deftest test-ox-pollen-special-block ())
;; (ert-deftest test-ox-pollen-keyword ())
;; (ert-deftest test-ox-pollen-link ())
;; (ert-deftest test-ox-pollen-plain-list ())
;; (ert-deftest test-ox-pollen-item ())
;; (ert-deftest test-ox-pollen-export-to-pollen ())

(defun test-ox-pollen-run-tests ()
  (interactive)
  (ert-run-tests-interactively "test-ox-pollen-"))
