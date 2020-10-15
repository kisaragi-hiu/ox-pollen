;;; ox-pollen.el --- Org export backend to Pollen markup -*- lexical-binding: t -*-

;; Copyright (C) 2020 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Keywords: org, wp, pollen
;; Version: 0.0.1
;; Package-Requires: ((org "9.4") (emacs "26.1"))
;; URL: https://kisaragi-hiu.com/projects/ox-pollen

;; This file is not part of GNU Emacs.

;; ox-pollen is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ox-pollen is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ox-pollen.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A to-be Org-to-Pollen exporter.

;;; Code:

(require 'ox)

(defgroup org-export-pollen nil
  "Options for the Pollen export backend."
  :group 'org-export)

(org-export-define-backend 'pollen
  (mapcar
   ;; org-pollen-bold, org-pollen-code, etc.
   (lambda (it)
     (cons it (intern (concat "ox-pollen-" (symbol-name it)))))
   '(bold
     center-block
     clock
     code
     drawer
     dynamic-block
     entity
     example-block
     export-block
     export-snippet
     fixed-width
     footnote-definition
     footnote-reference
     headline
     horizontal-rule
     inline-src-block
     inlinetask
     italic
     item
     keyword
     line-break
     link
     node-property
     paragraph
     plain-list
     plain-text
     planning
     property-drawer
     quote-block
     radio-target
     section
     special-block
     src-block
     statistics-cookie
     strike-through
     subscript
     superscript
     table
     table-cell
     table-row
     target
     template
     timestamp
     underline
     verbatim
     verse-block)))

(defun ox-pollen--block (command)
  "Return a function that creates creates a Pollen block COMMAND.

Calling that function with \"test\" should return ◊COMMAND{test}."
  (lambda (_obj contents _info) (format "◊%s{%s}" command contents)))

(defalias 'ox-pollen-verse-block (ox-pollen--block "verse"))
(defalias 'ox-pollen-superscript (ox-pollen--block "sup"))
(defalias 'ox-pollen-subscript (ox-pollen--block "sub"))
;; <strike> is deprecated in HTML5
(defalias 'ox-pollen-strikethrough (ox-pollen--block "s"))
;; "Bold" and "Italic" are not sementic in Org
(defalias 'ox-pollen-bold (ox-pollen--block "b"))
(defalias 'ox-pollen-italic (ox-pollen--block "i"))
(defalias 'ox-pollen-fixed-width (ox-pollen--block "code"))

(defun ox-pollen-paragraph (_obj contents _info)
  "Transcode paragraph element by unwrapping it."
  contents)

(defun ox-pollen-plain-text (text info)
  "Transcode a TEXT string into Pollen markup."
  ;; Modified from `org-md-plain-text'.
  ;; A lot of what it does is done by Pollen already, so delete them.
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  text)

(defun ox-pollen-section (_obj contents _info)
  "Transcode section element by unwrapping it."
  contents)

(defun ox-pollen-headline (obj _contents _info)
  "Transcode headline OBJ into h1, h2, h3...

This emits h7 and beyond, so define it in Pollen accordingly."
  (let ((level (org-element-property :level obj)))
    (format "◊h%s{%s}"
            level
            ;; We want `title' because it contains parsed links, for example
            (org-element-property :title obj))))

(defun ox-pollen-src-block (obj &rest _)
  (format "◊highlight['%s]{%s}"
          (org-element-property :language obj)
          (org-element-property :value obj)))

(defun ox-pollen-special-block (obj contents _info)
  "Transcode special block OBJ into Pollen."
  (format "◊%s{%s}"
          (downcase (org-element-property :type obj))
          contents))

(defun ox-pollen-keyword (obj _contents _info)
  (format "◊define-meta[%s]{%s}"
          (downcase (org-element-property :key obj))
          (org-element-property :value obj)))

(defun ox-pollen-link (link desc info)
  "Transcode LINK object into Pollen markup.

Almost completely copied from `org-md-link'."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                (t raw-path))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'pollen info))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          (`plain-text                  ; External file.
           (if (not desc)
               (format "◊link[\"%s\"]" path)
             (format "◊link[\"%s\"]{%s}" path desc)))
          (`headline
           (format
            "◊link[\"%s\"]{#%s}"
            ;; Reference.
            (or (org-element-property :CUSTOM_ID destination)
                (org-export-get-reference destination info))
            ;; Description.
            (cond ((org-string-nw-p desc))
                  ((org-export-numbered-headline-p destination info)
                   (mapconcat #'number-to-string
                              (org-export-get-headline-number destination info)
                              "."))
                  (t (org-export-data (org-element-property :title destination)
                                      info)))))
          (_
           (let ((description
                  (or (org-string-nw-p desc)
                      (let ((number (org-export-get-ordinal destination info)))
                        (cond
                         ((not number) nil)
                         ((atom number) (number-to-string number))
                         (t (mapconcat #'number-to-string number ".")))))))
             (when description
               (format "◊link[\"#%s\"]{%s}"
                       (org-export-get-reference destination info)
                       description)))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (cond ((not (string-equal type "file"))
                         (concat type ":" raw-path))
                        ((not (file-name-absolute-p raw-path)) raw-path)
                        (t (expand-file-name raw-path))))
            (caption (org-export-data
                      (org-export-get-caption
                       (org-export-get-parent-element link))
                      info)))
        (if (not (org-string-nw-p caption))
            (format "◊image[\"%s\"]{%s}" path caption)
          (format "◊image[\"%s\"]" path))))
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
              (org-export-resolve-coderef path info)))
     ((equal type "radio") desc)
     (t (if (not desc)
            (format "◊image[\"%s\"]" path)
          (format "◊image[\"%s\"]{%s}" path desc))))))

(defun ox-pollen-horizontal-rule (&rest _)
  "Transcode a horizontal-rule element."
  "◊hr[]")

(provide 'ox-pollen)
;;; ox-pollen.el ends here
