;;; ox-pollen.el --- Org export backend to Pollen markup -*- lexical-binding: t -*-

;; Copyright (C) 2020 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Keywords: org, wp, pollen
;; Version: 0.0.1
;; Package-Requires: ((org "9.1") (emacs "25.1") (dash "1.0"))
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
(require 'dash)

(defgroup org-export-pollen nil
  "Options for the Pollen export backend."
  :group 'org-export)

(defun ox-pollen--block (command)
  "Return a function returning a Pollen block COMMAND.

Calling that function with \"test\" should return ◊COMMAND{test}."
  (lambda (_obj contents _info) (format "◊%s{%s}" command contents)))

(defun ox-pollen--identity (_obj contents _info)
  "Return CONTENTS."
  contents)

(defun ox-pollen--discard (&rest _)
  "Discard an element."
  "")

(org-export-define-backend 'pollen
  `((bold                . ,(ox-pollen--block "b"))
    (center-block        . ,(ox-pollen--block "center"))
    ;; Not bothering implementing time tracking mechanism in an export.
    (clock               . ox-pollen--identity)
    (planning            . ox-pollen--discard)
    ;; I consider these three to be equivalent
    (code                . ,(ox-pollen--block "code"))
    (verbatim            . ox-pollen--identity)
    (inline-src-block    . ,(ox-pollen--block "code"))
    (drawer              . ox-pollen-drawer)
    (dynamic-block       . ox-pollen--identity)
    (entity              . ox-pollen-entity)
    (example-block       . ,(ox-pollen--block "example"))
    (fixed-width         . ,(ox-pollen--block "example"))
    ;; specific HTML export is... unneeded.
    (export-block        . ox-pollen--identity)
    (export-snippet      . ox-pollen--identity)
    (footnote-definition . ,(ox-pollen--block "reftxt"))
    (footnote-reference  . ,(ox-pollen--block "ref"))
    (headline            . ox-pollen-headline)
    (horizontal-rule     . ,(-const "◊hr[]"))
    (inlinetask          . ox-pollen--identity)
    (italic              . ,(ox-pollen--block "i"))
    (item                . ox-pollen-item)
    (keyword             . ox-pollen-keyword)
    (line-break          . ,(-const "◊br[]"))
    (link                . ox-pollen-link)
    (node-property       . ox-pollen-node-property)
    (paragraph           . ox-pollen--identity)
    (plain-list          . ox-pollen-plain-list)
    (plain-text          . ox-pollen-plain-text)
    (property-drawer     . ox-pollen-property-drawer)
    (quote-block         . ,(ox-pollen--block "blockquote"))
    (section             . ox-pollen--identity)
    (special-block       . ox-pollen-special-block)
    (src-block           . ox-pollen-src-block)
    (statistics-cookie   . ox-pollen-statistics-cookie)
    ;; <strike> is deprecated in HTML5
    (strike-through      . ,(ox-pollen--block "s"))
    (subscript           . ,(ox-pollen--block "sub"))
    (superscript         . ,(ox-pollen--block "sup"))
    (table               . ,(ox-pollen--block "table"))
    (table-cell          . ,(ox-pollen--block "th"))
    (table-row           . ,(ox-pollen--block "tr"))
    (target              . ox-pollen-target)
    ;; Radio Target in Org allows implicit linking, which we're not
    ;; going to implement in an exporter. Just treat it as a normal
    ;; target.
    (radio-target        . ox-pollen-target)
    (template            . ox-pollen-template)
    (timestamp           . ox-pollen-timestamp)
    (underline           . ,(ox-pollen--block "u"))
    (verse-block         . ,(ox-pollen--block "verse"))))

(defun ox-pollen-statistics-cookie (obj &rest _)
  (funcall
   (ox-pollen--block "statistics-cookie")
   (org-element-property :value obj)))

(defun ox-pollen-property-drawer (obj contents _info)
  "Transcode a property drawer into an HTML details element in Pollen Markup."
  (format
   "◊details{◊summary{Properties}
◊dd{%s}}"
   contents))

(defun ox-pollen-node-property (obj &rest _)
  "Transcode a node property."
  (format "◊dt{%s}\n◊dd{%s}"
          (org-element-property :key obj)
          (org-element-property :value obj)))

(defun ox-pollen-target (obj _contents _info)
  (format "◊span[#:id \"%s\"]" (org-element-property :value obj)))

(defun ox-pollen-template (contents _info)
  "Final processing of the converted document."
  (concat "#lang pollen\n\n" contents))

(defun ox-pollen-timestamp (obj contents _info)
  (funcall (ox-pollen--block "code")
           (org-element-property :raw-value obj)))

(defun ox-pollen-drawer (obj contents _info)
  "Transcode a drawer into an HTML details element in Pollen Markup."
  (format
   "◊details{◊summary{%s}
%s}"
   (org-element-property :drawer-name obj)
   contents))

(defun ox-pollen-entity (obj &rest _)
  "Transform an entity."
  (org-element-property :utf-8 obj))

(defun ox-pollen-plain-text (text info)
  "Transcode a TEXT string into Pollen markup."
  ;; Modified from `org-md-plain-text'.
  ;; A lot of what it does is done by Pollen already, so delete them.
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  text)

(defun ox-pollen-headline (obj contents _info)
  "Transcode headline OBJ into h1, h2, h3...

This emits h7 and beyond, so define it in Pollen accordingly."
  (let ((level (org-element-property :level obj)))
    (format "◊h%s[#:id \"%s\"]{%s}\n\n%s"
            level
            (downcase (org-element-property :raw-value obj))
            ;; We want `title' because it contains parsed links, for example
            (org-element-property :raw-value obj)
            contents)))

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
            (format "◊link[\"%s\"]" path)
          (format "◊link[\"%s\"]{%s}" path desc))))))

;;;; The fun part: Lists

(defun ox-pollen-plain-list (obj contents _info)
  (pcase (org-element-property :type obj)
    (`unordered
     (format "◊ul{%s}" contents))
    (`ordered
     (format "◊ol{%s}" contents))
    (`descriptive
     (format "◊dl{%s}" contents))))

(defun ox-pollen-item (obj contents _info)
  (pcase (org-element-property :type (org-export-get-parent obj))
    ((or 'unordered 'ordered)
     (format "◊li{%s}" contents))
    ('descriptive
     (format "◊dt{%s}\n◊dd{%s}"
             (car (org-element-property :tag obj))
             contents))))

;;;; Public interface (apart from being an ox backend)

(defun ox-pollen-export-to-pollen ()
  "Export current buffer to a Pollen markup file."
  (org-export-to-file 'pollen (org-export-output-file-name ".html.pm")))

(provide 'ox-pollen)
;;; ox-pollen.el ends here
