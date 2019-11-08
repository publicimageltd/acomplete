;;; acomplete.el --- completion with data included   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'seq)
(require 'cl-lib)
;; (require 'flib)

(defun acompleter-wrapface (face s)
  "Add FACE as a face property to S."
  (propertize s 'face face))

(defun acompleter-format-and-propertize (data format-string &rest args)
  "Add DATA as a text property to FORMAT-STRING."
  (let* ((s (apply 'format format-string args)))
    (propertize s 'data data)))

(defun acompleter-get-data (propertized-string)
  "Get data stored as a text property in PROPERTIZED-STRING."
  (get-text-property 0 'data propertized-string))

(defun acompleter-fn-or-format (string-fn s &rest args)
  "If STRING-FN is a function, pass S to it, else treat it as a format string with minimally one arg."
  (let* ((fn (cond
	      ((functionp string-fn) string-fn)
	      ((stringp   string-fn) (apply-partially #'format string-fn))
	      (t                     #'identity))))
    (apply fn s args)))

;; (acompleter-fn-or-format (lambda (s) (format "%s" s)) "hallo")
;; (acompleter-fn-or-format "%s" "hallo")

(defun acompleter-format-and-propertize-collection (collection string-fn &optional data-fn finalize-string-fn)
  "Build a list of strings with propertized data.

STRING-FN has to return a string presentation for an item in COLLECTION.

DATA-FN takes one data item as an argument and return the data to
be added to the string. If DATA-FN is nil, just add the item
itself.

FINALIZE-STRING-FN can modify the propertized string before it is
added to the resulting collection."
  (when collection
    (let* ((string-list     (mapcar (apply-partially #'acompleter-fn-or-format string-fn) collection))
	   (data-list       (mapcar (or data-fn #'identity) collection)))
      (mapcar (or finalize-string-fn #'identity)
	      (seq-mapn #'acompleter-format-and-propertize data-list string-list)))))
  
;; FIXME add completing-read-args
(cl-defun acompleter-completing-read (prompt acollection &key (require-match t) history)
  "Call `completing-read' with a collection of propertized string lists."
  (when-let* ((result (completing-read prompt acollection nil require-match nil history)))
    (acompleter-get-data result)))

;; (acompleter-completing-read "Test" (acompleter-format-and-propertize-collection '(1 2 3) #'number-to-string #'identity))


(provide 'acomplete)
;;; acomplete.el ends here
