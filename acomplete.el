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

;; Completion with data "packed" within the selected data.
;;
;; BASIC USAGE
;;
;; (acompleter-completing-read PROMPT ACOLLECTION)
;;
;; ACOLLECTION is a list of strings. The strings are propertized. The data is stored in the property `data'.
;;
;; The completing read function returns the associated data object,
;; not the string itself.
;;
;; There are convenience functions to create and access the data strings:
;; acompleter-format-and-propertize DATA FORMAT-STRING &rest ARGS
;;
;;   Add DATA as a property item to the formatted FORMAT-STRING. Args
;;   and FORMAT-STRING are passed to `format'.
;;
;; acompleter-format-and-propertize-collection COLL STRING-FN DATA-FN FINALIZE-FN
;;
;;    Propertize each item in COLL, successively. The string is
;;    created by STRING-FN, which has to return a string
;;    representation of each candidate in COLL. Similarly, DATA-FN
;;    returns the data to be packed into the string. If DATA-FN is
;;    ommitted, simply store the unpropertized string itself as its
;;    data. FINALIZE-FN can be used to add some final touch on the
;;    string, AFTER the data has been packed into it.
;;
;;    Alternatively, STRING-FN can also be a format string. This is a
;;    shortcut for passing the function
;;       (lambda (item) (format-string "some-format" item))
;;
;; EXAMPLE
;;
;; 1. Simple selection:
;;
;; (acompleter-completing-read "Select: "
;; 			    (acompleter-format-and-propertize-collection
;; 			     '(1 2 3)
;; 			     #'number-to-string))
;;
;; 2. Finalize string:

(acompleter-completing-read "Color: "
			    (acompleter-format-and-propertize-collection
			     ())


;; (acompleter-completing-read
;; "Test" (acompleter-format-and-propertize-collection '(1 2 3)
;; #'number-to-string #'identity))


;;; Code:

(require 'seq)
(require 'cl-lib)

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


(provide 'acomplete)
;;; acomplete.el ends here
