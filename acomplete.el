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
;; BASIC USAGE:
;;
;; (acomplete PROMPT COLLECTION :string-fn :data-fn :finalize-fn)
;;
;; SHORT DESCRIPTION:
;;
;; Let the user choose between the items in COLLECTION. The items can
;; be any kind of data. To determine how the data is presented to the
;; user, the item is first passed to STRING-FN before selection. The
;; selected item is either returned directly as it is, or passed to
;; DATA-FN for further modifications. FINALIZE-FN adds a final touch
;; to the selection strings.
;;
;; LONGER DESCRIPTION:
;;
;; In the most basic case, the items in COLLECTION are returned
;; unmodified. The item can be modified by DATA-FN, however. This
;; modification is taking place AFTER the string representation is
;; created. That is, STRING-FN has always to respond to the actual
;; item in COLLECTION, not to its possibly modified variant returned
;; by DATA-FN. You can think of DATA-FN as the function that modifies
;; the data item after its selection, even though in fact, the
;; modified data is already stored in the selection strings presented
;; to the user.
;;
;; To give your (unmodified) data a representation, use STRING-FN. The
;; value of STRING-FN can be either a function or itself a string. As
;; a function, STRING-FN takes the unmodified item and returns its
;; string representation. If STRING-FN is itself a string, however, it
;; is passed to the format function with the item as its argument.
;; Think of STRING-FN as 'the function which determines how the item
;; is represented to the user.'
;;
;; Finally, the string created so far (INCLUDING the property slot
;; with the modified data) can be mapped through :FINALIZE-FN, to add
;; some beauty.
;;
;; MORE DETAILS
;;
;; You can access the data slot by passing the propertized string to
;; the function `acomplete-get-data'. This is sometimes useful when
;; finalizing a string.
;;
;; EXAMPLES
;;
;; Simple choice between numbers:
;;
;; (acomplete "Select number: " '(1 2 3))
;;
;; -> returns the number (as an integer)
;;
;; Nicer strings:
;;
;; (acomplete "Select number: " '(1 2 3) :string-fn "Number %d.")
;;
;; -> returns the number (as an integer)
;;
;; Select a color:
;;
;;  (acomplete "Select color: " (defined-colors)
;;              :finalize-fn (lambda (color-string) (propertize color-string 'face `(:foreground ,color-string))))
;;
;; -> returns the color string
;; 
;; Select a buffer:
;;
;; (acomplete "Select buffer: " (buffer-list)
;;             :string-fn #'buffer-name)
;;
;;  -> returns the buffer object
;;
;; Select a currently open file:
;;
;; (acomplete "Select open file: " (seq-filter #'buffer-file-name (buffer-list))
;;           :data-fn #'buffer-file-name)
;;
;; -> returns the complete filename
;;
;; Select a color with a more elaborated string representation:
;;
;; (acomplete "Select color: " (defined-colors)
;; 	   :string-fn (lambda (color-string)
;; 			(concat color-string " "
;; 				(apply #'color-rgb-to-hex (append (color-name-to-rgb color-string) '(2)))))
;; 	   :finalize-fn (lambda (propertized-string)
;; 			  (let* ((color-string (acomplete-get-data propertized-string)))
;; 			    (propertize propertized-string 'face `(:foreground ,color-string)))))
;; -> returns the color name
;;
;; Select a buffer with no match required:
;;
;; (acomplete "Select buffer: " (buffer-list)
;; 	     :require-match nil))
;;

;;; Code:

(require 'seq)
(require 'cl-lib)

(defun acomplete-wrapface (face s)
  "Add FACE as a face property to S."
  (propertize s 'face face))

(defun acomplete-format-and-propertize (data format-string &rest args)
  "Add DATA as a text property to FORMAT-STRING."
  (let* ((s (apply 'format format-string args)))
    (propertize s 'data data)))

(defun acomplete-get-data (propertized-string)
  "Get data stored as a text property in PROPERTIZED-STRING."
  (get-text-property 0 'data propertized-string))

(defun acomplete-fn-or-format (string-fn s &rest args)
  "If STRING-FN is a function, pass S to it, else treat it as a format string with minimally one arg."
  (let* ((fn (cond
	      ((functionp string-fn) string-fn)
	      ((stringp   string-fn) (apply-partially #'format string-fn))
	      (t                     #'identity))))
    (apply fn s args)))

(defun acomplete-format-and-propertize-collection (collection string-fn &optional data-fn finalize-string-fn)
  "Build a list of strings with propertized data.

STRING-FN has to return a string presentation for an item in COLLECTION.

DATA-FN takes one data item as an argument and return the data to
be added to the string. If DATA-FN is nil, just add the item
itself.

FINALIZE-STRING-FN can modify the propertized string before it is
added to the resulting collection."
  (when collection
    (let* ((string-list     (mapcar (apply-partially #'acomplete-fn-or-format string-fn) collection))
	   (data-list       (mapcar (or data-fn #'identity) collection)))
      (mapcar (or finalize-string-fn #'identity)
	      (seq-mapn #'acomplete-format-and-propertize data-list string-list)))))
  
(cl-defun acomplete-completing-read (&key prompt acollection (require-match t) history)
  "Call `completing-read' with a collection of propertized string lists."
  (declare (indent 1))
  (let* ((result    (completing-read prompt acollection nil require-match nil history))
	 (has-data  (get-text-property 0 'data result)))
    (if (or require-match has-data)
	(acomplete-get-data result)
      result)))

;;;###autoload
(cl-defun acomplete (prompt collection
			    &key (string-fn "%s")
			    (data-fn #'identity)
			    (finalize-fn #'identity)
			    (require-match t)
			    history)
  "Offer completion on COLLECTION.

Collection will be represented to the user by mapping STRING-FN
to each item. If STRING-FN is a string, it is used as a format
string with the item as its single argument.

DATA-FN maps a data item to each item of the collection.

Before offering the strings for selection, the will eventually
passed to FINALIZE-FN."
  (acomplete-completing-read
      :prompt prompt
      :acollection (acomplete-format-and-propertize-collection
		    collection string-fn data-fn finalize-fn)
      :require-match require-match
      :history history))

;; (acomplete "Select: " '(1 2 3) :string-fn "Nummer %s.")

(provide 'acomplete)
;;; acomplete.el ends here
