# acomplete
Provide an API to allow completion with associated data.

# Feature
Completion with data "packed" within the selected data.

# BASIC USAGE:

```
(acomplete PROMPT COLLECTION &optional :string-fn FN :data-fn FN :finalize-fn FN)
```

# SHORT DESCRIPTION:

Let the user choose between the items in COLLECTION. The items can be
any kind of data. The string representation and the data associated
with it can be optionally  fine tuned with functions.

# LONGER DESCRIPTION:

In the most basic case, the user selects an item and the function
returns the item itself.

The item to be returned can be modified by __DATA-FN__, however. That is,
DATA-FN maps an item of COLLECTION to the item which will be returned
by the selection. DATA-FN defaults to `identity`.

The representation of the items of COLLECTION can be further
controlled by __STRING-FN__. Think of STRING-FN as 'the function which
determines how the item is represented to the user.' The value of
STRING-FN can be either a function or itself a string. As a function,
STRING-FN takes the original (!) item (that is, the item before it is
passed to DATA-FN) and returns its string representation. If STRING-FN
is itself a string, this string is simply passed to the `format`
function with the item as its only argument. STRING-FN defaults to
`"%s"`'.

For pretty printing, the string created so far can be modified via
__FINALIZE-FN__. Note that this function receives the complete
selection string as its argument, where the text property `data`
contains the associated return value. This function thus allows to
modify the string in response to its associated data (i.e., colorize
the string according to the data type). To access the data, use
`acomplete-get-data`, which receives the propertized string and
returns the associated data item.

Summary:

 `STRING-FN`: Receive a data item and return a string. Can be either a
 function or a string which is passed to `format`. Defaults to `"%s"`.
 
 `DATA-FN`: Modify the data item before it is 'packed' into the
 string as its associated return value. Defaults to `identity`.

`FINALIZE-FN`: Receive a propertized string and return a (propertized)
string. Defaults to `identity`.

# MORE DETAILS

You can access the data slot by passing the propertized string to
the function `acomplete-get-data'. This is sometimes useful when
finalizing a string.

# EXAMPLES

## Simple choice between numbers:

```
(acomplete "Select number: " '(1 2 3))
```
-> returns the number (as an integer)

Nicer strings:

```
(acomplete "Select number: " '(1 2 3) :string-fn "Number %d.")
```
-> returns the number (as an integer)

## Select a color:

```
 (acomplete "Select color: " (defined-colors)
             :finalize-fn (lambda (color-string) (propertize color-string 'face `(:foreground ,color-string))))
```

-> returns the color string

## Select a buffer:

```
(acomplete "Select buffer: " (buffer-list)
            :string-fn #'buffer-name)
```
 -> returns the buffer object

## Select a currently open file:

```
(acomplete "Select open file: " (seq-filter #'buffer-file-name (buffer-list))
          :data-fn #'buffer-file-name)
```
-> returns the complete filename

## Select a color with a more elaborated string representation:

```
(acomplete "Select color: " (defined-colors)
	   :string-fn (lambda (color-string)
			(concat color-string " "
				(apply #'color-rgb-to-hex (append (color-name-to-rgb color-string) '(2)))))
	   :finalize-fn (lambda (propertized-string)
			  (let* ((color-string (acomplete-get-data propertized-string)))
			    (propertize propertized-string 'face `(:foreground ,color-string)))))
```
-> returns the color name

## Select a buffer with no match required:

```
(acomplete "Select buffer: " (buffer-list)
	     :require-match nil)
```
