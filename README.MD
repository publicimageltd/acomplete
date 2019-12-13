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
with it can optionally be fine tuned with functions.

Using STRING-FN and DATA-FN: To determine how the data is presented to
the user, the item is first passed to STRING-FN before selection. If
no function is provided, just print the item as is. Then, the item to
be associated with the entry via DATA-FN is determined. If no data
function is provided, just use the item as is. Finally, FINALIZE-FN
adds a final touch to the selection strings.

 `STRING-FN`:  Recieve a data item and return a string. Can be either
 a function or a string which is passed to `format`. Defaults to
 `"%s"`. 
 
 `DATA-FN`:  Modifiy the data item before it is 'packed' into the string as its associated return value. Defaults to `identity`.

`FINALIZE-FN`: Receive a string and return a string. Defaults to `identity`.

# LONGER DESCRIPTION:

In the most basic case, the items in COLLECTION are returned
unmodified. The item can be modified by DATA-FN, however. This
modification is taking place AFTER the string representation is
created. That is, STRING-FN has always to respond to the actual item
in COLLECTION, not to its possibly modified variant returned by
DATA-FN. You can think of DATA-FN as the function that modifies the
data item after its selection (even though in fact, the modified data
is already stored in the selection strings presented to the user).

To give your (unmodified) data a representation, use STRING-FN. The
value of STRING-FN can be either a function or itself a string. As a
function, STRING-FN takes the unmodified item and returns its string
representation. If STRING-FN is itself a string, however, it is passed
to the format function with the item as its argument. Think of
STRING-FN as 'the function which determines how the item is
represented to the user.' STRING-FN defaults to `"%s"`'.

Finally, the string created so far (INCLUDING the property slot
with the modified data) can be mapped through :FINALIZE-FN, to add
some beauty.

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
