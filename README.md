# MEL: Elisp HTML Templating

<p align="center"><img src="./logo.png"/></p>
<p align="center">"Short and sweet HTML."</p>

# Usage

The `mel` function accepts any number of nodes and returns an HTML string.
Each node is a list of the following form:

```emacs-lisp
(TAG [attribute val...] CHILDREN...)
```

## Tags

`TAG` should be a symbol starting with the name of an HTML tag.

```emacs-lisp
(mel '(h1 "heading"))
```

Returns:

```html
<h1>heading</h1>
```

### Classes
The `.` separator can be used in a tag symbol name to indicate a class.

```emacs-lisp
(mel '(h1.class "heading"))
```

Returns:

```html
<h1 class="class">heading</h1>
```

It may be used multiple times.

```emacs-lisp
(mel '(h1.one.two "heading"))
```

Returns:

```html
<h1 class="one two">heading</h1>
```

As a special case, if a tag symbol begins with a `.`, a div tag is implied.

```emacs-lisp
(mel '(.class "content"))
```

Returns:

```html
<div class="class">content<div/>
```

### IDs

A single `#` separator can be used to associate an ID with a tag.
Note that the separator must be escaped with a `\` in elisp.

```emacs-lisp
(mel '(h1\#id "heading"))
```

Returns:

```html
<h1 id="id">heading</h1>
```

Class and ID separators may be mixed.

```emacs-lisp
(mel '(h1.one\#id.two "heading"))
```

Returns:

```html
<h1 class="one two" id="id">heading</h1>
```

## Attributes

An optional attribute vector may be added as the second element of a node list.
Each attribute must be a symbol (optionally a keyword) followed by its value.
The value will be coerced to its string representation.

```emacs-lisp
(mel '(h1 [:one "true" two false]))
```

Returns:

```html
<h1 one="true" two="false" />
```


## Children

Any elements of a node specified after the tag and optional attribute vector are the node's children.
They may be either strings or nodes.

```emacs-lisp
(mel '(p "example " (span "text")))
```

Returns:

```html
<p>example <span>text</span></p>
```

# The MEL file format

A MEL file consists of a body which contains one or more nodes as top-level sexps.
The forms are implicitly backquoted, so elisp may be used within each node via the `,` and `,@`
[backquote constructs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html). 
The `mel-read` function may be used to include other mel files.

```elisp
(html
 (head
  (meta[:charset UTF-8])
  (link[:rel icon :type image/png :href "/tmp/favicon.png"])
  (title ,(format-time-string "Test @%F %I:%M:%S%p")))
 (body
  (h1 "Heading 1")
  (h2 "Heading 2")
  (img[ :src ,(concat "file://" (expand-file-name "~/Pictures/test.png"))
        :alt "This is alt text."
        :width 400 :height 100])
  (a[:href "https://www.google.com"] "A link")
  (p.class "This is a " (b "classy") " paragraph.")
  (main (pre ,@(mel-read "/tmp/another.mel")))
  (p\#id.test.one.two "And this has an ID and class.")))
```
