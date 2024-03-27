# MEL: Elisp HTML Templating

![A honeycomb with the word 'mel' written in honey in the
center.](./logo.png) “Short and sweet HTML.”

## Usage

The `mel` function accepts any number of nodes and returns an HTML
string.Each node is a list of the following form:

    (TAG [attribute val...] CHILDREN...)

### Tags

`TAG` must be a symbol staring with the name of an HTML tag.

`(mel '(h1 "heading"))`

Returns:

`<h1>heading</h1>`

### Classes

The `.` separator can be used in a tag symbol name to indicate a class.

`(mel '(h1.class "heading"))`

Returns:

`<h1 class="class">heading</h1>`

It may be used multiple times.

`(mel '(h1.one.two "heading"))`

Returns:

`<h1 class="one two">heading</h1>`

As a special case, if a tag symbol begins with a `.`, a div tag is
implied.

`(mel '(\.class "content"))`

Returns:

`<div class="class">content</div>`

### IDs

A single `#` separator can be used to associate an ID with a tag.Note
that the separator must be escaped with a `\` in elisp.The `@` separator
is an alias for `#` which does not need to be escaped.

`(mel '(h1\#one "heading") '(h2@two "heading"))`

Returns:

`<h1 id="one">heading</h1><h2 id="two">heading</h2>`

### Attributes

An optional attribute vector may be added as the second element of a
node list.Each attribute must be a symbol (optionally a keyword)
followed by its value.The value will be coerced to its string
representation.

`(mel '(h1 [:one "true" :two false] "heading"))`

Returns:

`<h1 one="true" two="false">heading</h1>`

### Children

Any elements of a node specified after the tag and optional attribute
vector are the node's children.They may be either strings or nodes.

`(mel '(p "example " (span "text")))`

Returns:

`<p>example <span>text</span> </p>`

## Tempalte Files

An `htmel` file must contain an emacs-lisp program.When evaluated, the
value of the last expression must be a mel spec for an HTML document.For
example, the source for this page is stored in
`[./index.htmel](./index.htmel)`.

Content stored in other files can be included via the `mel-load`
function.

## File Inclusion

The `mel-load` function can be used to parse and load files into a
template.

`(mel-load "./include.mel")`

Returns:

`((p This paragraph was included from (code ./include.mel)))`
