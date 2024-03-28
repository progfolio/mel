
# MEL: Elisp HTML Templating

<p align="center"><img class="center" src="./logo.png" alt="A honeycomb with the word 'mel' written in honey in the center." /></p>

> Short and sweet HTML.

## Usage

The `mel` function accepts any number of nodes and returns an HTML string.
Each node is a list of the following form:

```emacs-lisp
(TAG [attribute val...] CHILDREN...)
```

### Tags

`TAG` must be a symbol staring with the name of an HTML tag.

### Classes

The `.` separator can be used in a tag symbol name to indicate a class.
It may be used multiple times.
As a special case, if a tag symbol begins with a `.`, a div tag is implied.

### IDs

A single `#` separator can be used to associate an ID with a tag.
Note that the separator must be escaped with a `\` in elisp.
The `@` separator is an alias for `#` which does not need to be escaped.

### Attributes

An optional attribute vector may be added as the second element of a node list.
Each attribute must be a symbol (optionally a keyword) followed by its value.
The value will be coerced to its string representation.

### Children

Any elements of a node specified after the tag and optional attribute vector are the node's children.
They may be either strings or nodes.

## Tempalte Files

An `htmel` file must contain an emacs-lisp program.
When evaluated, the return value of the last expression must be a mel spec for a document.
For example, the source for this page is stored in [./index.htmel](./index.htmel).
A `mel` file is similar to an htmel file, but the return value of each top-level sexp is collected into a list.
This is useful for including partial templates within other templates (see below).

## File Inclusion

Content stored in other files can be included via the `mel-load` function.
The `mel-load` function can be used to parse and load files into a template.
