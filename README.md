
# MEL: Elisp HTML Templating

> Short and sweet HTML.

## Usage

<p>The <code>mel</code> function accepts any number of nodes and returns
  an HTML string.Each node is a list of the following form:</p>

```emacs-lisp
(TAG [attribute val...] CHILDREN...)
```

### Tags

<p><code>TAG</code> must be a symbol staring with the name of an HTML
  tag.</p>

### Classes

<p>The <code>.</code> separator can be used in a tag symbol name to
  indicate a class.</p>
It may be used multiple times.<p>As a special case, if a tag symbol begins with a <code>.</code>, a
  div tag is implied.</p>

### IDs

<p>A single <code>#</code> separator can be used to associate an ID with
  a tag.Note that the separator must be escaped with a <code>\</code> in
  elisp.The <code>@</code> separator is an alias for <code>#</code> which
  does not need to be escaped.</p>

### Attributes

An optional attribute vector may be added as the second element of a node list.
Each attribute must be a symbol (optionally a keyword) followed by its value.
The value will be coerced to its string representation.
### Children

Any elements of a node specified after the tag and optional attribute vector are the node's children.
They may be either strings or nodes.
## Tempalte Files

<p>An <code>htmel</code> file must contain an emacs-lisp program.When
  evaluated, the value of the last expression must be a mel spec for an
  HTML document.For example, the source for this page is stored in
  <code>[./index.htmel](./index.htmel)</code>.</p>
<p>Content stored in other files can be included via the
  <code>mel-load</code> function.</p>

## File Inclusion

<p>The <code>mel-load</code> function can be used to parse and load
  files into a template.</p>
