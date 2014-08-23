# Flense
Flense is an experimental editor interface that takes the ideas behind Paredit to their ultimate conclusion, making structural editing first-class and disallowing raw text editing except within single atomic nodes. Write code as trees, not text.

## Editing layers

Flense characterizes each of the various transformations that can be carried out on a Clojure source tree as occurring on one of three distinct *editing layers*. From lowest to highest level, these layers are referred to as the *textual*, *structural* and *semantic* layers respectively.

### Textual

Of the three editing layers, it's the transformations performed here that look the most like traditional character-by-character text editing. Textual transformations are restricted in scope: in order to keep Flense's underlying conceptual model of tree transformation intact, text may only be edited directly within leaf nodes of the source tree. Operations categorized under this layer include the insertion and deletion of single characters within a string, symbol, keyword, number, or other atomic data structure.

### Structural

This editing layer is heavily inspired by Paredit and consists primarily of Paredit-style tree transformations involving branch nodes of the source tree. Slurping, barfing, splitting and joining of S-expressions all occur here, as do the common operations of inserting new nodes into the source tree and removing existing nodes from the tree entirely.

What differentiates transformations occurring here from those occurring on the semantic layer is *context*. Structural transformations are context-agnostic and could (at least in theory) apply just as well to XML or JSON data as to Clojure source, so long as the underlying tree model remains intact; semantic transformations, on the other hand, rely heavily on knowledge of the Clojure-specific meanings assigned to certain structural patterns to work their magic.

### Semantic

The semantic layer – so named because transformations occurring here make explicit use of Clojure semantics, and don't make sense in non-Clojure editing contexts – is the highest-level editing layer made available by Flense. You could refer to this layer as the "refactoring layer" without being entirely wrong; the transformations comprising this layer are diverse in terms of the specific structural transformations they perform, but all might be regarded as performing some sort of refactoring operation or another on the Clojure source tree.

Since only this layer is tied to Clojure semantics in particular, long-term future development of Flense or a Flense-like editor might well involve the introduction of non-Clojure semantic layers allowing for the high-level editing of tree structures with decidedly different semantics. In this regard, the semantic layer is roughly equivalent to a language-specific Emacs major mode.

## Features

### Implemented

* Most of the structural editing commands implemented by Paredit
* Robust structural editing API providing support for custom editing commands
* User-configurable keybinds

### Planned

* Declarative structural find-and-replace
* Various Clojure-specific semantic editing commands
* Inline code analysis, linting and refactoring suggestions
* Inline code evaluation at arbitrary levels of granularity

## Building

Flense currently requires [node-webkit](https://github.com/rogerwang/node-webkit) to run. You'll also need [npm](https://www.npmjs.org/) to install some of Flense's dependencies and [Leiningen](http://leiningen.org/) to compile the ClojureScript source.

```bash
cd path/to/flense
lein cljsbuild once
npm install
path/to/node-webkit .
```

This will launch Flense as a standalone GUI app.

## License
[MIT License](http://opensource.org/licenses/MIT). Hack away.
