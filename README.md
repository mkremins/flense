# Flense

Flense is an experimental editor interface for Clojure code that takes the ideas behind Paredit to their ultimate conclusion, making structural editing first-class and disallowing raw text editing except within single atomic nodes. Write code as trees, not text.

## Features

* Paredit-like structural editing commands
* Clojure-specific refactoring commands
* Commands are pure functions from zipper to zipper
* Automatic code formatting as you type
* Embeddable in other apps as a standalone editor component

## Usage

(Note that Flense is still very incomplete. I'd recommend keeping your distance for now.)

Add to your `project.clj`:

```clojure
[mkremins/flense "0.0-278"]
```

Build a Flense editor component:

```clojure
(ns foo.bar
  (:require [flense.editor :refer [editor-view]]
            [om.core :as om]))

(om/root editor-view app-state
  {:target ...
   :opts {:edit-chan ...
          :propagate-keypress? ...}})
```

Documentation of the expected `app-state` and `:opts` coming soon. Until then, the adventurous can look at [flense-nw](https://github.com/mkremins/flense-nw) for example usage.

Remember to include the stylesheets from `resources/stylesheets` in your HTML. `base.css` defines bare-minimum ground rules for code layout within Flense editor views, and should always be included; `theme.css` adds syntax highlighting and some typography improvements, and can be left out or replaced with your own theme if you want.

## License

[MIT License](http://opensource.org/licenses/MIT). Hack away.
