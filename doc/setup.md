# Setting up Flense

To embed a Flense editor component in your own project, you'll need to do a little bit of setup.

```clojure
(ns foo.bar
  (:require [flense.editor :as flense]
            [flense.model :refer [forms->document]]
            [om.core :as om]))

(defonce app-state
  (atom (forms->document ["Hello, world!"])))
```

`app-state` should be an atom containing a *document*: that is, an [xyzzy](https://github.com/mkremins/xyzzy) zipper over a Clojure parse tree. Flense provides a helper function (`flense.model/forms->document`) that takes a seq of Clojure forms and returns an appropriately structured zipper, which you can then use as a document.

```clojure
(om/root flense/editor app-state
  {:target (js/document.getElementById "flense")
   :opts {:line-length 72}})
```

Any `:opts` you pass in are used to perform additional, optional configuration. `:line-length` controls the suggested maximum number of characters per line: Flense will try to split up forms that serialize to this many characters or more across multiple lines. The default `:line-length` is 72.

```html
<html>
<head>
  <link rel="stylesheet" href="path/to/resources/stylesheets/base.css">
  <link rel="stylesheet" href="path/to/resources/stylesheets/theme.css">
</head>
<body>
  ...
</body>
</html>
```

Remember to include the Flense stylesheets (located in the [`resources/stylesheets`](https://github.com/mkremins/flense/tree/master/resources/stylesheets) directory of this repo) in your HTML. `base.css` defines bare-minimum ground rules for code layout within Flense editor views, and should always be included. `theme.css` adds syntax highlighting and some typography improvements, and can be left out or replaced with your own theme if you want.

## Notes

This example setup uses `om.core/root` to mount a Flense editor component directly onto the page, but you're free to use `om.core/build` to construct an editor component as the child of another component if you want.

Want to see a more fully-featured example? Take a look at [flense-nw](https://github.com/mkremins/flense-nw), specifically the `flense-nw.app` namespace.
