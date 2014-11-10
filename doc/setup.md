# Setting up Flense

To embed a Flense editor component in your own project, you'll need to do a little bit of setup.

```clojure
(ns foo.bar
  (:require [cljs.core.async :as async]
            [flense.editor :as flense]
            [flense.model :refer [forms->document]]
            [om.core :as om]))

(defonce edit-chan (async/chan))
```

`edit-chan` should be a core.async channel. We'll use this channel to issue commands to the embedded Flense editor component. Any function that is put onto this channel will be invoked as an *action*, receiving the current app state as an argument. If an action returns `nil` or `false`, it will be ignored and the app state will not change; otherwise, its return value will be treated as an updated copy of the app state and swapped in as a replacement to the original.

```clojure
(defonce app-state
  (atom (forms->document ["Hello, world!"])))
```

`app-state` should be an atom containing a *document*: that is, an [xyzzy](https://github.com/mkremins/xyzzy) zipper over a Clojure parse tree. Flense provides a helper function (`flense.model/forms->document`) that takes a seq of Clojure forms and returns an appropriately structured zipper, which you can then use as a document.

```clojure
(om/root flense/editor app-state
  {:target (.getElementById js/document "flense")
   :opts {:edit-chan edit-chan
          :line-length 72}})
```

Take a look at the `:opts` that we're passing in here. `:edit-chan` is required and should refer to the `edit-chan` we described above. `:line-length` is optional and controls the suggested maximum number of characters per line: Flense will try to split up forms that serialize to this many characters or more across multiple lines. The default `:line-length` is 72.

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

This example setup uses `om/root` to mount the Flense editor component directly onto the page, but you're free to use `om/build` or `om/build-all` to construct your editor components too if you want. Just ensure that the `:opts` are valid and that the highest-level app state to which the component has access is an appropriately structured zipper and you should be good to go.

Want to see a more fully-featured example? Take a look at [flense-nw](https://github.com/mkremins/flense-nw), specifically the `flense-nw.app` namespace.
