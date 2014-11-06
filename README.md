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
[mkremins/flense "0.0-SNAPSHOT"]
```

(You'll have to `lein install` this repo yourself first. I'm not regularly deploying snapshots to Clojars at this time.)

Now refer to [`setup.md`](https://github.com/mkremins/flense/blob/master/doc/setup.md) for a more detailed walkthrough of how to embed a Flense editor component in your app.

## License

[MIT License](http://opensource.org/licenses/MIT). Hack away.
