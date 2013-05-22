[![Build Status](https://secure.travis-ci.org/rolandwalker/express.png?branch=master)](http://travis-ci.org/rolandwalker/express)

Overview
========

Alternatives to Emacs `message`.

Quickstart
----------

```lisp
(require 'express)
(express-install-aliases)
 
(express "important message")
 
(with-message-logonly
  (do-something-noisy))
```

Explanation
-----------

Express.el provides alternatives to Emacs' built-in `message` function.

This library is generally only useful when programming in Emacs Lisp.
However, some end-users may find it useful to control messaging,
especially for the case of quietening chatty libraries in their
~/.emacs files (see "message alternatives" section below).

The principal `express` function by default works differently from
`message` in almost every respect.

The arguments to the familiar `message` function are a format string
followed by any number of arguments which may be substituted into the
format string.  This flexible syntax obviates any arguments to control
the *behavior* of `message`.

`Express`, by contrast, takes as its first argument a preformatted
value to display.  Subsequent arguments control its behavior.

function `express`
------------------

The full argument spec for the `express` function is:

	CONTENT &optional QUIET SECONDS NOCOLOR LOG NOTIFY POPUP

The docstring is included here:

Transiently and noticeably display CONTENT in the echo area.

CONTENT should be a pre-`format`ted if it is a string.

CONTENT will be coerced to a string if it is not a string.

Optional QUIET suppresses the bell, which is on by default.

Optional SECONDS determines the number of seconds CONTENT will be
displayed before reverting to the previous content of the echo
area.  Default is `express-message-seconds`.  If SECONDS is 0, or
non-numeric, the message is not timed out, and remains visible
until the next write to the echo area.

Optional NOCOLOR suppresses coloring the message with face held
in the variable `express-face`.

Optional LOG enables logging of CONTENT for any non-nil value.
If LOG is `'log-only`, then CONTENT goes only to the \*Messages\*
buffer and all other options are ignored.

Optional NOTIFY enables sending the message via the notifications
system of the underlying OS.  The default is nil.  If NOTIFY is
`'replace-echo`, then the notification will be used instead of the
echo area.  For any other non-nil value, the notification will be
used in addition to the echo area.

Optional POPUP enables sending the message via `popup-tip` from
popup.el.  The default is nil.  If POPUP is `'replace-echo`, then
the popup will be used instead of the echo area.  For any other
non-nil value, the popup will be used in addition to the echo area.

The behavior of `express` is very different from `message`:

* String CONTENT must already be passed through `format`.

* Non-strings are accepted for CONTENT.

* The content is displayed with added color.

* The bell is rung.

* CONTENT is not written to the messages buffer (log).

* After display, the previous contents of the echo area are restored.

The following forms using `message` and `express` are equivalent:

```lisp
(message "hello, %s" name)
 
(express (format "hello, %s" name) 'quiet 0 'nocolor 'log)
```

function `express*`
-------------------

The variant function `express*` has identical functionality to `express`
but takes CL-style arguments:

```lisp
(express* "hello" :quiet 0)
```

`message` alternatives
----------------------

The following functions provided by this library are drop-in
alternatives to `message` which may be useful in an `flet`
construct:

	express-message-nolog
	express-message-logonly
	express-message-highlight
	express-message-insert
	express-message-notify
	express-message-popup
	express-message-temp
	express-message-string

macros
------

The following macros modify the behavior of `message` within
the enclosing expression:

	express-with-message-nolog
	express-with-message-logonly
	express-with-message-highlight
	express-with-message-insert
	express-with-message-notify
	express-with-message-popup
	express-with-message-temp
	express-with-message-string

For example, the following code would redirect messages from a very
chatty library to the log:

```lisp
(express-with-message-nolog
  (require 'very-chatty-library))
```

The same method may also be handy with `defadvice`:

```lisp
(defadvice very-chatty-function (around very-chatty-redirect activate)
  (express-with-message-nolog
    ad-do-it))
```

Similarly, important messages may be redirected to a more visible
form:

```lisp
(defadvice an-important-function (around an-important-function activate)
  (express-with-message-notify
    ad-do-it))
```

Notes
-----

Running `express-install-aliases` or setting the corresponding
variable in customize will install convenience aliases outside
the `express-` namespace.  This is disabled by default.

The function `express-message-noformat` is also available, but it
is not quite a drop-in replacement for `message`.

Some of the functions require the availability of [notify.el](http://emacswiki.org/emacs/notify.el), [todochiku.el](http://www.emacswiki.org/emacs/ToDoChiKu),
or [popup.el](http://github.com/auto-complete/popup-el).  In all cases, the function will
degrade to an ordinary message if the external library is not
present.

Bugs
----

`message` is a subr.  Macros such as `express-with-message-logonly`
will only affect calls to `message` from Lisp.

Compatibility and Requirements
------------------------------

	GNU Emacs version 24.3-devel     : yes, at the time of writing
	GNU Emacs version 24.1 & 24.2    : yes
	GNU Emacs version 23.3           : yes
	GNU Emacs version 22.3 and lower : no

Uses if present: [string-utils.el](http://github.com/rolandwalker/string-utils), [notify.el](http://emacswiki.org/emacs/notify.el), [todochiku.el](http://www.emacswiki.org/emacs/ToDoChiKu),
[popup.el](http://github.com/auto-complete/popup-el)
