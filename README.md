Visual Format
==

This package provide a simple mode to visually format buffers without modifying
them.


### Motivation

Indeed, when working on big and dirty projects, we usually face the problem of
badly formatted source files (mixed tabs and spaces, messy indentation,
inconsistent code formatting, etc.)

The trivial (and naive) way of dealing with this kind of problems is to format
the files and commit them to the central repository. However, in real world
projects, it is not that simple!

I found myself working on some complex projects and with dirty formatting! So,
Emacs being Emacs, I know that I can do something to format the buffer visually
without actually modifying it. I've looked-up but didn't find a package that
does this.


### How it works?

The idea behind this package is quite simple, for a messy buffer, it creates a
temporary buffer with the same content and formats it with the function of your
choice (customize it via `visual-format-buffer-formatter-function`). Then,
transposes the spaces from the formatted buffer to the original buffers via
Emacs' _text properties_.

In order to know where to look, `visual-format` uses Tree Sitter (via Emacs 29+
`treesit`), it walks the AST for the original buffer and the formatted buffer
set the spaces in the original buffer according to these observed in the
formatted buffer.

#### Assumptions

In order to work, `visual-format` make the following assumption:

> [!CAUTION]
> The formatted buffer **should** have the same AST as the original buffer

In other words, if the used formatter adds some instructions the original code,
like replacing an `if-else` with a `switch-case` or something similar.
