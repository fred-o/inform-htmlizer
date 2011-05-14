An HTMLizer for Inform7 source
==============================

`inform-htmlize.el` is a utility written in Emacs lisp for
transforming [Inform7][i7] source code into a syntax-highlighted HTML
version.

Usage
=====

Invoke the interactive function `inform7-htmlize` to open a new buffer
with the contents of the active region HTMLized. If there is no active
region the entire contents of the current buffer is used instead.

If invoked with the universal argument (`C-u M-x inform7-htmlize`) a
standard HTML header and footer is added surrounding the HTMLized code
fragment.

The supplied style sheet [inform-htmlize.css][css] can be used as a
starting point for styling the generated HTML.

Limitations
===========

 * It doesn't handle line breaks inside string literals well, but
   since the Inform editor usually stores code without line breaks
   it's not really that much of a problem.
 * It doesn't handle comments at all.

[i7]:http://inform7.com
[css]:https://github.com/fred-o/inform-htmlizer/blob/master/inform-htmlize.css
