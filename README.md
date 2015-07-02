Emacs fontfile modes
--------------------

This is a collection of Major Modes for Emacs that are designed
to facilitate reading, ediing, and debugging files in various
font-development formats.

The formats supported are NOT binary font files themselves (such
as OpenType (.otf), TrueType (.ttf) or PostScript Type1 (.pfb). 
To edit these formats, you need a font editor, such as the
free-software application FontForge. [1]

The formats supported DO include some font source formats, but
none of these modes is intended to turn Emacs into a visual font
editor.  For that, you again need a font-editing application like
FontForge.

With all that negativity out of the way, here's what these modes
actually DO allow you to edit:

* OpenType feature-definition files (.fea)
* Unified Font Object (UFO) glyph files (.glif)
* FontForge spline-font database files (.sfd)
* FontForge native (i.e., non-Python) scripts (.pe or .ff)
* FontTools / TTX files (.ttx)


INSTALLATION:
-------------

These Emacs modes are defined in Emacs Lisp (.el) files. By
default, they expect to find themselves in your .emacs.d 
directory, so if you don't know what else to try, copy them
(including the containing folder "fontfile-modes") into the
directory called .emacs.d that lives inside your home folder.

Next, you can TEST each mode by opening the appropriate file in
Emacs, typing 
M-x

then typing 
eval-buffer

and opening a file of the coresponding format.

If you want Emacs to automatically load the relevant mode whenever
you open a supported file format, just add a hook to your .emacs
file.  For example, to automatically support .fea files, add
the lines:

(add-to-list 'load-path "~/.emacs.d/fontfile-modes")
(require 'fontfile-fea-mode)

anywhere in your .emacs file.

Currently, each mode is separate; it is provided in its own ELisp
function and must be invoked separately.  This is a decision on my
part because I did not think it exceedingly likely that a user
would need to edit all five file types in one session on a regular
basis.  Maintaining the syntax-highlighting and ElDoc structures
for all five modes would thus consume system resources
unnecessarily.

Here are the files to load for each mode:

* OpenType feature files: fontfile-fea-mode.el
* UFO glyph files: fontfile-ufo-mode.el
* FontForge SFD files: fontfile-sfd-mode.el
* FontForge scripts: fontfile-ffs-mode.el
* FontTools TTX files: fontfile-ttx-mode.el


USAGE:
------

All five modes support the following features:

* Syntax highlighting
* Auto-indentation
* Highlighting of well-know font table and tag types
* Optional help messages to define keywords and terms

*** NOTE ***
At the moment (May 1, 2015), I am in the middle of rewriting the
indentation code using Emacs SMIE, so autoindentation may be
broken. I hope to have this fixed again within a few days.
*** ∃TOИ ***

Syntax highlighting and indentation should work automatically. 
The built-in help requires you to turn on a Minor Mode called 
"Eldoc."  To do so, type
M-x

then type
turn-on-eldoc-mode

Subsequently, whenever you pause the mouse cursor over a 
keyword, function, or well-know table/tag, Emacs will show you
a brief description string in the message line at the bottom
of the window.

TTX and UFO's .glif are both XML-based formats, which enables
two additional features:

* Automatic validation (ensuring your edits remain well-formed XML)
* Element folding (letting you collapse or expand any element)

For TTX in particular, element folding can make navigating the
file much faster, since TTX files can be rather large.

References
----------

[1] - http://fontforge.github.io/
