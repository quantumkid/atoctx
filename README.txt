INSTALLATION:

Put atoctx.el somewhere that emacs can find it, or add it's path to the
set of load paths in .emacs:

(add-to-list 'load-path "path/to/atoctx")

Then just load it with

(load "atoctx.el")

or what is perhaps a bit more intelligent

(autoload 'convert-buffer-to-context "atoctx" 
	  "Converts the whole buffer to ConTeXt syntax" t)

although you won't have immediate access to the individial functions in
that case.
