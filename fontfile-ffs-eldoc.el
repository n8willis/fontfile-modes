(require 'thingatpt)
(require 'eldoc)
(setq eldoc-documentation-function 'fontfile-ffs-eldoc-function)

(setq fontfile-ffs-eldoc-obarray (make-vector 646 0))

(set (intern "aalt" fontfile-ffs-eldoc-obarray) "Access All Alternates")

(defun fontfile-ffs-eldoc-function ()
    "Returns a doc string appropriate for the current context, or nil."
    (symbol-value (intern-soft (thing-at-point 'symbol)
                               fontfile-ffs-eldoc-obarray)))
