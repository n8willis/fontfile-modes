;;
;; fontfile-ufo-mode
;; 
;; An Emacs major mode for editing UFO .glif files
;;
;; Author: Nathan Willis
;;         nwillis@glyphography.com
;;
;; License: GNU GPL v3 or later
;;
;;
;; Notes:
;;
;; - Defined to follow the UFO spec from
;;   
;;

;;
;; Top-level setup
;;
(require 'nxml-mode)
(push "~/.emacs.d/emacs-fontmodes/schemas/ufo-schemas.xml" rng-schema-locating-files)
(push (cons (concat "\\." (regexp-opt '("glif") t)
                      "\\'") 'nxml-mode) auto-mode-alist)


;;
;; Entry point
;;
(define-derived-mode fontfile-ufo-mode nxml-mode "UFO glyphs"
  "Major mode for editing Unified Font Object glyph files."
  :syntax-table fontfile-ufo-syntax-table

;;  (setq font-lock-defaults fontfile-ufo-font-lock-defaults)
)
;;
;; Boom goes the dynamite
;;
(provide 'fontfile-ufo-mode)
