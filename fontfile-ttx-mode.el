;;
;; fontfile-ttx-mode
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
;; - Syntax is currently based on a Trang-generated RelaxNG schema
;;   then augmented with a set of well-known OpenType and TrueType
;;   tables.
;;   
;;   References for these tables from 
;;   http://www.microsoft.com/typography/otspec/otff.htm#otttables
;;   http://fontforge.github.io/TrueOpenTables.html
;;   http://www.microsoft.com/en-us/Typography/SpecificationsOverview.aspx
;;   https://developer.apple.com/fonts/TrueType-Reference-Manual/
;;   http://scripts.sil.org/cms/scripts/page.php?site_id=projects&amp;item_id=graide
;;
;;   If there are other tables you would like to see recognized,
;;   please get in touch.  I would be happy to add more useful
;;   syntax information that the community needs.
;;

;;
;; Top-level setup
;;
(require 'nxml-mode)
(push "~/.emacs.d/emacs-fontmodes/schemas/ttx-schemas.xml" rng-schema-locating-files)
(push (cons (concat "\\." (regexp-opt '("ttx") t)
                      "\\'") 'nxml-mode) auto-mode-alist)


;;
;; Folding elements
;;
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;;
;; Eldoc documentation hints
;;

;; The documentation-string array was pulled out into a separate
;; file for brevity here and to make updating the docs easier.
(load-file "~/.emacs.d/fontfile-modes/fontfile-tables-eldoc.el")

;;
;; Entry point
;;
(define-derived-mode fontfile-ttx-mode nxml-mode "FontTools TTX"
  "Major mode for editing FontTools TTX files."
  :syntax-table fontfile-ttx-syntax-table

;;  (setq font-lock-defaults fontfile-ttx-font-lock-defaults)
)
;;
;; Boom goes the dynamite
;;
(provide 'fontfile-ttx-mode)
