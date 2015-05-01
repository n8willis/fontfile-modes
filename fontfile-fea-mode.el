;;
;; fontfile-fea-mode
;; 
;; An Emacs major mode for editing OpenType .fea files
;;
;; Author: Nathan Willis
;;         nwillis@glyphography.com
;;
;; License: GNU GPL v3 or later
;;
;;
;; Notes:
;;
;; - Defined to follow the .fea file spec from
;;   http://www.adobe.com/devnet/opentype/afdko/topic_feature_file_syntax.html
;;
;; - OpenType tag list from 
;;   https://www.microsoft.com/typography/otspec/ttoreg.htm
;;

;;
;; Top-level setup
;;
(defvar fontfile-fea-mode-hook nil)
(defvar fontfile-fea-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for OpenType FEA major mode")

(add-to-list 'auto-mode-alist '("\\.fea\\'" . fontfile-fea-mode))

;;
;; Syntax highlighting
;;

;; Define font-lock faces
;;
(defvar fontfile-fea-font-lock-defaults
`((

;; Operators
;; @foo designates "foo" as a class name.
;; 'bar designates "bar" for contextual substitution/positioning.
;; \N designates "N" as a CID.
( "[@']\\w+\\b" . font-lock-preprocessor-face)
( "[\\\\]\\w+\\b" . font-lock-function-name-face)

;; Numerals and user-defined variables
( "\\b[0-9]+\\b" . font-lock-builtin-face)
( "\\[\\w+\\]" . font-lock-variable-name-face)

;; Top-level keywords, to be highlighted in font-lock-keyword
;;
;; Note that "contourpoint" is not listed as a keyword in the
;; syntax spec's keyword list, but it clearly is a keyword in
;; the examples.
( "\\<\\(Ignore\\(?:\\(?:BaseGlyph\\|Ligature\\|Mark\\)s\\)\\|MarkAttachmentType\\|NULL\\|RightToLeft\\|UseMarkFilteringSet\\|an\\(?:chor\\(?:Def\\)?\\|on\\(?:ymous\\)?\\)\\|by\\|c\\(?:ontour\\(?:point\\)?\\|ursive\\)\\|device\\|e\\(?:num\\(?:erate\\)?\\|xclude\\(?:DFLT\\|_dflt\\)\\)\\|f\\(?:eature\\|rom\\)\\|i\\(?:gnore\\|nclude\\(?:DFLT\\|_dflt\\)?\\)\\|l\\(?:anguage\\(?:system\\)?\\|ookup\\(?:flag\\)?\\)\\|mark\\(?:Class\\)?\\|nameid\\|p\\(?:arameters\\|os\\(?:ition\\)?\\)\\|r\\(?:e\\(?:quired\\|versesub\\)\\|sub\\)\\|s\\(?:cript\\|ub\\(?:\\(?:stitut\\|tabl\\)e\\)?\\)\\|table\\|useExtension\\|valueRecordDef\\)\\>" . font-lock-keyword-face)

;; Operators and builtins. Seems we don't need this too much.
;( ,(regexp-opt fontfile-fea-operators 'words) . font-lock-function-name-face)
;( ,(regexp-opt fontfile-fea-builtins 'words) . font-lock-builtin-face)

;; Second-level keywords, to be highlighted as "types"
( "\\<\\(A\\(?:scender\\|ttach\\)\\|C\\(?:a\\(?:\\(?:pHeigh\\|retOffse\\)t\\)\\|odePageRange\\)\\|Descender\\|FontRevision\\|GlyphClassDef\\|HorizAxis\\.\\(?:Base\\(?:\\(?:Script\\|Tag\\)List\\)\\|MinMax\\)\\|Li\\(?:gatureCaretBy\\(?:Dev\\|Index\\|Pos\\)\\|neGap\\)\\|MarkAttachClass\\|Panose\\|Typo\\(?:Ascender\\|Descender\\|LineGap\\)\\|UnicodeRange\\|Ve\\(?:ndor\\|rt\\(?:A\\(?:dvanceY\\|xis\\.\\(?:Base\\(?:\\(?:Script\\|Tag\\)List\\)\\|MinMax\\)\\)\\|OriginY\\|Typo\\(?:Ascender\\|Descender\\|LineGap\\)\\)\\)\\|XHeight\\|sizemenuname\\|win\\(?:\\(?:A\\|De\\)scent\\)\\)\\>" . font-lock-type-face)

;; DFLT/dflt, which is a reserved word used in place
;; of a literal value in a few places, so will be treated like numerals
( "\\<\\(dflt\\)\\>" . font-lock-string-face)


;; Registered OpenType tags, to be highlighted as constants since
;; they are defined externally

;; The regular expressions are generated with regexp-opt. A log
;; of the Lisp used to define the list of OTF keywords and to
;; generate the regexp can be found in the file
;; otf-tag-regexp-opt.log

;; Top-level feature tags
( "\\<\\(a\\(?:alt\\|bv[fms]\\|frc\\|khn\\)\\|blw[fms]\\|c\\(?:2\\(?:[ps]c\\)\\|a\\(?:lt\\|se\\)\\|cmp\\|far\\|jct\\|lig\\|p\\(?:ct\\|sp\\)\\|swh\\|urs\\|v\\(?:0[1-9]\\|1[0-9]\\|2[0-9]\\|3[0-9]\\|4[0-9]\\|5[0-9]\\|6[0-9]\\|7[0-9]\\|8[0-9]\\|9[0-9]\\)\\)\\|d\\(?:ist\\|lig\\|nom\\)\\|expt\\|f\\(?:alt\\|in[23a]\\|rac\\|wid\\)\\|h\\(?:al[fnt]\\|ist\\|kna\\|lig\\|ngl\\|ojo\\|wid\\)\\|i\\(?:nit\\|\\(?:so\\|ta\\)l\\)\\|j\\(?:alt\\|p\\(?:04\\|78\\|83\\|90\\)\\)\\|kern\\|l\\(?:fbd\\|iga\\|jmo\\|num\\|ocl\\|tr[am]\\)\\|m\\(?:ark\\|ed[2i]\\|grk\\|kmk\\|set\\)\\|n\\(?:alt\\|lck\\|u\\(?:kt\\|mr\\)\\)\\|o\\(?:num\\|pbd\\|r\\(?:dn\\|nm\\)\\)\\|p\\(?:alt\\|cap\\|kna\\|num\\|re[fs]\\|st[fs]\\|wid\\)\\|qwid\\|r\\(?:and\\|krf\\|lig\\|phf\\|t\\(?:bd\\|l[am]\\)\\|uby\\)\\|s\\(?:alt\\|i\\(?:nf\\|ze\\)\\|m\\(?:cp\\|pl\\)\\|s\\(?:0[1-9]\\|1[0-9]\\|20\\)\\|u\\(?:[bp]s\\)\\|wsh\\)\\|t\\(?:itl\\|jmo\\|n\\(?:[au]m\\)\\|\\(?:ra\\|wi\\)d\\)\\|unic\\|v\\(?:a\\(?:lt\\|tu\\)\\|ert\\|hal\\|jmo\\|k\\(?:na\\|rn\\)\\|pal\\|rt2\\)\\|zero\\)\\>" . font-lock-constant-face)

;; Base feature tags
( "\\<\\(hang\\|i\\(?:cf[bt]\\|d\\(?:eo\\|tp\\)\\)\\|math\\|romn\\)\\>"  . font-lock-constant-face)

;; Script tags
( "\\<\\(a\\(?:r\\(?:ab\\|m[in]\\)\\|vst\\)\\|b\\(?:a\\(?:li\\|mu\\|tk\\)\\|eng\\|ng2\\|opo\\|ra[hi]\\|u\\(?:gi\\|hd\\)\\|yzm\\)\\|c\\(?:a\\(?:km\\|ns\\|ri\\)\\|h\\(?:am\\|er\\)\\|opt\\|prt\\|yrl\\)\\|d\\(?:ev[2a]\\|srt\\)\\|e\\(?:gyp\\|thi\\)\\|g\\(?:eor\\|jr2\\|lag\\|oth\\|rek\\|u\\(?:jr\\|r[2u]\\)\\)\\|h\\(?:an[gio]\\|ebr\\)\\|ital\\|ja\\(?:mo\\|va\\)\\|k\\(?:a\\(?:li\\|na\\)\\|h\\(?:[am]r\\)\\|nd[2a]\\|thi\\)\\|l\\(?:a\\(?:na\\|o\\|tn\\)\\|epc\\|i\\(?:mb\\|nb\\|su\\)\\|y\\(?:[cd]i\\)\\)\\|m\\(?:a\\(?:nd\\|th\\)\\|er[co]\\|l\\(?:m2\\|ym\\)\\|ong\\|tei\\|usc\\|ymr\\)\\|nko\\|o\\(?:gam\\|lck\\|r\\(?:kh\\|y[2a]\\)\\|sma\\)\\|p\\(?:h\\(?:ag\\|li\\|nx\\)\\|rti\\)\\|r\\(?:jng\\|unr\\)\\|s\\(?:a\\(?:mr\\|rb\\|ur\\)\\|h\\(?:aw\\|rd\\)\\|inh\\|ora\\|und\\|y\\(?:lo\\|rc\\)\\)\\|t\\(?:a\\(?:gb\\|kr\\|l[eu]\\|ml\\|vt\\)\\|el[2u]\\|fng\\|glg\\|ha[ai]\\|ibt\\|ml2\\)\\|ugar\\|vai\\|x\\(?:peo\\|sux\\)\\|yi\\)\\>" . font-lock-constant-face)

;; Lang tags
( "\\<\\(A\\(?:B[AK]\\|DY\\|F[KR]\\|GW\\|L[ST]\\|MH\\|PPH\\|R[AIK]\\|SM\\|TH\\|VR\\|WA\\|YM\\|ZE\\)\\|B\\(?:A[DGLU]\\|BR\\|C[HR]\\|E[LMN]\\|GR\\|H[IO]\\|I[KL]\\|KF\\|L[INT]\\|M[BL]\\|OS\\|R[EHIM]\\|SH\\|TI\\)\\|C\\(?:AT\\|EB\\|H[EGHIKPRU]\\|MR\\|O[PS]\\|R[ERT]\\|S[LY]\\)\\|D\\(?:A[NR]\\|CR\\|EU\\|GR\\|HV\\|IV\\|JR\\|N[GK]\\|RI\\|[UZ]N\\)\\|E\\(?:BI\\|CR\\|DO\\|FI\\|LL\\|NG\\|RZ\\|SP\\|TI\\|UQ\\|V[KN]\\|WE\\)\\|F\\(?:A[NR]\\|IN\\|JI\\|LE\\|NE\\|O[NS]\\|R[AIL]\\|TA\\|UL\\)\\|G\\(?:A[DEGLRW]\\|EZ\\|IL\\|MZ\\|ON\\|R[NO]\\|U[AJ]\\)\\|H\\(?:A[ILRUW]\\|BN\\|I[LN]\\|MA\\|ND\\|O\\|R[IV]\\|UN\\|YE\\)\\|I\\(?:BO\\|JO\\|LO\\|N[DGU]\\|PPH\\|R[IT]\\|S[LM]\\|TA\\|WR\\)\\|J\\(?:A[NV]\\|II\\|U[DL]\\)\\|K\\(?:A[BCLNRTZ]\\|EB\\|GE\\|H[AKMSVW]\\|I[KRS]\\|KN\\|LM\\|M[BNOS]\\|NR\\|O[DHKNPRZ]\\|PL\\|R[IK-NT]\\|S[HIM]\\|U[ILMRUY]\\|YK\\)\\|L\\(?:A[DHKMOTZ]\\|CR\\|DK\\|EZ\\|IN\\|M[ABW]\\|S[BM]\\|T[HZ]\\|U[BGHO]\\|VI\\)\\|M\\(?:A[JKLNPRW]\\|BN\\|C[HR]\\|DE\\|EN\\|IZ\\|KD\\|L[EGNRY]\\|N[DGIKX]\\|O[HKLNR]\\|RI\\|T[HS]\\|UN\\)\\|N\\(?:A[GNS]\\|CR\\|D[BG]\\|E[PW]\\|GR\\|HC\\|I[SU]\\|K[LO]\\|LD\\|O[GR]\\|SM\\|T[AO]\\|YN\\)\\|O\\(?:C[IR]\\|JB\\|R[IO]\\|SS\\)\\|P\\(?:A[ALNPS]\\|GR\\|IL\\|L[GK]\\|RO\\|TG\\)\\|QIN\\|R\\(?:AJ\\|BU\\|CR\\|IA\\|MS\\|O[MY]\\|SY\\|U[AS]\\)\\|S\\(?:A[DNTY]\\|E[KL]\\|GO\\|HN\\|I[BDG]\\|K[SY]\\|L[AV]\\|M[LO]\\|N[ADHK]\\|O[GT]\\|QI\\|R[BKR]\\|S[LM]\\|UR\\|V[AE]\\|W[AKZ]\\|XT\\|YR\\)\\|T\\(?:A[BJMT]\\|CR\\|EL\\|G[NRY]\\|H[AT]\\|IB\\|KM\\|MN\\|N[AEG]\\|OD\\|RK\\|SG\\|U[ALV]\\|WI\\)\\|U\\(?:DM\\|KR\\|RD\\|SB\\|YG\\|ZB\\)\\|V\\(?:EN\\|IT\\)\\|W\\(?:AG?\\|CR\\|EL\\|LF\\)\\|X\\(?:BD\\|HS\\)\\|Y\\(?:AK\\|BA\\|CR\\|I[CM]\\)\\|Z\\(?:H[HPST]\\|ND\\|UL\\)\\)\\>" . font-lock-constant-face)

)))
;;
;; End of font-lock definitions


;; Syntax table for comments
;;
(defvar fontfile-fea-syntax-table nil "Syntax table for `fontfile-fea-mode'.")
(setq fontfile-fea-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; bash style comment: “# …” 
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

	;; underscore _ is a valid word character
	(modify-syntax-entry ?_ "w" synTable)
	(modify-syntax-entry ?. "w" synTable)

        synTable))
;;
;; End of syntax table


;;
;; Indentation
;;

(defun fontfile-fea-indent-line ()
  "Indent current line as FEA code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; First line is always non-indented
    (let ((not-indented t) cur-indent (is-brackets (looking-at "^[ \t]*\\[\\]")))
      (if (looking-at "^[ \t]*\\(END\\|\\]\\)") ; If the line we are looking at is the end of a block, then decrease the indentation
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) default-tab-width)))
            (if (< cur-indent 0) ; We can't indent past the left margin
              (setq cur-indent 0)))
        (save-excursion
          (while not-indented ; Iterate backwards until we find an indentation hint
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(END\\|\\]\\)") ; This hint indicates that we need to indent at the level of the end token
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*\\(FEATURE\\|TABLE\\|SUBTABLE\\|INCLUDE\\|LANGUAGESYSTEM\\)") ; This hint indicates that we need to indent an extra level
                  (progn
                    (setq cur-indent (+ (current-indentation)
					(if is-brackets
					    (/ default-tab-width 2)
					  default-tab-width))) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation 


;;
;; Eldoc documentation hints
;;

;; The documentation-string array was pulled out into a separate
;; file for brevity here and to make updating the docs easier.
(load-file "~/.emacs.d/fontfile-modes/fontfile-fea-eldoc.el")

;;
;; Entry point
;;
(define-derived-mode fontfile-fea-mode fundamental-mode "OpenType Features"
  "Major mode for editing Adobe OpenType feature files."
  :syntax-table fontfile-fea-syntax-table

  (setq font-lock-defaults fontfile-fea-font-lock-defaults)
  (setq indent-line-function 'fontfile-fea-indent-line)
) 

;;
;; Boom goes the dynamite
;;
(provide 'fontfile-fea-mode)
