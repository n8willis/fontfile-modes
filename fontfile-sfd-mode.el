;;
;; fontfile-sfd-mode
;; 
;; An Emacs major mode for editing FontForge .sfd files
;;
;; Author: Nathan Willis
;;         nwillis@glyphography.com
;;
;; License: GNU GPL v3 or later
;;
;;
;; Notes:
;;
;; - Defined to follow the description at 
;;   http://fontforge.github.io/en-US/documentation/developers/sfdformat/
;;   and at 
;;   http://fontforge.github.io/sfdformat.html
;;
;; - OpenType tag list from 
;;   https://www.microsoft.com/typography/otspec/ttoreg.htm
;;
;; - TrueType table list from

;;
;; Top-level setup
;;
(defvar fontfile-sfd-mode-hook nil)
(defvar fontfile-sfd-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for FontForge SFD major mode")

(add-to-list 'auto-mode-alist '("\\.sfd\\'" . fontfile-sfd-mode))

;;
;; Syntax highlighting
;;

;; Define font-lock faces
;;
(defvar fontfile-sfd-font-lock-defaults
`((

;; Numerals and user-defined variables
;;( "\\b[0-9]+\\b" . font-lock-builtin-face)

;; The regular expressions are generated with regexp-opt. A log
;; of the Lisp used to define the list of SFD keywords and to
;; generate the regexp can be found in the file
;; sfd-keywords-regexp-opt.log

;; Top-level keywords, to be highlighted in font-lock-keyword
( "\\<\\(A\\(?:n\\(?:chorClass2\\|tiAlias\\)\\|scent\\)\\|B\\(?:ase\\(?:Horiz\\|Script\\)\\|egin\\(?:Chars\\|MMFonts\\|Private\\|SubFonts\\)\\|itmapFont\\|lueValues\\)\\|C\\(?:IDVersion\\|hainSub2\\|o\\(?:mments\\|pyright\\)\\|reationTime\\)\\|D\\(?:EI\\|e\\(?:faultBaseFilename\\|scent\\)\\|isplay\\(?:Layer\\|Size\\)\\)\\|E\\(?:n\\(?:coding\\|d\\(?:ASM\\|BitmapFont\\|FPST\\|Justify\\|MMSubroutine\\|Private\\|SplineFont\\)\\)\\|xtremaBound\\)\\|F\\(?:SType\\|amilyName\\|itToEm\\|ont\\(?:Log\\|Name\\)\\|ullName\\)\\|G\\(?:aspTable\\|rid\\)\\|Hhead\\(?:\\(?:A\\(?:Offse\\|scen\\)\\|D\\(?:Offse\\|escen\\)\\)t\\)\\|ItalicAngle\\|Justify\\|KernClass2\\|L\\(?:a\\(?:ngName\\|yer\\(?:Count\\)?\\)\\|\\(?:ineGa\\|ooku\\)p\\)\\|M\\(?:ATH\\|M\\(?:Axis\\(?:Map\\)?\\|C\\(?:DV\\|ounts\\)\\|N\\(?:DV\\|amedInstance\\)\\|\\(?:Position\\|Weight\\)s\\)\\|a\\(?:c\\(?:Context2\\|Feat\\|In\\(?:\\(?:dic\\|sert\\)2\\)\\|Kern2\\|Name\\|S\\(?:etting\\|tyle\\)\\)\\|rkAttach\\(?:\\(?:Classe\\|Set\\)s\\)\\)\\|odificationTime\\)\\|O\\(?:S2\\(?:CodePages\\|FamilyClass\\|LineGap\\|S\\(?:trikeY\\(?:Pos\\|Size\\)\\|u\\(?:b\\(?:X\\(?:Off\\|Size\\)\\|Y\\(?:Off\\|Size\\)\\)\\|p\\(?:X\\(?:Off\\|Size\\)\\|Y\\(?:Off\\|Size\\)\\)\\)\\)\\|Typo\\(?:\\(?:A\\(?:Offse\\|scen\\)\\|D\\(?:Offse\\|escen\\)\\)t\\)\\|UnicodeRanges\\|Ve\\(?:ndor\\|rsion\\)\\|Win\\(?:\\(?:A\\(?:Offse\\|scen\\)\\|D\\(?:Offse\\|escen\\)\\)t\\)\\|_\\(?:UseTypoMetrics\\|WeightWidthSlopeOnly\\)\\)\\|nlyBitmaps\\|rder2\\|tfFeatName\\)\\|P\\(?:FMFamily\\|anose\\|ickledData\\)\\|S\\(?:hortTable\\|plineFontDB\\)\\|T\\(?:TFW\\(?:eight\\|idth\\)\\|eXData\\|t\\(?:f?Table\\)\\)\\|U\\(?:Comments\\|FO\\(?:\\(?:A\\|De\\)scent\\)\\|nderline\\(?:Position\\|Width\\)\\|se\\(?:\\(?:Unique\\|XU\\)ID\\)\\)\\|V\\(?:LineGap\\|ersion\\)\\|W\\(?:eight\\|i\\(?:dthSeparation\\|nInfo\\)\\)\\|XUID\\|sfntRevision\\|woffM\\(?:ajor\\|etadata\\|inor\\)\\)\\>" . font-lock-keyword-face)


;; Second-level keywords
( "\\<\\(B\\(?:C\\(?:l\\(?:ass\\|sList\\)\\|overage\\)\\|DF\\(?:Char\\|EndProperties\\|RefChar\\|StartProperties\\)\\|String\\)\\|C\\(?:IDVersion\\|l\\(?:ass\\|sList\\)\\|overage\\)\\|EndChar\\|F\\(?:ClsList\\|String\\)\\|Jstf\\(?:DisableExtend\\|E\\(?:nableShrink\\|xtender\\)\\|Lang\\|Max\\(?:Extend\\|Shrink\\)\\|Prio\\)\\|Ordering\\|Re\\(?:gistry\\|place\\|solution\\)\\|S\\(?:criptPercentScaleDown\\|eqLookup\\|t\\(?:artChar\\|ring\\)\\|upplement\\)\\)\\>" . font-lock-type-face)

;; Third-level keywords
( "\\<\\(A\\(?:lt\\(?:\\(?:Uni\\|ernateSubs\\)2\\)\\|nchorPoint\\)\\|Back\\|C\\(?:OMMENT\\|o\\(?:lour\\|mment\\|unterMasks\\)\\)\\|DStem2\\|En\\(?:coding\\|d\\(?:Image\\|SplineSet\\|Ttf\\)\\)\\|F\\(?:ONT_DESCENT\\|ill\\(?:Gradient\\|Pattern\\)\\|lags\\|ore\\)\\|Glyph\\(?:C\\(?:lass\\|onstructionVertical\\)\\|VariantsVertical\\)\\|HStem\\|I\\(?:mage\\|nSpiro\\|sExtendedShape\\|talicCorrection\\)\\|Kerns2\\|L\\(?:Carets2\\|ayerCount\\|igature2\\)\\|MultipleSubs2\\|P\\(?:\\(?:airPos\\|osition\\)2\\)\\|Refer\\|S\\(?:plineSet\\|ubstitution2\\)\\|T\\(?:EX\\|ile\\(?:Bounds\\|Margin\\)\\|op\\(?:AccentHorizontal\\|RightVertex\\)\\|t\\(?:f?Instrs\\)\\)\\|UnlinkRmOvrlpSave\\|V\\(?:Stem\\|Width\\|alidated\\)\\|Width\\)\\>" . font-lock-builtin-face)


;; Fourth-level keywords
( "\\<\\(EndSpiro\\|PathFlags\\|Spiro\\)\\>" . font-lock-builtin-face)



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
(defvar fontfile-sfd-syntax-table nil "Syntax table for `fontfile-sfd-mode'.")
(setq fontfile-sfd-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; bash style comment: “# …” 
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))
;;
;; End of syntax table


;;
;; Indentation
;;

;; Currently broken; April 16 2015 - NPW
(defun fontfile-sfd-indent-line ()
  "Indent current line as SFD code."
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
;; Entry point
;;
(define-derived-mode fontfile-sfd-mode fundamental-mode "FontForge SFD"
  "Major mode for editing FontForge SFD files."
  :syntax-table fontfile-sfd-syntax-table

;(setq comment-start "#")
;(setq comment-end "")
(setq font-lock-defaults fontfile-sfd-font-lock-defaults)
(setq indent-line-function 'fontfile-sfd-indent-line)
) 

;;
;; Boom goes the dynamite
;;
(provide 'fontfile-sfd-mode)
