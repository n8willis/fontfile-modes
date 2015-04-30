;;
;; fontfile-ffs-mode
;; 
;; An Emacs major mode for editing FontForge native scripts
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
;;   http://fontforge.github.io/scripting.html
;;   and taking cues from Bernard Massot's excellent Vim work at
;;   http://www.vim.org/scripts/script.php?script_id=3801
;;

;;
;; Top-level setup
;;
(defvar fontfile-ffs-mode-hook nil)
(defvar fontfile-ffs-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for FontForge script major mode")

(add-to-list 'auto-mode-alist '("\\.pe\\'" . fontfile-ffs-mode))
(add-to-list 'auto-mode-alist '("\\.ff\\'" . fontfile-ffs-mode))

;;
;; Syntax highlighting
;;

;; Define font-lock faces
;;
(defvar fontfile-ffs-font-lock-defaults
`((

;; Numerals and user-defined variables
;;( "\\b[0-9]+\\b" . font-lock-builtin-face)

;; The regular expressions are generated with regexp-opt. A log
;; of the Lisp used to define the list of scripting keywords and to
;; generate the regexp can be found in the file
;; ffs-keywords-regexp-opt.log

;; Keywords, to be highlighted in font-lock-keyword
( "\\<\\(break\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:if\\|loop\\)\\)\\|foreach\\|if\\|return\\|shift\\|while\\)\\>" . font-lock-keyword-face)


;; Builtins
( "\\<\\(\\$\\(?:a\\(?:rg[cv]\\|scent\\)\\|bitmaps\\|c\\(?:id\\(?:copyright\\|f\\(?:\\(?:amilyna\\|ontan\\|ullna\\)me\\)\\|weight\\)\\|opyright\\|ur\\(?:cid\\|font\\)\\)\\|descent\\|em\\|f\\(?:amilyname\\|i\\(?:lename\\|rst\\(?:cid\\|font\\)\\)\\|on\\(?:dname\\|t\\(?:changed\\|name\\|version\\)\\)\\|ullname\\)\\|haspython\\|i\\(?:scid\\|talicangle\\)\\|loadState\\|m\\(?:acstyle\\|mcount\\)\\|next\\(?:cid\\|font\\)\\|order\\|p\\(?:\\(?:anos\\|rivateStat\\)e\\)\\|selection\\|trace\\|version\\|weight\\|[012]\\)\\)\\>" . font-lock-builtin-face)

;; Functions
( "\\<\\(A\\(?:Tan2\\|dd\\(?:A\\(?:TT\\|ccent\\|nchor\\(?:Class\\|Point\\)\\)\\|DHint\\|Extrema\\|HHint\\|Instrs\\|Lookup\\(?:Subtable\\)?\\|PosSub\\|SizeFeature\\|VHint\\)\\|pplySubstitution\\|rray\\|skUser\\|uto\\(?:Counter\\|Hint\\|Instr\\|Kern\\|Trace\\|Width\\)\\)\\|B\\(?:itmaps\\(?:Avail\\|Regen\\)\\|uild\\(?:Accented\\|\\(?:Composi\\|Duplica\\)te\\)\\)\\|C\\(?:ID\\(?:ChangeSubFont\\|Flatten\\(?:ByCMap\\)?\\|SetFontNames\\)\\|anonical\\(?:Contours\\|Start\\)\\|e\\(?:il\\|nterInWidth\\)\\|h\\(?:a\\(?:ngePrivateEntry\\|r\\(?:Cnt\\|Info\\)\\)\\|eckForAnchorClass\\|r\\)\\|l\\(?:ear\\(?:Background\\|CharCounterMasks\\|GlyphCounterMasks\\|Hints\\|Instrs\\|PrivateEntry\\|Table\\)?\\|ose\\)\\|o\\(?:mpare\\(?:\\(?:Font\\|Glyph\\)s\\)\\|n\\(?:trolAfmLigatureOutput\\|vert\\(?:ByCMap\\|ToCID\\)\\)\\|py\\(?:Anchors\\|FgToBg\\|GlyphFeatures\\|LBearing\\|R\\(?:Bearing\\|eference\\)\\|Unlinked\\|V?Width\\)?\\|rrectDirection\\|s\\)\\|ut\\)\\|D\\(?:e\\(?:fault\\(?:ATT\\|OtherSubrs\\|RoundToGrid\\|UseMyMetrics\\)\\|tach\\(?:\\(?:AndRemove\\)?Glyphs\\)\\)\\|ontAutoHint\\|rawsSomething\\)\\|E\\(?:rror\\|xp\\(?:andStroke\\|ort\\)?\\)\\|F\\(?:i\\(?:leAccess\\|nd\\(?:Intersections\\|OrAddCvtIndex\\)\\)\\|loor\\|ont\\(?:\\(?:Imag\\|sInFil\\)e\\)\\)\\|G\\(?:e\\(?:nerate\\(?:F\\(?:amily\\|eatureFile\\)\\)?\\|t\\(?:AnchorPoints\\|CvtAt\\|Env\\|FontBoundingBox\\|Lookup\\(?:Info\\|OfSubtable\\|\\(?:Subtable\\)?s\\)\\|MaxpValue\\|OS2Value\\|P\\(?:osSub\\|r\\(?:ef\\|ivateEntry\\)\\)\\|SubtableOfAnchor\\|T\\(?:TFName\\|eXParam\\)\\)\\)\\|lyphInfo\\)\\|H\\(?:Flip\\|asPr\\(?:eservedTable\\|ivateEntry\\)\\)\\|I\\(?:mport\\|n\\(?:Font\\|line\\|t\\(?:erpolateFonts\\)?\\)\\|s\\(?:Al\\(?:Num\\|pha\\)\\|Digit\\|Finite\\|HexDigit\\|Lower\\|Nan\\|Space\\|Upper\\)\\)\\|Join\\|Lo\\(?:ad\\(?:EncodingFile\\|Namelist\\(?:Dir\\)?\\|P\\(?:lugin\\(?:Dir\\)?\\|refs\\)\\|\\(?:String\\|Table\\)FromFile\\)\\|g\\|okupS\\(?:etFeatureList\\|toreLigatureInAfm\\)\\)\\|M\\(?:M\\(?:Axis\\(?:\\(?:Bound\\|Name\\)s\\)\\|BlendToNewFont\\|Change\\(?:Instance\\|Weight\\)\\|InstanceNames\\|WeightedName\\)\\|erge\\(?:F\\(?:eature\\|onts\\)\\|Kern\\|Lookup\\(?:\\(?:Subtable\\)?s\\)\\)\\|ove\\(?:Reference\\)?\\|ultipleEncodingsToReferences\\)\\|N\\(?:ameFromUnicode\\|e\\(?:arly\\(?:\\(?:Hv\\(?:Cp\\|Line\\)\\|Line\\)s\\)\\|w\\)\\|onLinearTransform\\)\\|O\\(?:pen\\|rd\\|utline\\|verlapIntersect\\)\\|P\\(?:aste\\(?:Into\\|WithOffset\\)?\\|o\\(?:s\\(?:\\(?:itionReferen\\|tNoti\\)ce\\)\\|w\\)\\|r\\(?:eloadCidmap\\|int\\(?:Font\\|Setup\\)?\\)\\)\\|Quit\\|R\\(?:and\\|e\\(?:a\\(?:dOtherSubrsFile\\|l\\)\\|encode\\|move\\(?:A\\(?:TT\\|\\(?:ll\\(?:V?Kern\\)\\|nchorClas\\)s\\)\\|DetachedGlyphs\\|Lookup\\(?:Subtable\\)?\\|Overlap\\|P\\(?:osSub\\|reservedTable\\)\\)\\|nameGlyphs\\|place\\(?:C\\(?:harCounterMasks\\|vtAt\\)\\|GlyphCounterMasks\\|WithReference\\)\\|vert\\(?:ToBackup\\)?\\)\\|o\\(?:tate\\|und\\(?:To\\(?:Cluster\\|Int\\)\\)?\\)\\)\\|S\\(?:a\\(?:meGlyphAs\\|ve\\(?:Prefs\\|TableToFile\\)?\\)\\|cale\\(?:ToEm\\)?\\|e\\(?:lect\\(?:All\\(?:InstancesOf\\)?\\|B\\(?:itmap\\|y\\(?:ATT\\|PosSub\\)\\)\\|Changed\\|Fewer\\(?:Singletons\\)?\\|HintingNeeded\\|I\\(?:f\\|nvert\\)\\|More\\(?:If\\|Singletons\\(?:If\\)?\\)?\\|None\\|Singletons\\(?:If\\)?\\|WorthOutputting\\)?\\|t\\(?:Char\\(?:C\\(?:nt\\|o\\(?:lor\\|mment\\|unterMask\\)\\)\\|Name\\)\\|Fon\\(?:dName\\|t\\(?:HasVerticalMetrics\\|Names\\|Order\\)\\)\\|G\\(?:asp\\|lyph\\(?:C\\(?:hanged\\|lass\\|o\\(?:lor\\|mment\\|unterMask\\)\\)\\|Name\\|TeX\\)\\)\\|ItalicAngle\\|Kern\\|LBearing\\|Ma\\(?:\\(?:cStyl\\|xpValu\\)e\\)\\|OS2Value\\|P\\(?:anose\\|ref\\)\\|RBearing\\|T\\(?:TFName\\|eXParams\\)\\|Uni\\(?:codeValue\\|queID\\)\\|V\\(?:Kern\\|Width\\)\\|Width\\)\\)\\|hadow\\|i\\(?:mplify\\|n\\|zeOf\\)\\|kew\\|qrt\\|tr\\(?:Join\\|Split\\|case\\(?:cmp\\|str\\)\\|ftime\\|len\\|rstr\\|s\\(?:kipint\\|tr\\|ub\\)\\|to[dl]\\)\\|ubstitutionPoints\\)\\|T\\(?:an\\|o\\(?:Lower\\|Mirror\\|String\\|Upper\\)\\|ransform\\|ypeOf\\)\\|U\\(?:CodePoint\\|cs4\\|n\\(?:\\(?:icodeFromNam\\|linkReferenc\\)e\\)\\|tf8\\)\\|V\\(?:Flip\\|KernFromHKern\\)\\|W\\(?:ireframe\\|orthOutputting\\|riteStringToFile\\)\\)\\>" . font-lock-function-face)



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
(defvar fontfile-ffs-syntax-table nil "Syntax table for `fontfile-ffs-mode'.")
(setq fontfile-ffs-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; bash style comment: “# …” 
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

	;; C++ style comment “// …” 
	(modify-syntax-entry ?\/ ". 12b" synTable)
	(modify-syntax-entry ?\n "> b" synTable)

	;; define comment for this style: “/* … */” 
	(modify-syntax-entry ?\/ ". 14" synTable)
	(modify-syntax-entry ?* ". 23" synTable)

        synTable))
;;
;; End of syntax table


;;
;; Indentation
;;

;; Currently broken; April 16 2015 - NPW
(defun fontfile-ffs-indent-line ()
  "Indent current line as FontForge script code."
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
(define-derived-mode fontfile-ffs-mode fundamental-mode "FontForge script"
  "Major mode for editing FontForge scripts."
  :syntax-table fontfile-ffs-syntax-table

(setq font-lock-defaults fontfile-ffs-font-lock-defaults)
(setq indent-line-function 'fontfile-ffs-indent-line)
) 

;;
;; Boom goes the dynamite
;;
(provide 'fontfile-ffs-mode)
