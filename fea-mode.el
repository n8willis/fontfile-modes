
;;
;; File spec from http://www.adobe.com/devnet/opentype/afdko/topic_feature_file_syntax.html
;; Tags from https://www.microsoft.com/typography/otspec/ttoreg.htm
;; Syntax highlighting

;; Top-level keywords, to be highlighted in font-lock-keyword
;;
;; Note that "contourpoint" is not listed as a keyword in the syntax spec's
;; keyword list, but it clearly is a keyword in the examples.
(defvar fea-keywords 
  '("anchor" "anchorDef" "anonymous" "anon" "by" "contour" "contourpoint" "cursive" "device" "enumerate" "enum" "excludeDFLT" "exclude_dflt" "feature" "from" "ignore" "IgnoreBaseGlyphs" "IgnoreLigatures" "IgnoreMarks" "MarkAttachmentType" "UseMarkFilteringSet" "include" "includeDFLT" "include_dflt" "language" "languagesystem" "lookup" "lookupflag" "mark" "markClass" "nameid" "NULL" "parameters" "position" "pos" "required" "RightToLeft" "reversesub" "rsub" "script" "substitute" "sub" "subtable" "table" "useExtension" "valueRecordDef") )

;; Operators
;;
;; = assigns a value.
;;
;; @foo designates "foo" as a class name.
;; -- :. we need a token definition for the after-@ part, highlighting
;;       these as variables....
;; 'bar designates bar for contextual substitution/positioning.
;; \n designates n as a CID.
(defvar fea-operators
  '("=" "'" "@" "\\" ) )

;; Builtins
(defvar fea-builtins
  '(";" "," "-" "\"" "{" "}" "[" "]" "<" ">" "(" ")") )

;; Second-level keywords, to be highlighted as "types"
(defvar fea-types
  '("HorizAxis.BaseTagList" "HorizAxis.BaseScriptList" "HorizAxis.MinMax" "VertAxis.BaseTagList" "VertAxis.BaseScriptList" "VertAxis.MinMax" "GlyphClassDef" "Attach" "LigatureCaretByDev" "LigatureCaretByIndex" "LigatureCaretByPos" "MarkAttachClass" "FontRevision" "CaretOffset" "Ascender" "Descender" "LineGap" "Panose" "TypoAscender" "TypoDescender" "TypoLineGap" "winAscent" "winDescent" "UnicodeRange" "CodePageRange" "XHeight" "CapHeight" "Vendor" "sizemenuname" "VertTypoAscender" "VertTypoDescender" "VertTypoLineGap" "VertOriginY" "VertAdvanceY") )

;; Numbers and DFLT/dflt, which is a reserved word used in place
;; of a literal value in a few places
      '(("[0-9]+" . 'font-lock-string-face))

;; Registered OpenType tags, to be highlighted as constants since they are
;; defined externally
(defvar opentype-lang-tags
  '("ABA" "ABK" "ADY" "AFK" "AFR" "AGW" "ALS" "ALT" "AMH" "APPH" "ARA" "ARI" "ARK" "ASM" "ATH" "AVR" "AWA" "AYM" "AZE" "BAD" "BAG" "BAL" "BAU" "BBR" "BCH" "BCR" "BEL" "BEM" "BEN" "BGR" "BHI" "BHO" "BIK" "BIL" "BKF" "BLI" "BLN" "BLT" "BMB" "BML" "BOS" "BRE" "BRH" "BRI" "BRM" "BSH" "BTI" "CAT" "CEB" "CHE" "CHG" "CHH" "CHI" "CHK" "CHP" "CHR" "CHU" "CMR" "COP" "COS" "CRE" "CRR" "CRT" "CSL" "CSY" "DAN" "DAR" "DCR" "DEU" "DGR" "DHV" "DIV" "DJR" "DNG" "DNK" "DRI" "DUN" "DZN" "EBI" "ECR" "EDO" "EFI" "ELL" "ENG" "ERZ" "ESP" "ETI" "EUQ" "EVK" "EVN" "EWE" "FAN" "FAR" "FIN" "FJI" "FLE" "FNE" "FON" "FOS" "FRA" "FRI" "FRL" "FTA" "FUL" "GAD" "GAE" "GAG" "GAL" "GAR" "GAW" "GEZ" "GIL" "GMZ" "GON" "GRN" "GRO" "GUA" "GUJ" "HAI" "HAL" "HAR" "HAU" "HAW" "HBN" "HIL" "HIN" "HMA" "HND" "HO" "HRI" "HRV" "HUN" "HYE" "IBO" "IJO" "ILO" "IND" "ING" "INU" "IPPH" "IRI" "IRT" "ISL" "ISM" "ITA" "IWR" "JAV" "JII" "JAN" "JUD" "JUL" "KAB" "KAC" "KAL" "KAN" "KAR" "KAT" "KAZ" "KEB" "KGE" "KHA" "KHK" "KHM" "KHS" "KHV" "KHW" "KIK" "KIR" "KIS" "KKN" "KLM" "KMB" "KMN" "KMO" "KMS" "KNR" "KOD" "KOH" "KOK" "KON" "KOP" "KOR" "KOZ" "KPL" "KRI" "KRK" "KRL" "KRM" "KRN" "KRT" "KSH" "KSI" "KSM" "KUI" "KUL" "KUM" "KUR" "KUU" "KUY" "KYK" "LAD" "LAH" "LAK" "LAM" "LAO" "LAT" "LAZ" "LCR" "LDK" "LEZ" "LIN" "LMA" "LMB" "LMW" "LSB" "LSM" "LTH" "LTZ" "LUB" "LUG" "LUH" "LUO" "LVI" "MAJ" "MAK" "MAL" "MAN" "MAP" "MAR" "MAW" "MBN" "MCH" "MCR" "MDE" "MEN" "MIZ" "MKD" "MLE" "MLG" "MLN" "MLR" "MLY" "MND" "MNG" "MNI" "MNK" "MNX" "MOH" "MOK" "MOL" "MON" "MOR" "MRI" "MTH" "MTS" "MUN" "NAG" "NAN" "NAS" "NCR" "NDB" "NDG" "NEP" "NEW" "NGR" "NHC" "NIS" "NIU" "NKL" "NKO" "NLD" "NOG" "NOR" "NSM" "NTA" "NTO" "NYN" "OCI" "OCR" "OJB" "ORI" "ORO" "OSS" "PAA" "PAL" "PAN" "PAP" "PAS" "PGR" "PIL" "PLG" "PLK" "PRO" "PTG" "QIN" "RAJ" "RCR" "RBU" "RIA" "RMS" "ROM" "ROY" "RSY" "RUA" "RUS" "SAD" "SAN" "SAT" "SAY" "SEK" "SEL" "SGO" "SHN" "SIB" "SID" "SIG" "SKS" "SKY" "SLA" "SLV" "SML" "SMO" "SNA" "SND" "SNH" "SNK" "SOG" "SOT" "SQI" "SRB" "SRK" "SRR" "SSL" "SSM" "SUR" "SVA" "SVE" "SWA" "SWK" "SWZ" "SXT" "SYR" "TAB" "TAJ" "TAM" "TAT" "TCR" "TEL" "TGN" "TGR" "TGY" "THA" "THT" "TIB" "TKM" "TMN" "TNA" "TNE" "TNG" "TOD" "TRK" "TSG" "TUA" "TUL" "TUV" "TWI" "UDM" "UKR" "URD" "USB" "UYG" "UZB" "VEN" "VIT" "WA" "WAG" "WCR" "WEL" "WLF" "XBD" "XHS" "YAK" "YBA" "YCR" "YIC" "YIM" "ZHH" "ZHP" "ZHS" "ZHT" "ZND" "ZUL") )

(defvar opentype-script-tags
  '("arab" "armn" "avst" "bali" "bamu" "batk" "beng" "bng2" "bopo" "brai" "brah" "bugi" "buhd" "byzm" "cans" "cari" "cakm" "cham" "cher" "hani" "copt" "cprt" "cyrl" "dsrt" "deva" "dev2" "egyp" "ethi" "geor" "glag" "goth" "grek" "gujr" "gjr2" "guru" "gur2" "hang" "jamo" "hano" "hebr" "kana" "armi" "phli" "prti" "java" "kthi" "knda" "knd2" "kana" "kali" "khar" "khmr" "lao" "latn" "lepc" "limb" "linb" "lisu" "lyci" "lydi" "mlym" "mlm2" "mand" "math" "mtei" "merc" "mero" "mong" "musc" "mymr" "talu" "nko" "ogam" "olck" "ital" "xpeo" "sarb" "orkh" "orya" "ory2" "osma" "phag" "phnx" "rjng" "runr" "samr" "saur" "shrd" "shaw" "sinh" "sora" "xsux" "sund" "sylo" "syrc" "tglg" "tagb" "tale" "lana" "tavt" "takr" "taml" "tml2" "telu" "tel2" "thaa" "thai" "tibt" "tfng" "ugar" "vai" "yi") )

;; This includes the cv01-cv99 character-variant tags and ss01-ss20
;; stylistic-variant tags, both of which might could be abbreviated
;; just to conserve precious, precious space
(defvar opentype-feature-tags
  '("aalt" "abvf" "abvm" "abvs" "afrc" "akhn" "blwf" "blwm" "blws" "calt" "case" "ccmp" "cfar" "cjct" "clig" "cpct" "cpsp" "cswh" "curs" "cv01" "cv02" "cv03" "cv04" "cv05" "cv06" "cv07" "cv08" "cv09" "cv10" "cv11" "cv12" "cv13" "cv14" "cv15" "cv16" "cv17" "cv18" "cv19" "cv20" "cv21" "cv22" "cv23" "cv24" "cv25" "cv26" "cv27" "cv28" "cv29" "cv30" "cv31" "cv32" "cv33" "cv34" "cv35" "cv36" "cv37" "cv38" "cv39" "cv40" "cv41" "cv42" "cv43" "cv44" "cv45" "cv46" "cv47" "cv48" "cv49" "cv50" "cv51" "cv52" "cv53" "cv54" "cv55" "cv56" "cv57" "cv58" "cv59" "cv60" "cv61" "cv62" "cv63" "cv64" "cv65" "cv66" "cv67" "cv68" "cv69" "cv70" "cv71" "cv72" "cv73" "cv74" "cv75" "cv76" "cv77" "cv78" "cv79" "cv80" "cv81" "cv82" "cv83" "cv84" "cv85" "cv86" "cv87" "cv88" "cv89" "cv90" "cv91" "cv92" "cv93" "cv94" "cv95" "cv96" "cv97" "cv98" "cv99" "c2pc" "c2sc" "dist" "dlig" "dnom" "expt" "falt" "fin2" "fin3" "fina" "frac" "fwid" "half" "haln" "halt" "hist" "hkna" "hlig" "hngl" "hojo" "hwid" "init" "isol" "ital" "jalt" "jp78" "jp83" "jp90" "jp04" "kern" "lfbd" "liga" "ljmo" "lnum" "locl" "ltra" "ltrm" "mark" "med2" "medi" "mgrk" "mkmk" "mset" "nalt" "nlck" "nukt" "numr" "onum" "opbd" "ordn" "ornm" "palt" "pcap" "pkna" "pnum" "pref" "pres" "pstf" "psts" "pwid" "qwid" "rand" "rkrf" "rlig" "rphf" "rtbd" "rtla" "rtlm" "ruby" "salt" "sinf" "size" "smcp" "smpl" "ss01" "ss02" "ss03" "ss04" "ss05" "ss06" "ss07" "ss08" "ss09" "ss10" "ss11" "ss12" "ss13" "ss14" "ss15" "ss16" "ss17" "ss18" "ss19" "ss20" "subs" "sups" "swsh" "titl" "tjmo" "tnam" "tnum" "trad" "twid" "unic" "valt" "vatu" "vert" "vhal" "vjmo" "vkna" "vkrn" "vpal" "vrt2" "zero") )

(defvar opentype-base-tags
  '("hang" "icfb" "icft" "ideo" "idtp" "math" "romn") )
