(require 'generic-x) ;; we need this
    
    (define-generic-mode 
      'fea-mode                         
      '("#")                           ;; comments start with '#'
      '("anchor" "anchorDef" "anonymous" "anon" "by" "contour" "cursive" "device" "enumerate" "enum" "excludeDFLT" "exclude_dflt" "feature" "from" "ignore" "IgnoreBaseGlyphs" "IgnoreLigatures" "IgnoreMarks" "MarkAttachmentType" "UseMarkFilteringSet" "include" "includeDFLT" "include_dflt" "language" "languagesystem" "lookup" "lookupflag" "mark" "markClass" "nameid" "NULL" "parameters" "position" "pos" "required" "RightToLeft" "reversesub" "rsub" "script" "substitute" "sub" "subtable" "table" "useExtension" "valueRecordDef")                     ;; keywords
      '(("=" . 'font-lock-operator)     ;; operators
      '(("[0-9]+" . 'font-lock-string-face))
        (";" . 'font-lock-builtin))     ;; built-ins
      '("\\.fea$")                      ;; files for which to activate this mode 
       nil                              
      "A mode for Adobe OpenType feature files"            
    )
