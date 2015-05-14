(require 'thingatpt)
(require 'eldoc)
(setq eldoc-documentation-function 'fontfile-ffs-eldoc-function)

(setq fontfile-ffs-eldoc-obarray (make-vector 350 0))

(set (intern "$argc" fontfile-ffs-eldoc-obarray) "number of arguments passed to the script, including $0")
(set (intern "$argv" fontfile-ffs-eldoc-obarray) "array of arguments to the current script")
(set (intern "$curfont" fontfile-ffs-eldoc-obarray) "filename in which the current font resides")
(set (intern "$firstfont" fontfile-ffs-eldoc-obarray) "filename of the font that is first on the font list; empty if no fonts are loaded")
(set (intern "$nextfont" fontfile-ffs-eldoc-obarray) "filename of the font that follows the current font on the list; empty if $curfont is the last fone")
(set (intern "$fontchanged" fontfile-ffs-eldoc-obarray) "1 if the current font has changed, 0 if it has not changed since last read or save")
(set (intern "$fontname" fontfile-ffs-eldoc-obarray) "name contained in the postscript FontName field")
(set (intern "$familyname" fontfile-ffs-eldoc-obarray) "name contained in the postscript FamilyName field")
(set (intern "$fullname" fontfile-ffs-eldoc-obarray) "name contained in the postscript FullName field")
(set (intern "$fondname" fontfile-ffs-eldoc-obarray) "indicates what FOND the current font should be put in under Generate Mac Family")
(set (intern "$weight" fontfile-ffs-eldoc-obarray) "name contained in the postscript Weight field")
(set (intern "$copyright" fontfile-ffs-eldoc-obarray) "name contained in the postscript Notice field")
(set (intern "$filename" fontfile-ffs-eldoc-obarray) "name of the file containing the font")
(set (intern "$0" fontfile-ffs-eldoc-obarray) "current script filename")
(set (intern "$1" fontfile-ffs-eldoc-obarray) "first argument to the current script")
(set (intern "$2" fontfile-ffs-eldoc-obarray) "second argument to the current script")
(set (intern "$fontversion" fontfile-ffs-eldoc-obarray) "string containing the font's version")
(set (intern "$iscid" fontfile-ffs-eldoc-obarray) "1 if the current font is a cid keyed font, 0 if not")
(set (intern "$cidfontanme" fontfile-ffs-eldoc-obarray) "fontname of the top-level cid-keyed font; empty for non-CID fonts")
(set (intern "$mmcount" fontfile-ffs-eldoc-obarray) "number of instances in a multiple master font; 0 for non multiple master fonts")
(set (intern "$cidfamilyname" fontfile-ffs-eldoc-obarray) "family name of the top-level cid-keyed font; empty for non-CID fonts")
(set (intern "$cidfullname" fontfile-ffs-eldoc-obarray) "full fontname of the top-level cid-keyed font; empty for non-CID fonts")
(set (intern "$cidweight" fontfile-ffs-eldoc-obarray) "weight name of the top-level cid-keyed font; empty for non-CID fonts")
(set (intern "$cidcopyright" fontfile-ffs-eldoc-obarray) "copyright notice of the top-level cid-keyed font; empty for non-CID fonts")
(set (intern "$italicangle" fontfile-ffs-eldoc-obarray) "value of the postscript italic angle field")
(set (intern "$loadState" fontfile-ffs-eldoc-obarray) "bitmask of non-fatal errors encountered when loading the font")
(set (intern "$privateState" fontfile-ffs-eldoc-obarray) "bitmask of some errors in the PostScript Private dictionary")
(set (intern "$curcid" fontfile-ffs-eldoc-obarray) "fontname of the current font")
(set (intern "$firstcid" fontfile-ffs-eldoc-obarray) "fontname of the first font within this cid font")
(set (intern "$nextcid" fontfile-ffs-eldoc-obarray) "fontname of the next font within this cid font; empty if the current sub-font is the last")
(set (intern "$macstyle" fontfile-ffs-eldoc-obarray) "value of the macstyle field")
(set (intern "$bitmaps" fontfile-ffs-eldoc-obarray) "array containing all bitmap pixelsizes generated for this font")
(set (intern "$order" fontfile-ffs-eldoc-obarray) "indicates whether the font uses quadratic or cubic splines; 2 for quadratic, 3 for cubic")
(set (intern "$em" fontfile-ffs-eldoc-obarray) "number of em-units used by the font")
(set (intern "$ascent" fontfile-ffs-eldoc-obarray) "ascent of the font ")
(set (intern "$descent" fontfile-ffs-eldoc-obarray) "descent of the font ")
(set (intern "$selection" fontfile-ffs-eldoc-obarray) "integer array indicating whether each glyph in the font is selected or not; 1=selected glyph, 0=unselected")
(set (intern "$panose" fontfile-ffs-eldoc-obarray) "array containing the 10 panose values for the font")
(set (intern "$trace" fontfile-ffs-eldoc-obarray) "if set to one, then FontForge will trace each procedure call")
(set (intern "$version" fontfile-ffs-eldoc-obarray) "tring containing the current version of FontForge")
(set (intern "$haspython" fontfile-ffs-eldoc-obarray) "returns 1 if Python scripting is available, 0 if it is not")
(set (intern "Close" fontfile-ffs-eldoc-obarray) "Close()")
(set (intern "ControlAfmLigatureOutput" fontfile-ffs-eldoc-obarray) "ControlAfmLigatureOutput( script, lang, ligature-tag-list )")
(set (intern "Export" fontfile-ffs-eldoc-obarray) "Export( format [, bitmap-size] )")
(set (intern "FontsInFile" fontfile-ffs-eldoc-obarray) "FontsInFile( filename )")
(set (intern "Generate" fontfile-ffs-eldoc-obarray) "Generate( filename [, bitmaptype [, fmflags [, res [, mult-sfd-file [, namelist-name]]]]] )")
(set (intern "GenerateFamily" fontfile-ffs-eldoc-obarray) "GenerateFamily( filename, bitmaptype, fmflags, array-of-font-filenames )")
(set (intern "Import" fontfile-ffs-eldoc-obarray) "Import( filename [, toback [, flags]] )")
(set (intern "MergeKern" fontfile-ffs-eldoc-obarray) "MergeKern( filename )")
(set (intern "MergeFeature" fontfile-ffs-eldoc-obarray) "MergeFeature( filename )")
(set (intern "New" fontfile-ffs-eldoc-obarray) "New()")
(set (intern "Open" fontfile-ffs-eldoc-obarray) "Open( filename [, flags] )")
(set (intern "PrintFont" fontfile-ffs-eldoc-obarray) "PrintFont( type [, pointsize [, sample-text/filename [, output-file]]] )")
(set (intern "PrintSetup" fontfile-ffs-eldoc-obarray) "PrintSetup( type [, printer [,  width, height]] )")
(set (intern "Quit" fontfile-ffs-eldoc-obarray) "Quit( status )")
(set (intern "Revert" fontfile-ffs-eldoc-obarray) "Revert()")
(set (intern "RevertToBackup" fontfile-ffs-eldoc-obarray) "RevertToBackup()")
(set (intern "Save" fontfile-ffs-eldoc-obarray) "Save( [filename] )")
(set (intern "FileAccess" fontfile-ffs-eldoc-obarray) "FileAccess( filename [, prot] )")
(set (intern "FontImage" fontfile-ffs-eldoc-obarray) "FontImage( filename, array [, width [, height]] )")
(set (intern "LoadStringFromFile" fontfile-ffs-eldoc-obarray) "LoadStringFromFile( filename )")
(set (intern "WriteStringToFile" fontfile-ffs-eldoc-obarray) "WriteStringToFile( \"string\", \"Filename\" [, append] )")
(set (intern "Clear" fontfile-ffs-eldoc-obarray) "Clear()")
(set (intern "ClearBackground" fontfile-ffs-eldoc-obarray) "ClearBackground()")
(set (intern "Copy" fontfile-ffs-eldoc-obarray) "Copy()")
(set (intern "CopyAnchors" fontfile-ffs-eldoc-obarray) "CopyAnchors()")
(set (intern "CopyFgToBg" fontfile-ffs-eldoc-obarray) "CopyFgToBg()")
(set (intern "CopyGlyphFeatures" fontfile-ffs-eldoc-obarray) "CopyGlyphFeatures()")
(set (intern "CopyLBearing" fontfile-ffs-eldoc-obarray) "CopyLBearing()")
(set (intern "CopyRBearing" fontfile-ffs-eldoc-obarray) "CopyRBearing()")
(set (intern "CopyReference" fontfile-ffs-eldoc-obarray) "CopyReference()")
(set (intern "CopyUnlinked" fontfile-ffs-eldoc-obarray) "CopyUnlinked(")
(set (intern "CopyVWidth" fontfile-ffs-eldoc-obarray) "CopyVWidth()")
(set (intern "CopyWidth" fontfile-ffs-eldoc-obarray) "CopyWidth()")
(set (intern "Cut" fontfile-ffs-eldoc-obarray) "Cut()")
(set (intern "Join" fontfile-ffs-eldoc-obarray) "Join( [fudge] )")
(set (intern "Paste" fontfile-ffs-eldoc-obarray) "Paste()")
(set (intern "PasteInto" fontfile-ffs-eldoc-obarray) "PasteInto()")
(set (intern "PasteWithOffset" fontfile-ffs-eldoc-obarray) "PasteWithOffset( xoff, yoff )")
(set (intern "ReplaceWithReference" fontfile-ffs-eldoc-obarray) "ReplaceWithReference( [fudge] )")
(set (intern "SameGlyphAs" fontfile-ffs-eldoc-obarray) "SameGlyphAs()")
(set (intern "UnlinkReference" fontfile-ffs-eldoc-obarray) "UnlinkReference()")
(set (intern "Select" fontfile-ffs-eldoc-obarray) "Select( arg1, arg2, ... )")
(set (intern "SelectAll" fontfile-ffs-eldoc-obarray) "SelectAll()")
(set (intern "SelectAllInstancesOf" fontfile-ffs-eldoc-obarray) "SelectAllInstancesOf( name1 [, ...] )")
(set (intern "SelectBitmap" fontfile-ffs-eldoc-obarray) "SelectBitmap( size )")
(set (intern "SelectByATT" fontfile-ffs-eldoc-obarray) "SelectByATT( type, tags, contents, search-type )")
(set (intern "SelectByPosSub" fontfile-ffs-eldoc-obarray) "SelectByPosSub( lookup-subtable-name, search_type )")
(set (intern "SelectChanged" fontfile-ffs-eldoc-obarray) "SelectChanged( [merge] )")
(set (intern "SelectFewer" fontfile-ffs-eldoc-obarray) "SelectFewer( arg1, arg2, ... )")
(set (intern "SelectFewerSingletons" fontfile-ffs-eldoc-obarray) "SelectFewerSingletons( arg1, ... )")
(set (intern "SelectHintingNeeded" fontfile-ffs-eldoc-obarray) "SelectHintingNeeded( [merge] )")
(set (intern "SelectIf" fontfile-ffs-eldoc-obarray) "SelectIf( arg1, arg2, ... )")
(set (intern "SelectInvert" fontfile-ffs-eldoc-obarray) "SelectInvert()")
(set (intern "SelectMore" fontfile-ffs-eldoc-obarray) "SelectMore( arg1, arg2, ... )")
(set (intern "SelectMoreIf" fontfile-ffs-eldoc-obarray) "SelectMoreIf( arg1, arg2, ... )")
(set (intern "SelectMoreSingletons" fontfile-ffs-eldoc-obarray) "SelectMoreSingletons( arg1, ... )")
(set (intern "SelectMoreSingletonsIf" fontfile-ffs-eldoc-obarray) "SelectMoreSingletonsIf( arg1, ... )")
(set (intern "SelectNone" fontfile-ffs-eldoc-obarray) "SelectNone()")
(set (intern "SelectSingletons" fontfile-ffs-eldoc-obarray) "SelectSingletons( arg1, ... )")
(set (intern "SelectSingletonsIf" fontfile-ffs-eldoc-obarray) "SelectSingletonsIf( arg1, ... )")
(set (intern "SelectWorthOutputting" fontfile-ffs-eldoc-obarray) "SelectWorthOutputting()")
(set (intern "AddAccent" fontfile-ffs-eldoc-obarray) "AddAccent( accent [, pos] )")
(set (intern "AddExtrema" fontfile-ffs-eldoc-obarray) "AddExtrema()")
(set (intern "ApplySubstitution" fontfile-ffs-eldoc-obarray) "ApplySubstitution( script, lang, tag )")
(set (intern "AutoTrace" fontfile-ffs-eldoc-obarray) "AutoTrace()")
(set (intern "BitmapsAvail" fontfile-ffs-eldoc-obarray) "BitmapsAvail( sizes [, rasterized] )")
(set (intern "BitmapsRegen" fontfile-ffs-eldoc-obarray) "BitmapsRegen( sizes )")
(set (intern "BuildAccented" fontfile-ffs-eldoc-obarray) "BuildAccented()")
(set (intern "BuildComposite" fontfile-ffs-eldoc-obarray) "BuildComposite()")
(set (intern "BuildDuplicate" fontfile-ffs-eldoc-obarray) "BuildDuplicate()")
(set (intern "CanonicalContours" fontfile-ffs-eldoc-obarray) "CanonicalContours()")
(set (intern "CanonicalStart" fontfile-ffs-eldoc-obarray) "CanonicalStart()")
(set (intern "CompareFonts" fontfile-ffs-eldoc-obarray) "CompareFonts( other-font-filename, output-filename, flags )")
(set (intern "CompareGlyphs" fontfile-ffs-eldoc-obarray) "CompareGlyphs( [pt_err [, spline_err [, pixel_off_frac [, bb_err [, compare_hints [, report_diffs_as_errors]]]]]] )")
(set (intern "CorrectDirection" fontfile-ffs-eldoc-obarray) "CorrectDirection( [unlinkrefs] )")
(set (intern "DefaultRoundToGrid" fontfile-ffs-eldoc-obarray) "DefaultRoundToGrid()")
(set (intern "DefaultUseMyMetrics" fontfile-ffs-eldoc-obarray) "DefaultUseMyMetrics()")
(set (intern "ExpandStroke" fontfile-ffs-eldoc-obarray) "ExpandStroke(width)")
(set (intern "FindIntersections" fontfile-ffs-eldoc-obarray) "FindIntersections()")
(set (intern "HFlip" fontfile-ffs-eldoc-obarray) "HFlip( [about-x] )")
(set (intern "Inline" fontfile-ffs-eldoc-obarray) "Inline( width, gap )")
(set (intern "InterpolateFonts" fontfile-ffs-eldoc-obarray) "InterpolateFonts( percentage, other-font-name [, flags] )")
(set (intern "MergeFonts" fontfile-ffs-eldoc-obarray) "MergeFonts( other-font-name [, flags] )")
(set (intern "Move" fontfile-ffs-eldoc-obarray) "Move( delta-x, delta-y )")
(set (intern "MoveReference" fontfile-ffs-eldoc-obarray) "MoveReference( delta-x, delta-y [, refname/ref-unicode]+ )")
(set (intern "NearlyHvCps" fontfile-ffs-eldoc-obarray) "NearlyHvCps( [error [, err-denom]] )")
(set (intern "NearlyHvLines" fontfile-ffs-eldoc-obarray) "NearlyHvLines( [error [, err-denom]] )")
(set (intern "NearlyLines" fontfile-ffs-eldoc-obarray) "NearlyLines( error )")
(set (intern "NonLinearTransform" fontfile-ffs-eldoc-obarray) "NonLinearTransform( x-expression, y-expression )")
(set (intern "Outline" fontfile-ffs-eldoc-obarray) "Outline( width )")
(set (intern "OverlapIntersect" fontfile-ffs-eldoc-obarray) "OverlapIntersect()")
(set (intern "PositionReference" fontfile-ffs-eldoc-obarray) "PositionReference( x, y [, refname/ref-unicode]+ )")
(set (intern "RemoveOverlap" fontfile-ffs-eldoc-obarray) "RemoveOverlap()")
(set (intern "Rotate" fontfile-ffs-eldoc-obarray) "Rotate( angle [, ox, oy] )")
(set (intern "RoundToCluster" fontfile-ffs-eldoc-obarray) "RoundToCluster( [within [, max]] )")
(set (intern "RoundToInt" fontfile-ffs-eldoc-obarray) "RoundToInt( [factor] )")
(set (intern "Scale" fontfile-ffs-eldoc-obarray) "Scale( factor [, yfactor] [, ox, oy] )")
(set (intern "ScaleToEm" fontfile-ffs-eldoc-obarray) "ScaleToEm( em-size )")
(set (intern "Shadow" fontfile-ffs-eldoc-obarray) "Shadow( angle, outline-width, shadow-width )")
(set (intern "Simplify" fontfile-ffs-eldoc-obarray) "Simplify()")
(set (intern "Skew" fontfile-ffs-eldoc-obarray) "Skew( angle [, ox, oy] )")
(set (intern "Transform" fontfile-ffs-eldoc-obarray) "Transform( t1, t2, t3, t4, t5, t6 )")
(set (intern "VFlip" fontfile-ffs-eldoc-obarray) "VFlip( [about-y] )")
(set (intern "Wireframe" fontfile-ffs-eldoc-obarray) "Wireframe( angle, outline-width, shadow-width )")
(set (intern "AddSizeFeature" fontfile-ffs-eldoc-obarray) "AddSizeFeature( default-size [, range-bottom, range-top, style-id, array-of-lang-names] )")
(set (intern "ChangePrivateEntry" fontfile-ffs-eldoc-obarray) "ChangePrivateEntry( key, val )")
(set (intern "ClearPrivateEntry" fontfile-ffs-eldoc-obarray) "ClearPrivateEntry( key )")
(set (intern "GetFontBoundingBox" fontfile-ffs-eldoc-obarray) "GetFontBoundingBox()")
(set (intern "GetMaxpValue" fontfile-ffs-eldoc-obarray) "GetMaxpValue( field-name )")
(set (intern "GetOS2Value" fontfile-ffs-eldoc-obarray) "GetOS2Value( field-name )")
(set (intern "GetPrivateEntry" fontfile-ffs-eldoc-obarray) "GetPrivateEntry( key )")
(set (intern "GetTeXParam" fontfile-ffs-eldoc-obarray) "GetTeXParam( index )")
(set (intern "GetTTFName" fontfile-ffs-eldoc-obarray) "GetTTFName( lang, nameid )")
(set (intern "HasPrivateEntry" fontfile-ffs-eldoc-obarray) "HasPrivateEntry( key )")
(set (intern "ScaleToEm" fontfile-ffs-eldoc-obarray) "ScaleToEm( em-size )")
(set (intern "SetFondName" fontfile-ffs-eldoc-obarray) "SetFondName( fondname )")
(set (intern "SetFontHasVerticalMetrics" fontfile-ffs-eldoc-obarray) "SetFontHasVerticalMetrics( flag )")
(set (intern "SetFontNames" fontfile-ffs-eldoc-obarray) "SetFontNames( fontname [, family [, fullname [, weight [, copyright-notice [, fontversion]]]]] )")
(set (intern "SetFontOrder" fontfile-ffs-eldoc-obarray) "SetFontOrder( order )")
(set (intern "SetGasp" fontfile-ffs-eldoc-obarray) "SetGasp( [ppem, flag [, ppem2, flag [, ...]]] )")
(set (intern "SetItalicAngle" fontfile-ffs-eldoc-obarray) "SetItalicAngle( angle [, denom] )")
(set (intern "SetMacStyle" fontfile-ffs-eldoc-obarray) "SetMacStyle( val )")
(set (intern "SetMaxpValue" fontfile-ffs-eldoc-obarray) "SetMaxpValue( field-name, value )")
(set (intern "SetOS2Value" fontfile-ffs-eldoc-obarray) "SetOS2Value( field-name, field-value )")
(set (intern "SetPanose" fontfile-ffs-eldoc-obarray) "SetPanose( array ) SetPanose( index, value )")
(set (intern "SetTeXParams" fontfile-ffs-eldoc-obarray) "SetTeXParams( type, design-size, slant, space, stretch, shrink, xheight, quad, extraspace [...] )")
(set (intern "SetTTFName" fontfile-ffs-eldoc-obarray) "SetTTFName( lang, nameid, utf8-string )")
(set (intern "SetUniqueID" fontfile-ffs-eldoc-obarray) "SetUniqueID( value )")
(set (intern "DrawsSomething" fontfile-ffs-eldoc-obarray) "DrawsSomething( [arg] )")
(set (intern "GetPosSub" fontfile-ffs-eldoc-obarray) "GetPosSub(lookup-subtable-name )")
(set (intern "GlyphInfo" fontfile-ffs-eldoc-obarray) "GlyphInfo(str )")
(set (intern "SetGlyphColor" fontfile-ffs-eldoc-obarray) "SetGlyphColor(color )")
(set (intern "SetGlyphComment" fontfile-ffs-eldoc-obarray) "SetGlyphComment(comment )")
(set (intern "SetGlyphChanged" fontfile-ffs-eldoc-obarray) "SetGlyphChanged(flag )")
(set (intern "SetGlyphClass" fontfile-ffs-eldoc-obarray) "SetGlyphClass(class-name )")
(set (intern "SetGlyphName" fontfile-ffs-eldoc-obarray) "SetGlyphName(name [, set-from-name-flag] )")
(set (intern "SetUnicodeValue" fontfile-ffs-eldoc-obarray) "SetUnicodeValue(uni [, set-from-value-flag] )")
(set (intern "SetGlyphTeX" fontfile-ffs-eldoc-obarray) "SetGlyphTeX(height, depth [, subpos, suppos] )")
(set (intern "WorthOutputting" fontfile-ffs-eldoc-obarray) "WorthOutputting( [arg] )")
(set (intern "AddAnchorClass" fontfile-ffs-eldoc-obarray) "AddAnchorClass(name, type, script-lang, tag, flags, merge-with )")
(set (intern "AddAnchorPoint" fontfile-ffs-eldoc-obarray) "AddAnchorPoint(name, type, x, y [, lig-index] )")
(set (intern "AddLookup" fontfile-ffs-eldoc-obarray) "AddLookup(new-lookup-name, type, flags, feature-script-lang-array [, after-lookup-name )")
(set (intern "AddLookupSubtable" fontfile-ffs-eldoc-obarray) "AddLookupSubtable(lookup-name, new-subtable-name [, after-subtable-name] )")
(set (intern "AddPosSub" fontfile-ffs-eldoc-obarray) "AddPosSub(subtable-name, {variant(s)|dx, dy, dadv_x, dadv_y|other-glyph-name, dx1, dy1, dadv_x1, dadv_y1, dx2, dy2, dadv_x2, dadv_y2})")
(set (intern "AddSizeFeature" fontfile-ffs-eldoc-obarray) "AddSizeFeature(default-size [, range-bottom, range-top, style-id, array-of-lang-names] )")
(set (intern "AddATT" fontfile-ffs-eldoc-obarray) "AddATT(type, script-lang, tag, flags, variant )")
(set (intern "ApplySubstitution" fontfile-ffs-eldoc-obarray) "ApplySubstitution(script, lang, tag )")
(set (intern "CheckForAnchorClass" fontfile-ffs-eldoc-obarray) "CheckForAnchorClass(name )")
(set (intern "DefaultATT" fontfile-ffs-eldoc-obarray) "DefaultATT(tag )")
(set (intern "GetAnchorPoints" fontfile-ffs-eldoc-obarray) "GetAnchorPoints()")
(set (intern "GetLookupInfo" fontfile-ffs-eldoc-obarray) "GetLookupInfo(lookup-name )")
(set (intern "GetLookups" fontfile-ffs-eldoc-obarray) "GetLookups(table-name )")
(set (intern "GetLookupSubtables" fontfile-ffs-eldoc-obarray) "GetLookupSubtables(lookup-name )")
(set (intern "GetLookupOfSubtable" fontfile-ffs-eldoc-obarray) "GetLookupOfSubtable(subtable-name )")
(set (intern "GetPosSub" fontfile-ffs-eldoc-obarray) "GetPosSub(lookup-subtable-name )")
(set (intern "GetSubtableOfAnchor" fontfile-ffs-eldoc-obarray) "GetSubtableOfAnchor(anchor-class-name )")
(set (intern "GenerateFeatureFile" fontfile-ffs-eldoc-obarray) "GenerateFeatureFile(filename [, lookup-name] )")
(set (intern "HasPreservedTable" fontfile-ffs-eldoc-obarray) "HasPreservedTable(tag )")
(set (intern "LoadTableFromFile" fontfile-ffs-eldoc-obarray) "LoadTableFromFile(tag, filename )")
(set (intern "LookupStoreLigatureInAfm" fontfile-ffs-eldoc-obarray) "LookupStoreLigatureInAfm(lookup-name, store-it )")
(set (intern "LookupSetFeatureList" fontfile-ffs-eldoc-obarray) "LookupSetFeatureList(lookup-name, feature-script-lang-array )")
(set (intern "MergeLookups" fontfile-ffs-eldoc-obarray) "MergeLookups(lookup-name1, lookup-name2 )")
(set (intern "MergeLookupSubtables" fontfile-ffs-eldoc-obarray) "MergeLookupSubtables(subtable-name1, subtable-name2 )")
(set (intern "RemoveATT" fontfile-ffs-eldoc-obarray) "RemoveATT(type, script-lang, tag )")
(set (intern "RemoveAnchorClass" fontfile-ffs-eldoc-obarray) "RemoveAnchorClass (name )")
(set (intern "RemoveLookup" fontfile-ffs-eldoc-obarray) "RemoveLookup( lookup-name )")
(set (intern "RemoveLookupSubtable" fontfile-ffs-eldoc-obarray) "RemoveLookupSubtable( subtable-name )")
(set (intern "RemovePosSub" fontfile-ffs-eldoc-obarray) "RemovePosSub( subtable-name )")
(set (intern "RemovePreservedTable" fontfile-ffs-eldoc-obarray) "RemovePreservedTable( tag )")
(set (intern "SaveTableToFile" fontfile-ffs-eldoc-obarray) "SaveTableToFile( tag,  filename )")
(set (intern "CharCnt" fontfile-ffs-eldoc-obarray) "CharCnt()")
(set (intern "DetachGlyphs" fontfile-ffs-eldoc-obarray) "DetachGlyphs()")
(set (intern "DetachAndRemoveGlyphs" fontfile-ffs-eldoc-obarray) "DetachAndRemoveGlyphs()")
(set (intern "LoadEncodingFile" fontfile-ffs-eldoc-obarray) "LoadEncodingFile( filename )")
(set (intern "MultipleEncodingsToReferences" fontfile-ffs-eldoc-obarray) "MultipleEncodingsToReferences()")
(set (intern "Reencode" fontfile-ffs-eldoc-obarray) "Reencode (encoding-name [,  force] )")
(set (intern "RemoveDetachedGlyphs" fontfile-ffs-eldoc-obarray) "RemoveDetachedGlyphs()")
(set (intern "RenameGlyphs" fontfile-ffs-eldoc-obarray) "RenameGlyphs( namelist-name )")
(set (intern "SameGlyphAs" fontfile-ffs-eldoc-obarray) "SameGlyphAs()")
(set (intern "SetCharCnt" fontfile-ffs-eldoc-obarray) "SetCharCnt( cnt )")
(set (intern "AddDHint" fontfile-ffs-eldoc-obarray) "AddDHint( x1, y1, x2, y2, unit.x, unit.y )")
(set (intern "AddHHint" fontfile-ffs-eldoc-obarray) "AddHHint( start, width )")
(set (intern "AddInstrs" fontfile-ffs-eldoc-obarray) "AddInstrs( thingamy, replace, instrs )")
(set (intern "AddVHint" fontfile-ffs-eldoc-obarray) "AddVHint( start, width )")
(set (intern "AutoCounter" fontfile-ffs-eldoc-obarray) "AutoCounter()")
(set (intern "AutoHint" fontfile-ffs-eldoc-obarray) "AutoHint()")
(set (intern "AutoInstr" fontfile-ffs-eldoc-obarray) "AutoInstr()")
(set (intern "ChangePrivateEntry" fontfile-ffs-eldoc-obarray) "ChangePrivateEntry( key, val )")
(set (intern "ClearGlyphCounterMasks" fontfile-ffs-eldoc-obarray) "ClearGlyphCounterMasks()")
(set (intern "ClearHints" fontfile-ffs-eldoc-obarray) "ClearHints()")
(set (intern "ClearInstrs" fontfile-ffs-eldoc-obarray) "ClearInstrs()")
(set (intern "ClearPrivateEntry" fontfile-ffs-eldoc-obarray) "ClearPrivateEntry( key )")
(set (intern "ClearTable" fontfile-ffs-eldoc-obarray) "ClearTable( tag )")
(set (intern "DontAutoHint" fontfile-ffs-eldoc-obarray) "DontAutoHint()")
(set (intern "FindOrAddCvtIndex" fontfile-ffs-eldoc-obarray) "FindOrAddCvtIndex( value [, sign-matters] )")
(set (intern "GetCvtAt" fontfile-ffs-eldoc-obarray) "GetCvtAt( index )")
(set (intern "GetPrivateEntry" fontfile-ffs-eldoc-obarray) "GetPrivateEntry( key )")
(set (intern "HasPrivateEntry" fontfile-ffs-eldoc-obarray) "HasPrivateEntry( key )")
(set (intern "ReplaceGlyphCounterMasks" fontfile-ffs-eldoc-obarray) "ReplaceGlyphCounterMasks( array )")
(set (intern "ReplaceCvtAt" fontfile-ffs-eldoc-obarray) "ReplaceCvtAt( index, value )")
(set (intern "SetGlyphCounterMask" fontfile-ffs-eldoc-obarray) "SetGlyphCounterMask( cg, hint-index, hint-index, ... )")
(set (intern "SubstitutionPoints" fontfile-ffs-eldoc-obarray) "SubstitutionPoints()")
(set (intern "AutoKern" fontfile-ffs-eldoc-obarray) "AutoKern( spacing, threshold, subtable-name [, kernfile] )")
(set (intern "AutoWidth" fontfile-ffs-eldoc-obarray) "AutoWidth( spacing )")
(set (intern "CenterInWidth" fontfile-ffs-eldoc-obarray) "CenterInWidth()")
(set (intern "SetKern" fontfile-ffs-eldoc-obarray) "SetKern (ch2, offset [, lookup-subtable] )")
(set (intern "RemoveAllKerns" fontfile-ffs-eldoc-obarray) "RemoveAllKerns()")
(set (intern "RemoveAllVKerns" fontfile-ffs-eldoc-obarray) "RemoveAllVKerns()")
(set (intern "SetLBearing" fontfile-ffs-eldoc-obarray) "SetLBearing( lbearing [, relative] )")
(set (intern "SetRBearing" fontfile-ffs-eldoc-obarray) "SetRBearing( rbearing [, relative] )")
(set (intern "SetVKern" fontfile-ffs-eldoc-obarray) "SetVKern( ch2, offset [, lookup-subtable] )")
(set (intern "SetVWidth" fontfile-ffs-eldoc-obarray) "SetVWidth( vertical-width [, relative] )")
(set (intern "SetWidth" fontfile-ffs-eldoc-obarray) "SetWidth( width [, relative] )")
(set (intern "VKernFromHKern" fontfile-ffs-eldoc-obarray) "VKernFromHKern()")
(set (intern "MMAxisBounds" fontfile-ffs-eldoc-obarray) "MMAxisBounds( axis )")
(set (intern "MMAxisNames" fontfile-ffs-eldoc-obarray) "MMAxisNames()")
(set (intern "MMBlendToNewFont" fontfile-ffs-eldoc-obarray) "MMBlendToNewFont( weights )")
(set (intern "MMChangeInstance" fontfile-ffs-eldoc-obarray) "MMChangeInstance( instance )")
(set (intern "MMChangeWeight" fontfile-ffs-eldoc-obarray) "MMChangeWeight( weights )")
(set (intern "MMInstanceNames" fontfile-ffs-eldoc-obarray) "MMInstanceNames()")
(set (intern "MMWeightedName" fontfile-ffs-eldoc-obarray) "MMWeightedName()")
(set (intern "CIDChangeSubFont" fontfile-ffs-eldoc-obarray) "CIDChangeSubFont( new-sub-font-name )")
(set (intern "CIDFlatten" fontfile-ffs-eldoc-obarray) "CIDFlatten()")
(set (intern "CIDFlattenByCMap" fontfile-ffs-eldoc-obarray) "CIDFlattenByCMap( cmap-filename )")
(set (intern "CIDSetFontNames" fontfile-ffs-eldoc-obarray) "CIDSetFontNames( fontname [, family [, fullname [, weight [, copyright-notice]]]] )")
(set (intern "ConvertToCID" fontfile-ffs-eldoc-obarray) "ConvertToCID( registry,  ordering,  supplement )")
(set (intern "ConvertByCMap" fontfile-ffs-eldoc-obarray) "ConvertByCMap( cmapfilename )")
(set (intern "PreloadCidmap" fontfile-ffs-eldoc-obarray) "PreloadCidmap( filename, registry, ordering, supplement )")
(set (intern "AskUser" fontfile-ffs-eldoc-obarray) "AskUser( question [, default-answer] )")
(set (intern "Error" fontfile-ffs-eldoc-obarray) "Error( str )")
(set (intern "PostNotice" fontfile-ffs-eldoc-obarray) "PostNotice( str )")
(set (intern "Print" fontfile-ffs-eldoc-obarray) "Print( arg1, arg2, arg3, ... )")
(set (intern "DefaultOtherSubrs" fontfile-ffs-eldoc-obarray) "DefaultOtherSubrs()")
(set (intern "GetPref" fontfile-ffs-eldoc-obarray) "GetPref( str )")
(set (intern "LoadEncodingFile" fontfile-ffs-eldoc-obarray) "LoadEncodingFile( filename )")
(set (intern "LoadNamelist" fontfile-ffs-eldoc-obarray) "LoadNamelist( filename )")
(set (intern "LoadNamelistDir" fontfile-ffs-eldoc-obarray) "LoadNamelistDir(  [directory-name] )")
(set (intern "LoadPlugin" fontfile-ffs-eldoc-obarray) "LoadPlugin( filename )")
(set (intern "LoadPluginDir" fontfile-ffs-eldoc-obarray) "LoadPluginDir(  [directory-name] )")
(set (intern "LoadPrefs" fontfile-ffs-eldoc-obarray) "LoadPrefs()")
(set (intern "ReadOtherSubrsFile" fontfile-ffs-eldoc-obarray) "ReadOtherSubrsFile( filename )")
(set (intern "SavePrefs" fontfile-ffs-eldoc-obarray) "SavePrefs()")
(set (intern "SetPref" fontfile-ffs-eldoc-obarray) "SetPref( str, val [, val2] )")
(set (intern "ATan2" fontfile-ffs-eldoc-obarray) "ATan2( val1, val2 )")
(set (intern "Ceil" fontfile-ffs-eldoc-obarray) "Ceil( real )")
(set (intern "Chr" fontfile-ffs-eldoc-obarray) "Chr( int )")
(set (intern "Cos" fontfile-ffs-eldoc-obarray) "Cos( val )")
(set (intern "Exp" fontfile-ffs-eldoc-obarray) "Exp( val )")
(set (intern "Floor" fontfile-ffs-eldoc-obarray) "Floor( real )")
(set (intern "Int" fontfile-ffs-eldoc-obarray) "Int( real )")
(set (intern "IsFinite" fontfile-ffs-eldoc-obarray) "IsFinite( real )")
(set (intern "IsNan" fontfile-ffs-eldoc-obarray) "IsNan( real )")
(set (intern "Log" fontfile-ffs-eldoc-obarray) "Log( val )")
(set (intern "Ord" fontfile-ffs-eldoc-obarray) "Ord( string [, pos] )")
(set (intern "Pow" fontfile-ffs-eldoc-obarray) "Pow( val1, val2 )")
(set (intern "Rand" fontfile-ffs-eldoc-obarray) "Rand()")
(set (intern "Real" fontfile-ffs-eldoc-obarray) "Real( int )")
(set (intern "Round" fontfile-ffs-eldoc-obarray) "Round( real )")
(set (intern "Sin" fontfile-ffs-eldoc-obarray) "Sin( val )")
(set (intern "Sqrt" fontfile-ffs-eldoc-obarray) "Sqrt( val )")
(set (intern "Strskipint" fontfile-ffs-eldoc-obarray) "Strskipint( str [, base] )")
(set (intern "Strtod" fontfile-ffs-eldoc-obarray) "Strtod( str )")
(set (intern "Strtol" fontfile-ffs-eldoc-obarray) "Strtol( str [, base] )")
(set (intern "Tan" fontfile-ffs-eldoc-obarray) "Tan( val )")
(set (intern "ToString" fontfile-ffs-eldoc-obarray) "ToString( arg )")
(set (intern "UCodePoint" fontfile-ffs-eldoc-obarray) "UCodePoint( int )")
(set (intern "NameFromUnicode" fontfile-ffs-eldoc-obarray) "NameFromUnicode( uni [, namelist] )")
(set (intern "UCodePoint" fontfile-ffs-eldoc-obarray) "UCodePoint( int )")
(set (intern "UnicodeFromName" fontfile-ffs-eldoc-obarray) "UnicodeFromName( name )")
(set (intern "Ucs4" fontfile-ffs-eldoc-obarray) "Ucs4( str )")
(set (intern "Utf8" fontfile-ffs-eldoc-obarray) "Utf8( int )")
(set (intern "Chr" fontfile-ffs-eldoc-obarray) "Chr( int )")
(set (intern "GetEnv" fontfile-ffs-eldoc-obarray) "GetEnv( str )")
(set (intern "NameFromUnicode" fontfile-ffs-eldoc-obarray) "NameFromUnicode( uni [, namelist] )")
(set (intern "Ord" fontfile-ffs-eldoc-obarray) "Ord( string [, pos] )")
(set (intern "Strcasecmp" fontfile-ffs-eldoc-obarray) "Strcasecmp( str1, str2 )")
(set (intern "Strcasestr" fontfile-ffs-eldoc-obarray) "Strcasestr( haystack, needle )")
(set (intern "Strftime" fontfile-ffs-eldoc-obarray) "Strftime( format [, isgmt [, locale]] )")
(set (intern "StrJoin" fontfile-ffs-eldoc-obarray) "StrJoin( array, delimiter )")
(set (intern "Strlen" fontfile-ffs-eldoc-obarray) "Strlen( str )")
(set (intern "Strrstr" fontfile-ffs-eldoc-obarray) "Strrstr( haystack, needle )")
(set (intern "Strskipint" fontfile-ffs-eldoc-obarray) "Strskipint( str [, base] )")
(set (intern "StrSplit" fontfile-ffs-eldoc-obarray) "StrSplit( str, delimiter [, max-count] )")
(set (intern "Strstr" fontfile-ffs-eldoc-obarray) "Strstr( haystack, needle )")
(set (intern "Strsub" fontfile-ffs-eldoc-obarray) "Strsub( str, start [, end] )")
(set (intern "Strtod" fontfile-ffs-eldoc-obarray) "Strtod( str )")
(set (intern "Strtol" fontfile-ffs-eldoc-obarray) "Strtol( str [, base] )")
(set (intern "ToString" fontfile-ffs-eldoc-obarray) "ToString( arg )")
(set (intern "IsAlNum" fontfile-ffs-eldoc-obarray) "IsAlNum( val )")
(set (intern "IsAlpha" fontfile-ffs-eldoc-obarray) "IsAlpha( val )")
(set (intern "IsDigit" fontfile-ffs-eldoc-obarray) "IsDigit( val )")
(set (intern "IsHexDigit" fontfile-ffs-eldoc-obarray) "IsHexDigit( val )")
(set (intern "IsLower" fontfile-ffs-eldoc-obarray) "IsLower( val )")
(set (intern "IsSpace" fontfile-ffs-eldoc-obarray) "IsSpace( val )")
(set (intern "IsUpper" fontfile-ffs-eldoc-obarray) "IsUpper( val )")
(set (intern "ToLower" fontfile-ffs-eldoc-obarray) "ToLower( val )")
(set (intern "ToMirror" fontfile-ffs-eldoc-obarray) "ToMirror( val )")
(set (intern "ToUpper" fontfile-ffs-eldoc-obarray) "ToUpper( val )")
(set (intern "Array" fontfile-ffs-eldoc-obarray) "Array( size )")
(set (intern "SizeOf" fontfile-ffs-eldoc-obarray) "SizeOf( arr )")
(set (intern "InFont" fontfile-ffs-eldoc-obarray) "InFont( arg )")
(set (intern "TypeOf" fontfile-ffs-eldoc-obarray) "TypeOf( any )")
(set (intern "ClearCharCounterMasks" fontfile-ffs-eldoc-obarray) "ClearCharCounterMasks()")
(set (intern "CharInfo" fontfile-ffs-eldoc-obarray) "CharInfo( str )")
(set (intern "ReplaceCharCounterMasks" fontfile-ffs-eldoc-obarray) "ReplaceCharCounterMasks( array )")
(set (intern "SetCharColor" fontfile-ffs-eldoc-obarray) "SetCharColor( color )")
(set (intern "SetCharComment" fontfile-ffs-eldoc-obarray) "SetCharComment( comment )")
(set (intern "SetCharCounterMask" fontfile-ffs-eldoc-obarray) "SetCharCounterMask( cg, hint-index, hint-index, ... )")
(set (intern "SetCharName" fontfile-ffs-eldoc-obarray) "SetCharName( name [, set-from-name-flag] )")

(defun fontfile-ffs-eldoc-function ()
    "Returns a doc string appropriate for the current context, or nil."
    (symbol-value (intern-soft (thing-at-point 'symbol)
                               fontfile-ffs-eldoc-obarray)))
