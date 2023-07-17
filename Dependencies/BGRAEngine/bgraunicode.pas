// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUnicode;
{ Implementation of Unicode bidi algorithm }
{ Author: circular }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  BGRAClasses, SysUtils;

type
  TUnicodeBidiClass = (ubcBoundaryNeutral, ubcSegmentSeparator, ubcParagraphSeparator, ubcWhiteSpace, ubcOtherNeutrals,
                      ubcCommonSeparator, ubcNonSpacingMark,
                      ubcLeftToRight, ubcEuropeanNumber, ubcEuropeanNumberSeparator, ubcEuropeanNumberTerminator,
                      ubcRightToLeft, ubcArabicLetter, ubcArabicNumber,
                      ubcUnknown,
                      ubcCombiningLeftToRight,   //ubcLeftToRight in Mc category
                      ubcMirroredNeutral);       //ubcOtherNeutrals with Mirrored property
  TUnicodeJoiningType = (ujtNonJoining{U}, ujtTransparent{T}, ujtRightJoining{R}, ujtLeftJoining{L},
                         ujtDualJoining{D}, ujtJoinCausing{C});
  TFontBidiMode = (fbmAuto, fbmLeftToRight, fbmRightToLeft);

const
  ubcNeutral = [ubcSegmentSeparator, ubcParagraphSeparator, ubcWhiteSpace, ubcOtherNeutrals];

  BIDI_FLAG_REMOVED = 1;                   //RLE, LRE, RLO, LRO, PDF and BN are supposed to be removed
  BIDI_FLAG_IMPLICIT_END_OF_PARAGRAPH = 2; //implicit end of paragraph (paragraph spacing below due to end of text)
  BIDI_FLAG_EXPLICIT_END_OF_PARAGRAPH = 4; //explicit end of paragraph (paragraph spacing below due to paragraph split)
  BIDI_FLAG_END_OF_LINE = 8;               //line break <br>
  BIDI_FLAG_LIGATURE_RIGHT = 16;           //joins to the letter on the right (possible for joining type R and D)
  BIDI_FLAG_LIGATURE_LEFT = 32;            //joins to the letter on the left (possible for joining type L and D)
  BIDI_FLAG_LIGATURE_BOUNDARY = 64;        //zero-width joiner or non-joiner
  BIDI_FLAG_LIGATURE_TRANSPARENT = 128;    //does not affect ligature
  BIDI_FLAG_RTL_SCRIPT = 256;              //script is written from right to left (arabic, N'Ko...)
  BIDI_FLAG_NON_SPACING_MARK = 512;        //it is a non-spacing mark
  BIDI_FLAG_COMBINING_LEFT = 1024;         //this letter is to be combined to the left of previous letter
  BIDI_FLAG_COMBINING_RIGHT = 2048;        //this letter is to be combined to the right of previous letter
  BIDI_FLAG_MULTICHAR_START = 4096;        //start of a multichar (letter + non spacing marks, non spacing marks)
  BIDI_FLAG_MIRRORED = 8192;               //the glyph is mirrored when in RTL text

type
  PUnicodeBidiInfo = ^TUnicodeBidiInfo;

  { TUnicodeBidiInfo }

  TUnicodeBidiInfo = packed record
  private
    function GetDiscardable: boolean;
    function GetEndOfLine: boolean;
    function GetEndOfParagraph: boolean;
    function GetExplicitEndOfParagraph: boolean;
    function GetHasLigatureLeft: boolean;
    function GetHasLigatureRight: boolean;
    function GetImplicitEndOfParagraph: boolean;
    function GetIsCombiningLeft: boolean;
    function GetIsCombiningRight: boolean;
    function GetIsMirrored: boolean;
    function GetLigatureBoundary: boolean;
    function GetLigatureTransparent: boolean;
    function GetMulticharStart: boolean;
    function GetNonSpacingMark: boolean;
    function GetRemoved: boolean;
    function GetRightToLeft: boolean;
    function GetParagraphRightToLeft: boolean;
    function GetRightToLeftScript: boolean;
  public
    ParagraphBidiLevel, BidiLevel: byte;
    Flags: Word;
    class operator =(const AInfo1, AInfo2: TUnicodeBidiInfo): boolean;
    property IsRemoved: boolean read GetRemoved;
    property IsRightToLeft: boolean read GetRightToLeft;
    property IsParagraphRightToLeft: boolean read GetParagraphRightToLeft;
    property IsEndOfLine: boolean read GetEndOfLine;
    property IsEndOfParagraph: boolean read GetEndOfParagraph;
    property IsExplicitEndOfParagraph: boolean read GetExplicitEndOfParagraph;
    property IsImplicitEndOfParagraph: boolean read GetImplicitEndOfParagraph;
    property HasLigatureRight: boolean read GetHasLigatureRight;
    property HasLigatureLeft: boolean read GetHasLigatureLeft;
    property IsLigatureBoundary: boolean read GetLigatureBoundary;
    property IsLigatureTransparent: boolean read GetLigatureTransparent;
    property IsDiscardable: boolean read GetDiscardable;
    property IsRightToLeftScript: boolean read GetRightToLeftScript;
    property IsNonSpacingMark: boolean read GetNonSpacingMark;
    property IsCombiningLeft: boolean read GetIsCombiningLeft;
    property IsCombiningRight: boolean read GetIsCombiningRight;
    property IsMulticharStart: boolean read GetMulticharStart;
    property IsMirrored: boolean read GetIsMirrored;
  end;

  TUnicodeBidiArray = packed array of TUnicodeBidiInfo;
  TUnicodeDisplayOrder = array of integer;

const
  //maximum nesting level of isolates and bidi-formatting blocks (char bidi level can actually be higher due to char properties)
  UNICODE_MAX_BIDI_DEPTH = 125;

  UNICODE_NO_BREAK_SPACE = $A0;
  UNICODE_LINE_SEPARATOR = $2028;      //equivalent of <br>
  UNICODE_PARAGRAPH_SEPARATOR = $2029; //equivalent of </p>
  UNICODE_NEXT_LINE = $0085;           //equivalent of CRLF

  //characters that split lines into top-level bidi blocks
  UNICODE_LEFT_TO_RIGHT_ISOLATE = $2066;
  UNICODE_RIGHT_TO_LEFT_ISOLATE = $2067;
  UNICODE_FIRST_STRONG_ISOLATE = $2068;
  UNICODE_POP_DIRECTIONAL_ISOLATE = $2069;

  //characters that split into bidi sub-blocks (called "formatting")
  UNICODE_LEFT_TO_RIGHT_EMBEDDING = $202A;
  UNICODE_RIGHT_TO_LEFT_EMBEDDING = $202B;
  UNICODE_LEFT_TO_RIGHT_OVERRIDE = $202D;
  UNICODE_RIGHT_TO_LEFT_OVERRIDE = $202E;
  UNICODE_POP_DIRECTIONAL_FORMATTING = $202C;

  //characters that mark direction without splitting the bidi block
  UNICODE_LEFT_TO_RIGHT_MARK = $200E;
  UNICODE_RIGHT_TO_LEFT_MARK = $200F;
  UNICODE_ARABIC_LETTER_MARK = $061C;

  //data separators
  UNICODE_INFORMATION_SEPARATOR_FOUR = $001C;   //end-of-file
  UNICODE_INFORMATION_SEPARATOR_THREE = $001D;  //section separator
  UNICODE_INFORMATION_SEPARATOR_TWO = $001E;    //record separator, kind of equivalent to paragraph separator
  UNICODE_INFORMATION_SEPARATOR_ONE = $001F;    //field separator, kind of equivalent to Tab

  //zero-width
  UNICODE_ZERO_WIDTH_SPACE = $200B;
  UNICODE_ZERO_WIDTH_NON_JOINER = $200C;
  UNICODE_ZERO_WIDTH_NO_BREAK_SPACE = $FEFF;   //byte order mark
  UNICODE_ZERO_WIDTH_JOINER = $200D;
  UNICODE_COMBINING_GRAPHEME_JOINER = $034F;

  //arabic letters
  UNICODE_ARABIC_TATWEEL = $0640;    //horizontal line that makes a ligature with most letters

  //ideographic punctuation
  UNICODE_IDEOGRAPHIC_COMMA = $3001;
  UNICODE_IDEOGRAPHIC_FULL_STOP = $3002;
  UNICODE_FULLWIDTH_COMMA = $FF0C;
  UNICODE_HORIZONTAL_ELLIPSIS = $2026;

  //bracket equivalence
  UNICODE_RIGHT_POINTING_ANGLE_BRACKET = $232A;
  UNICODE_RIGHT_ANGLE_BRACKET = $3009;

type //bracket matching
  TUnicodeBracketInfo = record
    IsBracket: boolean;
    OpeningBracket,ClosingBracket: LongWord;
  end;

{ Returns the Bidi class as defined by Unicode used to determine text direction }
function GetUnicodeBidiClass(u: LongWord): TUnicodeBidiClass;
{ Same as above but returns additional classes: ubcCombiningLeftToRight and ubcMirroredNeutral }
function GetUnicodeBidiClassEx(u: LongWord): TUnicodeBidiClass;
function GetUnicodeBracketInfo(u: LongWord): TUnicodeBracketInfo;
{ Returns how the letter can be joined to the surrounding letters (for example in arabic) }
function GetUnicodeJoiningType(u: LongWord): TUnicodeJoiningType;
{ Returns the Combining class defined by unicode for non-spacing marks and combining marks
  or 255 if the character is not to be combined }
function GetUnicodeCombiningClass(u: LongWord): byte;
function IsZeroWidthUnicode(u: LongWord): boolean;
{ Returns if the symbol can be mirrored horizontally for right-to-left text }
function IsUnicodeMirrored(u: LongWord): boolean;
function IsUnicodeParagraphSeparator(u: LongWord): boolean;
function IsUnicodeCrLf(u: LongWord): boolean;
function IsUnicodeSpace(u: LongWord): boolean;
function IsUnicodeIsolateOrFormatting(u: LongWord): boolean;
function IsModifierCombiningMark(u: LongWord): boolean;

{ Analyze unicode and return bidi levels for each character.
  baseDirection can be either UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE or UNICODE_FIRST_STRONG_ISOLATE }
function AnalyzeBidiUnicode(u: PLongWord; ALength: integer; baseDirection: LongWord): TUnicodeBidiArray;
function AnalyzeBidiUnicode(u: PLongWord; ALength: integer; ABidiMode: TFontBidiMode): TUnicodeBidiArray;

{ Determine diplay order, provided the display surface is horizontally infinite }
function GetUnicodeDisplayOrder(const AInfo: TUnicodeBidiArray): TUnicodeDisplayOrder; overload;
function GetUnicodeDisplayOrder(ALevels: PByte; ACount: integer): TUnicodeDisplayOrder; overload;
function GetUnicodeDisplayOrder(ABidiInfo: PUnicodeBidiInfo; AStride, ACount: integer): TUnicodeDisplayOrder; overload;

Implementation
End.
