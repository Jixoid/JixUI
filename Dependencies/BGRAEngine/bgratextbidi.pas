// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATextBidi;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAUTF8, BGRAUnicode, BGRATransform,
  BGRAUnicodeText;

type
  TBrokenLinesChangedEvent = procedure(ASender: TObject; AParagraphIndex: integer;
    ASubBrokenStart, ASubBrokenChangedCountBefore, ASubBrokenChangedCountAfter: integer;
    ASubBrokenTotalCountBefore, ASubBrokenTotalCountAfter: integer) of object;
  TParagraphLayoutSplitEvent = procedure(ASender: TObject; AParagraphIndex: integer;
      ASubBrokenIndex, ACharIndex: integer) of object;

  { TBidiCaretPos }

  TBidiCaretPos = record
    PartIndex: integer;

    Top, Bottom: TPointF;
    RightToLeft: boolean;

    PreviousTop, PreviousBottom: TPointF;
    PreviousRightToLeft: boolean;

    procedure Transform(AMatrix: TAffineMatrix);
  end;

  PPartInfo = ^TPartInfo;

  { TPartInfo }

  TPartInfo = record
         brokenLineIndex: integer;
         startIndex, endIndex: integer;
         bidiLevel: byte;
         modified: bytebool;
         rectF: TRectF;
         posCorrection: TPointF;
         function IsRightToLeft: boolean;
       end;

  PBrokenLineInfo = ^TBrokenLineInfo;

  { TBrokenLineInfo }

  TBrokenLineInfo = record
                unbrokenLineIndex: integer;
                startIndex, endIndex: integer;
                bidiLevel: byte;
                rectF: TRectF;
                usedWidth: single;
                firstPartIndex: integer;
                parts: array of TPartInfo;
                partCount: integer;
                function IsRightToLeft: boolean;
              end;

  PParagraphInfo = ^TParagraphInfo;
  TParagraphInfo = record
    alignment: TBidiTextAlignment;
    layoutComputed, overflow: boolean;
    rectF: TRectF;
    firstBrokenLineIndex: integer;
    firstPartIndex: integer;
    brokenLines: array of TBrokenLineInfo;
    brokenLineCount: integer;
  end;

  TBidiTextLayout = class;

  { TPartEnumerator }

  TPartEnumerator = record
  private
    FJustCreated: boolean;
    FParagraphIndex: integer;
    FBrokenLineIndex: integer;
    FPartIndex: integer;
    FLayout: TBidiTextLayout;
    FSubBrokenIndex: integer;
    FSubBrokenCount: integer;
    FCurBroken: PBrokenLineInfo;
    FSubPartIndex: integer;
    FSubPartCount: integer;
    FEndPartIndex: integer;
    function GetPartInfo: PPartInfo;
    procedure Update;
  public
    class function New(ALayout: TBidiTextLayout; AParagraphIndex: integer;
      ASubBrokenIndex: integer; ASubPartIndex: integer; AEndPartIndex: integer): TPartEnumerator; static;
    function GetNext: boolean;
    property Layout: TBidiTextLayout read FLayout;
    property ParagraphIndex: integer read FParagraphIndex;
    property BrokenLineIndex: integer read FBrokenLineIndex;
    property PartIndex: integer read FPartIndex;
    property PartInfo: PPartInfo read GetPartInfo;
    property BrokenLineInfo: PBrokenLineInfo read FCurBroken;
  end;

  { TBidiTextLayout }

  TBidiTextLayout = class
  private
    FAvailableHeight: single;
    FAvailableWidth: single;
    FClipMargin: integer;
    FOnBrokenLinesChanged: TBrokenLinesChangedEvent;
    FOnParagraphChanged: TParagraphEvent;
    FOnParagraphDeleted: TParagraphEvent;
    FOnParagraphMergedWithNext: TParagraphEvent;
    FOnParagraphSplit: TParagraphLayoutSplitEvent;
    FOnParagraphVerticalTrimChanged: TParagraphEvent;
    FParagraphSpacingAbove: single;
    FParagraphSpacingBelow: single;
    FTopLeft: TPointF;
    FMatrix, FMatrixInverse: TAffineMatrix;
    FTabSize: Single;
    FWordBreakHandler: TWordBreakHandler;
    function GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
    function GetBrokenLineCount: integer;
    function GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndPart(AIndex: integer): integer;
    function GetBrokenLineStartPart(AIndex: integer): integer;
    function GetBrokenLineUntransformedEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndIndex(AIndex: integer): integer;
    function GetBrokenLineParagraphIndex(AIndex: integer): integer;
    function GetBrokenLineUnbrokenIndex(AIndex: integer): integer;
    function GetBrokenLineInfo(AIndex: integer): PBrokenLineInfo;
    function GetBrokenLineRectF(AIndex: integer): TRectF;
    function GetBrokenLineRightToLeft(AIndex: integer): boolean;
    function GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineUntransformedStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineStartIndex(AIndex: integer): integer;
    function GetBrokenLineUsedWidth(AIndex: integer): single;
    function GetCharCount: integer;
    function GetFontBidiMode: TFontBidiMode;
    function GetLayoutComputed: boolean;
    function GetLineHeight: single;
    function GetMatrix: TAffineMatrix;
    function GetMatrixInverse: TAffineMatrix;
    function GetParagraphAffineBox(AIndex: integer): TAffineBox;
    function GetParagraphAlignment(AIndex: integer): TBidiTextAlignment;
    function GetParagraphCount: integer;
    function GetParagraphEndBrokenLine(AIndex: integer): integer;
    function GetParagraphEndIndex(AIndex: integer): integer;
    function GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
    function GetParagraphEndPart(AIndex: integer): integer;
    function GetParagraphInfo(AIndex: integer): PParagraphInfo;
    function GetParagraphRectF(AIndex: integer): TRectF;
    function GetParagraphRightToLeft(AIndex: integer): boolean;
    function GetParagraphStartBrokenLine(AIndex: integer): integer;
    function GetParagraphStartIndex(AIndex: integer): integer;
    function GetParagraphStartPart(AIndex: integer): integer;
    function GetPartAffineBox(AIndex: integer): TAffineBox;
    function GetPartBrokenLineIndex(AIndex: integer): integer;
    function GetPartCount: integer;
    function GetPartEnumerator(AFirstPart: integer): TPartEnumerator;
    function GetPartEnumerator(AFirstPart, ALastPartPlus1: integer): TPartEnumerator;
    function GetPartInfo(AIndex: integer): PPartInfo;
    function GetPartEndIndex(AIndex: integer): integer;
    function GetPartRectF(AIndex: integer): TRectF;
    function GetPartRightToLeft(AIndex: integer): boolean;
    function GetPartStartIndex(AIndex: integer): integer;
    function GetText: string;
    function GetTotalTextHeight: single;
    function GetUnicodeChar(APosition0: integer): LongWord;
    function GetUsedWidth: single;
    function GetUTF8Char(APosition0: integer): string4;
    procedure SetAvailableHeight(AValue: single);
    procedure SetAvailableWidth(AValue: single);
    procedure SetFontBidiMode(AValue: TFontBidiMode);
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer);
    procedure SetParagraphAlignment(AIndex: integer; AValue: TBidiTextAlignment);
    procedure SetParagraphSpacingAbove(AValue: single);
    procedure SetParagraphSpacingBelow(AValue: single);
    procedure SetTabSize(AValue: single);
    procedure SetTopLeft(AValue: TPointF);
    procedure ComputeMatrix;
  protected
    FAnalysis: TUnicodeAnalysis;
    FRenderer: TBGRACustomFontRenderer;
    FLineHeight: single;

    FParagraph: array of TParagraphInfo;
    FComputedBrokenLineCount: integer;
    FComputedPartCount: integer;

    FColor: TBGRAPixel;
    FTexture: IBGRAScanner;

    function TextSizeBidiOverride(sUTF8: string; ARightToLeft: boolean): TPointF;
    function TextSizeBidiOverrideSplit(AStartIndex, AEndIndex: integer; ARightToLeft: boolean; ASplitIndex: integer): TPointF;
    function TextFitInfoBidiOverride(sUTF8: string; AWidth: single; ARightToLeft: boolean): integer;
    function GetFontFullHeight: single;
    function GetFontBaseline: single;
    function GetFontOrientation: single;
    procedure TextOutBidiOverride(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; ARightToLeft: boolean);
    procedure TextPathBidiOverride(ADest: IBGRAPath; x, y: single; sUTF8: string; ARightToLeft: boolean);

    procedure AddPart(AStartIndex, AEndIndex: integer; ABidiLevel: byte; ARectF: TRectF; APosCorrection: TPointF; ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
    function GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartStartCaret(APartIndex: integer; APrevPart, APart: PPartInfo): TBidiCaretPos;
    function GetUntransformedPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartEndCaret(APartIndex: integer; APart: PPartInfo): TBidiCaretPos;
    function GetUntransformedParagraphAt(APosition: TPointF): integer; overload;

    function GetSameLevelString(startIndex,endIndex: integer): string; overload;
    function GetSameLevelString(startIndex,endIndex: integer; out nonDiscardedCount: integer): string; overload;
    function ComputeBidiTree(AMaxWidth: single; startIndex, endIndex: integer; bidiLevel: byte): TBidiTree;
    procedure AddPartsFromTree(APos: TPointF; ATree: TBidiTree; fullHeight, baseLine: single; ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
    procedure Init(ATextUTF8: string; ABidiMode: TFontBidiMode); virtual;
    procedure ComputeLayout; virtual;
    procedure CheckTextLayout;
    procedure NeedLayout;
    procedure InvalidateParagraphLayout(AParagraphIndex: integer);
    procedure InternalInvalidateParagraphLayout(AParagraphIndex: integer);
    procedure OffsetParagraph(AParagraphIndex: integer; ADeltaY: single; ADeltaBroken, ADeltaPart: integer);
    procedure OffsetParagraphCharIndex(AParagraphIndex: integer; ADeltaChar: integer);
    procedure TrimParagraphLayoutVertically(AParagraphIndex: integer);
    procedure InternalDrawText(ADest: TBGRACustomBitmap);
    procedure InternalPathText(ADest: IBGRAPath); overload;
    procedure InternalPathText(ADest: IBGRAPath; AClipRect: TRect); overload;
    procedure InternalDrawTextParts(ADest: TBGRACustomBitmap; AFirstPart, ALastPartPlus1: integer);
    procedure InternalPathTextParts(ADest: IBGRAPath; AFirstPart, ALastPartPlus1: integer); overload;
    procedure InternalPathTextParts(ADest: IBGRAPath; AClipRect: TRect; AFirstPart, ALastPartPlus1: integer); overload;
    procedure InternalRangeError;

    //unicode analysis events
    procedure BidiModeChanged({%H-}ASender: TObject);
    procedure CharDeleted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure CharInserted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure AnalysisChanged({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure ParagraphDeleted({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphMergedWithNext({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphSplit({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharIndex: integer);
    procedure InternalParagraphDeleted(AParagraphIndex: integer);
    property LayoutComputed: boolean read GetLayoutComputed;
  public
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; AFontBidiMode: TFontBidiMode); overload;
    destructor Destroy; override;
    procedure SetLayout(ARect: TRectF);
    procedure InvalidateLayout;
    procedure ComputeLayoutIfNeeded;
    function AddOverrideIfNecessary(var sUTF8: string; ARightToLeft: boolean): boolean;
    function GetTextPart(APartIndex: integer; AAddOverrideIfNecessary: boolean): string;

    procedure DrawText(ADest: TBGRACustomBitmap); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; AColor: TBGRAPixel); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner); overload;
    procedure PathText(ADest: IBGRAPath);
    procedure PathText(ADest: IBGRAPath; AClipRect: TRect);
    procedure DrawTextParts(ADest: TBGRACustomBitmap; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawTextParts(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawTextParts(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstPart, ALastPartPlus1: integer); overload;
    procedure PathTextParts(ADest: IBGRAPath; AFirstPart, ALastPartPlus1: integer); overload;
    procedure PathTextParts(ADest: IBGRAPath; AClipRect: TRect; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstPara, ALastParaPlus1: integer); overload;
    procedure PathParagraphs(ADest: IBGRAPath; AFirstPara, ALastParaPlus1: integer); overload;
    procedure PathParagraphs(ADest: IBGRAPath; AClipRect: TRect; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure PathBrokenLines(ADest: IBGRAPath; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure PathBrokenLines(ADest: IBGRAPath; AClipRect: TRect; AFirstBroken, ALastBrokenPlus1: integer); overload;

    procedure DrawCaret(ADest: TBGRACustomBitmap; ACharIndex: integer; AMainColor, ASecondaryColor: TBGRAPixel);
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel; ABorderColor: TBGRAPixel; APenWidth: single); overload;
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel); overload;

    function GetCaret(ACharIndex: integer): TBidiCaretPos;
    function GetUntransformedCaret(ACharIndex: integer): TBidiCaretPos;
    function GetCharIndexAt(APosition: TPointF; ABetweenGlyphs: boolean = true): integer;
    function GetTextEnveloppe(AStartIndex, AEndIndex: integer; APixelCenteredCoordinates: boolean = true; AMergeBoxes: boolean = true; AVerticalClip: boolean = false): ArrayOfTPointF;
    function GetUntransformedTextEnveloppe(AStartIndex, AEndIndex: integer; APixelCenteredCoordinates: boolean = true; AMergeBoxes: boolean = true; AVerticalClip: boolean = false): ArrayOfTPointF;
    function GetParagraphAt(ACharIndex: Integer): integer; overload;
    function GetParagraphAt(APosition: TPointF): integer; overload;
    function GetBrokenLineAt(ACharIndex: integer): integer;

    function InsertText(ATextUTF8: string; APosition: integer): integer;
    function InsertLineSeparator(APosition: integer): integer;
    function DeleteText(APosition, ACount: integer): integer;
    function DeleteTextBefore(APosition, ACount: integer): integer;
    function CopyText(APosition, ACount: integer): string;
    function CopyTextBefore(APosition, ACount: integer): string;
    function IncludeNonSpacingChars(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function IncludeNonSpacingCharsBefore(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function FindTextAbove(AFromPosition: integer): integer;
    function FindTextBelow(AFromPosition: integer): integer;

    property CharCount: integer read GetCharCount;
    property UTF8Char[APosition0: integer]: string4 read GetUTF8Char;
    property UnicodeChar[APosition0: integer]: LongWord read GetUnicodeChar;

    property BrokenLineCount: integer read GetBrokenLineCount;
    property BrokenLineParagraphIndex[AIndex: integer]: integer read GetBrokenLineParagraphIndex;
    property BrokenLineUnbrokenIndex[AIndex: integer]: integer read GetBrokenLineUnbrokenIndex;
    property BrokenLineStartIndex[AIndex: integer]: integer read GetBrokenLineStartIndex;
    property BrokenLineEndIndex[AIndex: integer]: integer read GetBrokenLineEndIndex;
    property BrokenLineStartPart[AIndex: integer]: integer read GetBrokenLineStartPart;
    property BrokenLineEndPart[AIndex: integer]: integer read GetBrokenLineEndPart;
    property BrokenLineRectF[AIndex: integer]: TRectF read GetBrokenLineRectF;
    property BrokenLineUsedWidth[AIndex: integer]: single read GetBrokenLineUsedWidth;
    property BrokenLineAffineBox[AIndex: integer]: TAffineBox read GetBrokenLineAffineBox;
    property BrokenLineRightToLeft[AIndex: integer]: boolean read GetBrokenLineRightToLeft;
    property BrokenLineStartCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineStartCaret;
    property BrokenLineEndCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineEndCaret;
    property OnBrokenLinesChanged: TBrokenLinesChangedEvent read FOnBrokenLinesChanged write FOnBrokenLinesChanged;

    property PartCount: integer read GetPartCount;
    property PartStartIndex[AIndex: integer]: integer read GetPartStartIndex;
    property PartEndIndex[AIndex: integer]: integer read GetPartEndIndex;
    property PartBrokenLineIndex[AIndex: integer]: integer read GetPartBrokenLineIndex;
    property PartStartCaret[AIndex: integer]: TBidiCaretPos read GetPartStartCaret;
    property PartEndCaret[AIndex: integer]: TBidiCaretPos read GetPartEndCaret;
    property PartRectF[AIndex: integer]: TRectF read GetPartRectF;
    property PartAffineBox[AIndex: integer]: TAffineBox read GetPartAffineBox;
    property PartRightToLeft[AIndex: integer]: boolean read GetPartRightToLeft;

    property TopLeft: TPointF read FTopLeft write SetTopLeft;
    property AvailableWidth: single read FAvailableWidth write SetAvailableWidth;
    property AvailableHeight: single read FAvailableHeight write SetAvailableHeight;
    property TabSize: single read FTabSize write SetTabSize;
    property ParagraphSpacingAbove: single read FParagraphSpacingAbove write SetParagraphSpacingAbove;
    property ParagraphSpacingBelow: single read FParagraphSpacingBelow write SetParagraphSpacingBelow;
    property ParagraphRectF[AIndex: integer]: TRectF read GetParagraphRectF;
    property ParagraphAffineBox[AIndex: integer]: TAffineBox read GetParagraphAffineBox;
    property ParagraphAlignment[AIndex: integer]: TBidiTextAlignment read GetParagraphAlignment write SetParagraphAlignment;
    property ParagraphStartIndex[AIndex: integer]: integer read GetParagraphStartIndex;
    property ParagraphEndIndex[AIndex: integer]: integer read GetParagraphEndIndex;
    property ParagraphEndIndexBeforeParagraphSeparator[AIndex: integer]: integer read GetParagraphEndIndexBeforeParagraphSeparator;
    property ParagraphRightToLeft[AIndex: integer]: boolean read GetParagraphRightToLeft;
    property ParagraphStartPart[AIndex: integer]: integer read GetParagraphStartPart;
    property ParagraphEndPart[AIndex: integer]: integer read GetParagraphEndPart;
    property ParagraphStartBrokenLine[AIndex: integer]: integer read GetParagraphStartBrokenLine;
    property ParagraphEndBrokenLine[AIndex: integer]: integer read GetParagraphEndBrokenLine;
    property ParagraphCount: integer read GetParagraphCount;
    property OnParagraphDeleted : TParagraphEvent read FOnParagraphDeleted write FOnParagraphDeleted;
    property OnParagraphMergedWithNext: TParagraphEvent read FOnParagraphMergedWithNext write FOnParagraphMergedWithNext;
    property OnParagraphSplit: TParagraphLayoutSplitEvent read FOnParagraphSplit write FOnParagraphSplit;
    property OnParagraphChanged: TParagraphEvent read FOnParagraphChanged write FOnParagraphChanged;
    property OnParagraphVerticalTrimChanged: TParagraphEvent read FOnParagraphVerticalTrimChanged write FOnParagraphVerticalTrimChanged;

    property UsedWidth: single read GetUsedWidth;
    property TotalTextHeight: single read GetTotalTextHeight;
    property LineHeight: single read GetLineHeight;

    property Matrix: TAffineMatrix read GetMatrix;
    property MatrixInverse: TAffineMatrix read GetMatrixInverse;
    property TextUTF8: string read GetText;
    property WordBreakHandler: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property ClipMargin: integer read FClipMargin write FClipMargin; // how many pixels can the text go outside of its box

    property FontRenderer: TBGRACustomFontRenderer read FRenderer write SetFontRenderer;
    property FontBidiMode: TFontBidiMode read GetFontBidiMode write SetFontBidiMode;
  end;

  { TBidiLayoutTree }

  TBidiLayoutTree = class(TBidiTree)
  private
    FBidiPos: single;
    FSize: TPointF;
    FTextUTF8: string;
    FNonDiscardedCount: integer;
    FLayout: TBidiTextLayout;
    FMaxWidth: single;
    function GetCumulatedBidiPos: single;
    function GetHeight: single;
    function GetLayout: TBidiTextLayout;
    function GetMaxWidth: single;
    function GetWidth: single;
    procedure UpdateBranchSize;
  public
    constructor Create(AData: pointer; AStartIndex, AEndIndex: integer; ABidiLevel: byte; AIsLeaf: boolean); override;
    procedure AddBranch(ABranch: TBidiTree); override;
    procedure Shorten(AEndIndex: integer); override;
    procedure AfterFinish; override;
    function TrySplit: boolean; override;
    property Layout: TBidiTextLayout read GetLayout;
    property MaxWidth: single read GetMaxWidth;
    property BidiPos: single read FBidiPos;
    property CumulatedBidiPos: single read GetCumulatedBidiPos;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
  end;

  TBidiLayoutTreeData = record
    Layout: TBidiTextLayout;
    MaxWidth: single;
  end;

Implementation
End.
