// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUnicodeText;

{$mode objfpc}{$H+}
{ $DEFINE DEBUG}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAUnicode, BGRAUTF8;

type
  TDeleteCharEvent = procedure(ASender: TObject; AParagraphIndex: integer; ACharStart, ACharCount: integer) of object;
  TInsertCharEvent = procedure(ASender: TObject; AParagraphIndex: integer; ACharStart, ACharCount: integer) of object;
  TParagraphEvent = procedure(ASender: TObject; AParagraphIndex: integer) of object;
  TParagraphSplitEvent = procedure(ASender: TObject; AParagraphIndex: integer; ACharIndex: integer) of object;
  TAnalysisChangedEvent = procedure(ASender: TObject; AParagraphIndex: integer; ACharStart, ACharCount: integer) of object;

  { TBidiTree }

  TBidiTree = class
  private
    FParent: TBidiTree;
    FData: pointer;
    FStartIndex, FEndIndex: integer;
    FBidiLevel: byte;
    FBranches: TList;
    FIsLeaf: boolean;
    function GetBranch(AIndex: integer): TBidiTree;
    function GetCount: integer;
    function GetIsRightToLeft: boolean;
  public
    constructor Create(AData: pointer; AStartIndex, AEndIndex: integer; ABidiLevel: byte; AIsLeaf: boolean); virtual;
    destructor Destroy; override;
    procedure AfterFinish; virtual;
    procedure AddBranch(ABranch: TBidiTree); virtual;
    procedure Shorten(AEndIndex: integer); virtual;
    function TrySplit: boolean; virtual;
    property StartIndex: integer read FStartIndex;
    property EndIndex: integer read FEndIndex;
    property BidiLevel: byte read FBidiLevel;
    property IsRightToLeft: boolean read GetIsRightToLeft;
    property Parent: TBidiTree read FParent;
    property IsLeaf: boolean read FIsLeaf;
    property Count: integer read GetCount;
    property Branch[AIndex: integer]: TBidiTree read GetBranch;
    property Data: pointer read FData;
  end;

  TBidiTreeAny = class of TBidiTree;

  { TUnicodeAnalysis }

  TUnicodeAnalysis = class
  private
    FOnAnalysisChanged: TAnalysisChangedEvent;
    FOnBidiModeChanged: TNotifyEvent;
    FOnCharDeleted: TDeleteCharEvent;
    FOnCharInserted: TInsertCharEvent;
    FOnParagraphMergedWithNext: TParagraphEvent;
    FOnParagraphDeleted: TParagraphEvent;
    FOnParagraphSplit: TParagraphSplitEvent;
    function GetParagraphRightToLeft(AIndex: integer): boolean;
  protected
    FText: string;
    FCharCount: integer;
    FBidi: TBidiUTF8Array;
    FParagraph: array of record
      firstUnbrokenLineIndex: integer;
    end;
    FParagraphCount: integer;

    FUnbrokenLine: array of record
      startIndex: integer;
      paragraphIndex: integer;
    end;
    FUnbrokenLineCount: integer;

    FBidiMode: TFontBidiMode;
    procedure Analyze;
    procedure CheckTextAnalysis;
    function GetUnbrokenLineParagraphIndex(AIndex: integer): integer;
    function GetUnbrokenLineStartIndex(AIndex: integer): integer;
    function GetUnbrokenLineEndIndex(AIndex: integer): integer;
    function GetParagraphEndIndex(AIndex: integer): integer;
    function GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
    function GetParagraphFirstUnbrokenLine(AIndex: integer): integer;
    function GetParagraphLastUnbrokenLinePlusOne(AIndex: integer): integer;
    function GetParagraphStartIndex(AIndex: integer): integer;
    function GetUnicodeChar(APosition0: integer): LongWord;
    function GetUTF8Char(APosition0: integer): string4;
    function GetBidiInfo(APosition0: integer): TUnicodeBidiInfo;
    procedure SetBidiMode(AValue: TFontBidiMode);

    procedure InternalDeleteBidiAndUTF8(ABidiStart, ABidiCount: integer);
    procedure InternalDeleteParagraph(AParagraphIndex: integer);
    procedure InternalDeleteText(APosition, ACount: integer);
    procedure InternalDeleteWithinParagraph(AParagraphIndex: integer;
      APosition, ACount: integer; AUpdateBidi: boolean);
    function InternalInsertText(APosition: integer;
      const ANewBidi: TBidiUTF8Array; ANewTextUTF8: string): integer;
    procedure InternalMergeParagraphWithNext(AParagraphIndex: integer);
    procedure InternalSplitParagraph(AParagraphIndex: integer);
    procedure InternalUpdateBidiIsolate(AParagraphIndex: integer; ABidiStart, ABidiCount: integer);
    procedure InternalUpdateUnbrokenLines(AParagraphIndex: integer);
    procedure CreateBidiTreeRec(ABidiTreeFactory: TBidiTreeAny; AData: pointer; ABidiTree: TBidiTree);
    procedure CheckCharRange(AStartIndex, AEndIndex: integer; AMinIndex, AMaxIndex: integer);
  public
    constructor Create(ATextUTF8: string; ABidiMode: TFontBidiMode);
    function GetParagraphAt(ACharIndex: integer): integer;
    function CopyTextUTF8(AStartIndex, ACount: integer): string;
    function CopyTextUTF8DiscardChars(AStartIndex,AEndIndex: integer; out ANonDiscardedCount: integer): string;
    function InsertText(ATextUTF8: string; APosition: integer): integer;
    function DeleteText(APosition, ACount: integer): integer;
    function DeleteTextBefore(APosition, ACount: integer): integer;
    function IncludeNonSpacingChars(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function IncludeNonSpacingCharsBefore(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function CreateBidiTree(ABidiTreeFactory: TBidiTreeAny; AData: pointer; AStartIndex, AEndIndex: integer;
                            AEmbeddingBidiLevel: integer): TBidiTree;

    property TextUTF8: string read FText;
    property UTF8Char[APosition0: integer]: string4 read GetUTF8Char;
    property UnicodeChar[APosition0: integer]: LongWord read GetUnicodeChar;
    property BidiInfo[APosition0: integer]: TUnicodeBidiInfo read GetBidiInfo;
    property CharCount: integer read FCharCount;
    property UnbrokenLineCount: integer read FUnbrokenLineCount;
    property UnbrokenLineStartIndex[AIndex: integer]: integer read GetUnbrokenLineStartIndex;
    property UnbrokenLineEndIndex[AIndex: integer]: integer read GetUnbrokenLineEndIndex;
    property UnbrokenLineParagraphIndex[AIndex: integer]: integer read GetUnbrokenLineParagraphIndex;
    property ParagraphCount: integer read FParagraphCount;
    property ParagraphFirstUnbrokenLine[AIndex:integer] : integer read GetParagraphFirstUnbrokenLine;
    property ParagraphLastUnbrokenLinePlusOne[AIndex:integer] : integer read GetParagraphLastUnbrokenLinePlusOne;
    property ParagraphStartIndex[AIndex: integer]: integer read GetParagraphStartIndex;
    property ParagraphEndIndex[AIndex: integer]: integer read GetParagraphEndIndex;
    property ParagraphEndIndexBeforeParagraphSeparator[AIndex: integer]: integer read GetParagraphEndIndexBeforeParagraphSeparator;
    property ParagraphRightToLeft[AIndex: integer]: boolean read GetParagraphRightToLeft;
    property BidiMode: TFontBidiMode read FBidiMode write SetBidiMode;
    property OnBidiModeChanged: TNotifyEvent read FOnBidiModeChanged write FOnBidiModeChanged;
    property OnCharDeleted: TDeleteCharEvent read FOnCharDeleted write FOnCharDeleted;
    property OnCharInserted: TInsertCharEvent read FOnCharInserted write FOnCharInserted;
    property OnParagraphDeleted: TParagraphEvent read FOnParagraphDeleted write FOnParagraphDeleted;
    property OnParagraphMergedWithNext: TParagraphEvent read FOnParagraphMergedWithNext write FOnParagraphMergedWithNext;
    property OnParagraphSplit: TParagraphSplitEvent read FOnParagraphSplit write FOnParagraphSplit;
    property OnAnalysisChanged: TAnalysisChangedEvent read FOnAnalysisChanged write FOnAnalysisChanged;
  end;

Implementation
End.
