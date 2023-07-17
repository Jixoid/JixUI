// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATypewriter;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
{$WARN 5092 off : Variable "$1" of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, Avl_Tree, BGRABitmapTypes, BGRACanvas2D, BGRATransform;

type
  TGlyphBoxes = array of record
    Glyph: string;
    Box: TAffineBox;
  end;

  { TBGRAGlyph }

  TBGRAGlyph = class
  protected
    FIdentifier: string;
    procedure WriteHeader(AStream: TStream; AName: string; AContentSize: longint);
    class procedure ReadHeader(AStream: TStream; out AName: string; out AContentSize: longint); static;
    function ContentSize: integer; virtual;
    function HeaderName: string; virtual;
    procedure WriteContent(AStream: TStream); virtual;
    procedure ReadContent(AStream: TStream); virtual;
  public
    Width,Height: single;
    constructor Create(AIdentifier: string); virtual;
    constructor Create(AStream: TStream); virtual;
    procedure Path({%H-}ADest: IBGRAPath; {%H-}AMatrix: TAffineMatrix; {%H-}AReverse: boolean= false); virtual;
    property Identifier: string read FIdentifier;
    procedure SaveToStream(AStream: TStream);
    class function LoadFromStream(AStream: TStream): TBGRAGlyph; static;
  end;

  TKerningInfo = class
    IdLeft, IdRight: string;
    KerningOffset: single;
  end;

  TGlyphPointCurveMode= TEasyBezierCurveMode;

const
  cmAuto = TEasyBezierCurveMode.cmAuto;
  cmCurve = TEasyBezierCurveMode.cmCurve;
  cmAngle = TEasyBezierCurveMode.cmAngle;

type
  { TBGRAPolygonalGlyph }

  TBGRAPolygonalGlyph = class(TBGRAGlyph)
  private
    function GetClosed: boolean;
    function GetMinimumDotProduct: single;
    function GetPoint(AIndex: integer): TPointF;
    function GetPointCount: integer;
    procedure SetClosed(AValue: boolean);
    procedure SetMinimumDotProduct(AValue: single);
    procedure SetPoint(AIndex: integer; AValue: TPointF);
    procedure SetQuadraticCurves(AValue: boolean);
  protected
    FQuadraticCurves: boolean;
    FEasyBezier: TEasyBezierCurve;
    function ContentSize: integer; override;
    function HeaderName: string; override;
    procedure WriteContent(AStream: TStream); override;
    procedure ReadContent(AStream: TStream); override;
    function PointTransformMatrix(APoint: PPointF; AData: pointer): TPointF;
    procedure Init;
  public
    Offset: TPointF;
    constructor Create(AIdentifier: string); override;
    constructor Create(AStream: TStream); override;
    constructor Create(AStream: TStream; AQuadratic: boolean);
    procedure SetPoints(const APoints: array of TPointF); overload;
    procedure SetPoints(const APoints: array of TPointF; const ACurveMode: array of TGlyphPointCurveMode); overload;
    procedure Path(ADest: IBGRAPath; AMatrix: TAffineMatrix; AReverse: boolean = false); override;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
    property Closed: boolean read GetClosed write SetClosed;
    property MinimumDotProduct: single read GetMinimumDotProduct write SetMinimumDotProduct;
    property Point[AIndex: integer]: TPointF read GetPoint write SetPoint;
    property PointCount: integer read GetPointCount;
  end;

  TBGRACustomTypeWriterHeader = record
    HeaderName: String;
    NbGlyphs: integer;
  end;

  TBGRAGlyphDisplayInfo = record
    Glyph: TBGRAGlyph;
    Matrix: TAffineMatrix;
    Mirrored, RTL: WordBool;
  end;

  TBGRATextDisplayInfo = array of TBGRAGlyphDisplayInfo;
  TBrowseGlyphCallbackFlag = (gcfMirrored, gcfMerged, gcfRightToLeft, gcfKerning);
  TBrowseGlyphCallbackFlags = set of TBrowseGlyphCallbackFlag;
  TBrowseGlyphCallback = procedure(ATextUTF8: string; AGlyph: TBGRAGlyph;
    AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean) of object;

  TTextFitInfoCallbackData = record
    WidthAccumulator, MaxWidth: single;
    CharCount: integer;
    ByteCount: integer;
    PrevGlyphId: string;
  end;

  TDisplayInfoCallbackData = record
    Align: TBGRATypeWriterAlignment;
    Matrix: TAffineMatrix;
    Info: TBGRATextDisplayInfo;
    InfoIndex: integer;
    PrevGlyphId: string;
  end;

  TTextSizeCallbackData = record
    Size: TPointF;
    PrevGlyphId: string;
  end;

  { TBGRACustomTypeWriter }

  TBGRACustomTypeWriter = class
  private
    FBidiMode: TFontBidiMode;
    FGlyphs: TAVLTree;
    FKerningInfos: TAVLTree;
    procedure GlyphCallbackForDisplayInfo({%H-}ATextUTF8: string;
      AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out
      AContinue: boolean);
    procedure GlyphCallbackForTextFitInfoBeforeTransform(ATextUTF8: string;
      AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
    procedure SetBidiMode(AValue: TFontBidiMode);
    procedure GlyphCallbackForTextSizeBeforeTransform({%H-}ATextUTF8: string;
      AGlyph: TBGRAGlyph; {%H-}AFlags: TBrowseGlyphCallbackFlags; AData: pointer; out AContinue: boolean);
  protected
    TypeWriterMatrix: TAffineMatrix;
    function FindGlyph(AIdentifier: string): TAVLTreeNode;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; virtual;
    procedure SetGlyph(AIdentifier: string; AValue: TBGRAGlyph);
    function GetDisplayInfo(ATextUTF8: string; X,Y: Single;
                  AAlign: TBGRATypeWriterAlignment): TBGRATextDisplayInfo;
    procedure GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft; AMirrored: boolean = false);
    procedure DrawLastPath(ADest: TBGRACanvas2D);
    procedure ClearGlyphs;
    procedure RemoveGlyph(AIdentifier: string);
    procedure AddGlyph(AGlyph: TBGRAGlyph);
    function GetGlyphMatrix(AGlyph: TBGRAGlyph; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    function GetTextMatrix(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    property Glyph[AIdentifier: string]: TBGRAGlyph read GetGlyph write SetGlyph;
    function CustomHeaderSize: integer; virtual;
    procedure WriteCustomHeader(AStream: TStream); virtual;
    function ReadCustomTypeWriterHeader(AStream: TStream): TBGRACustomTypeWriterHeader;
    procedure ReadAdditionalHeader({%H-}AStream: TStream); virtual;
    function HeaderName: string; virtual;
    procedure BrowseGlyphs(ATextUTF8: string; ACallback: TBrowseGlyphCallback; AData: pointer; ADisplayOrder: boolean);
    procedure BrowseAllGlyphs(ACallback: TBrowseGlyphCallback; AData: pointer);
    function FindKerning(AIdLeft, AIdRight: string): TAVLTreeNode;
    function GetKerningOffset(AIdBefore, AIdAfter: string; ARightToLeft: boolean): single; virtual;
    function ComputeKerning(AIdLeft, AIdRight: string): single; virtual;
  public
    OutlineMode: TBGRATypeWriterOutlineMode;
    DrawGlyphsSimultaneously : boolean;
    SubstituteBidiBracket: boolean;
    LigatureWithF: boolean;
    constructor Create;
    function GetTextSizeBeforeTransform(ATextUTF8 :string): TPointF;
    procedure TextFitInfoBeforeTransform(ATextUTF8: string; AMaxWidth: single; out ACharCount, AByteCount: integer; out AUsedWidth: single);
    procedure SaveGlyphsToFile(AFilenameUTF8: string);
    procedure SaveGlyphsToStream(AStream: TStream);
    procedure LoadGlyphsFromFile(AFilenameUTF8: string);
    procedure LoadGlyphsFromStream(AStream: TStream);
    procedure DrawGlyph(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft; AMirrored: boolean = false);
    procedure DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    procedure CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    function GetGlyphBox(AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextBox(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextGlyphBoxes(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    procedure NeedGlyphRange(AUnicodeFrom, AUnicodeTo: LongWord);
    procedure NeedGlyphAnsiRange;
    destructor Destroy; override;
    property BidiMode: TFontBidiMode read FBidiMode write SetBidiMode;
  end;

function ComputeEasyBezier(APoints: array of TPointF; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;
function ComputeEasyBezier(APoints: array of TPointF; ACurveMode: array of TGlyphPointCurveMode; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;

Implementation
End.
