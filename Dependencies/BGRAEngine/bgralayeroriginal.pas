// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALayerOriginal;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, BGRABitmap, BGRABitmapTypes, BGRATransform, BGRAMemDirectory, fgl
  {$IFDEF BGRABITMAP_USE_LCL},LCLType, LCLIntf{$ENDIF};

type
  PRectF = BGRABitmapTypes.PRectF;
  TAffineMatrix = BGRATransform.TAffineMatrix;
  TBGRALayerCustomOriginal = class;
  TBGRAOriginalDiff = class;
  TBGRALayerOriginalAny = class of TBGRALayerCustomOriginal;
  TOriginalMovePointEvent = procedure(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState) of object;
  TOriginalStartMovePointEvent = procedure(ASender: TObject; AIndex: integer; AShift: TShiftState) of object;
  TOriginalClickPointEvent = procedure(ASender: TObject; AIndex: integer; AShift: TShiftState) of object;
  TOriginalHoverPointEvent = procedure(ASender: TObject; AIndex: integer) of object;
  TOriginalChangeEvent = procedure(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff) of object;
  TOriginalEditingChangeEvent = procedure(ASender: TObject) of object;
  TOriginalEditorCursor = (oecDefault, oecMove, oecMoveW, oecMoveE, oecMoveN, oecMoveS,
                           oecMoveNE, oecMoveSW, oecMoveNW, oecMoveSE, oecHandPoint, oecText);
  TSpecialKey = (skUnknown, skBackspace, skTab, skReturn, skEscape,
                 skPageUp, skPageDown, skHome, skEnd,
                 skLeft, skUp, skRight, skDown,
                 skInsert, skDelete,
                 skNum0, skNum1, skNum2, skNum3, skNum4, skNum5, skNum6, skNum7, skNum8, skNum9,
                 skF1, skF2, skF3, skF4, skF5, skF6, skF7, skF8, skF9, skF10, skF11, skF12,
                 skA, skB, skC, skD, skE, skF, skG, skH, skI, skJ, skK, skL, skM, skN, skO, skP, skQ, skR, skS, skT, skU, skV, skW, skX, skY, skZ,
                 sk0, sk1, sk2, sk3, sk4, sk5, sk6, sk7, sk8, sk9,
                 skShift, skCtrl, skAlt);

const
  SpecialKeyStr: array[TSpecialKey] of string =
    ('Unknown', 'Backspace', 'Tab', 'Return', 'Escape',
     'PageUp', 'PageDown', 'Home', 'End',
     'Left', 'Up', 'Right', 'Down',
     'Insert', 'Delete',
     'Num0', 'Num1', 'Num2', 'Num3', 'Num4', 'Num5', 'Num6', 'Num7', 'Num8', 'Num9',
     'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'Shift', 'Ctrl', 'Alt');

{$IFDEF BGRABITMAP_USE_LCL}
const
  SpecialKeyToLCL: array[TSpecialKey] of Word =
    (VK_UNKNOWN, VK_BACK,VK_TAB,VK_RETURN,VK_ESCAPE,
     VK_PRIOR,VK_NEXT,VK_HOME,VK_END,
     VK_LEFT,VK_UP,VK_RIGHT,VK_DOWN,
     VK_INSERT,VK_DELETE,
     VK_NUMPAD0,VK_NUMPAD1,VK_NUMPAD2,VK_NUMPAD3,VK_NUMPAD4,VK_NUMPAD5,VK_NUMPAD6,VK_NUMPAD7,VK_NUMPAD8,VK_NUMPAD9,
     VK_F1,VK_F2,VK_F3,VK_F4,VK_F5,VK_F6,VK_F7,VK_F8,VK_F9,VK_F10,VK_F11,VK_F12,
     VK_A, VK_B, VK_C, VK_D, VK_E, VK_F, VK_G, VK_H, VK_I, VK_J, VK_K, VK_L, VK_M, VK_N, VK_O, VK_P, VK_Q, VK_R, VK_S, VK_T, VK_U, VK_V, VK_W, VK_X, VK_Y, VK_Z,
     VK_0, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9,
     VK_SHIFT, VK_CONTROL, VK_MENU);

  function LCLKeyToSpecialKey(AKey: Word; AShift: TShiftState): TSpecialKey;
{$ENDIF}

type
  TStartMoveHandlers = specialize TFPGList<TOriginalStartMovePointEvent>;
  TClickPointHandlers = specialize TFPGList<TOriginalClickPointEvent>;
  THoverPointHandlers = specialize TFPGList<TOriginalHoverPointEvent>;
  TBGRAOriginalPolylineStyle = (opsNone, opsSolid, opsDash, opsDashWithShadow);

  { TBGRAOriginalEditor }

  TBGRAOriginalEditor = class
  private
    FFocused: boolean;
    FOnFocusChanged: TNotifyEvent;
    function GetIsMovingPoint: boolean;
    function GetPointCoord(AIndex: integer): TPointF;
    function GetPointCount: integer;
    function GetPointHighlighted(AIndex: integer): boolean;
    procedure SetFocused(AValue: boolean);
    procedure SetPointHighlighted(AIndex: integer; AValue: boolean);
  protected
    FMatrix,FMatrixInverse: TAffineMatrix;          //view matrix from original coord
    FGridMatrix,FGridMatrixInverse: TAffineMatrix;  //grid matrix in original coord
    FGridActive: boolean;
    FPoints: array of record
      Origin, Coord: TPointF;
      OnMove, OnAlternateMove: TOriginalMovePointEvent;
      RightButton, Highlighted: boolean;
      SnapToPoint: integer;
      HitBox: TAffineBox;
    end;
    FPolylines: array of record
      Coords: array of TPointF;
      Closed: boolean;
      Style: TBGRAOriginalPolylineStyle;
      BackColor: TBGRAPixel;
    end;
    FPointSize: single;
    FPointMoving: integer;
    FPointWasMoved: boolean;
    FPointCoordDelta: TPointF;
    FMovingRightButton: boolean;
    FPrevMousePos: TPointF;
    FStartMoveHandlers: TStartMoveHandlers;
    FCurHoverPoint: integer;
    FHoverPointHandlers: THoverPointHandlers;
    FClickPointHandlers: TClickPointHandlers;
    FLastClickTime: TDateTime;
    FLastClickPos: TPointF;
    FDoubleClickTime: TDateTime;
    FConsecutiveClickCount: integer;
    function RenderPoint(ADest: TBGRABitmap; ACoord: TPointF; AAlternateColor: boolean; AHighlighted: boolean): TRect; virtual;
    function GetRenderPointBounds(ACoord: TPointF; AHighlighted: boolean): TRect; virtual;
    function RenderArrow(ADest: TBGRABitmap; AOrigin, AEndCoord: TPointF): TRect; virtual;
    function GetRenderArrowBounds(AOrigin, AEndCoord: TPointF): TRect; virtual;
    function RenderPolygon(ADest: TBGRABitmap; ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): TRect; virtual;
    function GetRenderPolygonBounds(ACoords: array of TPointF): TRect;
    procedure SetMatrix(AValue: TAffineMatrix);
    procedure SetGridMatrix(AValue: TAffineMatrix);
    procedure SetGridActive(AValue: boolean);
    function GetMoveCursor(APointIndex: integer): TOriginalEditorCursor; virtual;
    function GetFixedShiftForButton(AShift: TShiftState; ARightDown: boolean): TShiftState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddStartMoveHandler(AOnStartMove: TOriginalStartMovePointEvent);
    procedure AddClickPointHandler(AOnClickPoint: TOriginalClickPointEvent);
    procedure AddHoverPointHandler(AOnHoverPoint: TOriginalHoverPointEvent);
    function AddPoint(const ACoord: TPointF; AOnMove: TOriginalMovePointEvent; ARightButton: boolean = false; ASnapToPoint: integer = -1): integer;
    procedure AddPointAlternateMove(AIndex: integer; AOnAlternateMove: TOriginalMovePointEvent);
    function AddFixedPoint(const ACoord: TPointF; ARightButton: boolean = false): integer;
    function AddArrow(const AOrigin, AEndCoord: TPointF; AOnMoveEnd: TOriginalMovePointEvent; ARightButton: boolean = false): integer;
    function AddPolyline(const ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle): integer; overload;
    function AddPolyline(const ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): integer; overload;
    procedure SetHitBox(AIndex: integer; AHitBox: TAffineBox);
    procedure MouseMove(Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseUp(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}ViewX, {%H-}ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; out AHandled: boolean); virtual;
    procedure KeyUp({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; out AHandled: boolean); virtual;
    procedure KeyPress({%H-}UTF8Key: string; out AHandled: boolean); virtual;
    function GetPointAt(const ACoord: TPointF; ARightButton: boolean): integer;
    function Render(ADest: TBGRABitmap; const {%H-}ALayoutRect: TRect): TRect; virtual;
    function GetRenderBounds(const {%H-}ALayoutRect: TRect): TRect; virtual;
    function SnapToGrid(const ACoord: TPointF; AIsViewCoord: boolean): TPointF;
    function OriginalCoordToView(const AImageCoord: TPointF): TPointF;
    function ViewCoordToOriginal(const AViewCoord: TPointF): TPointF;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
    property GridMatrix: TAffineMatrix read FGridMatrix write SetGridMatrix;
    property GridActive: boolean read FGridActive write SetGridActive;
    property Focused: boolean read FFocused write SetFocused;
    property PointSize: single read FPointSize write FPointSize;
    property PointCount: integer read GetPointCount;
    property PointCoord[AIndex: integer]: TPointF read GetPointCoord;
    property PointHighlighted[AIndex: integer]: boolean read GetPointHighlighted write SetPointHighlighted;
    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
    property IsMovingPoint: boolean read GetIsMovingPoint;
    property ConsecutiveClickCount: integer read FConsecutiveClickCount;
  end;

  TBGRACustomOriginalStorage = class;
  ArrayOfSingle = array of single;

  TBGRAOriginalDiff = class
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); virtual; abstract;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); virtual; abstract;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; virtual; abstract;
    procedure Append(ADiff: TBGRAOriginalDiff); virtual; abstract;
    function IsIdentity: boolean; virtual; abstract;
  end;

  { TBGRALayerCustomOriginal }

  TBGRALayerCustomOriginal = class
  private
    FOnChange: TOriginalChangeEvent;
    FOnEditingChange: TOriginalEditingChangeEvent;
    FRenderStorage: TBGRACustomOriginalStorage;
    function GetDiffExpected: boolean;
    procedure SetOnChange(AValue: TOriginalChangeEvent);
    procedure SetRenderStorage(AValue: TBGRACustomOriginalStorage);
  protected
    FGuid: TGuid;
    function GetGuid: TGuid;
    procedure SetGuid(AValue: TGuid);
    procedure NotifyChange(ADiff: TBGRAOriginalDiff = nil); overload;
    procedure NotifyChange(ABounds: TRectF; ADiff: TBGRAOriginalDiff = nil); overload;
    procedure NotifyEditorChange;
    property DiffExpected: boolean read GetDiffExpected;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //one of the two Render functions must be overriden
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRect; virtual; abstract;
    procedure ConfigureEditor({%H-}AEditor: TBGRAOriginalEditor); virtual;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure LoadFromResource(AFilename: string);
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    function CreateEditor: TBGRAOriginalEditor; virtual;
    class function StorageClassName: RawByteString; virtual; abstract;
    class function CanConvertToSVG: boolean; virtual;
    function IsInfiniteSurface: boolean; virtual;
    function ConvertToSVG(const {%H-}AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; virtual;
    function Duplicate: TBGRALayerCustomOriginal; virtual;
    property Guid: TGuid read GetGuid write SetGuid;
    property OnChange: TOriginalChangeEvent read FOnChange write SetOnChange;
    property OnEditingChange: TOriginalEditingChangeEvent read FOnEditingChange write FOnEditingChange;
    property RenderStorage: TBGRACustomOriginalStorage read FRenderStorage write SetRenderStorage;
  end;

  TBGRALayerImageOriginal = class;

  { TBGRAImageOriginalDiff }

  TBGRAImageOriginalDiff = class(TBGRAOriginalDiff)
  protected
    FContentVersionBefore,FContentVersionAfter: integer;
    FImageBefore,FImageAfter: TBGRABitmap;
    FJpegStreamBefore,FJpegStreamAfter: TMemoryStream;
  public
    constructor Create(AFromOriginal: TBGRALayerImageOriginal);
    destructor Destroy; override;
    procedure ComputeDiff(AToOriginal: TBGRALayerImageOriginal);
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TBGRALayerImageOriginal }

  TBGRALayerImageOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetImageHeight: integer;
    function GetImageWidth: integer;
  protected
    FImage: TBGRABitmap;
    FJpegStream: TMemoryStream;
    FContentVersion: integer;
    FDiff: TBGRAImageOriginalDiff;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure InternalLoadImageFromStream(AStream: TStream; AUpdate: boolean);
    procedure InternalClear;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Clear;
    procedure LoadImageFromStream(AStream: TStream);
    procedure SaveImageToStream(AStream: TStream);
    procedure AssignImage(AImage: TBGRACustomBitmap);
    function GetImageCopy: TBGRABitmap;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
    property Width: integer read GetImageWidth;
    property Height: integer read GetImageHeight;
  end;

  { TBGRACustomOriginalStorage }

  TBGRACustomOriginalStorage = class
  protected
    FFormats: TFormatSettings;
    function GetBool(AName: utf8string): boolean;
    function GetBoolDef(AName: utf8string; ADefault: boolean): boolean;
    function GetColorArray(AName: UTF8String): ArrayOfTBGRAPixel;
    function GetInteger(AName: utf8string): integer;
    function GetIntegerDef(AName: utf8string; ADefault: integer): integer;
    function GetPointF(AName: utf8string): TPointF;
    function GetRectF(AName: utf8string): TRectF;
    function GetRect(AName: utf8string): TRect;
    function GetAffineMatrix(AName: utf8string): TAffineMatrix;
    function GetRawString(AName: utf8string): RawByteString; virtual; abstract;
    function GetSingle(AName: utf8string): single;
    function GetSingleArray(AName: utf8string): ArrayOfSingle;
    function GetSingleDef(AName: utf8string; ADefault: single): single;
    function GetColor(AName: UTF8String): TBGRAPixel;
    procedure SetBool(AName: utf8string; AValue: boolean);
    procedure SetColorArray(AName: UTF8String; AValue: ArrayOfTBGRAPixel);
    procedure SetInteger(AName: utf8string; AValue: integer);
    procedure SetPointF(AName: utf8string; AValue: TPointF);
    procedure SetRectF(AName: utf8string; AValue: TRectF);
    procedure SetRect(AName: utf8string; AValue: TRect);
    procedure SetAffineMatrix(AName: utf8string; const AValue: TAffineMatrix);
    procedure SetRawString(AName: utf8string; AValue: RawByteString); virtual; abstract;
    procedure SetSingle(AName: utf8string; AValue: single);
    procedure SetSingleArray(AName: utf8string; AValue: ArrayOfSingle);
    procedure SetColor(AName: UTF8String; AValue: TBGRAPixel);
    function GetDelimiter: char;
    function GetEmpty: boolean; virtual; abstract;
  public
    constructor Create;
    function Duplicate: TBGRACustomOriginalStorage; virtual; abstract;
    procedure RemoveAttribute(AName: utf8string); virtual; abstract;
    function HasAttribute(AName: utf8string): boolean; virtual; abstract;
    procedure RemoveObject(AName: utf8string); virtual; abstract;
    function CreateObject(AName: utf8string): TBGRACustomOriginalStorage; virtual; abstract;
    function OpenObject(AName: utf8string): TBGRACustomOriginalStorage; virtual; abstract;
    function ObjectExists(AName: utf8string): boolean; virtual; abstract;
    procedure EnumerateObjects(AList: TStringList); virtual; abstract;
    procedure EnumerateFiles(AList: TStringList); virtual; abstract;
    procedure RemoveFile(AName: utf8string); virtual; abstract;
    function GetFileStream(AName: UTF8String): TStream; virtual; abstract;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; virtual; abstract;
    function ReadBitmap(AName: UTF8String; ADest: TCustomUniversalBitmap): boolean; virtual; abstract;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean; AOwnStream: boolean = false); virtual; abstract;
    function FileExists(AName: UTF8String): boolean; virtual; abstract;
    function FloatEquals(AName: utf8string; AValue: single): boolean;
    function PointFEquals(AName: utf8string; const AValue: TPointF): boolean;
    function AffineMatrixEquals(AName: utf8string; const AValue: TAffineMatrix): boolean;
    property RawString[AName: utf8string]: RawByteString read GetRawString write SetRawString;
    property Int[AName: utf8string]: integer read GetInteger write SetInteger;
    property IntDef[AName: utf8string; ADefault: integer]: integer read GetIntegerDef;
    property Bool[AName: utf8string]: boolean read GetBool write SetBool;
    property BoolDef[AName: utf8string; ADefault: boolean]: boolean read GetBoolDef;
    property Float[AName: utf8string]: single read GetSingle write SetSingle;
    property FloatArray[AName: utf8string]: ArrayOfSingle read GetSingleArray write SetSingleArray;
    property FloatDef[AName: utf8string; ADefault: single]: single read GetSingleDef;
    property PointF[AName: utf8string]: TPointF read GetPointF write SetPointF;
    property RectangleF[AName: utf8string]: TRectF read GetRectF write SetRectF;
    property Rectangle[AName: utf8string]: TRect read GetRect write SetRect;
    property AffineMatrix[AName: utf8string]: TAffineMatrix read GetAffineMatrix write SetAffineMatrix;
    property Color[AName: UTF8String]: TBGRAPixel read GetColor write SetColor;
    property ColorArray[AName: UTF8String]: ArrayOfTBGRAPixel read GetColorArray write SetColorArray;
    property Empty: boolean read GetEmpty;
  end;

  { TBGRAMemOriginalStorage }

  TBGRAMemOriginalStorage = class(TBGRACustomOriginalStorage)
  protected
    FMemDir: TMemDirectory;
    FMemDirOwned: boolean;
    function GetRawString(AName: utf8string): RawByteString; override;
    procedure SetRawString(AName: utf8string; AValue: RawByteString); override;
    function GetEmpty: boolean; override;
  public
    destructor Destroy; override;
    constructor Create;
    constructor Create(AMemDir: TMemDirectory; AMemDirOwned: boolean = false);
    function Equals(Obj: TObject): boolean; override;
    function Duplicate: TBGRACustomOriginalStorage; override;
    procedure RemoveAttribute(AName: utf8string); override;
    function HasAttribute(AName: utf8string): boolean; override;
    procedure RemoveObject(AName: utf8string); override;
    function CreateObject(AName: utf8string): TBGRACustomOriginalStorage; override;
    function OpenObject(AName: utf8string): TBGRACustomOriginalStorage; override;
    function ObjectExists(AName: utf8string): boolean; override;
    procedure EnumerateObjects(AList: TStringList); override;
    procedure EnumerateFiles(AList: TStringList); override;
    procedure RemoveFile(AName: utf8string); override;
    function GetFileStream(AName: UTF8String): TStream; override;
    function ReadBitmap(AName: UTF8String; ADest: TCustomUniversalBitmap): boolean; override;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; override;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean; AOwnStream: boolean = false); override;
    function FileExists(AName: UTF8String): boolean; override;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromResource(AFilename: string);
    procedure CopyTo(AMemDir: TMemDirectory);
  end;

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);
function FindLayerOriginalClass(AStorageClassName: string): TBGRALayerOriginalAny;

Implementation
End.
