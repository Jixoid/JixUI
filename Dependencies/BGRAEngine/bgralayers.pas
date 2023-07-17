// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAGraphics, BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap,
  BGRAMemDirectory, BGRATransform, fgl, BGRALayerOriginal;

type
  TBGRACustomLayeredBitmap = class;
  TBGRACustomLayeredBitmapClass = class of TBGRACustomLayeredBitmap;

  { TBGRALayerOriginalEntry }

  TBGRALayerOriginalEntry = record
     Guid: TGuid;
     Instance: TBGRALayerCustomOriginal;
     class operator = (const AEntry1,AEntry2: TBGRALayerOriginalEntry): boolean;
  end;

function BGRALayerOriginalEntry(AGuid: TGuid): TBGRALayerOriginalEntry;
function BGRALayerOriginalEntry(AInstance: TBGRALayerCustomOriginal): TBGRALayerOriginalEntry;

type
  TBGRALayerOriginalList = specialize TFPGList<TBGRALayerOriginalEntry>;

  TBGRALayeredBitmap = class;
  TBGRALayeredBitmapClass = class of TBGRALayeredBitmap;

  TBGRALayeredBitmapSaveToStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapLoadFromStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapCheckStreamProc = function(AStream: TStream): boolean;
  TOriginalRenderStatus = (orsNone, orsDraft, orsPartialDraft, orsProof, orsPartialProof);

  { TBGRACustomLayeredBitmap }

  TBGRACustomLayeredBitmap = class(TGraphic)
  private
    FFrozenRange: array of record
      firstLayer,lastLayer: integer;
      image: TBGRABitmap;
      linearBlend: boolean;
    end;
    FLinearBlend: boolean;
    FMemDirectory: TMemDirectory;
    FMemDirectoryOwned: boolean;
    FSelectionDrawMode: TDrawMode;
    FSelectionLayerIndex: integer;
    FSelectionRect: TRect;
    FSelectionScanner: IBGRAScanner;
    FSelectionScannerOffset: TPoint;
    function GetDefaultBlendingOperation: TBlendOperation;
    function GetHasMemFiles: boolean;
    function GetLinearBlend: boolean;
    function GetSelectionVisible: boolean;
    procedure SetLinearBlend(AValue: boolean);

  protected
    function GetNbLayers: integer; virtual; abstract;
    function GetMemDirectory: TMemDirectory;
    function GetBlendOperation(Layer: integer): TBlendOperation; virtual; abstract;
    function GetLayerVisible(layer: integer): boolean; virtual; abstract;
    function GetLayerOpacity(layer: integer): byte; virtual; abstract;
    function GetLayerName(layer: integer): string; virtual;
    function GetLayerOffset(layer: integer): TPoint; virtual;
    function GetLayerFrozenRange(layer: integer): integer;
    function GetLayerFrozen(layer: integer): boolean; virtual;
    function GetLayerUniqueId(layer: integer): integer; virtual;
    function GetLayerOriginal({%H-}layer: integer): TBGRALayerCustomOriginal; virtual;
    function GetLayerOriginalKnown({%H-}layer: integer): boolean; virtual;
    function GetLayerOriginalMatrix({%H-}layer: integer): TAffineMatrix; virtual;
    function GetLayerOriginalGuid({%H-}layer: integer): TGuid; virtual;
    function GetLayerOriginalRenderStatus({%H-}layer: integer): TOriginalRenderStatus; virtual;
    function GetOriginalCount: integer; virtual;
    function GetOriginalByIndex({%H-}AIndex: integer): TBGRALayerCustomOriginal; virtual;
    function GetOriginalByIndexKnown({%H-}AIndex: integer): boolean; virtual;
    function GetOriginalByIndexLoaded({%H-}AIndex: integer): boolean; virtual;
    function GetOriginalByIndexClass({%H-}AIndex: integer): TBGRALayerOriginalAny; virtual;
    function GetTransparent: Boolean; override;
    function GetEmpty: boolean; override;

    function IndexOfOriginal(const AGuid: TGuid): integer; overload; virtual;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; virtual;

    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetMemDirectory(AValue: TMemDirectory);
    procedure SetTransparent(Value: Boolean); override;

    procedure SetLayerFrozen(layer: integer; AValue: boolean); virtual;
    function RangeIntersect(first1,last1,first2,last2: integer): boolean;
    procedure RemoveFrozenRange(index: integer);
    function ContainsFrozenRange(first,last: integer): boolean;
    function GetLayerDrawMode(AIndex: integer): TDrawMode;

  public
    procedure SaveToFile(const filenameUTF8: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToStreamAs(Stream: TStream; AExtension: string);
    constructor Create; override;
    destructor Destroy; override;
    function ToString: ansistring; override;
    procedure DiscardSelection;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; virtual;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; virtual; abstract;
    function ComputeFlatImage(ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override; overload;
    procedure Draw(Canvas: TCanvas; x,y: integer); overload;
    procedure Draw(Canvas: TCanvas; x,y: integer; firstLayer, lastLayer: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer; ASeparateXorMask: boolean; ADestinationEmpty: boolean = false); overload;
    procedure Draw(Dest: TBGRABitmap; AX,AY: integer; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false; ADestinationEmpty: boolean = false); overload;
    function DrawLayer(Dest: TBGRABitmap; X,Y: Integer; AIndex: integer; ASeparateXorMask: boolean = false; ADestinationEmpty: boolean = false): boolean;

    procedure FreezeExceptOneLayer(layer: integer); overload;
    procedure Freeze(firstLayer, lastLayer: integer); overload;
    procedure Freeze; overload;
    procedure Unfreeze; overload;
    procedure Unfreeze(layer: integer); overload;
    procedure Unfreeze(firstLayer, lastLayer: integer); overload;

    procedure NotifyLoaded; virtual;
    procedure NotifySaving; virtual;

    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset;
    property LayerFrozen[layer: integer]: boolean read GetLayerFrozen;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus;
    property SelectionScanner: IBGRAScanner read FSelectionScanner write FSelectionScanner;
    property SelectionScannerOffset: TPoint read FSelectionScannerOffset write FSelectionScannerOffset;
    property SelectionRect: TRect read FSelectionRect write FSelectionRect;
    property SelectionLayerIndex: integer read FSelectionLayerIndex write FSelectionLayerIndex;
    property SelectionDrawMode: TDrawMode read FSelectionDrawMode write FSelectionDrawMode;
    property SelectionVisible: boolean read GetSelectionVisible;
    property LinearBlend: boolean read GetLinearBlend write SetLinearBlend; //use linear blending unless specified
    property DefaultBlendingOperation: TBlendOperation read GetDefaultBlendingOperation;
    property MemDirectory: TMemDirectory read GetMemDirectory write SetMemDirectory;
    property MemDirectoryOwned: boolean read FMemDirectoryOwned write FMemDirectoryOwned;
    property HasMemFiles: boolean read GetHasMemFiles;
  end;

  TEmbeddedOriginalChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal;
                                            var ADiff: TBGRAOriginalDiff) of object;
  TEmbeddedOriginalEditingChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal) of object;
  TLayeredActionProgressEvent = procedure(ASender: TObject; AProgressPercent: integer) of object;
  TEmbeddedOriginalLoadErrorEvent = procedure (ASender: TObject; AError: string; var ARaise: boolean) of object;

  TBGRALayerInfo = record
    UniqueId: integer;
    Name: string;
    x, y: integer;
    Source: TBGRABitmap;
    blendOp: TBlendOperation;
    Opacity: byte;
    Visible: boolean;
    Owner: boolean;
    Frozen: boolean;
    OriginalMatrix: TAffineMatrix;
    OriginalRenderStatus: TOriginalRenderStatus;
    OriginalGuid: TGuid;
    OriginalInvalidatedBounds: TRectF;
  end;

  { TBGRALayeredBitmap }

  TBGRALayeredBitmap = class(TBGRACustomLayeredBitmap)
  private
    FNbLayers: integer;
    FLayers: array of TBGRALayerInfo;
    FOnActionDone: TNotifyEvent;
    FOnEditorFocusChanged: TNotifyEvent;
    FEditorFocused: boolean;
    FOnActionProgress: TLayeredActionProgressEvent;
    FOnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent;
    FOriginalChange: TEmbeddedOriginalChangeEvent;
    FOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent;
    FWidth,FHeight: integer;
    FOriginals: TBGRALayerOriginalList;
    FOriginalEditor: TBGRAOriginalEditor;
    FOriginalEditorOriginal: TGuid;
    FOriginalEditorViewMatrix: TAffineMatrix;
    procedure EditorFocusedChanged({%H-}Sender: TObject);
    function GetLayerOriginalClass(layer: integer): TBGRALayerOriginalAny;
    function GetOriginalEditor: TBGRAOriginalEditor;
    function GetOriginalGuid(AIndex: integer): TGUID;
    procedure SetEditorFocused(AValue: boolean);
    procedure SetOnActionDone(AValue: TNotifyEvent);
    procedure SetOnActionProgress(AValue: TLayeredActionProgressEvent);

  protected
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    function GetNbLayers: integer; override;
    function GetBlendOperation(Layer: integer): TBlendOperation; override;
    function GetLayerVisible(layer: integer): boolean; override;
    function GetLayerOpacity(layer: integer): byte; override;
    function GetLayerOffset(layer: integer): TPoint; override;
    function GetLayerName(layer: integer): string; override;
    function GetLayerFrozen(layer: integer): boolean; override;
    function GetLayerUniqueId(layer: integer): integer; override;
    function GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal; override;
    function GetLayerOriginalKnown(layer: integer): boolean; override;
    function GetLayerOriginalMatrix(layer: integer): TAffineMatrix; override;
    function GetLayerOriginalGuid(layer: integer): TGuid; override;
    function GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus; override;
    function GetOriginalCount: integer; override;
    function GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal; override;
    function GetOriginalByIndexKnown(AIndex: integer): boolean; override;
    function GetOriginalByIndexLoaded(AIndex: integer): boolean; override;
    function GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny; override;
    procedure SetBlendOperation(Layer: integer; op: TBlendOperation);
    procedure SetLayerVisible(layer: integer; AValue: boolean);
    procedure SetLayerOpacity(layer: integer; AValue: byte);
    procedure SetLayerOffset(layer: integer; AValue: TPoint);
    procedure SetLayerName(layer: integer; AValue: string);
    procedure SetLayerFrozen(layer: integer; AValue: boolean); override;
    procedure SetLayerUniqueId(layer: integer; AValue: integer);
    procedure SetLayerOriginalMatrix(layer: integer; AValue: TAffineMatrix);
    procedure SetLayerOriginalGuid(layer: integer; const AValue: TGuid);
    procedure SetLayerOriginalRenderStatus(layer: integer; AValue: TOriginalRenderStatus);

    procedure FindOriginal(AGuid: TGuid;
                out ADir: TMemDirectory;
                out AClass: TBGRALayerOriginalAny);
    procedure StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
    procedure OriginalChange(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff);
    procedure OriginalEditingChange(ASender: TObject);
    function GetLayerDirectory(ALayerIndex: integer; ACanCreate: boolean): TMemDirectory;
    procedure UpdateOriginalEditor(ALayerIndex: integer; AMatrix: TAffineMatrix;
      APointSize: single);

  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure LoadFromResource(AFilename: string);
    procedure SetSize(AWidth, AHeight: integer); virtual;
    procedure Clear; override;
    procedure ClearOriginals;
    procedure RemoveLayer(index: integer);
    procedure InsertLayer(index: integer; fromIndex: integer);
    procedure Assign(ASource: TBGRACustomLayeredBitmap; ASharedLayerIds: boolean = false;
                ACopyAdditionalMemData: boolean = false); overload;
    function MoveLayerUp(index: integer): integer;
    function MoveLayerDown(index: integer): integer;

    function AddLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;

    class function IsValidRegistryIndentifier(AIdentifier: string): boolean;
    function GetLayerRegistry(ALayerIndex: integer; ARegistryIdentifier: string): RawByteString;
    procedure SetLayerRegistry(ALayerIndex: integer; ARegistryIdentifier: string; AValue: RawByteString);
    procedure SaveLayerRegistryToStream(ALayerIndex: integer; AStream: TStream);
    procedure LoadLayerRegistryFromStream(ALayerIndex: integer; AStream: TStream);
    function GetGlobalRegistry(ARegistryIdentifier: string): RawByteString;
    procedure SetGlobalRegistry(ARegistryIdentifier: string; AValue: RawByteString);

    function AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean = true): integer;
    function AddOriginalFromStream(AStream: TStream; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStream(AStream: TStream; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    procedure SaveOriginalToStream(AIndex: integer; AStream: TStream); overload;
    procedure SaveOriginalToStream(const AGuid: TGuid; AStream: TStream); overload;
    function RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
    procedure DeleteOriginal(AIndex: integer);
    procedure NotifyLoaded; override;
    procedure NotifySaving; override;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean = false; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBounds: TRect; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBoundsF: TRectF; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginalIfNecessary(layer: integer; ADraft: boolean; var ABounds: TRect);
    function RenderOriginalsIfNecessary(ADraft: boolean = false): TRect;
    function RenderOriginalIfNecessary(const AGuid: TGuid; ADraft: boolean = false): TRect;
    procedure RemoveUnusedOriginals;
    procedure UnloadOriginals;
    procedure UnloadOriginal(AIndex: integer); overload;
    procedure UnloadOriginal(const AGuid: TGuid); overload;

    destructor Destroy; override;
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; virtual;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    function GetLayerIndexFromId(AIdentifier: integer): integer;
    function Duplicate(ASharedLayerIds: boolean = false): TBGRALayeredBitmap;
    function ProduceLayerUniqueId: integer;

    procedure RotateCW;
    procedure RotateCCW;
    procedure RotateUD; overload;
    procedure RotateUD(ALayerIndex: integer); overload;
    procedure HorizontalFlip; overload;
    procedure HorizontalFlip(ALayerIndex: integer); overload;
    procedure VerticalFlip; overload;
    procedure VerticalFlip(ALayerIndex: integer); overload;
    procedure Resample(AWidth, AHeight: integer; AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter = rfLinear);
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    function TakeLayerBitmap(layer: integer): TBGRABitmap;
    procedure ApplyLayerOffset(ALayerIndex: integer; APadWithTranparentPixels: boolean);

    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    procedure ClearEditor;
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure KeyDown(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyUp(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyPress(UTF8Key: string; out AHandled: boolean);

    property Width : integer read GetWidth;
    property Height: integer read GetHeight;
    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation write SetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible write SetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity write SetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[layer: integer]: TBGRABitmap read GetLayerBitmapDirectly;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset write SetLayerOffset;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId write SetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalClass[layer: integer]: TBGRALayerOriginalAny read GetLayerOriginalClass;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid write SetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix write SetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus write SetLayerOriginalRenderStatus;

    function IndexOfOriginal(const AGuid: TGuid): integer; overload; override;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; override;
    property OriginalCount: integer read GetOriginalCount;
    property Original[AIndex: integer]: TBGRALayerCustomOriginal read GetOriginalByIndex;
    property OriginalGuid[AIndex: integer]: TGUID read GetOriginalGuid;
    property OriginalKnown[AIndex: integer]: boolean read GetOriginalByIndexKnown;
    property OriginalClass[AIndex: integer]: TBGRALayerOriginalAny read GetOriginalByIndexClass;
    property OnOriginalChange: TEmbeddedOriginalChangeEvent read FOriginalChange write FOriginalChange;
    property OnOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent read FOriginalEditingChange write FOriginalEditingChange;
    property OnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent read FOnOriginalLoadError write FOnOriginalLoadError;
    property EditorFocused: boolean read FEditorFocused write SetEditorFocused;
    property OnEditorFocusChanged: TNotifyEvent read FOnEditorFocusChanged write FOnEditorFocusChanged;
    property OriginalEditor: TBGRAOriginalEditor read GetOriginalEditor;
    property OnActionProgress: TLayeredActionProgressEvent read FOnActionProgress write SetOnActionProgress;
    property OnActionDone: TNotifyEvent read FOnActionDone write SetOnActionDone;
  end;

  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;

procedure RegisterLayeredBitmapWriter(AExtensionUTF8: string; AWriter: TBGRALayeredBitmapClass);
procedure RegisterLayeredBitmapReader(AExtensionUTF8: string; AReader: TBGRACustomLayeredBitmapClass);
function TryCreateLayeredBitmapWriter(AExtensionUTF8: string): TBGRALayeredBitmap;
function TryCreateLayeredBitmapReader(AExtensionUTF8: string): TBGRACustomLayeredBitmap;

var
  LayeredBitmapSaveToStreamProc : TBGRALayeredBitmapSaveToStreamProc;
  LayeredBitmapLoadFromStreamProc : TBGRALayeredBitmapLoadFromStreamProc;
  LayeredBitmapCheckStreamProc: TBGRALayeredBitmapCheckStreamProc;

type
  TOnLayeredBitmapLoadStartProc = procedure(AFilenameUTF8: string) of object;
  TOnLayeredBitmapLoadProgressProc = procedure(APercentage: integer) of object;
  TOnLayeredBitmapLoadedProc = procedure() of object;

procedure OnLayeredBitmapLoadFromStreamStart;
procedure OnLayeredBitmapLoadStart(AFilenameUTF8: string);
procedure OnLayeredBitmapLoadProgress(APercentage: integer);
procedure OnLayeredBitmapLoaded;
procedure RegisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);
procedure UnregisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);

type
  TOnLayeredBitmapSaveStartProc = procedure(AFilenameUTF8: string) of object;
  TOnLayeredBitmapSaveProgressProc = procedure(APercentage: integer) of object;
  TOnLayeredBitmapSavedProc = procedure() of object;

procedure OnLayeredBitmapSaveToStreamStart;
procedure OnLayeredBitmapSaveStart(AFilenameUTF8: string);
procedure OnLayeredBitmapSaveProgress(APercentage: integer);
procedure OnLayeredBitmapSaved;
procedure RegisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc; AProgress: TOnLayeredBitmapSaveProgressProc;
     ADone: TOnLayeredBitmapSavedProc);
procedure UnregisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc; AProgress: TOnLayeredBitmapSaveProgressProc;
     ADone: TOnLayeredBitmapSavedProc);

const
  RenderTempSubDirectory = 'temp';

Implementation
End.
