// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVGType;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAClasses, SysUtils, BGRATransform, BGRABitmapTypes, BGRAUnits,
  DOM, BGRACanvas2D, fgl, BGRAGraphics;

type
  ArrayOfFloat = array of single;
  ArrayOfString = array of string;
  
  TSVGElement = class;
  TSVGElementList = specialize TFPGList<TSVGElement>;
  TSVGElementDictionary = specialize TFPGMap<string,TSVGElement>;
  TSVGFactory = class of TSVGElement;
  TIterateElementCallback = procedure(AElement: TSVGElement; AData: pointer; var ARecursive: boolean) of object;
  
  TSVGFillMode = (
     sfmEvenOdd = Ord(fmAlternate),
     sfmNonZero = Ord(fmWinding)
   );

  TSVGPaintOrder = (
     spoFillStrokeMarkers,
     spoFillMarkersStroke,
     spoStrokeFillMarkers,
     spoStrokeMarkersFill,
     spoMarkersFillStroke,
     spoMarkersStrokeFill
  );

  TSVGLengthAdjust = (
     slaSpacing,
     slaSpacingAndGlyphs
   );

  TSVGTextPathMethod = (
     stpmAlign,
     stpmStretch
   );

  TSVGTextPathSpacing = (
     stpsAuto,
     stpsExact
   );

  TSVGTextAnchor = (
     staStart,
     staMiddle,
     staEnd
   );
   
  TSVGTextDirection = (  
     stdLtr,
     stdRtl
   );
   
  TSVGObjectUnits = (
     souUserSpaceOnUse,
     souObjectBoundingBox
   );

  TSVGSpreadMethod = (
     ssmPad,
     ssmReflect,
     ssmRepeat
  );

  TSVGColorInterpolation = (
     sciStdRGB,
     sciLinearRGB
  );

  TSVGImageRendering = (
    sirAuto,
    sirSmooth,
    sirHighQuality,
    sirCrispEdges,
    sirPixelated
  );

  TSVGRenderingIntent = (
     sriAuto,
     sriPerceptual,
     sriRelativeColorimetric,
     sriSaturation,
     sriAbsoluteColorimetric
   );

  TSVGMarkerUnits = (
     smuStrokeWidth,
     smuUserSpaceOnUse
   );

  TSVGOrientAuto = (soaNone,soaAuto,soaAutoReverse);
  TSVGOrient = record
    auto: TSVGOrientAuto;
    angle: TSVGNumber;
  end;

  TFindStyleState = (fssNotSearched,
                     fssNotFound,
                     fssFound);
  TStyleAttribute = record
     attr  : string;
     pos   : integer;
  end;
  ArrayOfTStyleAttribute = array of TStyleAttribute;
  
  { TSVGViewBox }

  TSVGViewBox = record
    min, size: TPointF;
    function ToString: string;
    class function Parse(AValue: string): TSVGViewBox; static;
    class function DefaultValue: TSVGViewBox; static;
  end;
  TSVGSize = record
    width, height: TFloatWithCSSUnit;
  end;

  { TSVGPreserveAspectRatio }

  TSVGPreserveAspectRatio = record
     Preserve, Slice: boolean;
     HorizAlign: TAlignment;
     VertAlign: TTextLayout;
     function ToString: string;
     class function Parse(AValue: string): TSVGPreserveAspectRatio; static;
     class function DefaultValue: TSVGPreserveAspectRatio; static;
  end;

  TSVGRecomputeEvent = procedure(Sender: TObject) of object;
  TSVGLinkEvent = procedure(Sender: TObject; AElement: TSVGElement; ALink: boolean) of object;
  TSVGLinkListeners = specialize TFPGList<TSVGLinkEvent>;
  
  { TSVGDataLink }

  TSVGDataLink = class
   private
     FElements: TSVGElementDictionary;
     FStyles: TSVGElementList;
     FParent: TSVGDataLink;
     FChildren: TList;
     FLinkListeners: TSVGLinkListeners;
     function GetElement(AIndex: integer): TSVGElement;
     function GetStyle(AIndex: integer): TSVGElement;
     function IsValidIndex(const AIndex: integer; list: TSVGElementList): boolean;
     function FindTo(el: TSVGElement; list: TSVGElementList): integer;
     procedure NotifyLink(AElement: TSVGElement; ALink: boolean);
     procedure SetParent(AValue: TSVGDataLink);
   public
     constructor Create(AParent: TSVGDataLink);
     destructor Destroy; override;

     function ElementCount: integer;
     function StyleCount: integer;
     function FindElement(el: TSVGElement): integer;
     function FindElementById(AID: string; AClass: TSVGFactory): TSVGElement;
     function FindElementByRef(ARef: string; AClass: TSVGFactory): TSVGElement;
     function FindElementByRef(ARef: string; ANeedUrl: boolean; AClass: TSVGFactory; out ANotFound: boolean): TSVGElement;
     function FindStyle(el: TSVGElement): integer;
     function IsLinkElement(el: TSVGElement): boolean;
     function IsLinkStyle(el: TSVGElement): boolean;
     function IsLink(el: TSVGElement): boolean;
     function Link(el: TSVGElement): integer;
     procedure Unlink(el: TSVGElement);
     procedure UnlinkAll;
     procedure RegisterLinkListener(AHandler: TSVGLinkEvent; ARegister: boolean);

     property Styles[ID: integer]: TSVGElement read GetStyle;
     property Elements[AIndex: integer]: TSVGElement read GetElement;
     property Parent: TSVGDataLink read FParent write SetParent;
   end;

  { TSVGCustomElement }

  TSVGCustomElement = class
  protected
    FDomElem: TDOMElement;
    FUnits: TCSSUnitConverter;
    function GetDOMElement: TDOMElement; virtual;

    function GetAttributeFromElement(ANode: TDOMElement; AName: string; ACanInherit: boolean): string;
    function GetAttribute(AName: string; ADefault: string; ACanInherit: boolean): string; overload;
    function GetAttribute(AName: string; ADefault: string): string; overload;
    function GetAttribute(AName: string): string; overload;
    function GetAttributeNumber(AName: string; ADefault: TSVGNumber): TSVGNumber; overload;
    function GetArrayOfAttributeNumber(AName: string): ArrayOfTSVGNumber;
    function GetArrayOfAttributeNumber(AName: string; ACanInherit: boolean): ArrayOfTSVGNumber;
    function GetAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
    function GetArrayOfAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetHorizAttribute(AName: string; ADefault: TSVGNumber): TSVGNumber;
    function GetHorizAttribute(AName: string): TSVGNumber;
    function GetHorizAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfHorizAttributeWithUnit(AName: string;
      ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfHorizAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetVerticalAttribute(AName: string; ADefault: TSVGNumber): TSVGNumber;
    function GetVerticalAttribute(AName: string): TSVGNumber;
    function GetVerticalAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfVerticalAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfVerticalAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;

    function GetOrthoAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;

    function GetAttributeOrStyle(AName,ADefault: string; ACanInherit: boolean): string; overload;
    function GetAttributeOrStyle(AName,ADefault: string): string; overload;
    function GetAttributeOrStyle(AName: string): string; overload;
    function GetHorizAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetArrayOfHorizAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetOrthoAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit; overload;
    function GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit; overload;
    function GetArrayOfAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetVerticalAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit; overload;
    function GetVerticalAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetArrayOfVerticalAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetNamespaceCount: integer;
    function GetNamespacePrefix(AIndex: integer): string;
    function GetNamespaceURI(APrefix: string): string;

    class function GetPropertyFromStyleDeclarationBlock(ABlock: string;
      AProperty: string; ADefault: string): string;
    class procedure LocateStyleDeclaration(AText: string; AProperty: string;
      out AStartPos, AColonPos, AValueLength: integer);
    class procedure UpdateStyleDeclarationBlock(var ABlock: string; AProperty: string; AValue: string);
    class function RemovePropertyFromDeclarationBlock(var ABlock: string; AProperty: string): boolean;
    function GetInlineStyle(const AName,ADefault: string): string;
    function GetInlineStyleWithUnit(const AName: string): TFloatWithCSSUnit; overload;
    function GetInlineStyleWithUnit(const AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetStyleFromStyleSheet(const {%H-}AName,ADefault: string): string; virtual;
    function GetStyle(const AName,ADefault: string): string; overload;
    function GetStyle(const AName: string): string; overload;

    procedure SetAttribute(AName: string; AValue: TSVGNumber); virtual; overload;
    procedure SetAttribute(AName: string; AValue: string); virtual; overload;
    procedure SetAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfAttributeWithUnit(AName: string; const AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetHorizAttribute(AName: string; AValue: TSVGNumber);
    procedure SetHorizAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfHorizAttributeWithUnit(AName: string;
      AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetVerticalAttribute(AName: string; AValue: TSVGNumber);
    procedure SetArrayOfAttributeNumber(AName: string; AValue: ArrayOfTSVGNumber);
    procedure SetVerticalAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfVerticalAttributeWithUnit(AName: string; AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetOrthoAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfOrthoAttributeWithUnit(AName: string; AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetNamespaceURI(APrefix: string; AValue: string);

    procedure SetInlineStyle(AName: string; AValue: string); overload;
    procedure SetInlineStyle(AName: string; AValue: TFloatWithCSSUnit); overload;
  public
    procedure RemoveStyle(const AName: string);
    function HasAttribute(AName: string): boolean;
    function HasInlineStyle(AName: string): boolean;

    procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer; ARecursive: boolean); virtual;
    procedure ConvertToUnit(AUnit: TCSSUnit); virtual;
    function EnterFontSize(AIsRoot: boolean = false): TFloatWithCSSUnit; virtual;
    procedure ExitFontSize(APrevFontSize: TFloatWithCSSUnit); virtual;

    function MatrixToTransform(m: TAffineMatrix; AFromUnit: TCSSUnit): string;
    function TransformToMatrix(ATransform: string; AToUnit: TCSSUnit): TAffineMatrix;

    procedure RemoveNamespace(APrefix: string);
    function NeedNamespace(APrefix: string): boolean;
    property NamespaceURI[APrefix: string]: string read GetNamespaceURI write SetNamespaceURI;
    property NamespacePrefix[AIndex: integer]: string read GetNamespacePrefix;
    property NamespaceCount: integer read GetNamespaceCount;

    property Style[AName: string]: string read GetStyle write SetInlineStyle;
    property StyleDef[AName,ADefault: string]: string read GetStyle;
  end;

  { TSVGElement }

  TSVGElement = class(TSVGCustomElement)
  private
    FImportStyleState: TFindStyleState;
    FImportedStyles: ArrayOfTStyleAttribute;
    function GetClipPath: string;
    function GetFill: string;
    function GetFillColor: TBGRAPixel;
    function GetFillOpacity: single;
    function GetFillRule: string;
    function GetIsFillNone: boolean;
    function GetIsStrokeNone: boolean;
    function GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
    function GetMixBlendMode: TBlendOperation;
    function GetOpacity: single;
    function GetPaintOrder: TSVGPaintOrder;
    function GetStroke: string;
    function GetStrokeColor: TBGRAPixel;
    function GetStrokeLineCap: string;
    function GetStrokeLineCapLCL: TPenEndCap;
    function GetStrokeLineJoin: string;
    function GetStrokeLineJoinLCL: TPenJoinStyle;
    function GetStrokeMiterLimit: single;
    function GetStrokeOpacity: single;
    function GetStrokeWidth: TFloatWithCSSUnit;
    function GetStrokeDashArray: string;
    function GetStrokeDashArrayF: ArrayOfFloat;
    function GetStrokeDashOffset: TFloatWithCSSUnit;
    function GetTransform: string;
    function GetID: string;
    function GetClassAttr: string;
    function GetVisible: boolean;
    procedure SetClipPath(AValue: string);
    procedure SetFillColor(AValue: TBGRAPixel);
    procedure SetFillOpacity(AValue: single);
    procedure SetFillRule(AValue: string);
    procedure SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
    procedure SetMixBlendMode(AValue: TBlendOperation);
    procedure SetOpacity(AValue: single);
    procedure SetPaintOrder(AValue: TSVGPaintOrder);
    procedure SetStrokeColor(AValue: TBGRAPixel);
    procedure SetStrokeLineCap(AValue: string);
    procedure SetStrokeLineCapLCL(AValue: TPenEndCap);
    procedure SetStrokeLineJoin(AValue: string);
    procedure SetStrokeLineJoinLCL(AValue: TPenJoinStyle);
    procedure SetStrokeMiterLimit(AValue: single);
    procedure SetStrokeOpacity(AValue: single);
    procedure SetStrokeWidth(AValue: TFloatWithCSSUnit);
    procedure SetStrokeDashArray(AValue: string);
    procedure SetStrokeDashArrayF(AValue: ArrayOfFloat);
    procedure SetStrokeDashOffset(AValue: TFloatWithCSSUnit);
    procedure SetTransform(AValue: string);
    procedure SetID(AValue: string);
    procedure SetClassAttr(AValue: string);
    function FindStyleElementInternal(const classStr: string;
      out attributesStr: string): integer;
    procedure ImportStyles;
    procedure SetVisible(AValue: boolean);
  protected
    FDataLink: TSVGDataLink;
    procedure Init(ADocument: TDOMDocument; ATag: string; AUnits: TCSSUnitConverter); overload;
    procedure Init(AElement: TDOMElement; AUnits: TCSSUnitConverter); overload;
    procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    procedure InternalCopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    function GetStyleFromStyleSheet(const AName,ADefault: string): string; override;
    procedure ApplyFillStyle(ACanvas2D: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    procedure ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit); virtual;
    procedure SetDatalink(AValue: TSVGDataLink); virtual;
    procedure SetFill(AValue: string); virtual;
    procedure SetStroke(AValue: string); virtual;
    procedure Initialize; virtual;
    procedure Paint(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
  public
    constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; virtual;
    constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; virtual;
    class function GetDOMTag: string; virtual;
    destructor Destroy; override;
    procedure ListIdentifiers(AResult: TStringList); virtual;
    procedure RenameIdentifiers(AFrom, ATo: TStringList); virtual;
    procedure ConvertToUnit(AUnit: TCSSUnit); override;
    procedure Recompute; virtual;
    procedure Draw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
    procedure CopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
    procedure fillNone;
    procedure strokeNone;
    procedure strokeDashArrayNone;
    procedure transformNone;
    function fillMode: TSVGFillMode;
    property DataLink: TSVGDataLink read FDataLink write SetDataLink;
    property DOMElement: TDOMElement read GetDOMElement;
    property Units: TCSSUnitConverter read FUnits;
    property ID: string read GetID write SetID;
    property classAttr: string read GetClassAttr write SetClassAttr;
    property transform: string read GetTransform write SetTransform;
    property matrix[AUnit: TCSSUnit]: TAffineMatrix read GetMatrix write SetMatrix;
    property isFillNone: boolean read GetIsFillNone;
    property isStrokeNone: boolean read GetIsStrokeNone;
    property stroke: string read GetStroke write SetStroke;
    property strokeWidth: TFloatWithCSSUnit read GetStrokeWidth write SetStrokeWidth;
    property strokeColor: TBGRAPixel read GetStrokeColor write SetStrokeColor;
    property strokeOpacity: single read GetStrokeOpacity write SetStrokeOpacity;
    property strokeMiterLimit: single read GetStrokeMiterLimit write SetStrokeMiterLimit;
    property strokeLineJoin: string read GetStrokeLineJoin write SetStrokeLineJoin;
    property strokeLineJoinLCL: TPenJoinStyle read GetStrokeLineJoinLCL write SetStrokeLineJoinLCL;
    property strokeLineCap: string read GetStrokeLineCap write SetStrokeLineCap;
    property strokeLineCapLCL: TPenEndCap read GetStrokeLineCapLCL write SetStrokeLineCapLCL;
    property strokeDashArray: string read GetStrokeDashArray write SetStrokeDashArray;
    property strokeDashArrayF: ArrayOfFloat read GetStrokeDashArrayF write SetStrokeDashArrayF;
    property strokeDashOffset: TFloatWithCSSUnit read GetStrokeDashOffset write SetStrokeDashOffset;
    property fill: string read GetFill write SetFill;
    property fillColor: TBGRAPixel read GetFillColor write SetFillColor;
    property fillOpacity: single read GetFillOpacity write SetFillOpacity;
    property fillRule: string read GetFillRule write SetFillRule;
    property paintOrder: TSVGPaintOrder read GetPaintOrder write SetPaintOrder;
    property mixBlendMode: TBlendOperation read GetMixBlendMode write SetMixBlendMode;
    property opacity: single read GetOpacity write SetOpacity;
    property clipPath: string read GetClipPath write SetClipPath;
    property Visible: boolean read GetVisible write SetVisible;

    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property AttributeDef[AName,ADefault: string]: string read GetAttribute;
    property AttributeOrStyleDef[AName,ADefault: string]: string read GetAttributeOrStyle;
    property AttributeOrStyle[AName: string]: string read GetAttributeOrStyle;
    property AttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetAttributeWithUnit;
    property AttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetAttributeWithUnit write SetAttributeWithUnit;
    property ArrayOfAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfAttributeWithUnit;
    property ArrayOfAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfAttributeWithUnit write SetArrayOfAttributeWithUnit;

    property ArrayOfAttributeNumberInherit[AName: string; ACanInherit: boolean]: ArrayOfTSVGNumber read GetArrayOfAttributeNumber;
    property ArrayOfAttributeNumber[AName: string]: ArrayOfTSVGNumber read GetArrayOfAttributeNumber write SetArrayOfAttributeNumber;

    property HorizAttributeDef[AName: string; ADefault: TSVGNumber]: TSVGNumber read GetHorizAttribute;
    property HorizAttribute[AName: string]: TSVGNumber read GetHorizAttribute write SetHorizAttribute;
    property HorizAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeWithUnit;
    property HorizAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetHorizAttributeWithUnit write SetHorizAttributeWithUnit;
    property HorizAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeOrStyleWithUnit;
    property ArrayOfHorizAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeWithUnit;
    property ArrayOfHorizAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeWithUnit write SetArrayOfHorizAttributeWithUnit;
    property ArrayOfHorizAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeOrStyleWithUnit;

    property VerticalAttributeDef[AName: string; ADefault: TSVGNumber]: TSVGNumber read GetVerticalAttribute;
    property VerticalAttribute[AName: string]: TSVGNumber read GetVerticalAttribute write SetVerticalAttribute;
    property VerticalAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit;
    property VerticalAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit write SetVerticalAttributeWithUnit;
    property VerticalAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeOrStyleWithUnit;
    property ArrayOfVerticalAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeWithUnit;
    property ArrayOfVerticalAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeWithUnit write SetArrayOfVerticalAttributeWithUnit;
    property ArrayOfVerticalAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeOrStyleWithUnit;

    property OrthoAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit;
    property OrthoAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit write SetOrthoAttributeWithUnit;
    property OrthoAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeOrStyleWithUnit;
    property ArrayOfOrthoAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfOrthoAttributeWithUnit write SetArrayOfOrthoAttributeWithUnit;
    property ArrayOfOrthoAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfOrthoAttributeOrStyleWithUnit;
  end;

  { TSVGParser }

  TSVGParser = class
  private
    function GetDone: boolean;
  protected
    FPos: integer;
    FNumberError: boolean;
    FText: string;
  public
    constructor Create(AText: string);
    function ParseFloat: single;
    function ParseId: string;
    function ParseSymbol: char;
    function ParseTransform: TAffineMatrix;
    procedure SkipSymbol(ASymbol: char);
    procedure SkipUpToSymbol(ASymbol:char);
    procedure ClearError;
    property Position: integer read FPos write FPos;
    property NumberError: boolean read FNumberError;
    property Text: string read FText;
    property Done: boolean read GetDone;
  end;
  
  resourcestring
    rsInvalidIndex = 'Invalid index';

Implementation
End.
