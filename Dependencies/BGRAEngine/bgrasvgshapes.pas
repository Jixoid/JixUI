// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVGShapes;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAUnits, DOM, BGRAPath, BGRABitmapTypes,
  BGRACanvas2D, BGRASVGType, BGRAGraphics;

type
  TSVGContent = class;

  { TSVGElementWithContent }

  TSVGElementWithContent = class(TSVGElement)
  protected
    FContent: TSVGContent;
    FSubDatalink: TSVGDataLink;
    class function OwnDatalink: boolean; virtual;
    procedure SetDatalink(AValue: TSVGDataLink); override;
  public
    constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer;
      ARecursive: boolean); override;
    procedure ListIdentifiers(AResult: TStringList); override;
    procedure RenameIdentifiers(AFrom, ATo: TStringList); override;
    procedure ConvertToUnit(AUnit: TCSSUnit); override;
    destructor Destroy; override;
    procedure Recompute; override;
    property Content: TSVGContent read FContent;
  end;

  TSVGGradient = class;
  
  { TSVGElementWithGradient }

  TSVGElementWithGradient = class(TSVGElement)
    private
      FFillGradientElement, FStrokeGradientElement: TSVGGradient;
      FGradientElementsDefined, FRegisteredToDatalink: boolean;
      FFillCanvasGradient, FStrokeCanvasGradient: IBGRACanvasGradient2D;
      procedure DatalinkOnLink(Sender: TObject; AElement: TSVGElement;
        ALink: boolean);
      function EvaluatePercentage(fu: TFloatWithCSSUnit): single; { fu is a percentage of a number [0.0..1.0] }
      function GetFillGradientElement: TSVGGradient;
      function GetStrokeGradientElement: TSVGGradient;
      procedure ResetGradients;
      procedure FindGradientElements;
    protected
      procedure Initialize; override;
      procedure AddStopElements(ASVGGradient: TSVGGradient; canvas: IBGRACanvasGradient2D);
      function CreateCanvasLinearGradient(ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient;
        const origin: TPointF; const w,h: single; AUnit: TCSSUnit): IBGRACanvasGradient2D;
      function CreateCanvasRadialGradient(ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient;
        const origin: TPointF; const w,h: single; AUnit: TCSSUnit): IBGRACanvasGradient2D;
      procedure ApplyFillStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure SetDatalink(AValue: TSVGDataLink); override;
      procedure SetFill(AValue: string); override;
      procedure SetStroke(AValue: string); override;
    public
      destructor Destroy; override;
      procedure InitializeGradient(ACanvas2d: TBGRACanvas2D;
                const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
      property FillGradientElement: TSVGGradient read GetFillGradientElement;
      property StrokeGradientElement: TSVGGradient read GetStrokeGradientElement;
  end;       

  { TSVGLine }

  TSVGLine = class(TSVGElement)
    private
      function GetX1: TFloatWithCSSUnit;
      function GetX2: TFloatWithCSSUnit;
      function GetY1: TFloatWithCSSUnit;
      function GetY2: TFloatWithCSSUnit;
      procedure SetX1(AValue: TFloatWithCSSUnit);
      procedure SetX2(AValue: TFloatWithCSSUnit);
      procedure SetY1(AValue: TFloatWithCSSUnit);
      procedure SetY2(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property x1: TFloatWithCSSUnit read GetX1 write SetX1;
      property y1: TFloatWithCSSUnit read GetY1 write SetY1;
      property x2: TFloatWithCSSUnit read GetX2 write SetX2;
      property y2: TFloatWithCSSUnit read GetY2 write SetY2;
  end;

  { TSVGRectangle }

  TSVGRectangle = class(TSVGElementWithGradient)
    private
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGCircle }

  TSVGCircle = class(TSVGElementWithGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetR: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetR(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property r: TFloatWithCSSUnit read GetR write SetR;
  end;

  { TSVGEllipse }

  TSVGEllipse = class(TSVGElementWithGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGPath }

  TSVGPath = class(TSVGElementWithGradient)
    private
      FPath: TBGRAPath;
      FBoundingBox: TRectF;
      FBoundingBoxComputed: boolean;
      function GetBoundingBoxF: TRectF;
      function GetPath: TBGRAPath;
      function GetPathLength: TFloatWithCSSUnit;
      function GetData: string;
      procedure SetPathLength(AValue: TFloatWithCSSUnit);
      procedure SetData(AValue: string);
    protected
      function GetDOMElement: TDOMElement; override;
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      destructor Destroy; override;
      property d: string read GetData write SetData;
      property path: TBGRAPath read GetPath;
      property pathLength: TFloatWithCSSUnit read GetPathLength write SetPathLength;
      property boundingBoxF: TRectF read GetBoundingBoxF;
  end;

  { TSVGPolypoints }

  TSVGPolypoints = class(TSVGElementWithGradient)
    private
      FBoundingBox: TRectF;
      FBoundingBoxComputed: boolean;
      function GetBoundingBoxF: TRectF;
      function GetClosed: boolean;
      function GetPoints: string;
      function GetPointsF: ArrayOfTPointF;
      procedure SetPoints(AValue: string);
      procedure SetPointsF(AValue: ArrayOfTPointF);
      procedure ComputeBoundingBox(APoints: ArrayOfTPointF);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; AClosed: boolean; ADataLink: TSVGDataLink); overload;
      destructor Destroy; override;
      property points: string read GetPoints write SetPoints;
      property pointsF: ArrayOfTPointF read GetPointsF write SetPointsF;
      property closed: boolean read GetClosed;
      property boundingBoxF: TRectF read GetBoundingBoxF;
  end;

  { TSVGTextElement }

  TSVGTextElement = class(TSVGElementWithGradient);

  { TSVGTextElementWithContent }

  TSVGTextElementWithContent = class(TSVGTextElement)
    protected
      FContent: TSVGContent;
    public
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      destructor Destroy; override;
      procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer;
        ARecursive: boolean); override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property Content: TSVGContent read FContent;
  end;

  { TSVGTextPositioning }

  TSVGTextPositioning = class(TSVGTextElementWithContent)
    private
      function GetX: ArrayOfTFloatWithCSSUnit;
      function GetY: ArrayOfTFloatWithCSSUnit;
      function GetDX: ArrayOfTFloatWithCSSUnit;
      function GetDY: ArrayOfTFloatWithCSSUnit;
      function GetRotate: ArrayOfTSVGNumber;
      procedure SetX(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetY(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetDX(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetDY(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetRotate(AValue: ArrayOfTSVGNumber);
    public
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property x: ArrayOfTFloatWithCSSUnit read GetX write SetX;
      property y: ArrayOfTFloatWithCSSUnit read GetY write SetY;
      property dx: ArrayOfTFloatWithCSSUnit read GetDX write SetDX;
      property dy: ArrayOfTFloatWithCSSUnit read GetDY write SetDY;
      property rotate: ArrayOfTSVGNumber read GetRotate write SetRotate;
  end;

  { TSVGTRef }

  TSVGTRef = class(TSVGTextElement)
    private
      function GetXlinkHref: string;
      procedure SetXlinkHref(AValue: string);
    public
      class function GetDOMTag: string; override;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  ArrayOfTextParts = array of record
    Level: integer;
    BaseElement: TSVGElement;
    Text: string;
    SplitPos: integer;
    AbsoluteCoord: TPointF;
    PartStartCoord, PartEndCoord: TPointF;
    Bounds: TRectF;
    PosUnicode: integer;
    InheritedRotation: single;
  end;

  { TSVGText }

  TSVGText = class(TSVGTextPositioning)
    private
      FInGetSimpleText: boolean;
      function GetFontBold: boolean;
      function GetFontFamily: string;
      function GetFontFamilyList: ArrayOfString;
      function GetFontItalic: boolean;
      function GetFontSize: TFloatWithCSSUnit;
      function GetFontStyle: string;
      function GetFontStyleLCL: TFontStyles;
      function GetFontWeight: string;
      function GetSimpleText: string;
      function GetTextAnchor: TSVGTextAnchor;
      function GetTextDirection: TSVGTextDirection;
      function GetTextDecoration: string;
      function GetTextLength: TFloatWithCSSUnit;
      function GetLengthAdjust: TSVGLengthAdjust;
      procedure SetFontBold(AValue: boolean);
      procedure SetFontFamily(AValue: string);
      procedure SetFontFamilyList(AValue: ArrayOfString);
      procedure SetFontItalic(AValue: boolean);
      procedure SetFontSize(AValue: TFloatWithCSSUnit);
      procedure SetFontStyle(AValue: string);
      procedure SetFontStyleLCL(AValue: TFontStyles);
      procedure SetFontWeight(AValue: string);
      procedure SetSimpleText(AValue: string);
      procedure SetTextAnchor(AValue: TSVGTextAnchor);
      procedure SetTextDirection(AValue: TSVGTextDirection);
      procedure SetTextDecoration(AValue: string);
      procedure SetTextLength(AValue: TFloatWithCSSUnit);
      procedure SetLengthAdjust(AValue: TSVGLengthAdjust);
    protected
      procedure InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                                      ADraw: boolean; AAllTextBounds: TRectF;
                                      var APosition: TPointF;
                                      var ATextParts: ArrayOfTextParts); overload;
      procedure InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                                      ADraw: boolean; AAllTextBounds: TRectF;
                                      var APosition: TPointF;
                                      var ATextParts: ArrayOfTextParts; ALevel: integer;
                                      AStartPart, AEndPart: integer); overload;
      procedure InternalDrawOrComputePart(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                              AText: string; APosUnicode: integer; AInheritedRotation: single;
                              ADraw: boolean; AAllTextBounds: TRectF;
                              var APosition: TPointF; out ABounds: TRectF);
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure CleanText(var ATextParts: ArrayOfTextParts);
      function GetTRefContent(AElement: TSVGTRef): string;
      function GetAllText(AInheritedRotation: single): ArrayOfTextParts;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property textLength: TFloatWithCSSUnit read GetTextLength write SetTextLength;
      property lengthAdjust: TSVGLengthAdjust read GetLengthAdjust write SetLengthAdjust;
      property SimpleText: string read GetSimpleText write SetSimpleText;
      property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
      property fontFamily: string read GetFontFamily write SetFontFamily;
      property fontFamilyList: ArrayOfString read GetFontFamilyList write SetFontFamilyList;
      property fontWeight: string read GetFontWeight write SetFontWeight;
      property fontStyle: string read GetFontStyle write SetFontStyle;
      property fontStyleLCL: TFontStyles read GetFontStyleLCL write SetFontStyleLCL;
      property textDecoration: string read GetTextDecoration write SetTextDecoration;
      property fontBold: boolean read GetFontBold write SetFontBold;
      property fontItalic: boolean read GetFontItalic write SetFontItalic;
      property textAnchor: TSVGTextAnchor read GetTextAnchor write SetTextAnchor;
      property textDirection: TSVGTextDirection read GetTextDirection write SetTextDirection;
  end;

  { TSVGTSpan }

  TSVGTSpan = class(TSVGText)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGTextPath }

  TSVGTextPath = class(TSVGTextElementWithContent)
    private
      function GetStartOffset: TFloatWithCSSUnit;
      function GetMethod: TSVGTextPathMethod;
      function GetSpacing: TSVGTextPathSpacing;
      function GetXlinkHref: string;
      procedure SetStartOffset(AValue: TFloatWithCSSUnit);
      procedure SetMethod(AValue: TSVGTextPathMethod);
      procedure SetSpacing(AValue: TSVGTextPathSpacing);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property startOffset: TFloatWithCSSUnit read GetStartOffset write SetStartOffset;
      property method: TSVGTextPathMethod read GetMethod write SetMethod;
      property spacing: TSVGTextPathSpacing read GetSpacing write SetSpacing;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  { TSVGAltGlyph }

  TSVGAltGlyph = class(TSVGTextElementWithContent)
    private
      function GetGlyphRef: string;
      function GetFormat: string;
      function GetXlinkHref: string;
      procedure SetGlyphRef(AValue: string);
      procedure SetFormat(AValue: string);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property glyphRef: string read GetGlyphRef write SetGlyphRef;
      property format: string read GetFormat write SetFormat;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  { TSVGAltGlyphDef }

  TSVGAltGlyphDef = class(TSVGTextElementWithContent)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGAltGlyphItem }

  TSVGAltGlyphItem = class(TSVGTextElementWithContent)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGGlyphRef }

  TSVGGlyphRef = class(TSVGTextElement)
    private
      function GetX: TSVGNumber;
      function GetY: TSVGNumber;
      function GetDx: TSVGNumber;
      function GetDy: TSVGNumber;
      function GetGlyphRef: string;
      function GetFormat: string;
      function GetXlinkHref: string;
      procedure SetX(AValue: TSVGNumber);
      procedure SetY(AValue: TSVGNumber);
      procedure SetDx(AValue: TSVGNumber);
      procedure SetDy(AValue: TSVGNumber);
      procedure SetGlyphRef(AValue: string);
      procedure SetFormat(AValue: string);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property x: TSVGNumber read GetX write SetX;
      property y: TSVGNumber read GetY write SetY;
      property dx: TSVGNumber read GetDx write SetDx;
      property dy: TSVGNumber read GetDy write SetDy;
      property glyphRef: string read GetGlyphRef write SetGlyphRef;
      property format: string read GetFormat write SetFormat;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;
  
  { TSVGClipPath }

  TSVGClipPath = class(TSVGElementWithContent)
    private
      function GetExternalResourcesRequired: boolean;
      function GetClipPathUnits: TSVGObjectUnits;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetClipPathUnits(AValue: TSVGObjectUnits);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
      procedure InternalCopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property clipPathUnits: TSVGObjectUnits read GetClipPathUnits write SetClipPathUnits;
  end;   
  
  { TSVGColorProfile }

  TSVGColorProfile = class(TSVGElement)
    private
      function GetLocal: string;
      function GetName: string;
      function GetRenderingIntent: TSVGRenderingIntent;
      function GetXlinkHref: string;
      procedure SetLocal(AValue: string);
      procedure SetName(AValue: string);
      procedure SetRenderingIntent(AValue: TSVGRenderingIntent);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property local: string read GetLocal write SetLocal;
      property name: string read GetName write SetName;
      property renderingIntent: TSVGRenderingIntent read GetRenderingIntent write SetRenderingIntent;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;  
  
  { TSVGImage }

  TSVGImage = class(TSVGElement)
    private
      function GetBitmap: TBGRACustomBitmap;
      function GetExternalResourcesRequired: boolean;
      function GetImageRendering: TSVGImageRendering;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
      function GetXlinkHref: string;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetImageRendering(AValue: TSVGImageRendering);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
      procedure SetXlinkHref(AValue: string);
    protected
      FBitmap: TBGRACustomBitmap;
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); overload; override;
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); overload; override;
      destructor Destroy; override;
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      procedure SetBitmap(AValue: TBGRACustomBitmap; AOwned: boolean); overload;
      procedure SetBitmap(AStream: TStream; AMimeType: string); overload;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property imageRendering: TSVGImageRendering read GetImageRendering write SetImageRendering;
      property preserveAspectRatio: TSVGPreserveAspectRatio
       read GetPreserveAspectRatio write SetPreserveAspectRatio;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
      property Bitmap: TBGRACustomBitmap read GetBitmap;
  end;   
  
  { TSVGPattern }

  TSVGPattern = class(TSVGImage)
    private
      function GetPatternUnits: TSVGObjectUnits;
      function GetPatternContentUnits: TSVGObjectUnits;
      function GetPatternTransform: string;
      function GetViewBox: TSVGViewBox;
      procedure SetPatternUnits(AValue: TSVGObjectUnits);
      procedure SetPatternContentUnits(AValue: TSVGObjectUnits);
      procedure SetPatternTransform(AValue: string);
      procedure SetViewBox(AValue: TSVGViewBox);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property patternUnits: TSVGObjectUnits read GetPatternUnits write SetPatternUnits;
      property patternContentUnits: TSVGObjectUnits
       read GetPatternContentUnits write SetPatternContentUnits;
      property patternTransform: string read GetPatternTransform write SetPatternTransform;
      property viewBox: TSVGViewBox read GetViewBox write SetViewBox;
  end;
  
  { TSVGMarker }

  TSVGMarker = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetViewBox: TSVGViewBox;
      function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
      function GetRefX: TFloatWithCSSUnit;
      function GetRefY: TFloatWithCSSUnit;
      function GetMarkerWidth: TFloatWithCSSUnit;
      function GetMarkerHeight: TFloatWithCSSUnit;
      function GetMarkerUnits: TSVGMarkerUnits;
      function GetOrient: TSVGOrient;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetViewBox(AValue: TSVGViewBox);
      procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
      procedure SetRefX(AValue: TFloatWithCSSUnit);
      procedure SetRefY(AValue: TFloatWithCSSUnit);
      procedure SetMarkerWidth(AValue: TFloatWithCSSUnit);
      procedure SetMarkerHeight(AValue: TFloatWithCSSUnit);
      procedure SetMarkerUnits(AValue: TSVGMarkerUnits);
      procedure SetOrient(AValue: TSVGOrient);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property viewBox: TSVGViewBox read GetViewBox write SetViewBox;
      property preserveAspectRatio: TSVGPreserveAspectRatio
       read GetPreserveAspectRatio write SetPreserveAspectRatio;
      property refX: TFloatWithCSSUnit read GetRefX write SetRefX;
      property refY: TFloatWithCSSUnit read GetRefY write SetRefY;
      property markerWidth: TFloatWithCSSUnit read GetMarkerWidth write SetMarkerWidth;
      property markerHeight: TFloatWithCSSUnit read GetMarkerHeight write SetMarkerHeight;
      property markerUnits: TSVGMarkerUnits read GetMarkerUnits write SetMarkerUnits;
      property orient: TSVGOrient read GetOrient write SetOrient;
  end;
  
  { TSVGMask }

  TSVGMask = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetMaskUnits: TSVGObjectUnits;
      function GetMaskContentUnits: TSVGObjectUnits;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetMaskUnits(AValue: TSVGObjectUnits);
      procedure SetMaskContentUnits(AValue: TSVGObjectUnits);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property maskUnits: TSVGObjectUnits read GetMaskUnits write SetMaskUnits;
      property maskContentUnits: TSVGObjectUnits
       read GetMaskContentUnits write SetMaskContentUnits;
  end;      
  
  TConvMethod = (cmNone,cmHoriz,cmVertical,cmOrtho);
  
  { TSVGGradient } 

  TSVGGradient = class(TSVGElementWithContent)
    private
      function GetColorInterpolation: TSVGColorInterpolation;
      function GetGradientMatrix(AUnit: TCSSUnit): TAffineMatrix;
      function GetGradientTransform: string;
      function GetGradientUnits: TSVGObjectUnits;
      function GetHRef: string;
      function GetSpreadMethod: TSVGSpreadMethod;
      procedure SetColorInterpolation(AValue: TSVGColorInterpolation);
      procedure SetGradientMatrix(AUnit: TCSSUnit; AValue: TAffineMatrix);
      procedure SetGradientTransform(AValue: string);
      procedure SetGradientUnits(AValue: TSVGObjectUnits);
      procedure SetHRef(AValue: string);
      procedure SetSpreadMethod(AValue: TSVGSpreadMethod);
    protected
      InheritedGradients: TSVGElementList;//(for HRef)
      procedure Initialize; override;
      function GetInheritedAttribute(AValue: string;
        AConvMethod: TConvMethod; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    public
      destructor Destroy; override;
      procedure ScanInheritedGradients(const forceScan: boolean = false);
      property hRef: string read GetHRef write SetHRef;
      property gradientUnits: TSVGObjectUnits read GetGradientUnits write SetGradientUnits;
      property gradientTransform: string read GetGradientTransform write SetGradientTransform;
      property gradientMatrix[AUnit: TCSSUnit]: TAffineMatrix read GetGradientMatrix write SetGradientMatrix;
      property spreadMethod: TSVGSpreadMethod read GetSpreadMethod write SetSpreadMethod;
      property colorInterpolation: TSVGColorInterpolation read GetColorInterpolation write SetColorInterpolation;
  end;        

  { TSVGGradientLinear }

  { TSVGLinearGradient }

  TSVGLinearGradient = class(TSVGGradient)
    private
      function GetX1: TFloatWithCSSUnit;
      function GetX2: TFloatWithCSSUnit;
      function GetY1: TFloatWithCSSUnit;
      function GetY2: TFloatWithCSSUnit;
      procedure SetX1(AValue: TFloatWithCSSUnit);
      procedure SetX2(AValue: TFloatWithCSSUnit);
      procedure SetY1(AValue: TFloatWithCSSUnit);
      procedure SetY2(AValue: TFloatWithCSSUnit);
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property x1: TFloatWithCSSUnit read GetX1 write SetX1;
      property y1: TFloatWithCSSUnit read GetY1 write SetY1;
      property x2: TFloatWithCSSUnit read GetX2 write SetX2;
      property y2: TFloatWithCSSUnit read GetY2 write SetY2;
  end;

  { TSVGRadialGradient }

  TSVGRadialGradient = class(TSVGGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetR: TFloatWithCSSUnit;
      function GetFX: TFloatWithCSSUnit;
      function GetFY: TFloatWithCSSUnit;
      function GetFR: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetR(AValue: TFloatWithCSSUnit);
      procedure SetFX(AValue: TFloatWithCSSUnit);
      procedure SetFY(AValue: TFloatWithCSSUnit);
      procedure SetFR(AValue: TFloatWithCSSUnit);
    public
      class function GetDOMTag: string; override;
      procedure ConvertToUnit(AUnit: TCSSUnit); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property r: TFloatWithCSSUnit read GetR write SetR;
      property fx: TFloatWithCSSUnit read GetFX write SetFX;
      property fy: TFloatWithCSSUnit read GetFY write SetFY;
      property fr: TFloatWithCSSUnit read GetFR write SetFR;
  end;

  { TSVGStopGradient }

  TSVGStopGradient = class(TSVGElement)
    private
      function GetOffset: TFloatWithCSSUnit;
      function GetStopColor: TBGRAPixel;
      function GetStopOpacity: single;
      procedure SetOffset(AValue: TFloatWithCSSUnit);
      procedure SetStopColor(AValue: TBGRAPixel);
      procedure SetStopOpacity(AValue: single);
    public
      class function GetDOMTag: string; override;
      property offset: TFloatWithCSSUnit read GetOffset write SetOffset;
      property stopColor: TBGRAPixel read GetStopColor write SetStopColor;
      property stopOpacity: single read GetStopOpacity write SetStopOpacity;
  end;

  { TSVGDefine }

  TSVGDefine = class(TSVGElementWithContent)
    public
    class function GetDOMTag: string; override;
  end; 

  { TSVGGroup }

  TSVGGroup = class(TSVGElementWithContent)
  private
    function GetFontSize: TFloatWithCSSUnit;
    function GetIsLayer: boolean;
    function GetName: string;
    procedure SetFontSize(AValue: TFloatWithCSSUnit);
    procedure SetIsLayer(AValue: boolean);
    procedure SetName(AValue: string);
  protected
    procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    class function OwnDatalink: boolean; override;
    property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
  public
    class function GetDOMTag: string; override;
    procedure ConvertToUnit(AUnit: TCSSUnit); override;
    property IsLayer: boolean read GetIsLayer write SetIsLayer;
    property Name: string read GetName write SetName;
  end;

  { TSVGLink }

  TSVGLink = class(TSVGGroup)
  private
    function GetTarget: string;
    function GetXlinkHref: string;
    function GetXlinkTitle: string;
    procedure SetTarget(AValue: string);
    procedure SetXlinkHref(AValue: string);
    procedure SetXlinkTitle(AValue: string);
  public
    class function GetDOMTag: string; override;
    property XlinkHref: string read GetXlinkHref write SetXlinkHref;
    property XlinkTitle: string read GetXlinkTitle write SetXlinkTitle;
    property Target: string read GetTarget write SetTarget;
  end;
  
  { TSVGStyle }

  TSVGRuleset = record
    selector,
    declarations: string;
  end;
  ArrayOfTSVGStyleItem = packed array of TSVGRuleset;

  TSVGStyle = class(TSVGElement)
   private
     FRulesets: ArrayOfTSVGStyleItem;
     function GetRulesetCount: integer;
     procedure Parse(const s: String);
     function IsValidRulesetIndex(const AIndex: integer): boolean;
     function GetRuleset(const AIndex: integer): TSVGRuleset;
     procedure SetRuleset(const AIndex: integer; sr: TSVGRuleset);
     function Find(ARuleset: TSVGRuleset): integer; overload;
   protected
     procedure Initialize; override;
   public
     class function GetDOMTag: string; override;
     constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; override;
     destructor Destroy; override;
     procedure ConvertToUnit(AUnit: TCSSUnit); override;
     function Count: Integer;
     function Find(const AName: string): integer; overload;
     function Add(ARuleset: TSVGRuleset): integer;
     procedure Remove(ARuleset: TSVGRuleset);
     procedure Clear;
     procedure ReParse;
     property Ruleset[AIndex: integer]: TSVGRuleset read GetRuleset write SetRuleset;
     property RulesetCount: integer read GetRulesetCount;
  end;                  

  { TSVGContent }

  TSVGContent = class
    protected
      FDataLink: TSVGDataLink;
      FDomElem: TDOMElement;
      FDoc: TDOMDocument;
      FElements: TFPList;
      FUnits: TCSSUnitConverter;
      function GetDOMNode(AElement: TObject): TDOMNode;
      function GetElementDOMNode(AIndex: integer): TDOMNode;
      procedure AppendElement(AElement: TObject); overload;
      function ExtractElementAt(AIndex: integer): TObject;
      procedure InsertElementBefore(AElement: TSVGElement; ASuccessor: TSVGElement);
      function GetElement(AIndex: integer): TSVGElement;
      function GetElementObject(AIndex: integer): TObject;
      function GetIsSVGElement(AIndex: integer): boolean;
      function GetElementCount: integer;
      function GetUnits: TCSSUnitConverter;
      function TryCreateElementFromNode(ANode: TDOMNode): TObject; virtual;
    public
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink);
      destructor Destroy; override;
      procedure Clear;
      procedure ConvertToUnit(AUnit: TCSSUnit);
      procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer; ARecursive: boolean);
      procedure Recompute;
      procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit); overload;
      procedure Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); overload;
      procedure CopyPathTo(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit); overload;
      procedure CopyPathTo(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); overload;
      function AppendElement(ASVGType: TSVGFactory): TSVGElement; overload;
      procedure BringElement(AElement: TObject; AFromContent: TSVGContent); overload;
      procedure CopyElement(AElement: TObject);
      procedure RemoveElement(AElement: TObject);
      function AppendDOMText(AText: string): TDOMText;
      function AppendDefine: TSVGDefine;
      function AppendLinearGradient(x1,y1,x2,y2: single; AIsPercent: boolean): TSVGLinearGradient; overload;
      function AppendLinearGradient(x1,y1,x2,y2: single; AUnit: TCSSUnit): TSVGLinearGradient; overload;
      function AppendRadialGradient(cx,cy,r,fx,fy,fr: single; AIsPercent: boolean): TSVGRadialGradient; overload;
      function AppendRadialGradient(cx,cy,r,fx,fy,fr: single; AUnit: TCSSUnit): TSVGRadialGradient; overload;
      function AppendStop(AColor: TBGRAPixel; AOffset: single; AIsPercent: boolean): TSVGStopGradient;
      function AppendLine(x1,y1,x2,y2: single; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendLine(p1,p2: TPointF; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendCircle(cx,cy,r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendCircle(c: TPointF; r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendEllipse(cx,cy,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendEllipse(c,r: TPointF; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendPath(data: string; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPath(path: TBGRAPath; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPolygon(const points: array of single; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendPolygon(const points: array of TPointF; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendRect(x,y,width,height: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRect(origin,size: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendImage(x,y,width,height: single; ABitmap: TBGRACustomBitmap; ABitmapOwned: boolean; AUnit: TCSSUnit = cuCustom): TSVGImage; overload;
      function AppendImage(origin,size: TPointF; ABitmap: TBGRACustomBitmap; ABitmapOwned: boolean; AUnit: TCSSUnit = cuCustom): TSVGImage; overload;
      function AppendImage(x,y,width,height: single; ABitmapStream: TStream; AMimeType: string; AUnit: TCSSUnit = cuCustom): TSVGImage; overload;
      function AppendImage(origin,size: TPointF; ABitmapStream: TStream; AMimeType: string; AUnit: TCSSUnit = cuCustom): TSVGImage; overload;
      function AppendText(x,y: single; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendText(origin: TPointF; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendTextSpan(AText: string): TSVGTSpan;
      function AppendRoundRect(x,y,width,height,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRoundRect(origin,size,radius: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendGroup: TSVGGroup;
      function IndexOfElement(AElement: TObject): integer;
      property ElementCount: integer read GetElementCount;
      property Element[AIndex: integer]: TSVGElement read GetElement;
      property ElementObject[AIndex: integer]: TObject read GetElementObject;
      property ElementDOMNode[AIndex: integer]: TDOMNode read GetElementDOMNode;
      property IsSVGElement[AIndex: integer]: boolean read GetIsSVGElement;
      property Units: TCSSUnitConverter read GetUnits;
  end;

function GetSVGFactory(ATagName: string): TSVGFactory;
function CreateSVGElementFromNode(AElement: TDOMElement; AUnits: TCSSUnitConverter;
  ADataLink: TSVGDataLink): TSVGElement;

Implementation
End.
