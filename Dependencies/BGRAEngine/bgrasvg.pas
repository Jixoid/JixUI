// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVG;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, DOM, BGRAUnits, BGRASVGShapes,
  BGRACanvas2D, BGRASVGType, FPimage;

{ An SVG file has a width and height which describe the viewport size. It is not however really
  the size of the content. The latter is defined as a rectangle called viewbox. There are thus
  various ways of drawing an SVG file. If the viewbox is not specified, then it is a rectangle
  with origin (0,0) and with the same size as the viewport. In this case, a pixel in the viewbox
  corresponds to a pixel in the viewport. Otherwise, viewbox is scaled to fit the viewport.

  ------ Different DPIs -----------------

  The default DPI of the SVG file is the DPI used to convert between pixel units and other units
  like centimeters or inches. In general, this should not be used to scale the image, because it
  can make the SVG measures inconsistent. It should rather be set to the DPI which was used
  internally and which should be 96 according to CSS specifications. Some programs however
  use a different DPI so it can be useful to change it according to the source program.

  The destination DPI that can be specified when drawing can be used to scale the SVG. For example
  you can prepare your SVG to be at the correct size for 96 DPI display and then draw them
  on a display for which the apparent DPI is different (this can be set systemwide on Windows and
  Linux).

  ------ Various ways of scaling --------

  There are various functions to draw and also to get the presentation matrix. The latter is
  a matrix, not including the translation to x and y, that contains all transforms to apply
  to the viewbox coordinates. The drawing functions are Draw and StretchDraw and to get the
  presentation matrix the functions are GetPresentationMatrix and GetStretchPresentationMatrix
  respectively.

  Drawing in user coordinates, i.e. without taking account of the viewbox or viewport size. One
  can achieve this by calling the Draw function with an x and y coordinates and a destination
  unit/DPI. You would do that only if you want to make use of the viewbox offset or
  if you already have applied all necessary transforms. The corresponding presentation matrix
  is the identity.

  Drawing with a specific alignment in user coordinates. This can be done by calling Draw
  with horizontal and vertical alignment, x and y coordinates to align to, a destination unit/DPI
  and Scaled set to False.

  Drawing with a specific alignment, scaled to view port units. This will draw the SVG at its
  expected size but apply the alignment you want. This is what you would want to do if you
  want to customize the alignment. One can achieve this as above but by specifying the Scaled
  parameter to True.

  Drawing inside a specified rectangle, not preserving the aspect ratio. In this case, you
  want a certain size and the original size does not matter much. The viewbox of the SVG is
  stretched to fit the rectangle. One can achieve this by calling StretchDraw with the
  specified rectangle and the UseSvgAspectRatio to False.

  Drawing inside a rectangle, using the parameter of the SVG about the aspect ratio. In this case,
  the SVG may either be stretched as before, scaled to fit the rectangle or scaled to cover it.
  Also the SVG may be set with a certain horizontal and vertical alignment within the rectangle.
  One can draw this way by calling StretchDraw with the UseSvgAspectRatio set to True.
  You would typically use this method to show the SVG as it was intended to look like but in
  a custom container by suppling WidthAsPixel and HeightAsPixel for the rectangle size.

  Drawing inside a rectangle, preserving the aspect ratio. In this case, you call StretchDraw with
  a horizontal and vertical alignment, the rectangle and the Slice parameter. When Slice is set
  to False, the viewbox will fit the rectangle. If Slice is to True then the viewbox may overflow
  the rectangle so that it covers the whole surface. Using this function, one can have control
  over the way the SVG is scaled. Slice is useful for background.

  Drawing inside a rectangle of the size defined by the SVG. That will draw the SVG as it is
  supposed to look like. One can do that by calling StretchDraw with a unit parameter. You would
  typically use pixel units and this will draw to the size WidthAsPixel by HeightAsPixel.

  }

type
  TCSSUnit = BGRAUnits.TCSSUnit;

const
  cuCustom = BGRAUnits.cuCustom;
  cuPixel = BGRAUnits.cuPixel;
  cuCentimeter = BGRAUnits.cuCentimeter;
  cuMillimeter = BGRAUnits.cuMillimeter;
  cuInch = BGRAUnits.cuInch;
  cuPica = BGRAUnits.cuPica;
  cuPoint = BGRAUnits.cuPoint;
  cuFontEmHeight = BGRAUnits.cuFontEmHeight;
  cuFontXHeight = BGRAUnits.cuFontXHeight;
  cuPercent = BGRAUnits.cuPercent;

type

  { TSVGUnits }

  TSVGUnits = class(TCSSUnitConverter)
  private
    FOnRecompute: TSVGRecomputeEvent;
    procedure Recompute;
    procedure SetOnRecompute(AValue: TSVGRecomputeEvent);
  protected
    FSvg: TDOMElement;
    FContainerHeight: TFloatWithCSSUnit;
    FContainerWidth: TFloatWithCSSUnit;
    FDefaultDpi: PSingle;

    //fetched or computed
    FViewBox: TSVGViewBox;
    FPreserveAspectRatio: TSVGPreserveAspectRatio;
    FViewPortSize, FProportionalViewSize, FStretchedViewSize: TSVGSize;

    procedure SetContainerHeight(AValue: TFloatWithCSSUnit);
    procedure SetContainerWidth(AValue: TFloatWithCSSUnit);
    function GetDpiX: single; override;
    function GetDpiY: single; override;
    function GetCustomOrigin: TPointF;
    procedure SetCustomOrigin(AValue: TPointF);
    procedure SetViewBox(AValue: TSVGViewBox);
  public
    procedure SetDefaultDpiAndOrigin;
    constructor Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
    function GetStretchRectF(AViewPort: TRectF; const par: TSVGPreserveAspectRatio): TRectF;
    property ViewBox: TSVGViewBox read FViewBox write SetViewBox;
    property ViewPortSize: TSVGSize read FViewPortSize;
    property ProportionalViewSize: TSVGSize read FProportionalViewSize;
    property PreserveAspectRatio: TSVGPreserveAspectRatio read FPreserveAspectRatio;
    property StretchedViewSize: TSVGSize read FStretchedViewSize;
    property CustomOrigin: TPointF read GetCustomOrigin write SetCustomOrigin;
    property ContainerWidth: TFloatWithCSSUnit read FContainerWidth write SetContainerWidth;
    property ContainerHeight: TFloatWithCSSUnit read FContainerHeight write SetContainerHeight;
    property OnRecompute: TSVGRecomputeEvent read FOnRecompute write SetOnRecompute;
  end;

  { TBGRASVG }

  TBGRASVG = class(TSVGCustomElement)
  private
    function GetColor: TBGRAPixel;
    function GetComputedHeight: TFloatWithCSSUnit;
    function GetComputedWidth: TFloatWithCSSUnit;
    function GetContainerHeight: TFloatWithCSSUnit;
    function GetContainerHeightAsPixel: single;
    function GetContainerWidth: TFloatWithCSSUnit;
    function GetContainerWidthAsPixel: single;
    function GetFontSize: TFloatWithCSSUnit;
    function GetHeight: TFloatWithCSSUnit;
    function GetHeightAsCm: single;
    function GetHeightAsInch: single;
    function GetHeightAsPixel: single;
    function GetLayer(AIndex: integer): TSVGGroup;
    function GetLayerCount: integer;
    function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
    function GetUnits: TSVGUnits;
    function GetUTF8String: utf8string;
    function GetViewBox: TSVGViewBox; overload;
    function GetViewBox(AUnit: TCSSUnit): TSVGViewBox; overload;
    procedure GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
    function GetViewMin(AUnit: TCSSUnit): TPointF;
    function GetViewSize(AUnit: TCSSUnit): TPointF;
    function GetVisualHeight: TFloatWithCSSUnit;
    function GetVisualHeightAsPixel: single;
    function GetVisualWidth: TFloatWithCSSUnit;
    function GetVisualWidthAsPixel: single;
    function GetWidth: TFloatWithCSSUnit;
    function GetWidthAsCm: single;
    function GetWidthAsInch: single;
    function GetWidthAsPixel: single;
    function GetZoomable: boolean;
    procedure SetColor(AValue: TBGRAPixel);
    procedure SetContainerHeight(AValue: TFloatWithCSSUnit);
    procedure SetContainerHeightAsPixel(AValue: single);
    procedure SetContainerWidth(AValue: TFloatWithCSSUnit);
    procedure SetContainerWidthAsPixel(AValue: single);
    procedure SetDefaultDpi(AValue: single);
    procedure SetFontSize(AValue: TFloatWithCSSUnit);
    procedure SetHeight(AValue: TFloatWithCSSUnit);
    procedure SetHeightAsCm(AValue: single);
    procedure SetHeightAsInch(AValue: single);
    procedure SetHeightAsPixel(AValue: single);
    procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
    procedure SetUTF8String(AValue: utf8string);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetWidth(AValue: TFloatWithCSSUnit);
    procedure SetWidthAsCm(AValue: single);
    procedure SetWidthAsInch(AValue: single);
    procedure SetWidthAsPixel(AValue: single);
    procedure SetZoomable(AValue: boolean);
  protected
    FXml: TXMLDocument;
    FDefaultDpi: single;
    FContent: TSVGContent;
    FDataLink: TSVGDataLink;
    procedure Init(ACreateEmpty: boolean);
    function GetViewBoxAlignment(AHorizAlign: TAlignment; AVertAlign: TTextLayout; AUnit: TCSSUnit): TPointF;
    function GetViewBoxScale: TPointF;
    procedure UnitsRecompute(Sender: TObject);
    procedure SetAttribute(AName: string; AValue: string); override;
  public
    constructor Create; overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AStream: TStream); overload;
    constructor CreateFromString(AUTF8String: string);
    destructor Destroy; override;
    function Duplicate: TBGRASVG;
    procedure CropToViewBox(AScale: single = 1);
    procedure LoadFromFile(AFilenameUTF8: string);
    procedure LoadFromStream(AStream: TStream; AURI: UnicodeString = 'stream:');
    procedure LoadFromResource(AFilename: string);
    procedure SaveToFile(AFilenameUTF8: string);
    procedure SaveToStream(AStream: TStream);
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: TPointF); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; AUnit: TCSSUnit = cuPixel; AScale: boolean = true); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: single; AScale: boolean = true); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: TPointF; AScale: boolean = true); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x,y,w,h: single; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; r: TRectF; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
      AVertAlign: TTextLayout; x,y,w,h: single; ASlice: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
      AVertAlign: TTextLayout; r: TRectF; ASlice: boolean = false); overload;
    function GetStretchRectF(AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y,w,h: single; ASlice: boolean = false): TRectF; overload;
    function GetStretchRectF(x,y,w,h: single): TRectF; overload;
    function GetPresentationMatrix(AHorizAlign: TAlignment; AVertAlign: TTextLayout;
      AUnit: TCSSUnit; AScale: boolean): TAffineMatrix;
    function GetStretchPresentationMatrix(w,h: single; useSvgAspectRatio: boolean = false): TAffineMatrix; overload;
    function GetStretchPresentationMatrix(AHorizAlign: TAlignment; AVertAlign: TTextLayout; w,h: single; ASlice: boolean = false): TAffineMatrix; overload;
    function GetStretchPresentationMatrix(AUnit: TCSSUnit): TAffineMatrix; overload;
    function FindElementById(AID: string): TSVGElement; overload;
    function FindElementById(AID: string; AClass: TSVGFactory): TSVGElement; overload;
    procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer;
      ARecursive: boolean); override;
    procedure ConvertToUnit(AUnit: TCSSUnit); override; //except Width, Height, ContainerWidth, ContainerHeight
    property AsUTF8String: utf8string read GetUTF8String write SetUTF8String;
    property Units: TSVGUnits read GetUnits;
    property FontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
    property Color: TBGRAPixel read GetColor write SetColor;
    property Width: TFloatWithCSSUnit read GetWidth write SetWidth;
    property Height: TFloatWithCSSUnit read GetHeight write SetHeight;
    property ComputedWidth: TFloatWithCSSUnit read GetComputedWidth;
    property ComputedHeight: TFloatWithCSSUnit read GetComputedHeight;
    property WidthAsPixel: single read GetWidthAsPixel write SetWidthAsPixel;
    property HeightAsPixel: single read GetHeightAsPixel write SetHeightAsPixel;
    property WidthAsCm: single read GetWidthAsCm write SetWidthAsCm;
    property HeightAsCm: single read GetHeightAsCm write SetHeightAsCm;
    property WidthAsInch: single read GetWidthAsInch write SetWidthAsInch;
    property HeightAsInch: single read GetHeightAsInch write SetHeightAsInch;
    property ContainerWidth: TFloatWithCSSUnit read GetContainerWidth write SetContainerWidth;
    property ContainerWidthAsPixel: single read GetContainerWidthAsPixel write SetContainerWidthAsPixel;
    property ContainerHeight: TFloatWithCSSUnit read GetContainerHeight write SetContainerHeight;
    property ContainerHeightAsPixel: single read GetContainerHeightAsPixel write SetContainerHeightAsPixel;
    property Zoomable: boolean read GetZoomable write SetZoomable;
    property ViewBox: TSVGViewBox read GetViewBox write SetViewBox;
    property ViewBoxInUnit[AUnit: TCSSUnit]: TSVGViewBox read GetViewBox;
    property ViewMinInUnit[AUnit: TCSSUnit]: TPointF read GetViewMin;
    property ViewSizeInUnit[AUnit: TCSSUnit]: TPointF read GetViewSize;
    property VisualWidth: TFloatWithCSSUnit read GetVisualWidth;
    property VisualHeight: TFloatWithCSSUnit read GetVisualHeight;
    property VisualWidthAsPixel: single read GetVisualWidthAsPixel;
    property VisualHeightAsPixel: single read GetVisualHeightAsPixel;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property AttributeDef[AName: string; ADefault: string]: string read GetAttribute;
    property DefaultDpi: single read FDefaultDpi write SetDefaultDpi; //this is not saved in the SVG file
    property Content: TSVGContent read FContent;
    property DataLink: TSVGDataLink read FDataLink;//(for test or internal info)
    property preserveAspectRatio: TSVGPreserveAspectRatio read GetPreserveAspectRatio write SetPreserveAspectRatio;
    property Layer[AIndex: integer]: TSVGGroup read GetLayer;
    property LayerCount: integer read GetLayerCount;
  end;

  { TFPReaderSVG }

  TFPReaderSVG = class(TBGRAImageReader)
    private
      FRenderDpi: single;
      FWidth,FHeight: integer;
      FScale: single;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      constructor Create; override;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
      property RenderDpi: single read FRenderDpi write FRenderDpi;
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property Scale: single read FScale write FScale;
  end;

procedure RegisterSvgFormat;

Implementation
End.
