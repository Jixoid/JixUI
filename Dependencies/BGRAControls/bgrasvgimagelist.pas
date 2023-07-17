unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FGL,
  XMLConf, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  TListOfTStringList = TFPGObjectList<TStringList>;

  { TBGRASVGImageList }

  TBGRASVGImageList = class(TComponent)
  private
    FHeight: integer;
    FHorizontalAlignment: TAlignment;
    FItems: TListOfTStringList;
    FReferenceDPI: integer;
    FTargetRasterImageList: TImageList;
    FUseSVGAlignment: boolean;
    FVerticalAlignment: TTextLayout;
    FWidth: integer;
    FRasterized: boolean;
    procedure ReadData(Stream: TStream);
    procedure SetHeight(AValue: integer);
    procedure SetTargetRasterImageList(AValue: TImageList);
    procedure SetWidth(AValue: integer);
    procedure WriteData(Stream: TStream);
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount: integer;
    // Get SVG string
    function GetSVGString(AIndex: integer): string; overload;
    procedure Rasterize;
    procedure RasterizeIfNeeded;
    procedure QueryRasterize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ASVG: string): integer;
    procedure Remove(AIndex: integer);
    procedure Exchange(AIndex1, AIndex2: integer);
    procedure Replace(AIndex: integer; ASVG: string);
    function GetScaledSize(ATargetDPI: integer): TSize;
    // Get TBGRABitmap with custom width and height
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer): TBGRABitmap; overload;
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBGRABitmap; overload;
    // Get TBitmap with custom width and height
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer): TBitmap; overload;
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBitmap; overload;
    // Draw image with custom width and height. The Width and
    // Height property are in LCL coordinates.
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer); overload;
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean;
      AOpacity: byte = 255); overload;
    // Draw image with custom width, height and canvas scale. The Width and
    // Height property are in LCL coordinates. CanvasScale is useful on MacOS
    // where LCL coordinates do not match actual pixels.
    procedure Draw(AIndex: integer; ACanvasScale: single; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer); overload;
    procedure Draw(AIndex: integer; ACanvasScale: single; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean;
      AOpacity: byte = 255); overload;
    // Draw on the target BGRABitmap with specified Width and Height.
    procedure Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF); overload;
    procedure Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF;
      AUseSVGAlignment: boolean); overload;

    // Generate bitmaps for an image list
    procedure PopulateImageList(const AImageList: TImageList; AWidths: array of integer);
    property SVGString[AIndex: integer]: string read GetSVGString;
    property Count: integer read GetCount;
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property ReferenceDPI: integer read FReferenceDPI write FReferenceDPI default 96;
    property UseSVGAlignment: boolean read FUseSVGAlignment write FUseSVGAlignment default False;
    property HorizontalAlignment: TAlignment read FHorizontalAlignment write FHorizontalAlignment default taCenter;
    property VerticalAlignment: TTextLayout read FVerticalAlignment write FVerticalAlignment default tlCenter;
    property TargetRasterImageList: TImageList read FTargetRasterImageList write SetTargetRasterImageList default nil;
  end;

procedure Register;

Implementation
End.
