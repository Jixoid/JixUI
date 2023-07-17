// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVGOriginal;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap, BGRASVG, BGRATransform,
  BGRALayerOriginal, BGRAUnits, BGRALayers;

type
  TBGRALayerSVGOriginal = class;

  { TBGRASVGOriginalDiff }

  TBGRASVGOriginalDiff = class(TBGRAOriginalDiff)
  protected
    FContentVersionBefore,FContentVersionAfter: integer;
    FSvgStreamBefore,FSvgStreamAfter: TMemoryStream;
    FDpiBefore, FDpiAfter: single;
  public
    constructor Create(AFromOriginal: TBGRALayerSVGOriginal);
    procedure ComputeDiff(AToOriginal: TBGRALayerSVGOriginal);
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
    destructor Destroy; override;
  end;

  { TBGRALayerSVGOriginal }

  TBGRALayerSVGOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetDPI: single;
    function GetSvgHeight: single;
    function GetSvgWidth: single;
    procedure SetDPI(AValue: single);
  protected
    FSVG: TBGRASVG;
    FPresentationMatrix: TAffineMatrix;
    FDiff: TBGRASVGOriginalDiff;
    FContentVersion: integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ComputePresentation(AContainerWidth, AContainerHeight: integer; AScaleDPI: single);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveSVGToStream(AStream: TStream);
    procedure SetSVG(ASVG: TBGRASVG; AContainerWidth: integer = 640;
      AContainerHeight: integer = 480; AScaleDPI: single = 1);
    procedure LoadSVGFromStream(AStream: TStream; AContainerWidth: integer = 640;
      AContainerHeight: integer = 480; AScaleDPI: single = 1);
    function GetSVGCopy: TBGRASVG;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TBGRASVG; override;
    property Width: single read GetSvgWidth;
    property Height: single read GetSvgHeight;
    property DPI: single read GetDPI write SetDPI;
    property PresentationMatrix: TAffineMatrix read FPresentationMatrix;
  end;

  { TBGRALayeredSVG }

  TBGRALayeredSVG = class(TBGRALayeredBitmap)
    protected
      function GetMimeType: string; override;
      procedure InternalLoadFromStream(AStream: TStream);
      procedure InternalSaveToStream(AStream: TStream);
    public
      ContainerWidth, ContainerHeight, DefaultSvgDPI, DPI: integer;
      DefaultLayerName: string;
      constructor Create; overload; override;
      constructor Create(AWidth, AHeight: integer); overload; override;
      procedure LoadFromStream(AStream: TStream); override;
      procedure LoadFromFile(const filenameUTF8: string); override;
      procedure SaveToStream(AStream: TStream); override;
      procedure SaveToFile(const filenameUTF8: string); override;
  end;

Implementation
End.
