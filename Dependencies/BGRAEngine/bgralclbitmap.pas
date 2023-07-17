// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALCLBitmap;

{$mode objfpc}{$H+}
{$WARN 6018 off : unreachable code}
interface

uses
  BGRAClasses, SysUtils, Graphics, GraphType, BGRABitmapTypes, BGRADefaultBitmap;

type
  { TBGRALCLBitmap }

  TBGRALCLBitmap = class(TBGRADefaultBitmap)
  protected
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    procedure DoLoadFromBitmap; override;
    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: integer; AData: PBGRAPixel
      ): TBGRAPtrBitmap; override;
    procedure AssignRasterImage(ARaster: TRasterImage); virtual;
    procedure ExtractXorMask;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromResource(AFilename: string; AOptions: TBGRALoadingOptions); overload; override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    function MakeBitmapCopy(BackgroundColor: TColor; AMasked: boolean = False): TBitmap; override;
    procedure AssignToBitmap(ADestination: TBitmap);
    procedure LoadFromDevice({%H-}DC: HDC); override;
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
  end;

  { TBGRALCLPtrBitmap }

  TBGRALCLPtrBitmap = class(TBGRAPtrBitmap)

    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: integer; AData: PBGRAPixel): TBGRAPtrBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
  public
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
  end;

type

  { TBitmapTracker }

  TBitmapTracker = class(TBitmap)
  protected
    FUser: TBGRADefaultBitmap;
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(AUser: TBGRADefaultBitmap); overload;
  end;

Implementation
End.
