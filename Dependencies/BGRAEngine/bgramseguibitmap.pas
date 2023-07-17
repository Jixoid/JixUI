// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMSEguiBitmap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  BGRAText, msebitmap;

type

  { TBGRAMSEguiBitmap }

  TBGRAMSEguiBitmap = class(TBGRADefaultBitmap)
  protected
    procedure CopyDataToBitmap(AData: Pointer; AWidth,AHeight: integer; ALineOrder: TRawImageLineOrder; ABitmap: TBitmap);
    procedure RebuildBitmap; override;
    procedure DoLoadFromBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
    procedure InternalAssignBitmapPixels(ASource: TBitmap);
    function GetCanvas: TCanvas; override;
  public
    procedure Assign(ASource: TPersistent); override;
    destructor Destroy; override;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: HDC); override; //not available
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override; //not available
  end;
  
type
  { TBitmapTracker }

  TBitmapTracker = class(TMaskedBitmap)
  protected
    FUser: TBGRADefaultBitmap;
    procedure DoChange; override;
  public
    constructor Create(AUser: TBGRADefaultBitmap); overload;
  end;  

Implementation
End.
