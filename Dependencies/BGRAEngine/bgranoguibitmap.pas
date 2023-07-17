// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRANoGUIBitmap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  BGRAFreeType, EasyLazFreeType, LazFreeTypeFontCollection,
  BGRACanvas;

type

  { TBGRANoGUIBitmap }

  TBGRANoGUIBitmap = class(TBGRADefaultBitmap)
  private
    FPseudoCanvas: TBGRACanvas;
    function GetPseudoCanvas: TBGRACanvas;
  protected
    procedure RebuildBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure Init; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
  public
    destructor Destroy; override;
    procedure AssignToBitmap(ADestination: TBitmap);
    class procedure AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean = false); static;
    class procedure AddFreeTypeFontFile(AFilename: string; AUTF8: boolean = false); static;
    procedure Draw(ACanvas: TCanvas; x, y: integer; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; {%H-}Opaque: boolean=True); override;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: HDC); override; //not available
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override; //not available
    property Canvas: TBGRACanvas read GetPseudoCanvas;
  end;

Implementation
End.
