// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAfpGUIBitmap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  BGRAFreeType, EasyLazFreeType, LazFreeTypeFontCollection,
  BGRACanvas;

type

  { TBGRAfpGUIBitmap }

  TBGRAfpGUIBitmap = class(TBGRADefaultBitmap)
  private
    FPseudoCanvas: TBGRACanvas;
    function GetPseudoCanvas: TBGRACanvas;
    function GetBitmapTransparent: boolean;
    procedure SetBitmapTransparent(AValue: boolean);
  protected
    procedure RebuildBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure Init; override;
    procedure FreeData; override;
    procedure ReallocData; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
  public
    destructor Destroy; override;
    procedure AssignToBitmap(ADestination: TBitmap);
    class procedure AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean = false); static;
    class procedure AddFreeTypeFontFile(AFilename: string; AUTF8: boolean = false); static;
    procedure Draw(ACanvas: TCanvas; x, y: integer; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TGUICanvas; x, y: integer; {%H-}Opaque: boolean=True); overload;
    procedure Draw(ACanvas: TGUICanvas; Rect: TRect; {%H-}Opaque: boolean=True); overload;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: HDC); override; //not available
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override; //not available
    property BitmapTransparent: boolean read GetBitmapTransparent write SetBitmapTransparent;
    property Canvas: TBGRACanvas read GetPseudoCanvas;
  end;

Implementation
End.
