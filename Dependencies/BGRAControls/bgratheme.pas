// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRASVGImageList;

type
  TBGRAThemeButtonState = (btbsNormal, btbsHover, btbsActive, btbsDisabled);

  { TBGRAThemeSurface }

  TBGRAThemeSurface = class
  private
    FBitmap: TBGRABitmap;
    FBitmapRect: TRect;
    FCanvasScale: single;
    FDestCanvas: TCanvas;
    FLclDPI: integer;
    function GetBitmap: TBGRABitmap;
    function GetBitmapDPI: integer;
    procedure SetBitmapRect(AValue: TRect);
  public
    constructor Create(AControl: TCustomControl);
    constructor Create(ADestRect: TRect; ADestCanvas: TCanvas; ACanvasScale: single; ALclDPI: integer);
    destructor Destroy; override;
    procedure DrawBitmap;
    procedure DiscardBitmap;
    procedure BitmapColorOverlay(AColor: TBGRAPixel; AOperation: TBlendOperation = boTransparent); overload;
    function ScaleForCanvas(AValue: integer; AFromDPI: integer = 96): integer;
    function ScaleForBitmap(AValue: integer; AFromDPI: integer = 96): integer;
    function ScaleForBitmap(const ARect: TRect; AFromDPI: integer = 96): TRect;
    property DestCanvas: TCanvas read FDestCanvas;
    property DestCanvasDPI: integer read FLclDPI;
    property Bitmap: TBGRABitmap read GetBitmap;
    property BitmapRect: TRect read FBitmapRect write SetBitmapRect;
    property BitmapDPI: integer read GetBitmapDPI;
  end;

  TBGRATheme = class;

  { TBGRAThemeControl }

  TBGRAThemeControl = class(TCustomControl)
  private
    FTheme: TBGRATheme;
    procedure SetTheme(AValue: TBGRATheme);
  public
    destructor Destroy; override;
  published
    property Theme: TBGRATheme read FTheme write SetTheme;
  end;

  { TBGRATheme }

  TBGRATheme = class(TComponent)
  private
    FThemedControls: TList;
    function GetThemedControl(AIndex: integer): TBGRAThemeControl;
    function GetThemedControlCount: integer;
    procedure AddThemedControl(AControl: TBGRAThemeControl);
    procedure RemoveThemedControl(AControl: TBGRAThemeControl);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateThemedControls;

    function PreferredButtonWidth(const hasGlyph: boolean): Integer; virtual;
    function PreferredButtonHeight(const hasGlyph: boolean): Integer; virtual;

    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); virtual;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;

    property ThemedControlCount: integer read GetThemedControlCount;
    property ThemedControl[AIndex: integer]: TBGRAThemeControl read GetThemedControl;
  published

  end;

var
  BGRADefaultTheme: TBGRATheme;

procedure Register;

Implementation
End.
