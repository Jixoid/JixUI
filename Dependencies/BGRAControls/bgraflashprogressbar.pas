// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAFlashProgressBar;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LMessages,{$ENDIF} Forms, Controls, Graphics,
  {$IFNDEF FPC}Messages, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, Dialogs, BGRABitmap, BGRADrawerFlashProgressBar;

type
  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TBGRAGraphicCtrl)
  private
    FBGRA: TBGRABitmap;
    FDrawer: TBGRADrawerFlashProgressBar;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    function GetBackgroundColor: TColor;
    function GetBackgroundRandomize: boolean;
    function GetBackgroundRandomizeMaxIntensity: word;
    function GetBackgroundRandomizeMinIntensity: word;
    function GetBarColor: TColor;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
    procedure OnChangeDrawer(Sender: TObject);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
    {$ENDIF}
  published
    property Align;
    property Anchors;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property MinValue: integer Read GetMinValue Write SetMinValue;
    property MaxValue: integer Read GetMaxValue Write SetMaxValue;
    property Value: integer Read GetValue Write SetValue;
    property Color; deprecated 'User BarColor instead';
    property BarColor: TColor read GetBarColor write SetBarColor;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word read GetBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word read GetBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read GetBackgroundRandomize write SetBackgroundRandomize;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnredraw write FOnRedraw;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
