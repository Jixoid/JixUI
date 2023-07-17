// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Equivalent of standard lazarus TLabel but using BGRA Controls framework for text
  render.

  Functionality:
  - Customizable background (gradients etc.)
  - Customizable border (rounding etc.)
  - FontEx (shadow, word wrap, etc.)

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCLabel;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils,{$IFDEF FPC}LResources,{$ENDIF}
  types, Forms, Controls, Graphics, Dialogs,
  BCBasectrls, BGRABitmap, BGRABitmapTypes, BCTypes;

type

  { TCustomBCLabel }

  TCustomBCLabel = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBGRA: TBGRABitmapEx;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    FInnerMargin: single;
    FRounding: TBCRounding;
    procedure Render;
    procedure SetInnerMargin(AValue: single);
    procedure SetRounding(AValue: TBCRounding);
    procedure UpdateSize;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(AValue: TBCFont);
    procedure OnChangeProperty(Sender: TObject; {%H-}Data: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
    function GetStyleExtension: String; override;
  protected
    { Protected declarations }
    property AutoSize default True;
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property InnerMargin: single read FInnerMargin write SetInnerMargin;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  { TBCLabel }

  TBCLabel = class(TCustomBCLabel)
  published
    property Action;
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property Background;
    property Border;
    property BorderSpacing;
    property Caption;
    property Cursor;
    property Enabled;
    property FontEx;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property InnerMargin;
    property Left;
    property PopupMenu;
    property Rounding;
    property ShowHint;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
