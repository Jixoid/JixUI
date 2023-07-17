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
unit BGRAGraphicControl;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, Types,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, ExtCtrls, BGRABitmap, BCTypes;

type

  { TCustomBGRAGraphicControl }

  TCustomBGRAGraphicControl = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FOnRedraw:     TBGRARedrawEvent;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:   TBevelWidth;
    FBorderWidth:  TBorderWidth;
    FAlignment:    TAlignment;
    FColorOpacity: byte;
    function GetBitmapHeight: integer;
    function GetBitmapScale: double;
    function GetBitmapWidth: integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBitmapAutoScale(AValue: boolean);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetColorOpacity(const AValue: byte);
  protected
    { Protected declarations }
    FBGRA: TBGRABitmap;
    FBitmapAutoScale: boolean;
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer); virtual;
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    property BitmapAutoScale: boolean read FBitmapAutoScale write SetBitmapAutoScale default true;
    property BitmapScale: double read GetBitmapScale;
    property BitmapWidth: integer read GetBitmapWidth;
    property BitmapHeight: integer read GetBitmapHeight;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap;
    procedure DiscardBitmap;
    destructor Destroy; override;
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel
      Read FBevelOuter Write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
    property ColorOpacity: byte Read FColorOpacity Write SetColorOpacity;
    property Alignment: TAlignment Read FAlignment Write SetAlignment;
  end;

  TBGRAGraphicControl = class(TCustomBGRAGraphicControl)
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property BitmapAutoscale;
    property BitmapScale;
    property BorderWidth;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Color;
    property ColorOpacity;
    property Alignment;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF FPC}
    property OnPaint;
    {$ENDIF}
    property Caption;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
