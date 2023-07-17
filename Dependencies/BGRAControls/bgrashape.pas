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
unit BGRAShape;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BCTypes;

type
  TBGRAShapeType = (stRegularPolygon, stEllipse);

  { TBGRAShape }

  TBGRAShape = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FBorderColor: TColor;
    FBorderOpacity: byte;
    FBorderStyle: TPenStyle;
    FBorderWidth: integer;
    FBorderGradient: TBCGradient;
    FUseBorderGradient: boolean;
    FFillColor:  TColor;
    FFillOpacity: byte;
    FFillGradient: TBCGradient;
    FUseFillGradient: boolean;
    FRoundRadius: integer;
    FBGRA:       TBGRABitmap;
    FSideCount:  integer;
    FRatioXY:    single;
    FUseRatioXY: boolean;
    FAngle:      single;
    FShapeType:  TBGRAShapeType;
    procedure SetAngle(const AValue: single);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderGradient(const AValue: TBCGradient);
    procedure SetBorderOpacity(const AValue: byte);
    procedure SetBorderStyle(const AValue: TPenStyle);
    procedure SetBorderWidth(AValue: integer);
    procedure SetFillColor(const AValue: TColor);
    procedure SetFillGradient(const AValue: TBCGradient);
    procedure SetFillOpacity(const AValue: byte);
    procedure SetRatioXY(const AValue: single);
    procedure SetRoundRadius(AValue: integer);
    procedure SetShapeType(const AValue: TBGRAShapeType);
    procedure SetSideCount(AValue: integer);
    procedure SetUseBorderGradient(const AValue: boolean);
    procedure SetUseFillGradient(const AValue: boolean);
    procedure SetUseRatioXY(const AValue: boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property AutoSize;
    property Align;
    property Anchors;
    property Angle: single Read FAngle Write SetAngle {$IFDEF FPC}default 0{$ENDIF};
    property BorderWidth: integer Read FBorderWidth Write SetBorderWidth default 1;
    property BorderOpacity: byte Read FBorderOpacity Write SetBorderOpacity default 255;
    property BorderColor: TColor Read FBorderColor Write SetBorderColor;
    property BorderGradient: TBCGradient Read FBorderGradient Write SetBorderGradient;
    property BorderStyle: TPenStyle
      Read FBorderStyle Write SetBorderStyle default psSolid;
    property FillColor: TColor Read FFillColor Write SetFillColor;
    property FillOpacity: byte Read FFillOpacity Write SetFillOpacity;
    property FillGradient: TBCGradient Read FFillGradient Write SetFillGradient;
    property SideCount: integer Read FSideCount Write SetSideCount default 4;
    property RatioXY: single Read FRatioXY Write SetRatioXY {$IFDEF FPC}default 1{$ENDIF};
    property UseRatioXY: boolean Read FUseRatioXY Write SetUseRatioXY default False;
    property UseFillGradient: boolean Read FUseFillGradient
      Write SetUseFillGradient default False;
    property UseBorderGradient: boolean Read FUseBorderGradient
      Write SetUseBorderGradient default False;
    property ShapeType: TBGRAShapeType
      Read FShapeType Write SetShapeType default stRegularPolygon;
    property BorderSpacing;
    property Caption;
    property PopupMenu;
    property RoundRadius: integer Read FRoundRadius Write SetRoundRadius default 0;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
