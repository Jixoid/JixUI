// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCSVGViewer;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  {$IFDEF FPC}LResources, LCLType, {$ENDIF}
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BGRASVG, BGRAUnits, BCTypes;

type

  { TBCSVGViewer }

  TBCSVGViewer = class(TCustomBGRAGraphicControl)
  private
    FDrawCheckers: boolean;
    FHorizAlign: TAlignment;
    FProportional: boolean;
    FStretchMode: TBCStretchMode;
    FDestDPI: single;
    FUseSVGAlignment: boolean;
    FVertAlign: TTextLayout;
    Fx: single;
    Fy: single;
    function GetSVGString: string;
    procedure SetDrawCheckers(AValue: boolean);
    procedure SetFDestDPI(AValue: single);
    procedure SetSVGString(AValue: string);
    procedure SetFx(AValue: single);
    procedure SetFy(AValue: single);
    procedure SetHorizAlign(AValue: TAlignment);
    procedure SetProportional(AValue: boolean);
    procedure SetStretchMode(AValue: TBCStretchMode);
    procedure SetUseSVGAlignment(AValue: boolean);
    procedure SetVertAlign(AValue: TTextLayout);
  protected
    FSVG: TBGRASVG;
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResource(Resource: string);
    function GetSVGRectF: TRectF;
    function GetSVGContainerRectF: TRectF;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property BorderSpacing;
    property Constraints;
    property SVG: TBGRASVG read FSVG;
    property SVGString: string read GetSVGString write SetSVGString;
    property DestDPI: single read FDestDPI write SetFDestDPI {$IFDEF FPC} default
      96{$ENDIF};
    property x: single read Fx write SetFx {$IFDEF FPC} default 0{$ENDIF};
    property y: single read Fy write SetFy {$IFDEF FPC} default 0{$ENDIF};
    property HorizAlign: TAlignment read FHorizAlign write SetHorizAlign default
      taCenter;
    property VertAlign: TTextLayout read FVertAlign write SetVertAlign default tlCenter;
    property StretchMode: TBCStretchMode
      read FStretchMode write SetStretchMode default smStretch;
    property Proportional: boolean read FProportional write SetProportional default True;
    property DrawCheckers: boolean
      read FDrawCheckers write SetDrawCheckers default False;
    property UseSVGAlignment: boolean read FUseSVGAlignment write SetUseSVGAlignment default False;
    property Color;
    property ColorOpacity;
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
    property OnResize;
    property Caption;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
