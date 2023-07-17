// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ATShapeLine is a component which paints line (directions: left-right, up-down, diagonals), with or without arrows at both sides. Line width is option. Line color and arrow color are options. It is Lazarus port of Delphi component TLine (renamed since TLine id is busy with TAChart).

Original author: Gon Perez-Jimenez (Spain, 2002)
Ported to Lazarus by: Alexey Torgashin (Russia)

- I redone get/set of canvas.pen and canvas.brush: do it only inside Paint, before it was all accross the code, in getters, setters, etc. This gives crashes of IDE on changing props in Linux.
- I added any linewidth for any direction with arrow1=true and arrow2=true.
- I converted demo to Laz using ide converter.
- Icon added to component-pallette to 'Misc'.

For BGRAControls by: Lainz

- Using BGRABitmap antialiased drawing (2020-09-09)

Lazarus: 1.6+}

unit atshapelinebgra;

interface

{$mode delphi}

uses
  Graphics, SysUtils, Classes, Controls;

type
  TShapeLineDirection = (drLeftRight, drUpDown, drTopLeftBottomRight, drTopRightBottomLeft);

  { TShapeLineBGRA }

  TShapeLineBGRA = class(TGraphicControl)
  private
    { Private declarations }
    FLineDir: TShapeLineDirection;
    FArrow1: Boolean;
    FArrow2: Boolean;
    FArrowFactor: Integer;
    FLineWidth: integer;
    FLineColor: TColor;
    FArrowColor: TColor;

    procedure SetArrowColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineDir(AValue: TShapeLineDirection);
    procedure SetArrow1(Value: Boolean);
    procedure SetArrow2(Value: Boolean);
    procedure SetArrowFactor(Value: integer);
    procedure SetLineWidth(AValue: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DragCursor;
    property DragKind;
    property DragMode;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property Direction: TShapeLineDirection read FLineDir write SetLineDir default drLeftRight;
    property LineColor: TColor read FLineColor write SetLineColor;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property Arrow1: Boolean read FArrow1 write SetArrow1 default False;
    property Arrow2: Boolean read FArrow2 write SetArrow2 default False;
    property ArrowFactor: Integer read FArrowFactor write SetArrowFactor default 8;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
  end;

  procedure Register;

Implementation
End.
