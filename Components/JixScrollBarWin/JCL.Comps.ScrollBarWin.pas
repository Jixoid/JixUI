unit JCL.Comps.ScrollBarWin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.CompTypes,
  JCL.Utils,
  JCL.Comps.ScrollBar,
  BGRABitmap, BGRABitmapTypes, LResources, Forms, Controls, Graphics, Dialogs,
  Math;

type
  TJixScrollBarWin = class(TJixWinControl)
  private
    Var ClickP: TPoint;
    Var FEdgeSpace: Int8U;
    Var FEdge: Int8U;
    Var FStep: Int16U;
    Var FPage: Int16U;
    Var FLegPos: Int16U;
    Var FPosition: Int16U;
    Var FOnChange: mNotify;
    Var FOrientation: eScrollbarOrientation;
    Var FChangeable: Boolean;
    Var Animating_Hover: (jahNil, jahEnter, jahLeave);

    Var FBarCaption: String;

    Var VisualBarSize: Integer;

    Procedure SetEdgeSpace(Value: Int8U);
    Procedure SetStep(Value: Int16U);
    Procedure SetPage(Value: Int16U);
    Procedure SetPosition(Value: Int16U);
    Procedure SetOrientation(Value: eScrollbarOrientation);

    Procedure SetBarCaption(Value: String);

  protected
    Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mouseenter; Override;
    Procedure Mouseleave; Override;

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;
    Procedure SetPositionSlient(Value: Int16U);

  published
    Property EdgeSpace: Int8U    Read FEdgeSpace   Write SetEdgeSpace   Default 14;
    Property Step: Int16U        Read FStep        Write SetStep        Default 1;
    Property Page: Int16U        Read FPage        Write SetPage;
    Property Position: Int16U    Read FPosition    Write SetPosition;
    Property OnChange: mNotify   Read FOnChange    Write FOnChange;
    Property Orientation: eScrollbarOrientation
                                 Read FOrientation Write SetOrientation Default jsoHoriznal;
    Property Changeable: Boolean Read FChangeable  Write FChangeable    Default True;
    Property BarCaption: String  Read FBarCaption  Write SetBarCaption;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;
    Property StateHover;
    Property StateClick;
    Property StateBack;

    Property Static;
    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property OnChangeBounds;
    Property OnPaintBefore;
    Property OnPaintAfter;
    Property OnClick;
    Property OnDblClick;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property OnResize;
    Property PopupMenu;
  end;

  Procedure Register;

Implementation
End.
