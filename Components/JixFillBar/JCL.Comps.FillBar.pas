unit JCL.Comps.FillBar;

{$mode ObjFPC}{$H+}{$Inline ON}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.CompTypes,
  JCL.Utils,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes;

type
  eFillbarOrientation = (
    jfoVertical,
    jfoHoriznal
  );

  TJixFillBar = class(TJixControl)
  private
    Var ClickP: TPoint;
    Var FEdgeSpace: Int8U;
    Var FEdge: Int8U;
    Var FStep: Int16U;
    Var FMax: Int16S;
    Var FMin: Int16S;
    Var FLegPos: Int16S;
    Var FPosition: Int16S;
    Var FOnChange: mNotify;
    Var FOrientation: eFillbarOrientation;
    Var FChangeable: Boolean;
    Var Animating_Hover: (jahNil, jahEnter, jahLeave);

    Var FBarCaption: String;

    Procedure SetEdgeSpace(Value: Int8U);
    Procedure SetPosition(Value: Int16S);
    Procedure SetMax(Value: Int16S);
    Procedure SetMin(Value: Int16S);
    Function GetPage: Int16S;
    Procedure SetOrientation(Value: eFillbarOrientation);

    Procedure SetBarCaption(Value: String);

  protected
    Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mouseenter; Override;
    Procedure Mouseleave; Override;

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

  published
    Property EdgeSpace: Int8U    Read FEdgeSpace   Write SetEdgeSpace   Default 14;
    Property Step: Int16U        Read FStep        Write FStep          Default 1;
    Property Max: Int16S         Read FMax         Write SetMax         Default 100;
    Property Min: Int16S         Read FMin         Write SetMin         Default 0;
    Property Position: Int16S    Read FPosition    Write SetPosition;
    Property Page: Int16S        Read GetPage;
    Property OnChange: mNotify   Read FOnChange    Write FOnChange;
    Property Orientation: eFillbarOrientation
                                 Read FOrientation Write SetOrientation Default jfoHoriznal;
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
