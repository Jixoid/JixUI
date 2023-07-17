unit JCL.Comps.Spilt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs;

type
  eSplitOrientation = (
    jsoVertical,
    jsoHoriznal
  );

  TJixSpilt = class(TJixControl)
  Private
    Var FOrientation: eSplitOrientation;
    Procedure SetOrientation(Value: eSplitOrientation);

    Var LegMouse: TPoint;
    Var LegPos: TPoint;

    Var FTracking: Boolean;

  Protected
    Procedure Mousedown(Button: Tmousebutton; {%H-}Shift: Tshiftstate; {%H-}X, {%H-}Y: Integer); Override;
    Procedure Mousemove(Shift: Tshiftstate; {%H-}X, {%H-}Y: Integer); Override;
    Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;

  Public
    Constructor Create(AOwner: TComponent); Override;

  Published
    Property Orientation: eSplitOrientation  Read FOrientation Write SetOrientation Default jsoHoriznal;
    Property Tracking: Boolean               Read FTracking    Write FTracking      Default False;

    Property Tooltip;
    Property ShowTooltip;

    Property Static;
    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property StateNormal;
    Property StateHover;
    Property StateClick;

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
  End;

 Procedure Register;

Implementation
End.
