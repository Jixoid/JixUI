unit JCL.Comps.StaticLabel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  TJixLabel = class(TJixControl)
  Private

  Protected

  Public
    Constructor Create(AOwner: TComponent); Override;

  Published
    Property Tooltip;
    Property ShowTooltip;

    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    Property AutoSize Default True;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property StateNormal;

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
