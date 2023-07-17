unit JCL.Comps.Button;

{$mode ObjFPC}{$H+}{$Interfaces Corba}{$Inline On}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  TJixButton = class(TJixControl)
  Private

  Protected

  Public
    Constructor Create(AOwner: TComponent); Override;

  Published
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
