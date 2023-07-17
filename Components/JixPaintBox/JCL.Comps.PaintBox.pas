unit JCL.Comps.PaintBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  TJixPaintBox = class(TJixControl)
  private

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

  published
    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;

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
