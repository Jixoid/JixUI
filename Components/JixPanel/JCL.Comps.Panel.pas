unit JCL.Comps.Panel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  LResources;

type
  TJixPanel = class(TJixWinControl)
  private

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Procedure SetCustomRect(ARect: TRect; Use: Boolean);
    Procedure PaintControl(ABack, ABorder, AFont: Boolean);

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
