unit JixButton;

{$mode ObjFPC}{$H+}{$Interfaces Corba}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  JixPas, JixComponent, JForms;

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

    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property StateNormal;
    Property StateHover;
    Property StateClick;

    Property OnChangeBounds;
    Property OnPaint;
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

procedure Register;

implementation

Constructor TJixButton.Create(AOwner: TComponent);
Begin
  Inherited;

  Height:= 35;
  Width:= 120;
End;


procedure Register;
begin
  {$I jixbutton_icon.lrs}
  RegisterComponents('JixUI',[TJixButton]);
end;

end.
