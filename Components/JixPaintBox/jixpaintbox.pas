unit JixPaintBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  JixPas, JixComponent, JForms;

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

    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

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
  end;

procedure Register;

implementation

Constructor TJixPaintBox.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= True;

  Height:= 150;
  Width:= 200;
End;


procedure Register;
begin
  {$I jixpaintbox_icon.lrs}
  RegisterComponents('JixUI',[TJixPaintBox]);
end;

end.
