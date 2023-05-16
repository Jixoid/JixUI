unit JixPaintBoxWin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  JixComponent;

type
  TJixPaintBoxWin = class(TJixWinControl)
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

Constructor TJixPaintBoxWin.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= True;

  Height:= 150;
  Width:= 200;
End;


procedure Register;
begin
  {$I jixpaintboxwin_icon.lrs}
  RegisterComponents('JixUI',[TJixPaintBoxWin]);
end;

end.
