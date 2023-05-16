unit JixPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  JixPas, JixComponent, JForms;

type
  TJixPanel = class(TJixWinControl)
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

Constructor TJixPanel.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= True;

  Caption:= '';
  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);
End;


procedure Register;
begin
  {$I jixpanel_icon.lrs}
  RegisterComponents('JixUI',[TJixPanel]);
end;

end.
