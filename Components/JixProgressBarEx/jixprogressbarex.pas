unit JixProgressBarEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  JixProgressBar, JForms, JixComponent;

type
  TJixProgressBarEx = class(TJixProgressBar)
  private

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

  published
    Property StateNormal;
    Property StateHover;
    Property StateClick;
    Property Static;

  end;

procedure Register;

implementation

Constructor TJixProgressBarEx.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= False;

  ThemeName:= 'Default_Progress';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[1] as jTemplateCollectionItem);
End;

procedure Register;
begin
  {$I jixprogressbarex_icon.lrs}
  RegisterComponents('JixUI',[TJixProgressBarEx]);
end;

end.
