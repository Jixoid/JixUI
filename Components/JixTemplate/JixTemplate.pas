unit JixTemplate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCButton,
  JixPas, MiniJixoid_JCL;


{$Include Collection.map.Inc}
Type
  TJixTemplate = class(TComponent)
  private
    FTemplates: jTemplateCollection;

  protected

  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

  published
    Property Templates: jTemplateCollection Read FTemplates Write FTemplates;

  end;

procedure Register;

implementation
Uses
  JForms;


{$Include Collection.cod.Inc}

Constructor TJixTemplate.Create(AOwner: TComponent);
Begin
  Inherited;

  Templates:= jTemplateCollection.Create(jTemplateCollectionItem);

  FormManager.AddTemplate(Self);
End;

Destructor TJixTemplate.Destroy;
Begin
  Templates.Free;

  Inherited;
End;

procedure Register;
begin
  {$I JixTemplate_icon.lrs}
  RegisterComponents('JixUI',[TJixTemplate]);
end;

end.
