unit PortableJixForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, JForms;

type
  jPortableJixForm = class(jJixForm)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JixUI',[jPortableJixForm]);
end;

end.
