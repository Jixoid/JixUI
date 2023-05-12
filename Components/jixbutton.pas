unit JixButton;

{$mode ObjFPC}{$H+}{$Interfaces Corba}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCButton,
  JixComponent;

type
  TJixButton = class(TBCButton, jBGRAComponent)
  private
    FThemeName: String;

  protected
    Procedure SetThemeName(Value: String);
    Function GetThemeName: String;

  public

  published
    Property ThemeName: String Read GetThemeName Write SetThemeName;

  end;

procedure Register;

implementation


Procedure TJixButton.SetThemeName(Value: String);
Begin
  FThemeName:= Value;
End;

Function TJixButton.GetThemeName: String;
Begin
  Result:= FThemeName;
End;


procedure Register;
begin
  {$I jixbutton_icon.lrs}
  RegisterComponents('JixUI',[TJixButton]);
end;

end.
