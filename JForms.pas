Unit JForms;

{$Mode Delphi}{$M+}{$ModeSwitch ArrayOperators}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCLabel,
  BCButton, ExtCtrls, BCTypes, MiniJixoid_JCL, JixPas, JGL, Registry, StrUtils,
  JixTemplate, JixButton, JixComponent;

Procedure Delay(dt: DWORD);

{$Include JCL\JixForm.map.inc}
{$Include JCL\FormManager.map.inc}

implementation
Uses
  JDialogs, JixAPI;

{$Include JCL\JixForm.cod.inc}
{$Include JCL\FormManager.cod.inc}

Procedure Delay(dt: DWORD);
Var
  tc: DWORD;
Begin
  tc:= GetTickCount64;
  While (GetTickCount64 < tc +dt) and (not Application.Terminated) do
    Application.ProcessMessages;
End;



Procedure CreateJixUITemp; //Default JixUI Template
Var
  JixUITemp: TJixTemplate;
  Temp: jTemplateCollectionItem;
Begin
  JixUITemp:= TJixTemplate.Create(Nil);

  Temp:= JixUITemp.Templates.Add as jTemplateCollectionItem;

  Temp.CornerRound:= 8;

  With Temp.StateNormal do
  Begin
    Background.Style:= bbsColor;
    Background.Color:= $3357FF;
    Background.ColorOpacity:= 100;

    Border.Style:= bboNone;
  End;

  With Temp.StateHover do
  Begin
    Background.Style:= bbsColor;
    Background.Color:= $3357FF;
    Background.ColorOpacity:= 125;

    Border.Style:= bboSolid;
    Border.Color:= $3357FF;
    Border.Width:= 1;
  End;

  With Temp.StateClick do
  Begin
    Background.Style:= bbsColor;
    Background.Color:= $3357FF;
    Background.ColorOpacity:= 255;

    Border.Style:= bboNone;
  End;

  Temp.ThemeName:= 'Default';

  FormManager.AddTemplate(JixUITemp);
End;


Initialization
Begin
  FormManager:= jFormManager.Create;
  CreateJixUITemp;
End;

Finalization;
Begin
  FormManager.Free;
End;

end.


