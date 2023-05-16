{  $Id$  }
{
JForms
|
|- Ver: 0.4
|- Status: Alpha
|- Meaning: JixForm and FormManager
}
Unit JForms;

{$Mode Delphi}{$M+}{$ModeSwitch ArrayOperators}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCLabel,
  BCButton, ExtCtrls, BCTypes, MiniJixoid_JCL, JixPas, JGL,
  JixComponent, IDECommands, LCLType, MenuIntf;

Procedure Delay(dt: DWORD);

{$Include JCL\JixForm.map.inc}
{$Include JCL\FormManager.map.inc}

implementation

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

  // Normal
  Temp:= JixUITemp.Templates.Add as jTemplateCollectionItem;

  With Temp.StateNormal do
  Begin
    Background.Style:= jbgsColor;
    Background.Color:= $3357FF;
    Background.Opacity:= 100;

    Border.Style:= jbsClear;

    FontEx.Height:= 12;
  End;

  With Temp.StateHover do
  Begin
    Background.Style:= jbgsColor;
    Background.Color:= $3357FF;
    Background.Opacity:= 125;

    Border.ExColor:= $3357FF;

    FontEx.Height:= 12;
  End;

  With Temp.StateClick do
  Begin
    Background.Style:= jbgsColor;
    Background.Color:= $3357FF;
    Background.Opacity:= 255;

    Border.Style:= jbsClear;

    FontEx.Height:= 12;
  End;

  Temp.ThemeName:= 'Default';


  // Progress
  Temp:= JixUITemp.Templates.Add as jTemplateCollectionItem;

  With Temp.StateNormal do
  Begin
    Background.Style:= jbgsGradient;
    Background.Point1.Color:= $3357FF;
    Background.Point1.Opacity:= 100;
    Background.Point2.Color:= $3357FF;
    Background.Point2.Opacity:= 0;

    Border.ExColor:= $3357FF;
    Border.ExOpacity:= 100;
    Border.Style:= jbsSolid;

    FontEx.Height:= 12;
  End;

  With Temp.StateHover do
  Begin
    Background.Style:= jbgsGradient;
    Background.Point1.Color:= $3357FF;
    Background.Point1.Opacity:= 125;
    Background.Point2.Color:= $3357FF;
    Background.Point2.Opacity:= 0;

    Border.ExColor:= $3357FF;
    Border.ExOpacity:= 255;
    Border.Style:= jbsSolid;

    FontEx.Height:= 12;
  End;

  With Temp.StateClick do
  Begin
    Background.Style:= jbgsGradient;
    Background.Point1.Color:= $3357FF;
    Background.Point1.Opacity:= 255;
    Background.Point2.Color:= $3357FF;
    Background.Point2.Opacity:= 0;

    Border.ExColor:= $3357FF;
    Border.ExOpacity:= 255;
    Border.Style:= jbsSolid;

    FontEx.Height:= 12;
  End;

  Temp.ThemeName:= 'Default_Progress';
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


