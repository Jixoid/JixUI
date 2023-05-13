Unit JForms;

{$Mode Delphi}{$M+}{$ModeSwitch ArrayOperators}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCLabel,
  BCButton, ExtCtrls, BCTypes, MiniJixoid_JCL, JixPas, JGL, Registry, StrUtils,
  JixTemplate, JixComponent, IDECommands, LCLType, MenuIntf;

Procedure Delay(dt: DWORD);

{$Include JCL\JixForm.map.inc}
{$Include JCL\FormManager.map.inc}

Procedure Register;

implementation
Uses
  JDialogs;

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

    FontEx.Height:= 12;
  End;

  With Temp.StateHover do
  Begin
    Background.Style:= bbsColor;
    Background.Color:= $3357FF;
    Background.ColorOpacity:= 125;

    Border.Style:= bboSolid;
    Border.Color:= $3357FF;
    Border.Width:= 1;

    FontEx.Height:= 12;
  End;

  With Temp.StateClick do
  Begin
    Background.Style:= bbsColor;
    Background.Color:= $3357FF;
    Background.ColorOpacity:= 255;

    Border.Style:= bboNone;

    FontEx.Height:= 12;
  End;

  Temp.ThemeName:= 'Default';

  FormManager.AddTemplate(JixUITemp);
End;


Procedure DoOnMenuClick(Sender: TObject);
Var
  i: Integer;
  AForm: TForm;
Begin
  AForm:= TForm.Create(Nil);
  AForm.Height:= 0;
  AForm.Width:= 0;
  AForm.Position:= poWorkAreaCenter;
  AForm.Caption:= 'Please Wait...';
  AForm.BorderIcons:= [];
  AForm.Show;

  FormManager.FormList.Clear;

  For i:= 0 to Screen.FormCount-1 do
  Begin
    ShowMessage(Screen.Forms[i].Name);
    FormManager.FormList.Add(Screen.Forms[i]);
  End;

  AForm.Close;
  AForm.Free;
End;

Procedure Register;
Var
  IDEMenuItem: TIDEMenuItem;
  M: TMethod;
Begin
  Exit;

  IDEMenuItem := TIDEMenuItem.Create('Rescan JixUI');
  M.Data:= nil;
  M.Code:= @DoOnMenuClick;
  IDEMenuItem.OnClick:= TNotifyEvent(M);

  SourceEditorMenuRoot.AddLast(IDEMenuItem);
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


