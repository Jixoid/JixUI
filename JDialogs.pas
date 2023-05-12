{  $Id$  }
{
JixDialogs
|
|- Ver: 4.0
|- Status: Alpha, UnPublished
|- Meaning: JixUI Dialogs
|- Using: JixUI 4.0
|- Addictions
|  |
|  |- BGRAControls
|
|- Developing by Jixoid
   |
   |- Alfroce(Abdulkadir AYDIN) | Jixoid@gmail.com

}
Unit JDialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, JForms, JixPas, JGL;

Type
  CustomErrorFlag = (
    Close_Halt,
    Soft
  );

Procedure JixDialogsStart;

Function  TextAsk(Caption, Text: String; Var Value: String): Boolean;
Function  QuestAsk(Caption, Text: String): Boolean;
Function  TimeAsk(Caption: String; var Time: TTime): Boolean;
Function  DateAsk(Caption: String; var Date: TDate): Boolean;
Procedure CustomErrorSend(Caption, Log: String; Flag: CustomErrorFlag);

implementation
uses
  UnitTextAsk,
  UnitQuestAsk,
  UnitDialogTime,
  UnitDialogDate,
  CustomError,
  CustomErrorSoft;

Type
  jFormList = Specialize jList<TForm>;
  jBoolList = Specialize jList<Boolean>;

Function TimeAsk(Caption: String; var Time: TTime): Boolean;
Var
  i: Integer;
  Active: TForm;
  Forms: jFormList;
  Actives: jBoolList;
Begin
  Forms:= jFormList.Create;
  Actives:= jBoolList.Create;

  try
    Active:= Screen.ActiveForm;
    For i:= 0 to Screen.FormCount-1 do
    Begin
      Forms.Add(Screen.Forms[i]);
      Actives.Add(Screen.Forms[i].Enabled);
      Screen.Forms[i].Enabled:= False;
    end;

    FormDialogTime.Enabled:= True;
    FormDialogTime.Title.Caption:= Caption;

    FormDialogTime.Show;
    While FormDialogTime.Showing do
      Application.ProcessMessages;

    For i:= 0 to Forms.Count-1 do
      Forms[i].Enabled:= Actives[i];

    if Assigned(Active) then
      if Active.Showing then
        if Active <> FormDialogTime then
          Active.SetFocus;

  finally
    Forms.Free;
    Actives.Free;

    Time:= StrToTime(
      FormDialogTime.Edit1.Text+
      ':'+
      FormDialogTime.Edit2.Text
    );
    Result:= UnitDialogTime.Used;
  end;
end;

Function DateAsk(Caption: String; var Date: TDate): Boolean;
Var
  i: Integer;
  Active: TForm;
  Forms: jFormList;
  Actives: jBoolList;
Begin
  Forms:= jFormList.Create;
  Actives:= jBoolList.Create;

  try
    Active:= Screen.ActiveForm;
    For i:= 0 to Screen.FormCount-1 do
    Begin
      Forms.Add(Screen.Forms[i]);
      Actives.Add(Screen.Forms[i].Enabled);
      Screen.Forms[i].Enabled:= False;
    end;

    FormDialogDate.Enabled:= True;
    FormDialogDate.Title.Caption:= Caption;

    FormDialogDate.Show;
    While FormDialogDate.Showing do
      Application.ProcessMessages;

    For i:= 0 to Forms.Count-1 do
      Forms[i].Enabled:= Actives[i];

    if Assigned(Active) then
      if Active.Showing then
        if Active <> FormDialogDate then
          Active.SetFocus;

  finally
    Forms.Free;
    Actives.Free;

    Date:= StrToDate(
      FormDialogDate.Edit1.Text+
      '.'+
      (FormDialogDate.BCComboMount.ItemIndex+1).ToString+
      '.'+
      FormDialogDate.Edit2.Text
    );
    Result:= UnitDialogDate.Used;
  end;
end;

Function TextAsk(Caption, Text: String; Var Value: String): Boolean;
Var
  i: Integer;
  Active: TForm;
  Forms: jFormList;
  Actives: jBoolList;
Begin
  Forms:= jFormList.Create;
  Actives:= jBoolList.Create;

  try
    Active:= Screen.ActiveForm;
    For i:= 0 to Screen.FormCount-1 do
    Begin
      Forms.Add(Screen.Forms[i]);
      Actives.Add(Screen.Forms[i].Enabled);
      Screen.Forms[i].Enabled:= False;
    end;

    FormTextAsk.Enabled:= True;
    FormTextAsk.Title.Caption:= Caption;
    FormTextAsk.LabelSubCapion.Caption:= Text;
    FormTextAsk.EditText.Text:= Value;

    FormTextAsk.Show;
    While FormTextAsk.Showing do
      Application.ProcessMessages;

    For i:= 0 to Forms.Count-1 do
      Forms[i].Enabled:= Actives[i];

    if Assigned(Active) then
      if Active.Showing then
        if Active <> FormTextAsk then
          Active.SetFocus;

  finally
    Forms.Free;
    Actives.Free;

    Value:= FormTextAsk.EditText.Text;
    Result:= UnitTextAsk.Used;
  end;
end;

Function QuestAsk(Caption, Text: String): Boolean;
Var
  i: Integer;
  Active: TForm;
  Forms: jFormList;
  Actives: jBoolList;
Begin
  Forms:= jFormList.Create;
  Actives:= jBoolList.Create;

  try
    Active:= Screen.ActiveForm;
    For i:= 0 to Screen.FormCount-1 do
    Begin
      Forms.Add(Screen.Forms[i]);
      Actives.Add(Screen.Forms[i].Enabled);
      Screen.Forms[i].Enabled:= False;
    end;

    FormQuestAsk.Enabled:= True;
    FormQuestAsk.Title.Caption:= Caption;
    FormQuestAsk.LabelSubCapion.Caption:= Text;

    FormQuestAsk.Show;
    While FormQuestAsk.Showing do
      Application.ProcessMessages;

    For i:= 0 to Forms.Count-1 do
      Forms[i].Enabled:= Actives[i];

    if Assigned(Active) then
      if Active.Showing then
        if Active <> FormQuestAsk then
          Active.SetFocus;

  finally
    Forms.Free;
    Actives.Free;

    Result:= UnitQuestAsk.Used;
  end;
end;

Procedure CustomErrorSend(Caption, Log: String; Flag: CustomErrorFlag);
Var
  CustomErrorSoft: TFormCustomErrorSoft;
Begin
  FormCustomError.LabelCaption.Caption:=Caption;
  FormCustomError.MemoLog.Lines.SetText(PChar(Log));
  FormCustomError.Height:= 70;

  Case Flag of
    CustomErrorFlag.Close_Halt: Begin
      FormCustomError.ShowModal;
      Halt;
    end;
    CustomErrorFlag.Soft: Begin
      Application.CreateForm(TFormCustomErrorSoft, CustomErrorSoft);
      CustomErrorSoft.BCLabelCaption.Caption:= Caption;
      CustomErrorSoft.BCLabelInfo.Caption   := Log;
      CustomErrorSoft.Show;
      CustomErrorSoft.Down;
    end;
  end;
end;

Procedure JixDialogsStart;
Begin
  Application.CreateForm(TFormCustomError, FormCustomError);
  Application.CreateForm(TFormTextAsk, FormTextAsk);
  Application.CreateForm(TFormQuestAsk, FormQuestAsk);
  Application.CreateForm(TFormDialogTime, FormDialogTime);
  Application.CreateForm(TFormDialogDate, FormDialogDate);
End;

end.

