{  $Id$  }
{
JForms
|
|- Ver: 0.6
|- Status: Beta
|- Meaning: FormManager
}
Unit JCL.Theme;

{$Mode Delphi}{$M+}{$ModeSwitch ArrayOperators}

{$Warn 5062 off}
interface

Uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Generics,
  JCL.CompTypes,
  JCL.Controls,
  JCL.Comps.Template,
  JCL.Forms,
  Controls, Forms, ExtCtrls, Dialogs, Graphics, JCL.Auxiliary;

Type
  sFormManagerFlags = set of(
    fmfIgnoreNeedShowing
  );
Var
  FormManagerFlags: sFormManagerFlags = [];

Type
  jItem = Class
    Public
      Color: jARGB;
      Focus: Boolean;
      BIcons: TBorderIcons;
  End;

  mBasicARGB = Function: jARGB;
  mBasicBoolean = Function: Boolean;

  jFormManager = Class
    Protected
      Controller: TTimer;
      FFormList: jList<TForm>;
      FFormItem: jList<jItem>;

      CompTemps: jList<TJixTemplate>;

      Procedure TimerEvent(Sender: TObject); Virtual;

    Private
      OGetterActiveColor: mBasicARGB;
      OGetterInActiveColor: mBasicARGB;
      OGetterDarkTheme: mBasicBoolean;

      Function GetActiveColor: jARGB; Virtual;
      Function GetInActiveColor: jARGB; Virtual;
      Function GetDarkTheme: Boolean; Virtual;

    Public
      Property FormList: jList<TForm>          Read FFormList            Write FFormList;
      Property FormItem: jList<jItem>          Read FFormItem            Write FFormItem;

      Property GetterActiveColor: mBasicARGB   Read OGetterActiveColor   Write OGetterActiveColor;
      Property GetterInActiveColor: mBasicARGB Read OGetterInActiveColor Write OGetterInActiveColor;
      Property GetterDarkTheme: mBasicBoolean  Read OGetterDarkTheme     Write OGetterDarkTheme;

      Property ActiveColor: jARGB Read GetActiveColor;
      Property InActiveColor: jARGB Read GetInActiveColor;
      Property DarkTheme: Boolean Read GetDarkTheme;

    Public
      Function FindFormTitleColor(Form: TForm): jARGB; Virtual;
      Function FindFormColor(Form: TForm): jARGB; Virtual;

      Procedure AddTemplate(Templates: TJixTemplate);
      Property Templates: jList<TJixTemplate> Read CompTemps;

    Public
      Constructor Create; Virtual;
      Destructor Destroy; Override;
      Procedure AddForm(Form: TForm);
      Procedure DeleteForm(Form: TForm);

      Procedure ForceReSet(Form: TForm);
      Procedure ForceReSetAll;
  End;

Var
  FormManager: jFormManager;

Implementation
End.
