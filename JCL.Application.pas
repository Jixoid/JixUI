Unit JCL.Application;

{$Mode ObjFpc}{$H+}{$Inline On}{$Macro On}{$ModeSwitch ArrayOperators}
{$ModeSwitch AdvancedRecords}{$ModeSwitch TypeHelpers}

Interface

Uses
  Classes, SysUtils, Forms, CustApp, JCL.Utils, ShlObj, comobj, Win32Int,
  InterfaceBase, JCL.Dialogs, JCL.Generics;

Type
  eProgressStyle = (
    jpsNoProgress,
    jpsIndeterminate,
    jpsNormal,
    jpsError,
    jpsPaused
  );

  jHandlerList = Specialize jList<mNotifyB>;
  jApplication = Class(TApplication)
    Private
      Var FAddOnRuned: jHandlerList;

      Var FTaskBarList: ITaskbarList3;
      Var FAppHandle: THandle;

      Procedure RunLoop;

    Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Initialize; Override;
      Procedure Run;
      Procedure SetProgressPosition(Value: IntPer);
      Procedure SetProgressStyle(Value: eProgressStyle);
      Procedure AddOnRunedHandler(Handler: mNotifyB);
      Procedure RemoveOnRunedHandler(Handler: mNotifyB);

    Public
      Property TaskBarList: ITaskbarList3 Read FTaskBarList Write FTaskBarList;
  End;

Var
  Application: jApplication;

Implementation
End.
