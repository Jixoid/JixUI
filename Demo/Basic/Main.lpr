Program Main;

{$Mode ObjFpc}{$H+}

Uses
  {$IFDEF UNIX}
  Cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  Athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, UnitMain, JixUI, JDialogs
  { you can add units after this };

{$R *.res}

Begin
  Requirederivedformresource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.Createform(Tformmain, Formmain);

  //JixDialogsStart;
  Application.Run;
End.

