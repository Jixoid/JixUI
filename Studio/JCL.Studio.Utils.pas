Unit JCL.Studio.Utils;

{$Mode ObjFpc}{$H+}

Interface

Uses
  Classes, SysUtils, JCL.Utils, JCL.Comps.Syn, JCL.Comps.Panel, JCL.Application;

Type
  aPointer = Array Of Pointer;

  mOpenCod = Procedure(Value: StrA; Highliter: TJixSynHighliter); StdCall;
  mOpenJob = Function(Name: StrA): TJixPanel; StdCall;
  mProgVal = Procedure(Value: IntPer); StdCall;
  mProgTyp = Procedure(Value: eProgressStyle); StdCall;
  mProgNam = Procedure(Value: StrA); StdCall;
  mSetMesP = Procedure(Value: IntPer); StdCall;
  mGetList = Function: Pointer; StdCall;
  mGetMemo = Function: Pointer; StdCall;

  rIDEMethods = Record
    ProjectName: Pointer;
    ProjectPath: Pointer;
    PluginName: Pointer;
    PluginPath: Pointer;
    IsProjectOpen: Pointer;

    OpenCod: mOpenCod;
    OpenJob: mOpenJob;
    ProgVal: mProgVal;
    ProgTyp: mProgTyp;
    ProgNam: mProgNam;
    SetMesP: mSetMesP;
    GetList: mGetList;
    GetMemo: mGetMemo;
  End;

  rPlugMethods = Record
    Forms: aPointer;

    EventRun: mProcedureB;
    EventORun: mProcedureB;
    EventBuild: mProcedureB;
    EventCompile: mProcedureB;
    EventClean: mProcedureB;
    EventParams: mProcedureB;
  End;

  mGetMs = Procedure(AIDEMs: rIDEMethods); StdCall;
  mSetMs = Function: rPlugMethods; StdCall;

Implementation
End.
