{  $Id$  }
{
Jedab
|
|- Ver: 9.8
|- Status: Alpha
|- Meaning: Jixoid Extended Database
}
Unit JCL.Jedab;

{$mode ObjFPC}{$H+}{$Inline On}{$ModeSwitch AdvancedRecords}

Interface

Uses
  Classes, SysUtils,
  JCL.Generics,
  JCL.Utils,
  JCL.Dialogs,
  LazFileUtils;

{$I Jedab/Term.inc}

Type
  TJedabManager = Class;

Type
  TJedabEnc = Class abstract
    Private
      OEncode: mEncEncode;
      ODecode: mEncDecode;

    Public
      Password: StrA;
      Property Encode: mEncEncode Read OEncode Write OEncode;
      Property Decode: mEncDecode Read ODecode Write ODecode;
  end;


  TJedabSubClass = Class
    Private
      AddLog: mAddLog;
      Upper: TJedabManager;

    Public
      Constructor Create(DefaultParent: TJedabManager);
  End;

  TOpen = Class(TJedabSubClass)
    Public
      Procedure LoadFromFile(var Select: jdbDB; FPath: StrA);
  End;
  TSave = Class(TJedabSubClass)
    Public
      Procedure SaveAsFile(Select: jdbDB; FPath: StrA);
  End;
  TResize = Class(TJedabSubClass)
    Public
      Procedure DB(var Select: jdbDB; TableCount: Int64U);
      Procedure Table(var Select: jdbDB; TableID, X,Y: Int64U);
  End;
  TAdd = Class(TJedabSubClass)
    Public
      Procedure Table(var Select: jdbDB);
      Procedure Table(var Select: jdbDB; X,Y: Int64U);
      Procedure Row(var Select: jdbDB; TableID: Int64U);
      Procedure Column(var Select: jdbDB; TableID: Int64U);
  End;
  TDelete = Class(TJedabSubClass)
    Public
      Procedure Table(var Select: jdbDB; TableID: Int64U);
      Procedure Row(var Select: jdbDB; TableID, RowID: Int64U);
      Procedure Column(var Select: jdbDB; TableID, ColumnID: Int64U);
  End;
  TImport = Class(TJedabSubClass)
    Public
      Procedure DB(var Select: jdbDB; Select2: jdbDB);
      Procedure Table(var Select: jdbDB; TableID: Int64U; Select2: jdbTable);
      Procedure Row(var Select: jdbDB; TableID, RowID: Int64U; Select2: jdbRow);
      Procedure Cell(var Select: jdbDB; TableID, RowID, ColumnID: Int64U; Select2: jdbCell);
  End;
  TExport = Class(TJedabSubClass)
    Public
      Function DB(var Select: jdbDB): jdbDB;
      Function Table(var Select: jdbDB; TableID: Int64U): jdbTable;
      Function Row(var Select: jdbDB; TableID, RowID: Int64U): jdbRow;
      Function Cell(var Select: jdbDB; TableID, RowID, ColumnID: Int64U): jdbCell;
  End;
  TFind = Class(TJedabSubClass)
    Public
      Function DB(KeyWord: StrA; var Select: jdbDB; Flags: sJedabFindOptions): aJedabFindResults;
      Function Table(KeyWord: StrA; var Select: jdbDB; TableID: Int64U; Flags: sJedabFindOptions): aJedabFindResults;
      Function Row(KeyWord: StrA; var Select: jdbDB; TableID, RowID: Int64U; Flags: sJedabFindOptions): aJedabFindResults;
      Function Column(KeyWord: StrA; var Select: jdbDB; TableID, ColumnID: Int64U; Flags: sJedabFindOptions): aJedabFindResults;
  End;

  TJedabManager = Class
    Protected
      Function GetLogs: StrA;

    Private
      OLog: lLogs;
      OEncryption: TJedabEnc;
      //Function GetCell(var DB: TDB; TableID, RowID, ColumnID: Int64U): UString;

    Public
      Open: TOpen;
      Save: TSave;
      Resize: TResize;
      Add: TAdd;
      Delete: TDelete;
      Import: TImport;
      Export: TExport;
      Find: TFind;

      Constructor Create;
      Procedure AddLog(Text: StrA; Flag: eLogFlag = jlfNormal);

      Property Log: lLogs Read OLog;
      Property Log_Text: StrA Read GetLogs;

      Property Encryption: TJedabEnc Read OEncryption Write OEncryption;
  End;

Var
  JedabMng: TJedabManager;

Implementation
End.
