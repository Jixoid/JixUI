{  $Id$  }
{
Jedab
|
|- Ver: 9.8
|- Status: Alpha, UnPublished
|- Meaning: Jixoid Extended Database
|- Addictions
|  |
|  |- BGRAControls
|  |- JixDialogs
|
|- Developing by Jixoid
   |
   |- Alfroce(Abdulkadir AYDIN) | Jixoid@gmail.com

}
Unit Jedab;


Interface

Uses
  Classes, SysUtils, FileUtil, JGL, JDialogs;

{$I Jedab/Term.inc}

Type
  TJedabManager = Class;

Type
  TJedabEnc = Class abstract
    Private
      OEncode: TEncEncode;
      ODecode: TEncDecode;

    Public
      Password: UString;
      Property Encode: TEncEncode Read OEncode Write OEncode;
      Property Decode: TEncDecode Read ODecode Write ODecode;
  end;


  TJedabSubClass = Class
    Private
      AddLog: TAddLog;
      Upper: TJedabManager;

    Public
      Constructor Create(DefaultParent: TJedabManager);
  End;

  TOpen = Class(TJedabSubClass)
    Public
      Procedure LoadFromLegacyFile(var Select: TDB; FPath: UString);
      Procedure LoadFromFile(var Select: TDB; FPath: UString);
  End;
  TSave = Class(TJedabSubClass)
    Public
      Procedure SaveAsLegacyFile(Select: TDB; FPath: UString);
      Procedure SaveAsFile(Select: TDB; FPath: UString);
  End;
  TResize = Class(TJedabSubClass)
    Public
      Procedure DB(var Select: TDB; TableCount: Integer);
      Procedure Table(var Select: TDB; TableID: Integer; X,Y: Integer);
  End;
  TAdd = Class(TJedabSubClass)
    Public
      Procedure Table(var Select: TDB);
      Procedure Row(var Select: TDB; TableID: Integer);
      Procedure Column(var Select: TDB; TableID: Integer);
  End;
  TImport = Class(TJedabSubClass)
    Public
      Procedure DB(var Select: TDB; Select2: TDB);
      Procedure Table(var Select: TDB; TableID: Integer; Select2: TTable);
      Procedure Row(var Select: TDB; TableID, RowID: Integer; Select2: TRow);
      Procedure Cell(var Select: TDB; TableID, RowID, ColumnID: Integer; Select2: TCell);
  End;
  TExport = Class(TJedabSubClass)
    Public
      Function DB(var Select: TDB): TDB;
      Function Table(var Select: TDB; TableID: Integer): TTable;
      Function Row(var Select: TDB; TableID, RowID: Integer): TRow;
      Function Cell(var Select: TDB; TableID, RowID, ColumnID: Integer): TCell;
  End;
  TFind = Class(TJedabSubClass)
    Public
      Function DB(KeyWord: UString; var Select: TDB; Flags: JedabFindOptions): JedabFindResults;
      Function Table(KeyWord: UString; var Select: TDB; TableID: Integer; Flags: JedabFindOptions): JedabFindResults;
      Function Row(KeyWord: UString; var Select: TDB; TableID, RowID: Integer; Flags: JedabFindOptions): JedabFindResults;
      Function Column(KeyWord: UString; var Select: TDB; TableID, ColumnID: Integer; Flags: JedabFindOptions): JedabFindResults;
  End;

  TJedabManager = Class
    Protected
      Function GetLogs: UString;

    Private
      OLog: TLogs;
      OEncryption: TJedabEnc;
      //Function GetCell(var DB: TDB; TableID, RowID, ColumnID: Integer): UString;

    Public
      Open: TOpen;
      Save: TSave;
      Resize: TResize;
      Add: TAdd;
      Import: TImport;
      Export: TExport;
      Find: TFind;

      Constructor Create;
      Procedure AddLog(Text: UString; Flag: TLogFlag = jlfNormal);

      Property Log: TLogs Read OLog;
      Property Log_Text: UString Read GetLogs;

      Property Encryption: TJedabEnc Read OEncryption Write OEncryption;
  End;

Var
  JedabMng: TJedabManager;

Implementation

{$I Jedab/Open.inc}    // Added Debug
{$I Jedab/Save.inc}    // Added Debug
{$I Jedab/Resize.inc}  // Added Debug
{$I Jedab/Add.inc}     // Added Debug
{$I Jedab/Import.inc}  // Added Debug
{$I Jedab/Export.inc}  // Added Debug
{$I Jedab/Find.inc}    // Added Debug
{$I Jedab/Jedab.inc}

Initialization
Begin
  JedabMng:= TJedabManager.Create;
end;

Finalization
Begin
  JedabMng.Free;
End;

End.
