Type
  AOS = Array Of String;
  AOI = Array Of Integer;
  AOC = Array Of Char;
  UString = UnicodeString;

  jJedabFindResult = Class
    Protected
      Table: Integer;
      Row: Integer;
      Column: Integer;
    Public
      Property LocTable: Integer  Read Table  Write Table;
      Property LocRow: Integer    Read Row    Write Row;
      Property LocColumn: Integer Read Column Write Column;
  End;
  JedabFindResult = Record
    LocTable: Integer;
    LocRow: Integer;
    LocColumn: Integer;
  End;
  jJedabFindResultList = Specialize jList<jJedabFindResult>;
  JedabFindResults = Array Of JedabFindResult;

  JedabFindOption = (
    Only1Check,
    Ignore_BigLittleHarmony,
    Ignore_FullText,
    NoLog,
    Clear
  );
  JedabFindOptions = set of JedabFindOption;

  JedabCellType = (
    Str,
    Int,
    Sin,
    Boo
  );
  TCell = Record
    Text: UString;
    CType: JedabCellType;
    CLeng: Int64;
  End;
  TRow = Record
    Name: UString;
    Cells: Array Of TCell;
  End;
  TTable = Record
    Name: UString;
    Rows: Array Of TRow;

    //Size
    X,Y: Integer;
  End;
  TDB = Record
    Name: UString;
    Tables: Array Of TTable;
  End;
  
  TLogFlag = (
    jlfNormal,
    jlfError,
    jlfInfo
  );
  TLog = Record
    Text: UString;
    Time: TDateTime;
    Flag: TLogFlag;
  End;
  TLogs = Array Of TLog;
  
  TAddLog = Procedure(Text: UString; Flag: TLogFlag = jlfNormal) of object;

Type
  TEncEncode = Function(Source: UString): UString of object; Register;
  TEncDecode = Function(Source: UString): UString of object; Register;
