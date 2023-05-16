Procedure TOpen.LoadFromLegacyFile(var Select: TDB; FPath: UString);
Var
  i,j,
  R,C: Integer;
  Cac: String;
  inf: Text;
  Rub: TDB;
Begin
  try
    AddLog('Open: File from "'+FPath+'" [Mode: Legacy, Status: Starting]');

    if not FileExists(FPath) then
    Begin
      AddLog('Open: File from "'+FPath+'" [Mode: Legacy, Status: Fail, Error: File not exists]', jlfError);
      Exit;
    End;

    //Resize DB
    Select:= Rub;
    SetLength(
      Select.Tables,
      Length(Select.Tables)+1
    );

    //Prepare Open File
    AssignFile(inf, FPath);
    Reset(inf);
    Readln(inf, Cac);
    if Cac <> 'Jedab' then
    Begin
      Close(inf);
      AddLog('Open: File from "'+FPath+'" [Mode: Legacy, Status: Fail, Error: Invailed format]', jlfError);
      Exit;
    End;

    //Load Size Information
    Readln(inf, Cac); R:= StrToInt(Cac);
    Readln(inf, Cac); C:= StrToInt(Cac);

    //ReSize DB
    SetLength(Select.Tables[0].Rows, R);
    For i:= 0 to Length(Select.Tables[0].Rows) do
      SetLength(Select.Tables[0].Rows[i].Cells, C);

    //Import DB
    For i:= 0 to R-1 do
      For j:= 0 to C-1 do
      Begin
        Readln(inf, Cac);
        Select.Tables[0].Rows[i].Cells[j].Text:= Cac;
      End;

    Close(inf);
    AddLog('Open: File from "'+FPath+'" [Mode: Legacy, Status: Success]');
  except
    AddLog('Open: File from "'+FPath+'" [Mode: Legacy, Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TOpen.LoadFromFile(var Select: TDB; FPath: UString);
Var
  Rn, Cn: Integer;
  i,j: Integer;
  
  Tot2: Array[0..32000] Of Char;
  Tot: UString;
  C: Char;
  Cac: Array[0..32000] Of Char;
  inf: TFileStream;
  Rub: TDB;
Begin
  try
    AddLog('Open: File from "'+FPath+'" [Status: Starting]');

    if not FileExists(FPath) then
    Begin
      AddLog('Open: File from "'+FPath+'" [Status: Fail, Error: File not exists]', jlfError);
      Exit;
    End;

    //NewDB
    Select:= Rub;
    Select.Name:= ExtractFileName(ExtractFileNameWithOutExt(FPath));

    inf:= TFilestream.Create(FPath, fmOpenRead);
    inf.position:= 0;

    Cac:= '';
    inf.Read(Cac, 5);
    if Cac <> 'Jedab' then
    Begin
      inf.Free;
      AddLog('Open: File from "'+FPath+'" [Status: Fail, Error: Invailed format]', jlfError);
      Exit;
    End;

    inf.Read(C, 1);
    Repeat
      //Create Table
      Tot:= '';
      While C<>'{' do
      Begin
        Tot += C;
        inf.Read(C, 1);
      End;
      SetLength(Select.Tables, Length(Select.Tables)+1);
      Select.Tables[High(Select.Tables)].Name:= Tot;

      //Resize
      inf.Read(C, 1); Tot:= '';
      While C<>'|' do
      Begin
        Tot += C;
        inf.Read(C, 1);
      End; Rn:= StrToInt(Tot);
      inf.Read(C, 1); Tot:= '';
      While C<>':' do
      Begin
        Tot += C;
        inf.Read(C, 1);
      End; Cn:= StrToInt(Tot);

      //ReSize Table and load inf
      SetLength(Select.Tables[High(Select.Tables)].Rows, Rn);
      For i:= 0 to Length(Select.Tables[High(Select.Tables)].Rows)-1 do
        SetLength(Select.Tables[High(Select.Tables)].Rows[i].Cells, Cn);

      Select.Tables[High(Select.Tables)].X:= Cn;
      Select.Tables[High(Select.Tables)].Y:= Rn;

      //Row
      For i:= 0 to Rn-1 do
      Begin
        inf.Read(C, 1); Tot:= '';
        While C<>'{' do
        Begin
          Tot += C;
          inf.Read(C, 1);
        End;
        Select.Tables[High(Select.Tables)].Rows[i].Name:= Tot;

        For j:= 0 to Cn-1 do
        Begin
          inf.Read(C, 1); Tot:= '';
          While C<>'/' do
          Begin
            Tot += C;
            inf.Read(C, 1);
          End;
          With Select.Tables[High(Select.Tables)].Rows[i].Cells[j] do
            Case Tot Of
              'str':CType:= JedabCellType.Str;
              'int':CType:= JedabCellType.Int;
              'sin':CType:= JedabCellType.Sin;
              'boo':CType:= JedabCellType.Boo;
            End;

          inf.Read(C, 1); Tot:= '';
          While C<>'/' do
          Begin
            Tot += C;
            inf.Read(C, 1);
          End;
          Select.Tables[High(Select.Tables)].Rows[i].Cells[j].CLeng:= StrToInt(Tot);

          inf.Read(C, 1); Tot:= '';
          While C<>'=' do
          Begin
            Tot += C;
            inf.Read(C, 1);
          End;
          Tot2:= '';
          inf.Read(Tot2, StrToInt(Tot));
          Select.Tables[High(Select.Tables)].Rows[i].Cells[j].Text:= Tot2;
        End;
      End;

      inf.Read(C, 1);
    Until C= ';';

    inf.Free;
    AddLog('Open: File from "'+FPath+'" [Status: Success]');
  except
    AddLog('Open: File from "'+FPath+'" [Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
