Procedure TSave.SaveAsLegacyFile(Select: TDB; FPath: UString);
Var
  i,j: Integer;
  Cac: String;
  inf: Text;
Begin
  try
    AddLog('Save: As "'+FPath+'" [Status: Starting, Mode: Legacy]');

    //Prepare Save File
    AssignFile(inf, FPath);
    ReWrite(inf);
    Writeln(inf, 'Jedab');

    Writeln(inf, Length(Select.Tables[0].Rows).ToString);
    Writeln(inf, Length(Select.Tables[0].Rows[0].Cells).ToString);

    //Export DB
    For i:= 0 to Length(Select.Tables[0].Rows)-1 do
      For j:= 0 to Length(Select.Tables[0].Rows[i].Cells)-1 do
      Begin
        Writeln(inf, Select.Tables[0].Rows[i].Cells[j].Text);
      End;

    CloseFile(inf);
    AddLog('Save: As "'+FPath+'" [Status: Saved, Mode: Legacy]');
  except
    AddLog('Save: As "'+FPath+'" [Status: Error, Mode: Legacy]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TSave.SaveAsFile(Select: TDB; FPath: UString);
Var
  i,j,ji: Integer;
  Cac: Array[0..32000] Of Char;
  inf: TFileStream;
Begin
  try
    AddLog('Save: As "'+FPath+'" [Status: Starting]');

    if FileExists(FPath) then  DeleteFile(FPath);

    inf:= TFileStream.Create(FPath, fmCreate);
    // File is openend

    Cac:= 'Jedab';
    inf.Write(Cac, 5);
    // Write Label

    for i:= 0 to High(Select.Tables) do
    Begin
      Cac:= Select.Tables[i].Name;
      inf.Write(Cac, Length(Select.Tables[i].Name));

      Cac:= '{ ';
      inf.Write(Cac, 1);

      Cac:= Length(Select.Tables[i].Rows).ToString;
      inf.Write(Cac, Length(Length(Select.Tables[i].Rows).ToString));

      Cac:= '| ';
      inf.Write(Cac, 1);

      Cac:= Select.Tables[i].X.ToString;
      inf.Write(Cac, Length(Select.Tables[i].X.ToString));

      Cac:= ': ';
      inf.Write(Cac, 1);

      for j:= 0 to High(Select.Tables[i].Rows) do
      Begin
        Cac:= Select.Tables[i].Rows[j].Name;
        inf.Write(Cac, Length(Select.Tables[i].Rows[j].Name));

        Cac:= '{ ';
        inf.Write(Cac, 1);

        for ji:= 0 to High(Select.Tables[i].Rows[j].Cells) do
        Begin

          Case Select.Tables[i].Rows[j].Cells[ji].CType Of
            JedabCellType.Str: Cac:= 'str';
            JedabCellType.Int: Cac:= 'int';
            JedabCellType.Sin: Cac:= 'sin';
            JedabCellType.Boo: Cac:= 'boo';
          End;
          inf.Write(Cac, 3);

          Cac:= '/ ';
          inf.Write(Cac, 1);

          Cac:= Select.Tables[i].Rows[j].Cells[ji].CLeng.ToString;
          inf.Write(Cac, Length(Select.Tables[i].Rows[j].Cells[ji].CLeng.ToString));

          Cac:= '/ ';
          inf.Write(Cac, 1);

          Cac:= Length(Select.Tables[i].Rows[j].Cells[ji].Text).ToString;
          inf.Write(Cac, Length(Length(Select.Tables[i].Rows[j].Cells[ji].Text).ToString));

          Cac:= '= ';
          inf.Write(Cac, 1);

          Cac:= Select.Tables[i].Rows[j].Cells[ji].Text;
          inf.Write(Cac, Length(Select.Tables[i].Rows[j].Cells[ji].Text));
        end;
      end;
    end;

    Cac:= '; ';
    inf.Write(Cac, 1);
    inf.Free;
    AddLog('Save: As "'+FPath+'" [Status: Saved]');
  except
    AddLog('Save: As "'+FPath+'" [Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
