Function TFind.DB(KeyWord: UString; var Select: TDB; Flags: JedabFindOptions): JedabFindResults;
Var
  i: Integer;
Begin
  try
    if (Clear in Flags) then Result:= Nil;

    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" DB [Status: Starting]');

    For i:= 0 to High(Select.Tables) do
    Begin

      Result:= Self.Table(KeyWord, Select, i, Flags+[NoLog]-[Clear]);

    End;
    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" DB [Status: Finded, ResultCount: '+Length(Result).ToString+']');
  except
    AddLog('Find: For "'+Select.Name+'" DB [Status: Error, ResultCount: '+Length(Result).ToString+']');
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TFind.Table(KeyWord: UString; var Select: TDB; TableID: Integer; Flags: JedabFindOptions): JedabFindResults;
Var
  i: Integer;
Begin
  try
    if (Clear in Flags) then Result:= Nil;

    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Table [Status: Starting, TableID: '+TableID.ToString+']');

    For i:= 0 to High(Select.Tables[TableID].Rows) do
    Begin

      Result:= Self.Row(KeyWord, Select, TableID, i, Flags+[NoLog]-[Clear]);

    End;
    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Table [Status: Finded, TableID: '+TableID.ToString+', ResultCount: '+Length(Result).ToString+']');
  except
    AddLog('Find: For "'+Select.Name+'" in Table [Status: Error, TableID: '+TableID.ToString+', ResultCount: '+Length(Result).ToString+']');
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TFind.Row(KeyWord: UString; var Select: TDB; TableID, RowID: Integer; Flags: JedabFindOptions): JedabFindResults;
Var
  i: Integer;
  b: Boolean;

  CompareText: UString;
Begin
  try
    if (Clear in Flags) then Result:= Nil;

    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Row [Status: Starting, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']');

    if (Ignore_BigLittleHarmony in Flags) then
      KeyWord:= UpCase(KeyWord);

    For i:= 0 to High(Select.Tables[TableID].Rows[RowID].Cells) do
    Begin
      CompareText:= Select.Tables[TableID].Rows[RowID].Cells[i].Text;

      if (Ignore_BigLittleHarmony in Flags) then
        CompareText:= UpCase(CompareText);


      if (Ignore_FullText in Flags) then
        b:= (Pos(KeyWord, CompareText) <> 0)
      else
        b:= (KeyWord = CompareText);


      if b then
      Begin
        SetLength(Result, Length(Result)+1);
        With Result[High(Result)] do
        Begin
          LocTable:= TableID;
          LocRow:= RowID;
          LocColumn:= i;
        End;

        if (Only1Check in Flags) then Break;
      End;

    End;

    if Not(NoLog in Flags) then            if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Row [Status: Finded, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', ResultCount: '+Length(Result).ToString+']');
  except
    AddLog('Find: For "'+Select.Name+'" in Row [Status: Error, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', ResultCount: '+Length(Result).ToString+']');
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TFind.Column(KeyWord: UString; var Select: TDB; TableID, ColumnID: Integer; Flags: JedabFindOptions): JedabFindResults;
Var
  i: Integer;
  b: Boolean;

  CompareText: UString;
Begin
  try
    if (Clear in Flags) then Result:= Nil;

    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Column [Status: Starting, TableID: '+TableID.ToString+', ColumnID: '+ColumnID.ToString+']');

    if (Ignore_BigLittleHarmony in Flags) then
      KeyWord:= UpCase(KeyWord);

    For i:= 0 to High(Select.Tables[TableID].Rows) do
    Begin
      CompareText:= Select.Tables[TableID].Rows[i].Cells[ColumnID].Text;

      if (Ignore_BigLittleHarmony in Flags) then
        CompareText:= UpCase(CompareText);


      if (Ignore_FullText in Flags) then
        b:= (Pos(KeyWord, CompareText) <> 0)
      else
        b:= (KeyWord = CompareText);


      if b then
      Begin
        SetLength(Result, Length(Result)+1);
        With Result[High(Result)] do
        Begin
          LocTable:= TableID;
          LocRow:= i;
          LocColumn:= ColumnID;
        End;

        if (Only1Check in Flags) then Break;
      End;

    End;

    if Not(NoLog in Flags) then
      AddLog('Find: For "'+Select.Name+'" in Column [Status: Finded, TableID: '+TableID.ToString+', ColumnID: '+ColumnID.ToString+', ResultCount: '+Length(Result).ToString+']');
  except
    AddLog('Find: For "'+Select.Name+'" in Column [Status: Error, TableID: '+TableID.ToString+', ColumnID: '+ColumnID.ToString+', ResultCount: '+Length(Result).ToString+']');
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

