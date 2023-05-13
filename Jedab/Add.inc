Procedure TAdd.Table(var Select: TDB);
Begin
  try
    AddLog('Add: Table [Status: Starting]');

    SetLength(
      Select.Tables,
      Length(Select.Tables)+1
    );

    AddLog('Add: Table [Status: Added]');
  except
    AddLog('Add: Table [Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TAdd.Row(var Select: TDB; TableID: Integer);
Begin
  try
    AddLog('Add: Row [Status: Starting, TableID: '+TableID.ToString+']');

    SetLength(
      Select.Tables[TableID].Rows,
      Length(Select.Tables[TableID].Rows)+1
    );
    SetLength(
      Select.Tables[TableID].Rows[High(Select.Tables[TableID].Rows)].Cells,
      Select.Tables[TableID].X
    );

    AddLog('Add: Row [Status: Added, TableID: '+TableID.ToString+']');
  except
    AddLog('Add: Row [Status: Error, TableID: '+TableID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TAdd.Column(var Select: TDB; TableID: Integer);
Var
  i: Integer;
Begin
  try
    AddLog('Add: Collumn [Status: Starting, TableID: '+TableID.ToString+']');

    For i:= 0 to High(Select.Tables[TableID].Rows) do
      SetLength(
        Select.Tables[TableID].Rows[i].Cells,
        Length(Select.Tables[TableID].Rows[i].Cells)+1
      );

    AddLog('Add: Collumn [Status: Added, TableID: '+TableID.ToString+']');
  except
    AddLog('Add: Collumn [Status: Error, TableID: '+TableID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
