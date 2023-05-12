Procedure TImport.DB(var Select: TDB; Select2: TDB);
Begin
  try
    AddLog('Import: DB [Status: Starting]');
    Select:= Select2;
    AddLog('Import: DB [Status: Imported]');
  except
    AddLog('Import: DB [Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TImport.Table(var Select: TDB; TableID: Integer; Select2: TTable);
Begin
  try
    AddLog('Import: Table [Status: Starting, TableID: '+TableID.ToString+']');
    Select.Tables[TableID]:= Select2;
    AddLog('Import: Table [Status: Imported, TableID: '+TableID.ToString+']');
  except
    AddLog('Import: Table [Status: Error, TableID: '+TableID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TImport.Row(var Select: TDB; TableID, RowID: Integer; Select2: TRow);
Begin
  try
    AddLog('Import: Row [Status: Starting, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']');
    Select.Tables[TableID].Rows[RowID]:= Select2;
    AddLog('Import: Row [Status: Imported, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']');
  except
    AddLog('Import: Row [Status: Error, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TImport.Cell(var Select: TDB; TableID, RowID, ColumnID: Integer; Select2: TCell);
Begin
  try
    AddLog('Import: Cell [Status: Starting, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']');
    Select.Tables[TableID].Rows[RowID].Cells[ColumnID]:= Select2;
    AddLog('Import: Cell [Status: Imported, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']');
  except
    AddLog('Import: Cell [Status: Error, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
