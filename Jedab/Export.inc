Function TExport.DB(var Select: TDB): TDB;
Begin
  try
    AddLog('Export: DB [Status: Starting]');
    Result:= Select;
    AddLog('Export: DB [Status: Exported]');
  except
    AddLog('Export: DB [Status: Error]', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TExport.Table(var Select: TDB; TableID: Integer): TTable;
Begin
  try
    AddLog('Export: Table [Status: Starting, TableID: '+TableID.ToString+']');
    Result:= Select.Tables[TableID];
    AddLog('Export: Table [Status: Exported, TableID: '+TableID.ToString+']');
  except
    AddLog('Export: Table [Status: Error, TableID: '+TableID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TExport.Row(var Select: TDB; TableID, RowID: Integer): TRow;
Begin
  try
    AddLog('Export: Row [Status: Starting, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']');
    Result:= Select.Tables[TableID].Rows[RowID];
    AddLog('Export: Row [Status: Exported, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']');
  except
    AddLog('Export: Row [Status: Error, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Function TExport.Cell(var Select: TDB; TableID, RowID, ColumnID: Integer): TCell;
Begin
  try
    AddLog('Export: Cell [Status: Starting, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']');
    Result:= Select.Tables[TableID].Rows[RowID].Cells[ColumnID];
    AddLog('Export: Cell [Status: Exported, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']');
  except
    AddLog('Export: Cell [Status: Error, TableID: '+TableID.ToString+', RowID: '+RowID.ToString+', CollumnID: '+ColumnID.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
