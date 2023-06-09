Procedure TResize.DB(var Select: TDB; TableCount: Integer);
Begin
  try
    AddLog('Resize: DB [Status: Starting, TableCount: '+TableCount.ToString+']');

    SetLength(Select.Tables, TableCount);

    AddLog('Resize: DB [Status: Added, TableCount: '+TableCount.ToString+']');
  except
    AddLog('Resize: DB [Status: Error, TableCount: '+TableCount.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;

Procedure TResize.Table(var Select: TDB; TableID: Integer; X,Y: Integer);
Var
  i: Integer;
Begin
  try
    AddLog('Resize: DB [Status: Starting, TableID: '+TableID.ToString+', X: '+X.ToString+', Y: '+Y.ToString+']');

    Select.Tables[TableID].X:= X;
    Select.Tables[TableID].Y:= Y;

    SetLength(Select.Tables[TableID].Rows, Y);

    For i:= 0 to High(Select.Tables[TableID].Rows) do
      SetLength(Select.Tables[TableID].Rows[i].Cells, X);

    AddLog('Resize: DB [Status: Added, TableID: '+TableID.ToString+', X: '+X.ToString+', Y: '+Y.ToString+']');
  except
    AddLog('Resize: DB [Status: Error, TableID: '+TableID.ToString+', X: '+X.ToString+', Y: '+Y.ToString+']', TLogFlag.jlfError);
    CustomErrorSend(
      'Oops, Error encountered in Jedab',
      'This can help you'+#13+'Jedab Logs:'+#13+Upper.Log_Text,
      CustomErrorFlag.Close_Halt
    );
  end;
End;
