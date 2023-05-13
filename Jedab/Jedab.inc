Constructor TJedabManager.Create;
Begin
  Inherited;

  Open:= TOpen.Create(Self);
  Save:= TSave.Create(Self);
  Resize:= TResize.Create(Self);
  Add:= TAdd.Create(Self);
  Import:= TImport.Create(Self);
  Export:= TExport.Create(Self);
  Find:= TFind.Create(Self);
End;

//Function TJedabManager.GetCell(var DB: TDB; TableID, RowID, ColumnID: Integer): UString;
//Begin
//  Result:= DB.Tables[TableID].Rows;
//end;

Procedure TJedabManager.AddLog(Text: UString; Flag: TLogFlag = jlfNormal);
Begin
  SetLength(OLog, Length(OLog)+1);
  
  OLog[High(OLog)].Text:= Text;
  OLog[High(OLog)].Time:= Now;
  OLog[High(OLog)].Flag:= Flag;
End;

Function TJedabManager.GetLogs: UString;

  Function FlagToString(Flag: TLogFlag): String; Register; Inline;
  Begin
    Case Flag of
      jlfNormal: Result:= 'Normal';
      jlfInfo: Result:= 'Info';
      jlfError: Result:= 'Error';
      Else Result:= 'UnKnown';
    end;
  end;

Var
  i: Integer;
Begin
  Result:= '';

  For i:= High(Log) downto 0 do
  Begin
    Result+= FormatDateTime('HH:MM:SS| ', Log[i].Time)+FlagToString(Log[i].Flag)+' | '+Log[i].Text+#13;
  end;
end;

{Jedab Sub Class}
Constructor TJedabSubClass.Create(DefaultParent: TJedabManager);
Begin
  AddLog:= @DefaultParent.AddLog;
  Upper:= DefaultParent;
End;
