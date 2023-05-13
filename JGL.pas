{  $Id$  }
{
JGL
|
|- Ver: 1.1
|- Status: Alpha
|- Meaning: Jixoid Generic List
}
{$Mode ObjFPC}{$H+}

Unit JGL;

Interface

Uses
  {$Ifdef Unix} CThreads, {$Endif}
  Classes, SysUtils;

Type
  jListItem = Class

  End;

  Generic jList<T> = Class(TPersistent)
    Private
      FItems: Array Of T;
      
    Private
      Function GetItem(Index: LongWord): T;
      Procedure SetItem(Index: LongWord; Value: T);
      
    Public
      Procedure Clear;
      Procedure Assign(Source: TPersistent); Override;
      Procedure Add(Value: T);
      Function Add: T;
      Procedure Delete(Index: LongWord);
      Procedure Delete(Value: T);
      Function Count: LongWord;
      Function Find(Item: T): Int64;
      Function Exists(Item: T): Boolean;
      
      Property Items[Index: LongWord]: T Read GetItem Write SetItem; Default;
  End;

Implementation

Procedure jList.Clear;
Begin
  FItems:= [];
end;

Procedure jList.Assign(Source: TPersistent);
Begin
  if Source = Nil then Exit;

  if Source is jList then
  Begin
    FItems:= Copy(jList(Source).FItems, 1, Length(jList(Source).FItems));
  end;

  Inherited;
end;

Function jList.GetItem(Index: LongWord): T;
Begin
  Result:= FItems[Index];
End;

Procedure jList.SetItem(Index: LongWord; Value: T);
Begin
  FItems[Index]:= Value;
End;

Procedure jList.Add(Value: T);
Begin
  SetLength(FItems, Length(FItems)+1);
  FItems[High(FItems)]:= Value;
End;

Function jList.Add: T;
Var
  AItem: T;
Begin
  SetLength(FItems, Length(FItems)+1);
  FItems[High(FItems)]:= AItem;

  Result:= AItem;
end;

Procedure jList.Delete(Index: LongWord);
Var
  i: LongWord;
Begin
  for i:= Index to High(FItems) do
    FItems[i]:= FItems[i+1];
  
  SetLength(FItems, Length(FItems)-1);
End;

Function jList.Count: LongWord;
Begin
  Result:= Length(FItems);
End;

Procedure jList.Delete(Value: T);
Var
  i: Int64;
Begin
  i:= Find(Value);

  if i <> -1 then
    Self.Delete(i);
End;

Function jList.Find(Item: T): Int64;
Var
  i: LongWord;
Begin
  Result:= -1;
  
  for i:= 0 to Length(FItems)-1 do
    if FItems[i] = Item then
    Begin
      Result:= i;
      Exit;
    End;
End;

Function jList.Exists(Item: T): Boolean;
Var
  i: Integer;
Begin
  Result:= False;

  for i:= 0 to Length(FItems)-1 do
    if FItems[i] = Item then
    Begin
      Result:= True;
      Exit;
    End;
End;

End.
