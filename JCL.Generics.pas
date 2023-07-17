{  $Id$  }
{
JCL.Generics
|
|- Ver: 1.8
|- Status: Beta
|- Meaning: Jixoid Generic List
}
{$Mode ObjFPC}{$H+}

Unit JCL.Generics;

Interface

Uses
  Classes, SysUtils, Dialogs;

Type
  jListItem = Class

  End;

  Generic jList<T> = Class
    Private
      FItems: Array Of T;
      
    Public
      Procedure Clear;
      Procedure Assign(Source: jList);
      Procedure Add(Value: T);
      Procedure Insert(Index: LongWord; Value: T);
      Procedure Delete(Index: LongWord);
      Procedure FindAndDelete(Value: T);
      Function Count: LongWord;
      Procedure ReSetLength(NewLength: LongWord);
      Function Find(Item: T): Int64;
      Function Exists(Item: T): Boolean;

      Procedure Swap(Index, ToIndex: LongWord);
      Function LastItem: T;
      Function GetItem(Index: LongWord): T;
      Procedure SetItem(Index: LongWord; Value: T);
      Property Items[Index: LongWord]: T Read GetItem Write SetItem; Default;

      Type mLoopProcedure = Procedure(Obj: T);

      //Loops
      Procedure Foreach(Proc: mLoopProcedure);
  End;

Implementation
End.
