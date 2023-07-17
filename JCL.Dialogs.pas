{  $Id$  }
{
JixDialogs
|
|- Ver: 4.4
|- Status: Alpha
|- Meaning: JixUI Dialogs
|- Using: JixUI 4.4

}
Unit JCL.Dialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, PropEdits, TypInfo,
  JCL.Utils,
  JCL.Generics;

Type
  CustomErrorFlag = (
    Close_Halt,
    Soft
  );

Procedure JixDialogsStart;

Procedure ShowMessage(Message: String);
Function TextAsk(Caption, Text: String; Var Value: String): Boolean;
Function IntAsk(Caption, Text: String; Max, Min: Int16S; Var Value: Int16S): Boolean;
Function QuestAsk(Caption, Text: String): Boolean;
Function TimeAsk(Caption: String; var Time: TTime): Boolean;
Function DateAsk(Caption: String; var Date: TDate): Boolean;
Function CustomErrorSend(Caption, Log: String; Flag: CustomErrorFlag): TForm;

Implementation
End.
