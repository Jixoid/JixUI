unit JCL.Comps.Syn.Any;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Comps.Syn,
  JCL.Utils,
  LResources, Forms, Controls, Graphics, Dialogs;

Type
  yNext = Function(Word: String; X,Y: Int32U; IsFolded: Boolean; TheSyn: TJixSyn): rCharStyle of object;
  yStartLine = Procedure(Line: String) of object;

  TJixSynAny = class(TJixSynHighliter)
    Private
      Var FOnStart: mProcedureO;
      Var FOnStartLine: yStartLine;
      Var FOnNext: yNext;

    Public
      Constructor Create(Aowner: Tcomponent); Override;

      Procedure Start; Override;
      Procedure StartLine(Line: String); Override;
      Function Next(Word: String; X,Y: Int32U; IsFolded: Boolean; TheSyn: TJixSyn): rCharStyle; Override;

    Published
      Property OnStart: mProcedureO     Read FOnStart      Write FOnStart;
      Property OnStartLine: yStartLine  Read FOnStartLine  Write FOnStartLine;
      Property OnNext: yNext            Read FOnNext       Write FOnNext;
  end;

Procedure Register;

Implementation
End.
