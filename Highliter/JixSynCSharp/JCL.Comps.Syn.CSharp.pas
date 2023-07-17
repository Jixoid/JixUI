unit JCL.Comps.Syn.CSharp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Comps.Syn,
  JCL.Utils,
  LResources, Forms, Controls, Graphics, Dialogs;

type
  TJixSynCSharp = class(TJixSynHighliter)
    Private
      Var FUseFolding: Boolean;
      Var FAccessLimit: jSynTextStyle;
      Var FSpecific: jSynTextStyle;
      Var FCommenentS: jSynTextStyle;
      Var FCommenentB: jSynTextStyle;
      Var FCompOption: jSynTextStyle;
      Var FStaticText: jSynTextStyle;
      Var FNumber: jSynTextStyle;

    Public
      Constructor Create(Aowner: Tcomponent); Override;
      Destructor Destroy; Override;

      Procedure Start; Override;
      Procedure StartLine({%H-}Line: String); Override;
      Function Next(Word: String; X,Y: Int32U; {%H-}IsFolded: Boolean; TheSyn: TJixSyn): rCharStyle; Override;

    Published
      Property UseFolding: Boolean        Read FUseFolding  Write FUseFolding Default True;
      Property AccessLimit: jSynTextStyle Read FAccessLimit Write FAccessLimit;
      Property Specific: jSynTextStyle    Read FSpecific    Write FSpecific;
      Property CommenentS: jSynTextStyle  Read FCommenentS  Write FCommenentS;
      Property CommenentB: jSynTextStyle  Read FCommenentB  Write FCommenentB;
      Property CompOption: jSynTextStyle  Read FCompOption  Write FCompOption;
      Property StaticText: jSynTextStyle  Read FStaticText  Write FStaticText;
      Property Number: jSynTextStyle      Read FNumber      Write FNumber;

  end;

  Procedure Register;

Implementation
End.
