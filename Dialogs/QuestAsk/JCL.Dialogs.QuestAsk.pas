unit JCL.Dialogs.QuestAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Dialogs,
  JCL.Forms,
  LResources, Forms, Controls, Graphics, Dialogs, UnitQuestAsk;

type
  TJixDQuestAsk = class(TJixDialog)
  private
    Var FText: TCaption;

    Var FThemeName: String;

    Var FTextPosition: eHorzAlign;
    Var FButtonsPosition: eHorzAlignNC;

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Function Execute: Boolean; Override;

  published
    Property Text: TCaption            Read FText       Write FText;

    Property ThemeName: String         Read FThemeName  Write FThemeName;

    Property TextPosition: eHorzAlign      Read FTextPosition    Write FTextPosition    Default haCenter;
    Property ButtonsPosition: eHorzAlignNC Read FButtonsPosition Write FButtonsPosition Default haRight;

  end;

  Procedure Register;

Implementation
End.
