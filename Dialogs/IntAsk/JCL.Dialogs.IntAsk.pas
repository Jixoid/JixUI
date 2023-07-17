unit JCL.Dialogs.IntAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  JCL.Dialogs,
  JCL.Forms,
  LResources, Forms, Controls, Graphics, Dialogs, UnitIntAsk;

type
  TJixDIntAsk = class(TJixDialog)
  private
    Var FReply: Int16S;
    Var FMax: Int16S;
    Var FMin: Int16S;
    Var FText: TCaption;
    Var FClearReply: Boolean;

    Var FThemeName: String;

    Var FTextPosition: eHorzAlign;
    Var FButtonsPosition: eHorzAlignNC;

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Function Execute: Boolean; Override;

  published
    Property Text: TCaption            Read FText       Write FText;
    Property ClearReply: Boolean       Read FClearReply Write FClearReply Default True;
    Property Reply: Int16S             Read FReply      Write FReply;
    Property Max: Int16S               Read FMax        Write FMax        Default 100;
    Property Min: Int16S               Read FMin        Write FMin        Default 0;

    Property ThemeName: String         Read FThemeName  Write FThemeName;

    Property TextPosition: eHorzAlign      Read FTextPosition    Write FTextPosition    Default haCenter;
    Property ButtonsPosition: eHorzAlignNC Read FButtonsPosition Write FButtonsPosition Default haRight;

  end;

  Procedure Register;

Implementation
End.
