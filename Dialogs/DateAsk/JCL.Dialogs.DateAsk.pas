unit JCL.Dialogs.DateAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Dialogs,
  JCL.Forms,
  LResources, Forms, Controls, Graphics, Dialogs, UnitDialogDate;

type
  TJixDDateAsk = class(TJixDialog)
  private
    Var FReply: TDate;
    Var FClearReply: Boolean;

    Var FThemeName: String;

    Var FTextPosition: eHorzAlign;
    Var FButtonsPosition: eHorzAlignNC;

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

    Function Execute: Boolean; Override;

  published
    Property ClearReply: Boolean       Read FClearReply Write FClearReply Default True;
    Property Reply: TDate              Read FReply  Write FReply;

    Property ThemeName: String         Read FThemeName  Write FThemeName;

    Property TextPosition: eHorzAlign      Read FTextPosition    Write FTextPosition    Default haCenter;
    Property ButtonsPosition: eHorzAlignNC Read FButtonsPosition Write FButtonsPosition Default haRight;

  end;

  Procedure Register;

Implementation
End.
