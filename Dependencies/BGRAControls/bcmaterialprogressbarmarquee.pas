unit BCMaterialProgressBarMarquee;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  ExtCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TBCMaterialProgressBarMarquee }

  TBCMaterialProgressBarMarquee = class(TBGRAGraphicControl)
  private
    FBarColor: TColor;
    progressbasr_cx, progressbar_cw: integer;
    progressbar_x, progressbar_w: integer;
    progressbar_increase: boolean;
    FTimer: TTimer;
    procedure SetBarColor(AValue: TColor);
    procedure TimerOnTimer(Sender: TObject);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
  public
    procedure DiscardBitmap;
    procedure RedrawBitmapContent; override;
    constructor Create(AOwner: TComponent); override;
  published
    property BarColor: TColor read FBarColor write SetBarColor;
    property Visible;
  end;

procedure Register;

Implementation
End.
