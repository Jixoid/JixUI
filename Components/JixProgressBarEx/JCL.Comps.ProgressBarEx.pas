unit JCL.Comps.ProgressBarEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Comps.ProgressBar,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs;

type
  TJixProgressBarEx = class(TJixProgressBar)
  private

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

  published
    Property StateNormal;
    Property StateHover;
    Property StateClick;
    Property Static;

  end;

  Procedure Register;

Implementation
End.
