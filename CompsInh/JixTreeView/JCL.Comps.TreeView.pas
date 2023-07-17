unit JCL.Comps.TreeView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  JCL.Controls, JCL.Comps.Template, JCL.Utils, JCL.Theme;

Const
  NewDefaultTreeViewOptions = [
    tvoShowRoot, tvoShowLines, tvoShowButtons, tvoHideSelection, tvoToolTips,
    tvoKeepCollapsedNodes, tvoAutoItemHeight
  ];

Type
  TJixTreeView = class(TTreeView, iJixThemable)
    Private
      Var FThemeName: String;

    Protected
      Procedure SetThemeName(Value: ThemeStr); Virtual;
      Function GetThemeName: ThemeStr; Virtual;

    Public
      Constructor Create(AnOwner: TComponent); Override;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Virtual;

    published
      Property ThemeName: ThemeStr  Read GetThemeName  Write SetThemeName;

      Property ExpandSignType Default tvestPlusMinus;
      Property Options Default NewDefaultTreeViewOptions;
  End;

procedure Register;

Implementation
End.
