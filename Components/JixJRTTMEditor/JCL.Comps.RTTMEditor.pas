Unit JCL.Comps.RTTMEditor;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  JCL.Controls,
  JCL.ControlsEx,
  JCL.RTPM,
  JCL.Comps.Edit,
  JCL.Comps.CheckBox,
  JCL.Comps.ComboBox,
  JCL.Theme,
  JCL.Utils,
  JCL.Comps.Template,
  JCL.Comps.Panel,
  JCL.Auxiliary;

Type
  TJixJRTTMEditor = class(TJixScrollingControl)
    Private
      Var FGutterSize: Int16U;
      Procedure SetGutterSize(Value: Int16U);

      Var FGutterColor: TColor;
      Procedure SetGutterColor(Value: TColor);

    Protected
      Var PropertyList: lRTTIProperty;
      Var Edit: jRTTI;
      Var Editor: TControl;

      Var FLegIndex: Int32S;
      Var FItemIndex: Int32S;
      Procedure SetItemIndex(Value: Int32S);

      Var FSplitter: Int32U;
      Procedure SetSplitter(Value: Int32U);

      Var FSelectedObject: TObject;
      Procedure SetSelectedObject(Value: TObject);

      Var FSubPanel: TJixPanel;
      Procedure InterfacePaintBefore(Sender: TObject);
      Procedure InterfacePaintAfter(Sender: TObject);
      Procedure InterfacePaintRow(Index, Sub, TopL: Int32U);
      Procedure InterfaceMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);

      Var FBorderLineColor: TColor;
      Procedure SetBorderLineColor(Value: TColor);

      Var FEditorEdit: TJixEdit;
      Var FEditorCheck: TJixCheckBox;
      Var FEditorCombo: TJixComboBox;

      Procedure CheckBoxClick(Sender: TObject);

    Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Override;
      Procedure StateChanged({%H-}ASender: TObject); Override;

      Procedure SetItem(Index: Int32S);

      Property ItemIndex: Int32S Read FItemIndex Write SetItemIndex;

    Published
      Property SelectedObject: TObject Read FSelectedObject  Write SetSelectedObject;

      Property Splitter: Int32U        Read FSplitter        Write SetSplitter        Default 100;
      Property GutterSize: Int16U      Read FGutterSize      Write SetGutterSize      Default 16;
      Property GutterColor: TColor     Read FGutterColor     Write SetGutterColor;
      Property BorderLineColor: TColor Read FBorderLineColor Write SetBorderLineColor Default clGray;

      Property IgnoreTempStyle;

      Property StateBack;
      Property StateNormal;
      Property StateHover;
      Property StateClick;

      Property Align;
      Property BorderSpacing;
      Property Anchors;
      Property AutoSize;
      Property Enabled;
      Property Visible;

      Property OnChangeBounds;
      Property OnPaintBefore;
      Property OnPaintAfter;
      Property OnClick;
      Property OnDblClick;
      Property OnMouseDown;
      Property OnMouseEnter;
      Property OnMouseLeave;
      Property OnMouseMove;
      Property OnMouseUp;
      Property OnMouseWheel;
      Property OnMouseWheelDown;
      Property OnMouseWheelUp;
      Property OnResize;
      Property PopupMenu;
  End;

Procedure Register;

Implementation
End.
