{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit Jixui;

{$warn 5023 off : no warning about unused units}
Interface

uses
  JForms, JixPas, JGL, Jedab, JDialogs, JixComponent, JixButton, JixComboBox, 
  JixCheckBox, JixRadioButton, JixPanel, JixPanelEx, JixProgressBar, 
  JixProgressBarEx, JixPaintBox, JixPaintBoxWin, JixScrollBar, 
  LazarusPackageIntf;

Implementation

Procedure Register;
Begin
  Registerunit('JixComponent', @Jixcomponent.Register);
  Registerunit('JixButton', @Jixbutton.Register);
  Registerunit('JixComboBox', @Jixcombobox.Register);
  Registerunit('JixCheckBox', @Jixcheckbox.Register);
  Registerunit('JixRadioButton', @Jixradiobutton.Register);
  Registerunit('JixPanel', @Jixpanel.Register);
  Registerunit('JixPanelEx', @Jixpanelex.Register);
  Registerunit('JixProgressBar', @Jixprogressbar.Register);
  Registerunit('JixProgressBarEx', @Jixprogressbarex.Register);
  Registerunit('JixPaintBox', @Jixpaintbox.Register);
  Registerunit('JixPaintBoxWin', @Jixpaintboxwin.Register);
  Registerunit('JixScrollBar', @Jixscrollbar.Register);
End;

Initialization
  Registerpackage('JixUI', @Register);
End.
