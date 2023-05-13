{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit Jixui;

{$warn 5023 off : no warning about unused units}
Interface

uses
  JForms, JixPas, JGL, Jedab, JDialogs, PortableForm, PortableJixForm, 
  JixComponent, JixTemplate, JixComboBox, JixCheckBox, JixRadioButton, 
  JixButton, LazarusPackageIntf;

Implementation

Procedure Register;
Begin
  Registerunit('JForms', @Jforms.Register);
  Registerunit('PortableForm', @Portableform.Register);
  Registerunit('PortableJixForm', @Portablejixform.Register);
  Registerunit('JixTemplate', @Jixtemplate.Register);
  Registerunit('JixComboBox', @Jixcombobox.Register);
  Registerunit('JixCheckBox', @Jixcheckbox.Register);
  Registerunit('JixRadioButton', @Jixradiobutton.Register);
  Registerunit('JixButton', @Jixbutton.Register);
End;

Initialization
  Registerpackage('JixUI', @Register);
End.
