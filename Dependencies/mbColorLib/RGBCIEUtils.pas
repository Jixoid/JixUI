unit RGBCIEUtils;

interface

uses
  SysUtils, LCLIntf, Graphics, Math;

const
  {// Observer= 2°, Illuminant= D65    - Daylignt
  ref_X =  95.047;
  ref_Z = 108.883;
  // Observer= 10°, Illuminant= D65   - Daylight
  ref_X =  94.811;
  ref_Z = 35.2;

  // Observer= 2°, Illuminant= A      - Incadescent
  ref_X =  109.850;
  ref_Z = 35.585;
  // Observer= 10°, Illuminant= A     - Incadescent
  ref_X =  111.144;
  ref_Z = 35.2;

  // Observer= 2°, Illuminant= C
  ref_X =  98.074;
  ref_Z = 118.232;
  // Observer= 10°, Illuminant= C
  ref_X =  97.285;
  ref_Z = 116.145;
                }
  // Observer= 2°, Illuminant= D50
  ref_X =  96.422;
  ref_Z = 82.521;{
  // Observer= 10°, Illuminant= D50 - Photoshop
  ref_X =  96.72;
  ref_Z = 81.427; }

  {// Observer= 2°, Illuminant= D55
  ref_X =  95.682;
  ref_Z = 92.149;
  // Observer= 10°, Illuminant= D55
  ref_X =  95.799;
  ref_Z = 90.926;

  // Observer= 2°, Illuminant= D75
  ref_X =  94.972;
  ref_Z = 122.638;
  // Observer= 10°, Illuminant= D75
  ref_X =  94.416;
  ref_Z = 12.641;

  // Observer= 2°, Illuminant= F2     - Fluorescent
  ref_X =  99.187;
  ref_Z = 67.395;
  // Observer= 10°, Illuminant= F2    - Fluorescent
  ref_X =  103.28;
  ref_Z = 69.026;

  // Observer= 2°, Illuminant= F7
  ref_X =  95.044;
  ref_Z = 108.755;
  // Observer= 10°, Illuminant= F7
  ref_X =  95.792;
  ref_Z = 107.678;

  // Observer= 2°, Illuminant= F11
  ref_X =  100.966;
  ref_Z = 64.370;
  // Observer= 10°, Illuminant= F11
  ref_X =  103.866;
  ref_Z = 65.627;   }

type
  xyz = record
    x: Double;
    y: Double;
    z: Double;
  end;

function LabToXYZ(l, a, b: double): xyz;
function XYZToRGB(space: xyz): TColor;
function LabToRGB(l, a, b: double): TColor;
function RGBToXYZ(c: TColor): xyz;
procedure RGBToLab(clr: TColor; var l, a, b: double);
procedure XYZToLab(space: xyz; var l, a, b: double);
procedure LCHToLab(lum, c, h: double; var l, a, b: double);
procedure LabToLCH(l, a, b: double; var lum, c, h: double);
function LCHToRGB(l, c, h: double): TColor;
procedure RGBToLCH(clr: TColor; var l, c, h: double);
function GetCIEXValue(c: TColor): double;
function GetCIEYValue(c: TColor): double;
function GetCIEZValue(c: TColor): double;
function GetCIELValue(c: TColor): double;
function GetCIEAValue(c: TColor): double;
function GetCIEBValue(c: TColor): double;
function GetCIECValue(c: TColor): double;
function GetCIEHValue(c: TColor): double;

Implementation
End.
