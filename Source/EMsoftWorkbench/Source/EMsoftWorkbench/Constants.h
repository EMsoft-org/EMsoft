/* ============================================================================
* Copyright (c) 2009-2016 BlueQuartz Software, LLC
*
* Redistribution and use in source and binary forms, with or without modification,
* are permitted provided that the following conditions are met:
*
* Redistributions of source code must retain the above copyright notice, this
* list of conditions and the following disclaimer.
*
* Redistributions in binary form must reproduce the above copyright notice, this
* list of conditions and the following disclaimer in the documentation and/or
* other materials provided with the distribution.
*
* Neither the name of BlueQuartz Software, the US Air Force, nor the names of its
* contributors may be used to endorse or promote products derived from this software
* without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
* USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
* The code contained herein was partially funded by the followig contracts:
*    United States Air Force Prime Contract FA8650-07-D-5800
*    United States Air Force Prime Contract FA8650-10-D-5210
*    United States Prime Contract Navy N00173-07-C-2068
*
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#ifndef _emsoftworkbenchconstants_h_
#define _emsoftworkbenchconstants_h_

#include <math.h>

#include <QtCore/QString>

#if defined (_MSC_VER)
#define WIN32_LEAN_AND_MEAN   // Exclude rarely-used stuff from Windows headers
#endif

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
namespace EMsoftWorkbenchConstants
{ 
  using EnumType = unsigned int;

  enum class InfoStringFormat : EnumType
  {
    HtmlFormat = 0,
//      JsonFormat,
//      TextFormat,
//      XmlFormat,
    UnknownFormat
  };

  namespace Constants
  {
  static const float k_Pif = static_cast<float>(M_PI);
  static const double k_Pi = M_PI;
  static const double k_SqrtPi = sqrt(M_PI);
  static const double k_2OverSqrtPi = 2.0 / sqrt(M_PI);
  static const double k_HalfOfSqrtPi = sqrt(M_PI) / 2.0;
  static const double k_2Pi = 2.0 * M_PI;
  static const double k_1OverPi = 1.0 / M_PI;
  static const double k_PiOver180 = M_PI / 180.0;
  static const double k_360OverPi = 360.0 / M_PI;
  static const double k_180OverPi = 180.0 / M_PI;
  static const double k_PiOver2 = M_PI / 2.0;
  static const double k_PiOver4 = M_PI / 4.0;
  static const double k_PiOver8 = M_PI / 8.0;
  static const double k_PiOver12 = M_PI / 12.0;
  static const double k_Sqrt2 = sqrt(2.0);
  static const double k_Sqrt3 = sqrt(3.0);
  static const double k_HalfSqrt2 = 0.5 * sqrt(2.0);
  static const double k_1OverRoot2 = 1.0 / sqrt(2.0);
  static const double k_1OverRoot3 = 1.0 / sqrt(3.0);
  static const double k_Root3Over2 = sqrt(3.0) / 2.0;
  static const double k_DegToRad = M_PI / 180.0;
  static const double k_RadToDeg = 180.0 / M_PI;
  static const double k_1Point3 = 1.0 + 1.0 / 3.0;
  static const double k_1Over3 = 1.0 / 3.0;

  static const double k_ACosNeg1 = acos(-1.0);
  static const double k_ACos1 = acos(1.0);

  static const double k_Tan_OneEigthPi = tan(k_PiOver8);
  static const double k_Cos_OneEigthPi = cos(k_PiOver8);
  static const double k_Cos_ThreeEightPi = cos(3.0 * k_PiOver8);
  static const double k_Sin_ThreeEightPi = sin(3.0 * k_PiOver8);

  static const int AnorthicType = 0; // Triclinic
  static const int CyclicType = 1;
  static const int DihedralType = 2;
  static const int TetrahedralType = 3;
  static const int OctahedralType = 4;

  static const int NoAxisOrder = 0;
  static const int TwoFoldAxisOrder = 2;
  static const int ThreeFoldAxisOrder = 3;
  static const int FourFoldAxisOrder = 4;
  static const int SixFoldAxisOrder = 6;

  static const int FZtarray[32] = { AnorthicType, AnorthicType,CyclicType,CyclicType,CyclicType,
                                    DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,CyclicType,
                                    DihedralType,DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,
                                    DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,CyclicType,
                                    DihedralType,DihedralType,DihedralType,DihedralType,TetrahedralType,
                                    TetrahedralType,OctahedralType,TetrahedralType,OctahedralType };

  static const int FZoarray[32] = { NoAxisOrder,NoAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,
                                    TwoFoldAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,FourFoldAxisOrder,
                                    FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,
                                    FourFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,
                                    ThreeFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,
                                    SixFoldAxisOrder,SixFoldAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder};
  }

  namespace StringConstants
  {
    const QString GBCD("GBCD");

    const QString Statistics("Statistics");
    const QString StatsType("StatsType");
  }

}

#endif /* _emsoftworkbenchconstants_h_ */

