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

#include "SampleCubochoricSpaceWidget.h"

#include <QtCore/QJsonObject>

#include "Common/Constants.h"

#include "Modules/PatternDisplayModule/PatternDisplay_UI.h"

#include "OrientationLib/OrientationLibConstants.h"
#include "OrientationLib/OrientationMath/OrientationTransforms.hpp"

using OrientationListArrayType = std::list<DOrientArrayType>;
using OrientationTransformsType = OrientationTransforms<DOrientArrayType, double>;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SampleCubochoricSpaceWidget::SampleCubochoricSpaceWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: AbstractAngleWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SampleCubochoricSpaceWidget::~SampleCubochoricSpaceWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::setupGui()
{
  QIntValidator* intValidator = new QIntValidator(ptGrpNumLE);
  ptGrpNumLE->setValidator(intValidator);
  connect(ptGrpNumLE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  QDoubleValidator* dblValidator = new QDoubleValidator(misorientationAngLE);
  misorientationAngLE->setValidator(dblValidator);
  connect(misorientationAngLE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  dblValidator = new QDoubleValidator(refOrientationX_LE);
  refOrientationX_LE->setValidator(dblValidator);
  connect(refOrientationX_LE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  dblValidator = new QDoubleValidator(refOrientationY_LE);
  refOrientationY_LE->setValidator(dblValidator);
  connect(refOrientationY_LE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  dblValidator = new QDoubleValidator(refOrientationZ_LE);
  refOrientationZ_LE->setValidator(dblValidator);
  connect(refOrientationZ_LE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  dblValidator = new QDoubleValidator(numOfSamplingPtsLE);
  numOfSamplingPtsLE->setValidator(dblValidator);
  connect(numOfSamplingPtsLE, SIGNAL(textChanged(const QString&)), this, SLOT(lineEditChanged(const QString&)));

  misorientationAngLabel->hide();
  misorientationAngLE->hide();
  refOrientationLabel->hide();
  refOrientationX_LE->hide();
  refOrientationY_LE->hide();
  refOrientationZ_LE->hide();

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::lineEditChanged(const QString& text)
{
  Q_UNUSED(text)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::valuesChanged()
{
  FloatArrayType::Pointer eulerAngles = getEulerAngles();

  if(eulerAngles == FloatArrayType::NullPointer())
  {
    numOfAnglesLineEdit->setText("0");
  }
  else
  {
    numOfAnglesLineEdit->setText(QString::number(eulerAngles->getNumberOfTuples()));
  }

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SampleCubochoricSpaceWidget::hasValidAngles()
{
  return (numOfAnglesLineEdit->text().toInt() > 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::readSession(QJsonObject& obj)
{
  samplingModeCB->setCurrentIndex(obj[EMsoftWorkbenchConstants::IOStrings::SamplingMode].toInt());
  ptGrpNumLE->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::PointGroupNum].toInt()));
  misorientationAngLE->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::MisorientationAngle].toDouble()));

  QJsonObject refOrientationObj = obj[EMsoftWorkbenchConstants::IOStrings::ReferenceOrientation].toObject();
  refOrientationX_LE->setText(QString::number(refOrientationObj[EMsoftWorkbenchConstants::IOStrings::X].toDouble()));
  refOrientationY_LE->setText(QString::number(refOrientationObj[EMsoftWorkbenchConstants::IOStrings::Y].toDouble()));
  refOrientationZ_LE->setText(QString::number(refOrientationObj[EMsoftWorkbenchConstants::IOStrings::Z].toDouble()));

  offsetSamplingGridChkBox->setChecked(obj[EMsoftWorkbenchConstants::IOStrings::OffsetSamplingGrid].toBool());
  numOfSamplingPtsLE->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::NumberOfSamplingPoints].toDouble()));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::writeSession(QJsonObject& obj)
{
  obj[EMsoftWorkbenchConstants::IOStrings::SamplingMode] = samplingModeCB->currentIndex();
  obj[EMsoftWorkbenchConstants::IOStrings::PointGroupNum] = ptGrpNumLE->text().toInt();
  obj[EMsoftWorkbenchConstants::IOStrings::MisorientationAngle] = misorientationAngLE->text().toDouble();

  QJsonObject refOrientationObj;
  refOrientationObj[EMsoftWorkbenchConstants::IOStrings::X] = refOrientationX_LE->text().toDouble();
  refOrientationObj[EMsoftWorkbenchConstants::IOStrings::Y] = refOrientationY_LE->text().toDouble();
  refOrientationObj[EMsoftWorkbenchConstants::IOStrings::Z] = refOrientationZ_LE->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::ReferenceOrientation] = refOrientationObj;

  obj[EMsoftWorkbenchConstants::IOStrings::OffsetSamplingGrid] = offsetSamplingGridChkBox->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::NumberOfSamplingPoints] = numOfSamplingPtsLE->text().toDouble();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::createModificationConnections(PatternDisplay_UI* ui)
{
  // Line Edits
  connect(ptGrpNumLE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(misorientationAngLE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(refOrientationX_LE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(refOrientationY_LE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(refOrientationZ_LE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(numOfSamplingPtsLE, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });

  // Checkboxes
  connect(offsetSamplingGridChkBox, &QCheckBox::stateChanged, [=] { emit ui->moduleParametersChanged(); });

  // Combo Boxes
  connect(samplingModeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { emit ui->moduleParametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::void_on_offsetSamplingGridChkBox_stateChanged(int state)
{
  Q_UNUSED(state)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::on_samplingModeCB_currentIndexChanged(int index)
{
  if(index == 0)
  {
    ptGrpNumLabel->show();
    ptGrpNumLE->show();
    offsetSamplingGridChkBox->show();

    misorientationAngLabel->hide();
    misorientationAngLE->hide();
    refOrientationLabel->hide();
    refOrientationX_LE->hide();
    refOrientationY_LE->hide();
    refOrientationZ_LE->hide();
  }
  else
  {
    ptGrpNumLabel->hide();
    ptGrpNumLE->hide();
    offsetSamplingGridChkBox->hide();

    misorientationAngLabel->show();
    misorientationAngLE->show();
    refOrientationLabel->show();
    refOrientationX_LE->show();
    refOrientationY_LE->show();
    refOrientationZ_LE->show();
  }

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SampleCubochoricSpaceWidget::RodriguesComposition(const DOrientArrayType& sigma, DOrientArrayType& rod)
{
  DOrientArrayType rho(3), rhomis(3);
  rho[0] = -rod[0] * rod[3];
  rho[1] = -rod[1] * rod[3];
  rho[2] = -rod[2] * rod[3];

  // perform the Rodrigues rotation composition with sigma to get rhomis
  double denom = 1.0f + (sigma[0] * rho[0] + sigma[1] * rho[1] + sigma[2] * rho[2]);
  if(denom == 0.0f)
  {
    double len;
    len = sqrt(sigma[0] * sigma[0] + sigma[1] * sigma[1] + sigma[2] * sigma[2]);
    rod[0] = sigma[0] / len;
    rod[1] = sigma[1] / len;
    rod[2] = sigma[2] / len;
    rod[3] = std::numeric_limits<double>::infinity(); // set this to infinity
  }
  else
  {
    rhomis[0] = (rho[0] - sigma[0] + (rho[1] * sigma[2] - rho[2] * sigma[1])) / denom;
    rhomis[1] = (rho[1] - sigma[1] + (rho[2] * sigma[0] - rho[0] * sigma[2])) / denom;
    rhomis[2] = (rho[2] - sigma[2] + (rho[0] * sigma[1] - rho[1] * sigma[0])) / denom;
    // revert rhomis to a four-component Rodrigues vector
    double len;
    len = sqrt(rhomis[0] * rhomis[0] + rhomis[1] * rhomis[1] + rhomis[2] * rhomis[2]);
    if(len != 0.0f)
    {
      rod[0] = -rhomis[0] / len;
      rod[1] = -rhomis[1] / len;
      rod[2] = -rhomis[2] / len;
      rod[3] = len;
    }
    else
    {
      rod[0] = 0.0;
      rod[1] = 0.0;
      rod[2] = 0.0;
      rod[3] = 0.0;
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SampleCubochoricSpaceWidget::IsinsideFZ(double* rod, int FZtype, int FZorder)
{
  bool insideFZ = false;
  // dealing with 180 rotations is needed only for
  // FZtypes 0 and 1; the other FZs are always finite.
  switch(FZtype)
  {
  case EMsoftWorkbenchConstants::Constants::AnorthicType:
    insideFZ = true; // all points are inside the FZ
    break;
  case EMsoftWorkbenchConstants::Constants::CyclicType:
    insideFZ = insideCyclicFZ(rod, FZorder); // infinity is checked inside this function
    break;
  case EMsoftWorkbenchConstants::Constants::DihedralType:
    if(rod[3] != std::numeric_limits<double>::infinity())
    {
      insideFZ = insideDihedralFZ(rod, FZorder);
    }
    break;
  case EMsoftWorkbenchConstants::Constants::TetrahedralType:
    if(rod[3] != std::numeric_limits<double>::infinity())
    {
      insideFZ = insideCubicFZ(rod, EMsoftWorkbenchConstants::Constants::TetrahedralType);
    }
    break;
  case EMsoftWorkbenchConstants::Constants::OctahedralType:
    if(rod[3] != std::numeric_limits<double>::infinity())
    {
      insideFZ = insideCubicFZ(rod, EMsoftWorkbenchConstants::Constants::OctahedralType);
    }
    break;
  default:
    insideFZ = false;
    break;
  }
  return insideFZ;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SampleCubochoricSpaceWidget::insideCyclicFZ(const double* rod, int order)
{

  bool insideFZ = false;

  if(rod[3] != std::numeric_limits<double>::infinity())
  {
    // check the z-component vs. tan(pi/2n)
    insideFZ = fabs(rod[2] * rod[3]) <= LPs::BP[order - 1];
  }
  else if(rod[2] == 0.0)
  {
    insideFZ = true;
  }
  return insideFZ;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SampleCubochoricSpaceWidget::insideDihedralFZ(const double* rod, int order)
{

  bool res = false, c1 = false, c2 = false;
  double r[3] = {rod[0] * rod[3], rod[1] * rod[3], rod[2] * rod[3]};
  const double r1 = 1.0;

  // first, check the z-component vs. tan(pi/2n)  (same as insideCyclicFZ)
  c1 = fabs(r[2]) <= LPs::BP[order - 1];
  res = false;

  // check the square boundary planes if c1=true
  if(c1)
  {
    switch(order)
    {
    case EMsoftWorkbenchConstants::Constants::TwoFoldAxisOrder:
      c2 = (fabs(r[0]) <= r1) && (fabs(r[1]) <= r1);
      break;
    case EMsoftWorkbenchConstants::Constants::ThreeFoldAxisOrder:
      c2 = fabs(LPs::srt * r[0] + 0.50 * r[1]) <= r1;
      c2 = c2 && (fabs(LPs::srt * r[0] - 0.50 * r[1]) <= r1);
      c2 = c2 && (fabs(r[1]) <= r1);
      break;
    case EMsoftWorkbenchConstants::Constants::FourFoldAxisOrder:
      c2 = (fabs(r[0]) <= r1) && (fabs(r[1]) <= r1);
      c2 = c2 && ((LPs::r22 * fabs(r[0] + r[1]) <= r1) && (LPs::r22 * fabs(r[0] - r[1]) <= r1));
      break;
    case EMsoftWorkbenchConstants::Constants::SixFoldAxisOrder:
      c2 = fabs(0.50 * r[0] + LPs::srt * r[1]) <= r1;
      c2 = c2 && (fabs(LPs::srt * r[0] + 0.50 * r[1]) <= r1);
      c2 = c2 && (fabs(LPs::srt * r[0] - 0.50 * r[1]) <= r1);
      c2 = c2 && (fabs(0.50 * r[0] - LPs::srt * r[1]) <= r1);
      c2 = c2 && (fabs(r[1]) <= r1);
      c2 = c2 && (fabs(r[0]) <= r1);
      break;
    default:
      res = false;
      break;
    }
    res = c2;
  }
  return res;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SampleCubochoricSpaceWidget::insideCubicFZ(const double* rod, int ot)
{
  bool res = false, c1 = false, c2 = false;
  std::vector<double> r(3);
  r[0] = rod[0] * rod[3];
  r[1] = rod[1] * rod[3];
  r[2] = rod[2] * rod[3];
  const double r1 = 1.0;

  // primary cube planes (only needed for octahedral case)
  if(ot == EMsoftWorkbenchConstants::Constants::OctahedralType)
  {

    using OrientationTransformsType = OrientationTransforms<std::vector<double>, double>;

    c1 = OrientationTransformsType::OMHelperType::maxval(OrientationTransformsType::OMHelperType::absValue(r)) <= LPs::BP[3];
  }
  else
  {
    c1 = true;
  }

  // octahedral truncation planes, both for tetrahedral and octahedral point groups
  c2 = ((fabs(r[0]) + fabs(r[1]) + fabs(r[2])) <= r1);

  // if both c1 and c2, then the point is inside
  if(c1 && c2)
  {
    res = true;
  }

  return res;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer SampleCubochoricSpaceWidget::getEulerAngles()
{
  OrientationListArrayType FZlist;

  double numOfSamplingPts = numOfSamplingPtsLE->text().toDouble();
  if(samplingModeCB->currentIndex() == 0)
  {
    int ptGrpNum = ptGrpNumLE->text().toInt();

    // here we perform the actual calculation; once we have the FZlist,
    // we can allocate the data array and copy all entries
    double x, y, z, delta;
    int32_t FZtype, FZorder;

    // step size for sampling of grid; maximum total number of samples = pow(2*getNumsp()+1,3)
    delta = (0.50 * LPs::ap) / static_cast<double>(numOfSamplingPts);

    // do we need to shift this array away from the origin?
    double gridShift = 0.0f;
    if(offsetSamplingGridChkBox->isChecked())
    {
      gridShift = 0.5f;
    }

    // determine which function we should call for this point group symmetry
    FZtype = EMsoftWorkbenchConstants::Constants::FZtarray[ptGrpNum - 1];
    FZorder = EMsoftWorkbenchConstants::Constants::FZoarray[ptGrpNum - 1];

    // loop over the cube of volume pi^2; note that we do not want to include
    // the opposite edges/facets of the cube, to avoid double counting rotations
    // with a rotation angle of 180 degrees.  This only affects the cyclic groups.
    int Np = numOfSamplingPts;
    int Totp = pow(2 * Np + 1, 3);
    int Dn = Totp / 10;
    int Dc = Dn;
    int Di = 0;
    int Dg = 0;

    // eliminate points for which any of the coordinates lies outside the cube with semi-edge length "edge"
    double edge = 0.5 * LPs::ap;

    for(int i = -Np + 1; i < Np + 1; i++)
    {
      x = (static_cast<double>(i) + gridShift) * delta;

      if(fabs(x) <= edge)
      {

        for(int j = -Np + 1; j < Np + 1; j++)
        {
          y = (static_cast<double>(j) + gridShift) * delta;

          if(fabs(y) <= edge)
          {

            for(int k = -Np + 1; k < Np + 1; k++)
            {
              z = (static_cast<double>(k) + gridShift) * delta;

              if(fabs(z) <= edge)
              {

                // convert to Rodrigues representation
                DOrientArrayType cu(x, y, z);
                DOrientArrayType rod(4);
                OrientationTransformsType::cu2ro(cu, rod);

                // If insideFZ=true, then add this point to FZlist
                bool b = IsinsideFZ(rod.data(), FZtype, FZorder);
                if(b)
                {
                  FZlist.push_back(rod);
                  Dg += 1;
                }
                Di += 1;
              }
            }
          }
        }
      }

      // report on status of computation
      if(Di > Dc)
      {
        // Error: "Euler angle triplets tested: %1 of %2; triplets inside Rodrigues fundamental zone: %3 ").arg(QString::number(Di),QString::number(Totp),QString::number(Dg));

        Dc += Dn;
      }
    }
  }

  // here are the misorientation sampling cases:
  else
  {
    // here we perform the actual calculation; once we have the FZlist,
    // we can allocate the data array and copy all entries
    double x, y, z, delta, omega, semi;

    double refOrientationX = refOrientationX_LE->text().toDouble();
    double refOrientationY = refOrientationY_LE->text().toDouble();
    double refOrientationZ = refOrientationZ_LE->text().toDouble();
    double misorientationAngle = misorientationAngLE->text().toDouble();

    // step size for sampling of grid; the edge length of the cube is (pi ( w - sin(w) ))^1/3 with w the misorientation angle
    omega = misorientationAngle * SIMPLib::Constants::k_Pi / 180.0f;
    semi = pow(SIMPLib::Constants::k_Pi * (omega - sin(omega)), 1.0 / 3.0) * 0.5;
    delta = semi / static_cast<double>(numOfSamplingPts);

    // convert the reference orientation to a 3-component Rodrigues vector sigma
    DOrientArrayType sigm(4), sigma(3), referenceOrientation(3);
    referenceOrientation[0] = static_cast<double>(refOrientationX * SIMPLib::Constants::k_Pi / 180.0f);
    referenceOrientation[1] = static_cast<double>(refOrientationY * SIMPLib::Constants::k_Pi / 180.0f);
    referenceOrientation[2] = static_cast<double>(refOrientationZ * SIMPLib::Constants::k_Pi / 180.0f);
    OrientationTransformsType::eu2ro(referenceOrientation, sigm);
    sigma[0] = sigm[0] * sigm[3];
    sigma[1] = sigm[1] * sigm[3];
    sigma[2] = sigm[2] * sigm[3];

    if(samplingModeCB->currentIndex() == 1)
    {
      // set counter parameters for the loop over the sub-cube surface
      int Np = numOfSamplingPts;
      int Totp = 24 * Np * Np + 2;
      int Dn = Totp / 20;
      int Dc = Dn;
      int Dg = 0;

      // x-y bottom and top planes
      for(int i = -Np; i <= Np; i++)
      {
        x = static_cast<double>(i) * delta;
        for(int j = -Np; j <= Np; j++)
        {
          y = static_cast<double>(j) * delta;
          // convert to Rodrigues representation and apply Rodrigues composition formula
          {
            DOrientArrayType cu(-x, -y, -semi);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
          {
            DOrientArrayType cu(-x, -y, semi);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
        }
      }
      // y-z  planes
      for(int j = -Np; j <= Np; j++)
      {
        y = static_cast<double>(j) * delta;
        for(int k = -Np + 1; k <= Np - 1; k++)
        {
          z = static_cast<double>(k) * delta;
          // convert to Rodrigues representation and apply Rodrigues composition formula
          {
            DOrientArrayType cu(-semi, -y, -z);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
          {
            DOrientArrayType cu(semi, -y, -z);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
        }
      }
      // finally the x-z  planes
      for(int i = -Np + 1; i <= Np - 1; i++)
      {
        x = static_cast<double>(i) * delta;
        for(int k = -Np + 1; k <= Np - 1; k++)
        {
          z = static_cast<double>(k) * delta;
          // convert to Rodrigues representation and apply Rodrigues composition formula
          {
            DOrientArrayType cu(-x, -semi, -z);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
          {
            DOrientArrayType cu(-x, semi, -z);
            DOrientArrayType rod(4);
            OrientationTransformsType::cu2ro(cu, rod);
            RodriguesComposition(sigma, rod);
            FZlist.push_back(rod);
            Dg += 1;
          }
        }
      }

      // report on status of computation
      if(Dg > Dc)
      {
        // Error: "Euler angle triplets generated: %1 out of %2").arg(QString::number(Dg),QString::number(Totp));

        Dc += Dn;
      }
    }
    else
    {
      // set counter parameters for the loop over the sub-cube surface
      int Np = numOfSamplingPts;
      int Totp = pow((2 * Np + 1), 3); // see misorientation sampling paper for this expression
      int Dn = Totp / 20;
      int Dc = Dn;
      int Dg = 0;

      for(int i = -Np; i <= Np; i++)
      {
        x = static_cast<double>(i) * delta;
        for(int j = -Np; j <= Np; j++)
        {
          y = static_cast<double>(j) * delta;
          for(int k = -Np; k <= Np; k++)
          {
            z = static_cast<double>(k) * delta;
            // convert to Rodrigues representation and apply Rodrigues composition formula
            {
              DOrientArrayType cu(-x, -y, -z);
              DOrientArrayType rod(4);
              OrientationTransformsType::cu2ro(cu, rod);
              RodriguesComposition(sigma, rod);
              FZlist.push_back(rod);
              Dg += 1;
            }
          }
        }
        // report on status of computation
        if(Dg > Dc)
        {
          // Error: "Euler angle triplets generated: %1 out of %2").arg(QString::number(Dg),QString::number(Totp));

          Dc += Dn;
        }
      }
    }
  }

  // resize the EulerAngles array to the number of items in FZlist; don't forget to redefine the hard pointer
  FloatArrayType::Pointer eulerAngles = FloatArrayType::CreateArray(FZlist.size(), QVector<size_t>(1, 3), "Euler Angles");

  // copy the Rodrigues vectors as Euler angles into the eulerAngles array; convert doubles to floats along the way
  int j = -1;
  for(const DOrientArrayType& rod : FZlist)
  {
    j += 1;
    DOrientArrayType eu(3, 0.0f);
    OrientationTransformsType::ro2eu(rod, eu);

    eulerAngles->setComponent(j, 0, static_cast<float>(eu[0]));
    eulerAngles->setComponent(j, 1, static_cast<float>(eu[1]));
    eulerAngles->setComponent(j, 2, static_cast<float>(eu[2]));
  }

  return eulerAngles;
}
