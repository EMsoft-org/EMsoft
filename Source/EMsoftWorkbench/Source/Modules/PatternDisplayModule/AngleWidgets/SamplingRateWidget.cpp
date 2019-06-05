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

#include "SamplingRateWidget.h"

#include <QtCore/QJsonObject>

#include "Modules/PatternDisplayModule/PatternDisplay_UI.h"

#include "Common/Constants.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SamplingRateWidget::SamplingRateWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: AbstractAngleWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SamplingRateWidget::~SamplingRateWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::setupGui()
{
  QDoubleValidator* dblValidator = new QDoubleValidator(phi1StartLineEdit);
  phi1StartLineEdit->setValidator(dblValidator);
  connect(phi1StartLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi1MStepLineEdit);
  phi1MStepLineEdit->setValidator(dblValidator);
  connect(phi1MStepLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi1EndLineEdit);
  phi1EndLineEdit->setValidator(dblValidator);
  connect(phi1EndLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phiStartLineEdit);
  phiStartLineEdit->setValidator(dblValidator);
  connect(phiStartLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phiMStepLineEdit);
  phiMStepLineEdit->setValidator(dblValidator);
  connect(phiMStepLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phiEndLineEdit);
  phiEndLineEdit->setValidator(dblValidator);
  connect(phiEndLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi2StartLineEdit);
  phi2StartLineEdit->setValidator(dblValidator);
  connect(phi2StartLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi2MStepLineEdit);
  phi2MStepLineEdit->setValidator(dblValidator);
  connect(phi2MStepLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi2EndLineEdit);
  phi2EndLineEdit->setValidator(dblValidator);
  connect(phi2EndLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi1AngleLineEdit);
  phi1AngleLineEdit->setValidator(dblValidator);
  connect(phi1AngleLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phiAngleLineEdit);
  phiAngleLineEdit->setValidator(dblValidator);
  connect(phiAngleLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  dblValidator = new QDoubleValidator(phi2AngleLineEdit);
  phi2AngleLineEdit->setValidator(dblValidator);
  connect(phi2AngleLineEdit, SIGNAL(textChanged(QString)), this, SLOT(lineEditChanged(QString)));

  connect(phi1CB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));
  connect(phiCB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));
  connect(phi2CB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::lineEditChanged(const QString& text) const
{
  Q_UNUSED(text)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::checkBoxChanged(int index) const
{
  Q_UNUSED(index)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::valuesChanged() const
{
  std::vector<float> eulerAngles = getEulerAngles();

  if(eulerAngles.empty())
  {
    numOfAnglesLineEdit->setText("0");
  }
  else
  {
    numOfAnglesLineEdit->setText(QString::number(eulerAngles.size() / 3));
  }

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SamplingRateWidget::hasValidAngles() const
{
  return (numOfAnglesLineEdit->text().toInt() > 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::readSession(QJsonObject& obj)
{
  QJsonObject phi1Obj = obj[EMsoftWorkbenchConstants::IOStrings::Phi1].toObject();
  phi1StartLineEdit->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::Start].toDouble()));
  phi1EndLineEdit->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::End].toDouble()));
  phi1MStepLineEdit->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::Step].toDouble()));
  phi1AngleLineEdit->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::Angle].toDouble()));
  phi1CB->setChecked(phi1Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());

  QJsonObject phiObj = obj[EMsoftWorkbenchConstants::IOStrings::Phi].toObject();
  phiStartLineEdit->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::Start].toDouble()));
  phiEndLineEdit->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::End].toDouble()));
  phiMStepLineEdit->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::Step].toDouble()));
  phiAngleLineEdit->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::Angle].toDouble()));
  phiCB->setChecked(phiObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());

  QJsonObject phi2Obj = obj[EMsoftWorkbenchConstants::IOStrings::Phi2].toObject();
  phi2StartLineEdit->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::Start].toDouble()));
  phi2EndLineEdit->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::End].toDouble()));
  phi2MStepLineEdit->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::Step].toDouble()));
  phi2AngleLineEdit->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::Angle].toDouble()));
  phi2CB->setChecked(phi2Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());

  degreesRB->setChecked(obj[EMsoftWorkbenchConstants::IOStrings::DegreesChecked].toBool());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::writeSession(QJsonObject& obj) const
{
  QJsonObject phi1Obj;
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::Start] = phi1StartLineEdit->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::End] = phi1EndLineEdit->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::Step] = phi1MStepLineEdit->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::Angle] = phi1AngleLineEdit->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phi1CB->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi1] = phi1Obj;

  QJsonObject phiObj;
  phiObj[EMsoftWorkbenchConstants::IOStrings::Start] = phiStartLineEdit->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::End] = phiEndLineEdit->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::Step] = phiMStepLineEdit->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::Angle] = phiAngleLineEdit->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phiCB->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi] = phiObj;

  QJsonObject phi2Obj;
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::Start] = phi2StartLineEdit->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::End] = phi2EndLineEdit->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::Step] = phi2MStepLineEdit->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::Angle] = phi2AngleLineEdit->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phi2CB->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi2] = phi2Obj;

  obj[EMsoftWorkbenchConstants::IOStrings::DegreesChecked] = degreesRB->isChecked();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::createModificationConnections(PatternDisplay_UI* ui) const
{
  // Line Edits
  connect(phi1StartLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi1EndLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi1MStepLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi1AngleLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phiStartLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phiEndLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phiMStepLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phiAngleLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi2StartLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi2EndLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi2MStepLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(phi2AngleLineEdit, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });

  // Checkboxes
  connect(phi1CB, &QCheckBox::stateChanged, [=] { emit ui->moduleParametersChanged(); });
  connect(phiCB, &QCheckBox::stateChanged, [=] { emit ui->moduleParametersChanged(); });
  connect(phi2CB, &QCheckBox::stateChanged, [=] { emit ui->moduleParametersChanged(); });

  // Radio Buttons
  connect(degreesRB, static_cast<void (QRadioButton::*)(void)>(&QRadioButton::pressed), [=] { emit ui->moduleParametersChanged(); });
  connect(radiansRB, static_cast<void (QRadioButton::*)(void)>(&QRadioButton::pressed), [=] { emit ui->moduleParametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> SamplingRateWidget::getEulerAngles() const
{
  double phi1Start = phi1StartLineEdit->text().toDouble();
  double phi1MStep = phi1MStepLineEdit->text().toDouble();
  double phi1End = phi1EndLineEdit->text().toDouble();
  double phiStart = phiStartLineEdit->text().toDouble();
  double phiMStep = phiMStepLineEdit->text().toDouble();
  double phiEnd = phiEndLineEdit->text().toDouble();
  double phi2Start = phi2StartLineEdit->text().toDouble();
  double phi2MStep = phi2MStepLineEdit->text().toDouble();
  double phi2End = phi2EndLineEdit->text().toDouble();

  if(!phi1CB->isChecked())
  {
    phi1Start = phi1AngleLineEdit->text().toDouble();
    phi1End = phi1AngleLineEdit->text().toDouble();
    phi1MStep = 1;
  }
  if(!phiCB->isChecked())
  {
    phiStart = phiAngleLineEdit->text().toDouble();
    phiEnd = phiAngleLineEdit->text().toDouble();
    phiMStep = 1;
  }
  if(!phi2CB->isChecked())
  {
    phi2Start = phi2AngleLineEdit->text().toDouble();
    phi2End = phi2AngleLineEdit->text().toDouble();
    phi2MStep = 1;
  }

  if(phi1MStep == 0 || phiMStep == 0 || phi2MStep == 0)
  {
    return std::vector<float>();
  }

  int phi1Dim = ((phi1End - phi1Start) / phi1MStep) + 1;
  int phiDim = ((phiEnd - phiStart) / phiMStep) + 1;
  int phi2Dim = ((phi2End - phi2Start) / phi2MStep) + 1;

  std::vector<float> floatArray(phi1Dim * phiDim * phi2Dim * 3);

  int index = 0;
  for(int i = phi1Start; i <= phi1End; i = i + phi1MStep)
  {
    for(int j = phiStart; j <= phiEnd; j = j + phiMStep)
    {
      for(int k = phi2Start; k <= phi2End; k = k + phi2MStep)
      {
        float val1 = static_cast<float>(i);
        float val2 = static_cast<float>(j);
        float val3 = static_cast<float>(k);

        // Convert to radians, if necessary
        if(degreesRB->isChecked())
        {
          val1 = val1 * k_PiOver180;
          val2 = val2 * k_PiOver180;
          val3 = val3 * k_PiOver180;
        }

        floatArray.at(index * 3 + 0) = val1;
        floatArray.at(index * 3 + 1) = val2;
        floatArray.at(index * 3 + 2) = val3;
        index++;
      }
    }
  }

  return floatArray;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phi1CB_stateChanged(int state) const
{
  bool enabled = static_cast<bool>(state);
  phi1StartLineEdit->setEnabled(enabled);
  phi1MStepLineEdit->setEnabled(enabled);
  phi1EndLineEdit->setEnabled(enabled);
  phi1AngleLineEdit->setDisabled(enabled);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phiCB_stateChanged(int state) const
{
  bool enabled = static_cast<bool>(state);
  phiStartLineEdit->setEnabled(enabled);
  phiMStepLineEdit->setEnabled(enabled);
  phiEndLineEdit->setEnabled(enabled);
  phiAngleLineEdit->setDisabled(enabled);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phi2CB_stateChanged(int state) const
{
  bool enabled = static_cast<bool>(state);
  phi2StartLineEdit->setEnabled(enabled);
  phi2MStepLineEdit->setEnabled(enabled);
  phi2EndLineEdit->setEnabled(enabled);
  phi2AngleLineEdit->setDisabled(enabled);
}
