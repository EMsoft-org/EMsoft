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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SamplingRateWidget::SamplingRateWidget(QWidget *parent, Qt::WindowFlags windowFlags) :
  AbstractAngleWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SamplingRateWidget::~SamplingRateWidget()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::setupGui()
{
  QDoubleValidator* dblValidator = new QDoubleValidator(phi1StartLineEdit);
  phi1StartLineEdit->setValidator(dblValidator);
  connect(phi1StartLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi1StepLineEdit);
  phi1StepLineEdit->setValidator(dblValidator);
  connect(phi1StepLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi1EndLineEdit);
  phi1EndLineEdit->setValidator(dblValidator);
  connect(phi1EndLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phiStartLineEdit);
  phiStartLineEdit->setValidator(dblValidator);
  connect(phiStartLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phiStepLineEdit);
  phiStepLineEdit->setValidator(dblValidator);
  connect(phiStepLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phiEndLineEdit);
  phiEndLineEdit->setValidator(dblValidator);
  connect(phiEndLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi2StartLineEdit);
  phi2StartLineEdit->setValidator(dblValidator);
  connect(phi2StartLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi2StepLineEdit);
  phi2StepLineEdit->setValidator(dblValidator);
  connect(phi2StepLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi2EndLineEdit);
  phi2EndLineEdit->setValidator(dblValidator);
  connect(phi2EndLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi1AngleLineEdit);
  phi1AngleLineEdit->setValidator(dblValidator);
  connect(phi1AngleLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phiAngleLineEdit);
  phiAngleLineEdit->setValidator(dblValidator);
  connect(phiAngleLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  dblValidator = new QDoubleValidator(phi2AngleLineEdit);
  phi2AngleLineEdit->setValidator(dblValidator);
  connect(phi2AngleLineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(lineEditChanged(const QString &)));

  connect(phi1CB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));
  connect(phiCB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));
  connect(phi2CB, SIGNAL(stateChanged(int)), this, SLOT(checkBoxChanged(int)));

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::lineEditChanged(const QString &text)
{
  Q_UNUSED(text)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::checkBoxChanged(int index)
{
  Q_UNUSED(index)

  valuesChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::valuesChanged()
{
  FloatArrayType::Pointer eulerAngles = getEulerAngles();

  if (eulerAngles == FloatArrayType::NullPointer())
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
bool SamplingRateWidget::hasValidAngles()
{
  return (numOfAnglesLineEdit->text().toInt() > 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer SamplingRateWidget::getEulerAngles()
{
  double phi1Start = phi1StartLineEdit->text().toDouble();
  double phi1Step = phi1StepLineEdit->text().toDouble();
  double phi1End = phi1EndLineEdit->text().toDouble();
  double phiStart = phiStartLineEdit->text().toDouble();
  double phiStep = phiStepLineEdit->text().toDouble();
  double phiEnd = phiEndLineEdit->text().toDouble();
  double phi2Start = phi2StartLineEdit->text().toDouble();
  double phi2Step = phi2StepLineEdit->text().toDouble();
  double phi2End = phi2EndLineEdit->text().toDouble();

  if (phi1CB->isChecked() == false)
  {
    phi1Start = phi1AngleLineEdit->text().toDouble();
    phi1End = phi1AngleLineEdit->text().toDouble();
    phi1Step = 1;
  }
  if (phiCB->isChecked() == false)
  {
    phiStart = phiAngleLineEdit->text().toDouble();
    phiEnd = phiAngleLineEdit->text().toDouble();
    phiStep = 1;
  }
  if (phi2CB->isChecked() == false)
  {
    phi2Start = phi2AngleLineEdit->text().toDouble();
    phi2End = phi2AngleLineEdit->text().toDouble();
    phi2Step = 1;
  }

  if (phi1Step == 0 || phiStep == 0 || phi2Step == 0)
  {
    return FloatArrayType::NullPointer();
  }

  int phi1Dim = ((phi1End - phi1Start) / phi1Step) + 1;
  int phiDim = ((phiEnd - phiStart) / phiStep) + 1;
  int phi2Dim = ((phi2End - phi2Start) / phi2Step) + 1;

  FloatArrayType::Pointer floatArray = FloatArrayType::CreateArray(phi1Dim*phiDim*phi2Dim, QVector<size_t>(1, 3), "Euler Angles");

  int index = 0;
  for (int i = phi1Start; i <= phi1End; i = i + phi1Step)
  {
    for (int j = phiStart; j <= phiEnd; j = j + phiStep)
    {
      for (int k = phi2Start; k <= phi2End; k = k + phi2Step)
      {
        float val1 = static_cast<float>(i);
        float val2 = static_cast<float>(j);
        float val3 = static_cast<float>(k);

        // Convert to radians, if necessary
        if (degreesRB->isChecked())
        {
          val1 = val1 * k_PiOver180;
          val2 = val2 * k_PiOver180;
          val3 = val3 * k_PiOver180;
        }

        floatArray->setComponent(index, 0, val1);
        floatArray->setComponent(index, 1, val2);
        floatArray->setComponent(index, 2, val3);
        index++;
      }
    }
  }

  return floatArray;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phi1CB_stateChanged(int state)
{
  bool enabled = static_cast<bool>(state);
  phi1StartLineEdit->setEnabled(enabled);
  phi1StepLineEdit->setEnabled(enabled);
  phi1EndLineEdit->setEnabled(enabled);
  phi1AngleLineEdit->setDisabled(enabled);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phiCB_stateChanged(int state)
{
  bool enabled = static_cast<bool>(state);
  phiStartLineEdit->setEnabled(enabled);
  phiStepLineEdit->setEnabled(enabled);
  phiEndLineEdit->setEnabled(enabled);
  phiAngleLineEdit->setDisabled(enabled);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SamplingRateWidget::on_phi2CB_stateChanged(int state)
{
  bool enabled = static_cast<bool>(state);
  phi2StartLineEdit->setEnabled(enabled);
  phi2StepLineEdit->setEnabled(enabled);
  phi2EndLineEdit->setEnabled(enabled);
  phi2AngleLineEdit->setDisabled(enabled);
}




