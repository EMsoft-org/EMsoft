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

#include "SingleAngleWidget.h"

#include <QtCore/QJsonObject>

#include "Modules/PatternDisplayModule/PatternDisplay_UI.h"

#include "Common/Constants.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SingleAngleWidget::SingleAngleWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: AbstractAngleWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SingleAngleWidget::~SingleAngleWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SingleAngleWidget::setupGui()
{
  QDoubleValidator* dblValidator = new QDoubleValidator(eulerPhi1);
  eulerPhi1->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(eulerPhi);
  eulerPhi->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(eulerPhi2);
  eulerPhi2->setValidator(dblValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SingleAngleWidget::hasValidAngles()
{
  // This can never be false, because even empty angle line edits will be counted as 0's.
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SingleAngleWidget::readSession(QJsonObject& obj)
{
  QJsonObject eulerObj = obj[EMsoftWorkbenchConstants::IOStrings::EulerAngle].toObject();
  eulerPhi1->setText(QString::number(eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi1].toDouble()));
  eulerPhi->setText(QString::number(eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi].toDouble()));
  eulerPhi2->setText(QString::number(eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi2].toDouble()));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SingleAngleWidget::writeSession(QJsonObject& obj)
{
  QJsonObject eulerObj;
  eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi1] = eulerPhi1->text().toDouble();
  eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi] = eulerPhi->text().toDouble();
  eulerObj[EMsoftWorkbenchConstants::IOStrings::Phi2] = eulerPhi2->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::EulerAngle] = eulerObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SingleAngleWidget::createModificationConnections(PatternDisplay_UI* ui)
{
  // Line Edits
  connect(eulerPhi1, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(eulerPhi, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
  connect(eulerPhi2, &QLineEdit::textEdited, [=] { emit ui->moduleParametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer SingleAngleWidget::getEulerAngles()
{
  FloatArrayType::Pointer floatArray = FloatArrayType::CreateArray(1, QVector<size_t>(1, 3), "Euler Angles");
  floatArray->setComponent(0, 0, eulerPhi1->text().toDouble() * k_PiOver180); // Convert to radians
  floatArray->setComponent(0, 1, eulerPhi->text().toDouble() * k_PiOver180);
  floatArray->setComponent(0, 2, eulerPhi2->text().toDouble() * k_PiOver180);

  return floatArray;
}
