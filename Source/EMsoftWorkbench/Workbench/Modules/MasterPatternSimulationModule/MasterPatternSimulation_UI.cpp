/* ============================================================================
 * Copyright (c) 2009-2017 BlueQuartz Software, LLC
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

#include "MasterPatternSimulation_UI.h"
#if defined(_MSC_VER)
#define _MATH_DEFINES_DEFINED
#endif
#include <initializer_list>

#include <QtConcurrent>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>
#include <QtCore/QThreadPool>
#include <QtWidgets/QFileDialog>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "EMsoftApplication.h"

#include "Common/Constants.h"
#include "Common/FileIOTools.h"
#include "Common/PatternTools.h"

#include "QtSupport/QtSSettings.h"

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulation_UI::MasterPatternSimulation_UI(QWidget* parent)
: IModuleUI(parent)
{
  setupUi(this);

  m_Controller = new MasterPatternSimulationController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulation_UI::~MasterPatternSimulation_UI() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::setupGui()
{
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  validateData();

  int numOfCores = m_Controller->getNumCPUCores();
  numOfOpenMPThreadsSB->setMaximum(numOfCores);
  numOfOpenMPThreadsSB->setValue(numOfCores);
  numOfOpenMPThreadsSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createValidators()
{
  //  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
  //  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createModificationConnections()
{
  // Spin Boxes
  connect(smallestDSpacingSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(numOfMPPixelsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersXSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersYSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersZSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(numOfOpenMPThreadsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });

  // Line Edits
  connect(mpFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(mcFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createWidgetConnections() const
{
  connect(simulateBtn, &QPushButton::clicked, this, &MasterPatternSimulation_UI::slot_simulateBtn_clicked);

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller, &MasterPatternSimulationController::errorMessageGenerated, this, &MasterPatternSimulation_UI::notifyErrorMessage);
  connect(m_Controller, &MasterPatternSimulationController::warningMessageGenerated, this, &MasterPatternSimulation_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  connect(mcSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.h5";
    if(!mcFilePathLE->text().isEmpty())
    {
      proposedFile = mcFilePathLE->text();
    }

    QString filePath = FileIOTools::GetOpenPathFromDialog("Select Monte Carlo File", "Monte Carlo File (*.h5);;All Files (*.*)", proposedFile);
    if(filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    mcFilePathLE->setText(filePath);
  });

  connect(mpSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.h5";
    if(!mpFilePathLE->text().isEmpty())
    {
      proposedFile = mpFilePathLE->text();
    }

    QString filePath = FileIOTools::GetSavePathFromDialog("Select Master Pattern File", "Master Pattern File (*.h5);;All Files (*.*)", proposedFile);
    if(filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    mpFilePathLE->setText(filePath);
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::slot_simulateBtn_clicked()
{
  if(simulateBtn->text() == "Cancel")
  {
    m_Controller->setCancel(true);
    setRunning(false);
    return;
  }

  setRunning(true);
  clearModuleIssues();

  MasterPatternSimulationController::MasterPatternSimulationData data = getSimulationData();

  simulateBtn->setText("Cancel");
  inputGrpBox->setDisabled(true);
  compParamGrpBox->setDisabled(true);
  outputGrpBox->setDisabled(true);

  // Single-threaded for now, but we can multi-thread later if needed
  //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
  for(int i = 0; i < 1; i++)
  {
    m_Watcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
    connect(m_Watcher.data(), SIGNAL(finished()), this, SLOT(threadFinished()));

    QFuture<void> future = QtConcurrent::run(m_Controller, &MasterPatternSimulationController::createMasterPattern, data);
    m_Watcher->setFuture(future);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::threadFinished()
{
  m_Controller->setCancel(false);

  simulateBtn->setText("Simulate");
  inputGrpBox->setEnabled(true);
  compParamGrpBox->setEnabled(true);
  outputGrpBox->setEnabled(true);

  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::validateData()
{
  clearModuleIssues();

  MasterPatternSimulationController::MasterPatternSimulationData data = getSimulationData();
  if(m_Controller->validateMasterPatternValues(data))
  {
    simulateBtn->setEnabled(true);
  }
  else
  {
    simulateBtn->setDisabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::readModuleSession(QJsonObject& obj)
{
  readComputationalParameters(obj);

  mcFilePathLE->setText(obj[ioConstants::InputMonteCarloFileName].toString());
  mpFilePathLE->setText(obj[ioConstants::OutputFileName].toString());

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::readComputationalParameters(QJsonObject& obj)
{
  QJsonObject compParamObj = obj[ioConstants::CompParam].toObject();

  if(!compParamObj.isEmpty())
  {
    smallestDSpacingSB->blockSignals(true);
    numOfMPPixelsSB->blockSignals(true);
    betheParametersXSB->blockSignals(true);
    betheParametersYSB->blockSignals(true);
    betheParametersZSB->blockSignals(true);
    numOfOpenMPThreadsSB->blockSignals(true);

    smallestDSpacingSB->setValue(compParamObj[ioConstants::SmallestDSpacing].toDouble());
    numOfMPPixelsSB->setValue(compParamObj[ioConstants::NumOfMasterPatternPxls].toInt());

    QJsonObject betheParamObj = compParamObj[ioConstants::BetheParameters].toObject();
    betheParametersXSB->setValue(betheParamObj[ioConstants::Bethe_X].toInt());
    betheParametersYSB->setValue(betheParamObj[ioConstants::Bethe_Y].toInt());
    betheParametersZSB->setValue(betheParamObj[ioConstants::Bethe_Z].toInt());

    numOfOpenMPThreadsSB->setValue(compParamObj[ioConstants::NumOfOpenMPThreads].toInt());

    smallestDSpacingSB->blockSignals(false);
    numOfMPPixelsSB->blockSignals(false);
    betheParametersXSB->blockSignals(false);
    betheParametersYSB->blockSignals(false);
    betheParametersZSB->blockSignals(false);
    numOfOpenMPThreadsSB->blockSignals(false);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::writeModuleSession(QJsonObject& obj) const
{
  QJsonObject compParamObj;

  writeComputationalParameters(compParamObj);

  obj[ioConstants::CompParam] = compParamObj;
  obj[ioConstants::OutputFileName] = mpFilePathLE->text();
  obj[ioConstants::InputMonteCarloFileName] = mcFilePathLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::writeComputationalParameters(QJsonObject& obj) const
{
  obj[ioConstants::SmallestDSpacing] = smallestDSpacingSB->value();
  obj[ioConstants::NumOfMasterPatternPxls] = numOfMPPixelsSB->value();

  QJsonObject betheParamObj;
  betheParamObj[ioConstants::Bethe_X] = betheParametersXSB->value();
  betheParamObj[ioConstants::Bethe_Y] = betheParametersYSB->value();
  betheParamObj[ioConstants::Bethe_Z] = betheParametersZSB->value();
  obj[ioConstants::BetheParameters] = betheParamObj;

  obj[ioConstants::NumOfOpenMPThreads] = numOfOpenMPThreadsSB->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationData MasterPatternSimulation_UI::getSimulationData() const
{
  MasterPatternSimulationController::MasterPatternSimulationData data;
  data.smallestDSpacing = smallestDSpacingSB->value();
  data.numOfMPPixels = numOfMPPixelsSB->value();
  data.betheParametersX = betheParametersXSB->value();
  data.betheParametersY = betheParametersYSB->value();
  data.betheParametersZ = betheParametersZSB->value();
  data.numOfOpenMPThreads = numOfOpenMPThreadsSB->value();
  data.inputFilePath = mcFilePathLE->text();
  data.outputFilePath = mpFilePathLE->text();
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::setController(MasterPatternSimulationController* value)
{
  m_Controller = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController* MasterPatternSimulation_UI::getController() const
{
  return m_Controller;
}

