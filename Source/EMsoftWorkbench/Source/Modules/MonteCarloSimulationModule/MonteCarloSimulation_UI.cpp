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

#include "MonteCarloSimulation_UI.h"

#if defined (_MSC_VER)
#define _MATH_DEFINES_DEFINED
#endif

#include <QtCore/QJsonDocument>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QThreadPool>

#include <QtConcurrent>
#include <initializer_list>

#include <QtWidgets/QFileDialog>

#include "EMsoftLib/EMsoftLib.h"
#include "EMsoftLib/EMsoftStringConstants.h"

#include "EMsoftWorkbench/EMsoftApplication.h"

#include "Common/Constants.h"
#include "Common/QtSSettings.h"
#include "Common/PatternTools.h"
#include "Common/FileIOTools.h"

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulation_UI::MonteCarloSimulation_UI(QWidget* parent) :
  IModuleUI(parent)
{
  setupUi(this);

  m_Controller = new MonteCarloSimulationController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulation_UI::~MonteCarloSimulation_UI()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::setupGui()
{  
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  validateData();

  // Show/Hide the correct widgets
  on_mcModeCB_currentIndexChanged(0);

  QStringList choices = m_Controller->getPlatformInfo();
  gpuPlatformCB->insertItems(0, choices);
  if (gpuPlatformCB->count() > 0) 
  {
     gpuPlatformCB->setCurrentIndex(0); 
  }
#if 1
  choices = m_Controller->getDeviceInfo(1); // Force the first Platform. This really only works on macOS
  gpuDeviceCB->insertItems(0, choices);
  if (gpuDeviceCB->count() > 0)
  {
    gpuDeviceCB->setCurrentIndex(0);
  }
  #endif
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::on_gpuPlatformCB_currentIndexChanged(int index)
{
  #if 1
  QStringList choices = m_Controller->getDeviceInfo(index + 1);
  gpuDeviceCB->clear();
  gpuDeviceCB->insertItems(0, choices);
  if (gpuDeviceCB->count() > 0) { gpuDeviceCB->setCurrentIndex(0); }
  #endif
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::createValidators()
{
//  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
//  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::createModificationConnections()
{
  // Spin Boxes
  connect(sampleTiltAngleSigSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(sampleRotAngleOmegaSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(sampleStartTiltAngleSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(sampleEndTiltAngleSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(sampleTiltStepSizeSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(acceleratingVoltageSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(minEnergyConsiderSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(energyBinSizeSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(maxDepthConsiderSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(depthStepSizeSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(numOfPixelsNSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(numOfEPerWorkitemSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(totalNumOfEConsideredSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(multiplierForTotalNumOfESB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged();  });
  connect(gpuPlatformCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged();  });
  connect(gpuDeviceCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged();  });
  connect(globalWorkGroupSizeSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged();  });

  // Combo Boxes
  connect(mcModeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged();  });

  // Line Edits
  connect(csFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(mcFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::createWidgetConnections()
{
  connect(createMonteCarloBtn, &QPushButton::clicked, this, &MonteCarloSimulation_UI::slot_createMonteCarloBtn_clicked);

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller, &MonteCarloSimulationController::errorMessageGenerated, this, &MonteCarloSimulation_UI::notifyErrorMessage);
  connect(m_Controller, &MonteCarloSimulationController::warningMessageGenerated, this, &MonteCarloSimulation_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(const QString &)), this, SLOT(appendToStdOut(const QString &)));

  connect(m_Controller, SIGNAL(updateMCProgress(int, int, float)), this, SLOT(updateMCProgress(int, int, float)));

  connect(csSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.xtal";
    QString filePath = FileIOTools::GetOpenPathFromDialog("Select Input File", "Crystal Structure File (*.xtal);;All Files (*.*)", proposedFile);
    if(true == filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    csFilePathLE->setText(filePath);
  });

  connect(mcSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.h5";
    QString filePath = FileIOTools::GetSavePathFromDialog("Select Output File", "Monte Carlo File (*.h5);;All Files (*.*)", proposedFile);
    if(true == filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    mcFilePathLE->setText(filePath);
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::updateMCProgress(int loopCompleted, int totalLoops, float bseYield)
{
  QString msg = QString("<b>Loop:</b> %1/%2 | <b>BSE Yield:</b> %3%").arg(loopCompleted).arg(totalLoops).arg(bseYield);
  mcProgressLabel->setText(msg);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloSimulation_UI::validateData()
{
  clearModuleIssues();

  MonteCarloSimulationController::MonteCarloSimulationData data = getCreationData();
  if (m_Controller->validateMonteCarloValues(data) == true)
  {
    createMonteCarloBtn->setEnabled(true);
    return true;
  }
  else
  {
    createMonteCarloBtn->setDisabled(true);
    return false;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::slot_createMonteCarloBtn_clicked()
{
  if (createMonteCarloBtn->text() == "Cancel")
  {
    m_Controller->setCancel(true);
    setRunning(false);
    return;
  }
  else
  {
    setRunning(true);
    clearModuleIssues();

    MonteCarloSimulationController::MonteCarloSimulationData data = getCreationData();

    createMonteCarloBtn->setText("Cancel");
    inputGrpBox->setDisabled(true);
    monteCarloGrpBox->setDisabled(true);
    gpuGrpBox->setDisabled(true);
    outputGrpBox->setDisabled(true);

    // Single-threaded for now, but we can multi-thread later if needed
    //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
    for (int i = 0; i < 1; i++)
    {
      m_Watcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
      connect(m_Watcher.data(), SIGNAL(finished()), this, SLOT(threadFinished()));

      QFuture<void> future = QtConcurrent::run(m_Controller, &MonteCarloSimulationController::createMonteCarlo, data);
      m_Watcher->setFuture(future);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::threadFinished()
{
  m_Controller->setCancel(false);

  createMonteCarloBtn->setText("Simulate");
  inputGrpBox->setEnabled(true);
  monteCarloGrpBox->setEnabled(true);
  gpuGrpBox->setEnabled(true);
  outputGrpBox->setEnabled(true);

  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::changeEvent(QEvent* event)
{
  if (event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::readModuleSession(QJsonObject &obj)
{
  readMonteCarloParameters(obj);
  readGPUParameters(obj);

  csFilePathLE->setText(obj[ioConstants::InputCrystalFileName].toString());
  mcFilePathLE->setText(obj[ioConstants::OutputFileName].toString());

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::readMonteCarloParameters(QJsonObject &obj)
{
  QJsonObject monteCarloObj = obj[ioConstants::MonteCarlo].toObject();

  if (monteCarloObj.isEmpty() == false)
  {
    sampleTiltAngleSigSB->blockSignals(true);
    sampleRotAngleOmegaSB->blockSignals(true);
    sampleStartTiltAngleSB->blockSignals(true);
    sampleEndTiltAngleSB->blockSignals(true);
    sampleTiltStepSizeSB->blockSignals(true);
    acceleratingVoltageSB->blockSignals(true);
    minEnergyConsiderSB->blockSignals(true);
    energyBinSizeSB->blockSignals(true);
    maxDepthConsiderSB->blockSignals(true);
    depthStepSizeSB->blockSignals(true);
    numOfPixelsNSB->blockSignals(true);

    mcModeCB->setCurrentIndex(monteCarloObj[ioConstants::MonteCarloMode].toInt());
    sampleTiltAngleSigSB->setValue(monteCarloObj[ioConstants::SampleTiltAngleSigma].toDouble());
    sampleRotAngleOmegaSB->setValue(monteCarloObj[ioConstants::SampleRotationAngleOmega].toDouble());
    sampleStartTiltAngleSB->setValue(monteCarloObj[ioConstants::SampleStartTiltAngle].toDouble());
    sampleEndTiltAngleSB->setValue(monteCarloObj[ioConstants::SampleEndTiltAngle].toDouble());
    sampleTiltStepSizeSB->setValue(monteCarloObj[ioConstants::SampleTiltStepSize].toDouble());
    acceleratingVoltageSB->setValue(monteCarloObj[ioConstants::AcceleratingVoltage].toDouble());
    minEnergyConsiderSB->setValue(monteCarloObj[ioConstants::MinEnergyToConsider].toDouble());
    energyBinSizeSB->setValue(monteCarloObj[ioConstants::EnergyBinSize].toDouble());
    maxDepthConsiderSB->setValue(monteCarloObj[ioConstants::MaxDepthToConsider].toDouble());
    depthStepSizeSB->setValue(monteCarloObj[ioConstants::DepthStepSize].toDouble());
    numOfPixelsNSB->setValue(monteCarloObj[ioConstants::NumberOfXPixelsN].toInt());

    sampleTiltAngleSigSB->blockSignals(false);
    sampleRotAngleOmegaSB->blockSignals(false);
    sampleStartTiltAngleSB->blockSignals(false);
    sampleEndTiltAngleSB->blockSignals(false);
    sampleTiltStepSizeSB->blockSignals(false);
    acceleratingVoltageSB->blockSignals(false);
    minEnergyConsiderSB->blockSignals(false);
    energyBinSizeSB->blockSignals(false);
    maxDepthConsiderSB->blockSignals(false);
    depthStepSizeSB->blockSignals(false);
    numOfPixelsNSB->blockSignals(false);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::readGPUParameters(QJsonObject &obj)
{
  QJsonObject gpuObj = obj[ioConstants::GPU].toObject();

  if (gpuObj.isEmpty() == false)
  {
    numOfEPerWorkitemSB->blockSignals(true);
    totalNumOfEConsideredSB->blockSignals(true);
    multiplierForTotalNumOfESB->blockSignals(true);
    gpuPlatformCB->blockSignals(true);
    gpuDeviceCB->blockSignals(true);
    globalWorkGroupSizeSB->blockSignals(true);

    numOfEPerWorkitemSB->setValue(gpuObj[ioConstants::NumberOfElectronsPerWorkitem].toInt());
    totalNumOfEConsideredSB->setValue(gpuObj[ioConstants::TotalNumOfElectronsToBeConsidered].toInt());
    multiplierForTotalNumOfESB->setValue(gpuObj[ioConstants::MultiplierForTotalNumberOfElectrons].toInt());
    gpuPlatformCB->setCurrentIndex(gpuObj[ioConstants::GPUPlatformID].toInt());
    gpuDeviceCB->setCurrentIndex(gpuObj[ioConstants::GPUDeviceID].toInt());
    globalWorkGroupSizeSB->setValue(gpuObj[ioConstants::GlobalWorkGroupSize].toInt());

    numOfEPerWorkitemSB->blockSignals(false);
    totalNumOfEConsideredSB->blockSignals(false);
    multiplierForTotalNumOfESB->blockSignals(false);
    gpuPlatformCB->blockSignals(false);
    gpuDeviceCB->blockSignals(false);
    globalWorkGroupSizeSB->blockSignals(false);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::writeModuleSession(QJsonObject &obj)
{ 
  QJsonObject monteCarloObj;
  QJsonObject gpuObj;

  writeMonteCarloParameters(monteCarloObj);
  writeGPUParameters(gpuObj);

  obj[ioConstants::MonteCarlo] = monteCarloObj;
  obj[ioConstants::GPU] = gpuObj;
  obj[ioConstants::OutputFileName] = mcFilePathLE->text();
  obj[ioConstants::InputCrystalFileName] = csFilePathLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::writeMonteCarloParameters(QJsonObject &obj)
{
  obj[ioConstants::MonteCarloMode] = mcModeCB->currentIndex();
  obj[ioConstants::SampleTiltAngleSigma] = sampleTiltAngleSigSB->value();
  obj[ioConstants::SampleRotationAngleOmega] = sampleRotAngleOmegaSB->value();
  obj[ioConstants::SampleStartTiltAngle] = sampleStartTiltAngleSB->value();
  obj[ioConstants::SampleEndTiltAngle] = sampleEndTiltAngleSB->value();
  obj[ioConstants::SampleTiltStepSize] = sampleTiltStepSizeSB->value();
  obj[ioConstants::AcceleratingVoltage] = acceleratingVoltageSB->value();
  obj[ioConstants::MinEnergyToConsider] = minEnergyConsiderSB->value();
  obj[ioConstants::EnergyBinSize] = energyBinSizeSB->value();
  obj[ioConstants::MaxDepthToConsider] = maxDepthConsiderSB->value();
  obj[ioConstants::DepthStepSize] = depthStepSizeSB->value();
  obj[ioConstants::NumberOfXPixelsN] = numOfPixelsNSB->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::writeGPUParameters(QJsonObject &obj)
{
  obj[ioConstants::NumberOfElectronsPerWorkitem] = numOfEPerWorkitemSB->value();
  obj[ioConstants::TotalNumOfElectronsToBeConsidered] = totalNumOfEConsideredSB->value();
  obj[ioConstants::MultiplierForTotalNumberOfElectrons] = multiplierForTotalNumOfESB->value();
  obj[ioConstants::GPUPlatformID] = gpuPlatformCB->currentIndex();
  obj[ioConstants::GPUDeviceID] = gpuDeviceCB->currentIndex();
  obj[ioConstants::GlobalWorkGroupSize] = globalWorkGroupSizeSB->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulation_UI::on_mcModeCB_currentIndexChanged(int index)
{
  sampleTiltAngleSigLabel->hide();
  sampleTiltAngleSigSB->hide();
  sampleRotAngleOmegaLabel->hide();
  sampleRotAngleOmegaSB->hide();
  sampleStartTiltAngleLabel->hide();
  sampleStartTiltAngleSB->hide();
  sampleEndTiltAngleLabel->hide();
  sampleEndTiltAngleSB->hide();
  sampleTiltStepSizeLabel->hide();
  sampleTiltStepSizeSB->hide();

  MonteCarloSimulationController::MonteCarloMode mcMode = static_cast<MonteCarloSimulationController::MonteCarloMode>(index);

  if (mcMode == MonteCarloSimulationController::MonteCarloMode::EBSD)
  {
    sampleTiltAngleSigLabel->show();
    sampleTiltAngleSigSB->show();
    sampleRotAngleOmegaLabel->show();
    sampleRotAngleOmegaSB->show();
  }
  else if (mcMode == MonteCarloSimulationController::MonteCarloMode::ECP)
  {
    sampleStartTiltAngleLabel->show();
    sampleStartTiltAngleSB->show();
    sampleEndTiltAngleLabel->show();
    sampleEndTiltAngleSB->show();
    sampleTiltStepSizeLabel->show();
    sampleTiltStepSizeSB->show();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulationController::MonteCarloSimulationData MonteCarloSimulation_UI::getCreationData()
{
  MonteCarloSimulationController::MonteCarloSimulationData data;
  data.mcMode = mcModeCB->currentIndex() + 1;
  data.sampleTiltAngleSig = sampleTiltAngleSigSB->value();
  data.sampleRotAngleOmega = sampleRotAngleOmegaSB->value();
  data.sampleStartTiltAngle = sampleStartTiltAngleSB->value();
  data.sampleEndTiltAngle = sampleEndTiltAngleSB->value();
  data.sampleTiltStepSize = sampleTiltStepSizeSB->value();
  data.acceleratingVoltage = acceleratingVoltageSB->value();
  data.minEnergyConsider = minEnergyConsiderSB->value();
  data.energyBinSize = energyBinSizeSB->value();
  data.maxDepthConsider = maxDepthConsiderSB->value();
  data.depthStepSize = depthStepSizeSB->value();
  data.numOfPixelsN = numOfPixelsNSB->value();
  data.numOfEPerWorkitem = numOfEPerWorkitemSB->value();
  data.totalNumOfEConsidered = totalNumOfEConsideredSB->value();
  data.multiplierForTotalNumOfE = multiplierForTotalNumOfESB->value();
  data.gpuPlatformID = gpuPlatformCB->currentIndex();
  data.gpuDeviceID = gpuDeviceCB->currentIndex();
  data.globalWorkGroupSize = globalWorkGroupSizeSB->value();
  data.inputFilePath = csFilePathLE->text();
  data.outputFilePath = mcFilePathLE->text();
  return data;
}

