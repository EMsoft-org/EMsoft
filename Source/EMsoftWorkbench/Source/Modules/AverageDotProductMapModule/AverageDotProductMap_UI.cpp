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

#include "AverageDotProductMap_UI.h"
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

#include "EMsoftWorkbench/EMsoftApplication.h"

#include "Common/FileIOTools.h"
#include "Common/PatternTools.h"

#include "QtSupport/QtSSettings.h"

#include "H5Support/QH5Utilities.h"

#include "Modules/AverageDotProductMapModule/Constants.h"

namespace ioConstants = AverageDotProductMapModuleConstants::IOStrings;

using InputType = AverageDotProductMapController::ADPMapData::InputType;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AverageDotProductMap_UI::AverageDotProductMap_UI(QWidget* parent)
: IModuleUI(parent)
, m_Ui(new Ui::AverageDotProductMap_UI())
{
  m_Ui->setupUi(this);

  m_Controller = new AverageDotProductMapController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AverageDotProductMap_UI::~AverageDotProductMap_UI() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::setupGui()
{
  m_Ui->hdf5SelectionWidget->setInputFileLabelText("Pattern Data File");
  m_Ui->hdf5SelectionWidget->setOneSelectionOnly(true);

  // Add limits to all spinboxes
  initializeSpinBoxLimits();

  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  validateData();

  // Run this once so that the HDF5 widget can be either disabled or enabled
  listenInputTypeChanged(m_Ui->inputTypeCB->currentIndex());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::initializeSpinBoxLimits()
{
  m_Ui->ipfHeightSB->setMinimum(1);
  m_Ui->ipfHeightSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->ipfWidthSB->setMinimum(1);
  m_Ui->ipfWidthSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->patternHeightSB->setMinimum(1);
  m_Ui->patternHeightSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->patternWidthSB->setMinimum(1);
  m_Ui->patternWidthSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->roiSB_1->setMinimum(0);
  m_Ui->roiSB_1->setMaximum(std::numeric_limits<int>::max());

  m_Ui->roiSB_2->setMinimum(0);
  m_Ui->roiSB_2->setMaximum(std::numeric_limits<int>::max());

  m_Ui->roiSB_3->setMinimum(0);
  m_Ui->roiSB_3->setMaximum(std::numeric_limits<int>::max());

  m_Ui->roiSB_4->setMinimum(0);
  m_Ui->roiSB_4->setMaximum(std::numeric_limits<int>::max());

  m_Ui->binningFactorSB->setMinimum(1);
  m_Ui->binningFactorSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->binningXSB->setMinimum(1);
  m_Ui->binningXSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->binningYSB->setMinimum(1);
  m_Ui->binningYSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->maskPatternSB->setMinimum(0);
  m_Ui->maskPatternSB->setMaximum(std::numeric_limits<int>::max());

  m_Ui->maskRadiusSB->setMinimum(0.0f);
  m_Ui->maskRadiusSB->setMaximum(std::numeric_limits<float>::max());

  m_Ui->hipassSB->setMinimum(0.0f);
  m_Ui->hipassSB->setMaximum(std::numeric_limits<float>::max());

  m_Ui->numOfRegionsSB->setMinimum(0);
  m_Ui->numOfRegionsSB->setMaximum(std::numeric_limits<int>::max());

  int numOfCores = m_Controller->getNumCPUCores();
  m_Ui->numOfThreadsSB->setMinimum(1);
  m_Ui->numOfThreadsSB->setMaximum(numOfCores);
  m_Ui->numOfThreadsSB->setValue(numOfCores);
  m_Ui->numOfThreadsSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::createValidators()
{
  //  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
  //  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::createModificationConnections()
{
  // Spin Boxes
  connect(m_Ui->patternHeightSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->patternWidthSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->roiSB_1, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->roiSB_2, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->roiSB_3, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->roiSB_4, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->binningFactorSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->binningXSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->binningYSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->ipfWidthSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->ipfHeightSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->maskPatternSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->numOfRegionsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->numOfThreadsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->maskRadiusSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(m_Ui->hipassSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });

  // Checkboxes
  connect(m_Ui->roiCB, &QCheckBox::stateChanged, this, &AverageDotProductMap_UI::listenROICheckboxStateChanged);

  // Combo Boxes
  connect(m_Ui->inputTypeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), this, &AverageDotProductMap_UI::listenInputTypeChanged);

  // Line Edits
  connect(m_Ui->hdf5SelectionWidget, &HDF5DatasetSelectionWidget::parametersChanged, [=] { parametersChanged(); });

  connect(m_Ui->outputFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::createWidgetConnections()
{
  connect(m_Ui->generateBtn, &QPushButton::clicked, this, &AverageDotProductMap_UI::listenGenerateBtnPressed);

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller, &AverageDotProductMapController::errorMessageGenerated, this, &AverageDotProductMap_UI::notifyErrorMessage);
  connect(m_Controller, &AverageDotProductMapController::warningMessageGenerated, this, &AverageDotProductMap_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  connect(m_Ui->outputSelectBtn, &QPushButton::clicked, this, &AverageDotProductMap_UI::listenOutputFileSelectBtnClicked);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::listenInputTypeChanged(int index)
{
  InputType inputType = static_cast<InputType>(index + 1);
  switch(inputType)
  {
    case InputType::TSLHDF:
    case InputType::BrukerHDF:
    case InputType::OxfordHDF:
      m_Ui->hdf5SelectionWidget->setDatasetSelectionEnabled(true);
      break;
    default:
      m_Ui->hdf5SelectionWidget->setDatasetSelectionEnabled(false);
      break;
  }

  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::listenROICheckboxStateChanged(int state)
{
  Qt::CheckState checkState = static_cast<Qt::CheckState>(state);
  m_Ui->roiSB_1->setEnabled(checkState == Qt::Checked);
  m_Ui->roiSB_2->setEnabled(checkState == Qt::Checked);
  m_Ui->roiSB_3->setEnabled(checkState == Qt::Checked);
  m_Ui->roiSB_4->setEnabled(checkState == Qt::Checked);

  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::listenOutputFileSelectBtnClicked()
{
  QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.tif";
  if(!m_Ui->outputFilePathLE->text().isEmpty())
  {
    proposedFile = m_Ui->outputFilePathLE->text();
  }

  QString filePath = FileIOTools::GetSavePathFromDialog("Set Output File", "Image File (*.tif);;All Files (*.*)", proposedFile);
  if(filePath.isEmpty())
  {
    return;
  }

  filePath = QDir::toNativeSeparators(filePath);
  emSoftApp->setOpenDialogLastDirectory(filePath);

  m_Ui->outputFilePathLE->setText(filePath);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::listenGenerateBtnPressed()
{
  if(m_Ui->generateBtn->text() == "Cancel")
  {
    m_Controller->setCancel(true);
    setRunning(false);
    return;
  }

  setRunning(true);
  clearModuleIssues();

  AverageDotProductMapController::ADPMapData data = getData();

  m_Ui->generateBtn->setText("Cancel");
  m_Ui->inputGrpBox->setDisabled(true);
  m_Ui->compParamGrpBox->setDisabled(true);
  m_Ui->outputGrpBox->setDisabled(true);

  // Single-threaded for now, but we can multi-thread later if needed
  //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
  for(int i = 0; i < 1; i++)
  {
    m_Watcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
    connect(m_Watcher.data(), SIGNAL(finished()), this, SLOT(threadFinished()));

    QFuture<void> future = QtConcurrent::run(m_Controller, &AverageDotProductMapController::createADPMap, data);
    m_Watcher->setFuture(future);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::threadFinished()
{
  m_Controller->setCancel(false);

  m_Ui->generateBtn->setText("Generate");
  m_Ui->inputGrpBox->setEnabled(true);
  m_Ui->compParamGrpBox->setEnabled(true);
  m_Ui->outputGrpBox->setEnabled(true);

  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AverageDotProductMap_UI::validateData()
{
  clearModuleIssues();

  AverageDotProductMapController::ADPMapData data = getData();
  if(m_Controller->validateADPMapValues(data))
  {
    m_Ui->generateBtn->setEnabled(true);
    return true;
  }

  m_Ui->generateBtn->setDisabled(true);
  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::readModuleSession(QJsonObject& obj)
{
  readInputParameters(obj);

  readComputationalParameters(obj);

  readOutputParameters(obj);

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::readInputParameters(QJsonObject& obj)
{
  QJsonObject inputParamObj = obj[ioConstants::InputParam].toObject();

  m_Ui->inputTypeCB->blockSignals(true);
  m_Ui->inputTypeCB->setCurrentIndex(inputParamObj[ioConstants::InputType].toInt());
  m_Ui->inputTypeCB->blockSignals(false);

  m_Ui->hdf5SelectionWidget->readParameters(inputParamObj);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::readComputationalParameters(QJsonObject& obj)
{
  QJsonObject compParamObj = obj[ioConstants::CompParam].toObject();

  if(!compParamObj.isEmpty())
  {
    m_Ui->patternHeightSB->blockSignals(true);
    m_Ui->patternWidthSB->blockSignals(true);
    m_Ui->roiCB->blockSignals(true);
    m_Ui->roiSB_1->blockSignals(true);
    m_Ui->roiSB_2->blockSignals(true);
    m_Ui->roiSB_3->blockSignals(true);
    m_Ui->roiSB_4->blockSignals(true);
    m_Ui->binningFactorSB->blockSignals(true);
    m_Ui->binningXSB->blockSignals(true);
    m_Ui->binningYSB->blockSignals(true);
    m_Ui->ipfHeightSB->blockSignals(true);
    m_Ui->ipfWidthSB->blockSignals(true);
    m_Ui->maskPatternSB->blockSignals(true);
    m_Ui->maskRadiusSB->blockSignals(true);
    m_Ui->hipassSB->blockSignals(true);
    m_Ui->numOfRegionsSB->blockSignals(true);
    m_Ui->numOfThreadsSB->blockSignals(true);

    m_Ui->patternHeightSB->setValue(compParamObj[ioConstants::PatternHeight].toInt());
    m_Ui->patternWidthSB->setValue(compParamObj[ioConstants::PatternWidth].toInt());
    m_Ui->roiCB->setChecked(compParamObj[ioConstants::UseROI].toBool());
    m_Ui->roiSB_1->setValue(compParamObj[ioConstants::ROI_1].toInt());
    m_Ui->roiSB_2->setValue(compParamObj[ioConstants::ROI_2].toInt());
    m_Ui->roiSB_3->setValue(compParamObj[ioConstants::ROI_3].toInt());
    m_Ui->roiSB_4->setValue(compParamObj[ioConstants::ROI_4].toInt());
    m_Ui->binningFactorSB->setValue(compParamObj[ioConstants::BinningFactor].toInt());
    m_Ui->binningXSB->setValue(compParamObj[ioConstants::BinningX].toInt());
    m_Ui->binningYSB->setValue(compParamObj[ioConstants::BinningY].toInt());
    m_Ui->ipfHeightSB->setValue(compParamObj[ioConstants::IPFHeight].toInt());
    m_Ui->ipfWidthSB->setValue(compParamObj[ioConstants::IPFWidth].toInt());
    m_Ui->maskPatternSB->setValue(compParamObj[ioConstants::MaskPattern].toInt());
    m_Ui->maskRadiusSB->setValue(compParamObj[ioConstants::MaskRadius].toDouble());
    m_Ui->hipassSB->setValue(compParamObj[ioConstants::HipassFilter].toDouble());
    m_Ui->numOfRegionsSB->setValue(compParamObj[ioConstants::NumberOfRegions].toInt());
    m_Ui->numOfThreadsSB->setValue(compParamObj[ioConstants::NumberOfThreads].toInt());

    m_Ui->patternHeightSB->blockSignals(false);
    m_Ui->patternWidthSB->blockSignals(false);
    m_Ui->roiCB->blockSignals(false);
    m_Ui->roiSB_1->blockSignals(false);
    m_Ui->roiSB_2->blockSignals(false);
    m_Ui->roiSB_3->blockSignals(false);
    m_Ui->roiSB_4->blockSignals(false);
    m_Ui->binningFactorSB->blockSignals(false);
    m_Ui->binningXSB->blockSignals(false);
    m_Ui->binningYSB->blockSignals(false);
    m_Ui->ipfHeightSB->blockSignals(false);
    m_Ui->ipfWidthSB->blockSignals(false);
    m_Ui->maskPatternSB->blockSignals(false);
    m_Ui->maskRadiusSB->blockSignals(false);
    m_Ui->hipassSB->blockSignals(false);
    m_Ui->numOfRegionsSB->blockSignals(false);
    m_Ui->numOfThreadsSB->blockSignals(false);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::readOutputParameters(QJsonObject& obj)
{
  QJsonObject outputParamObj = obj[ioConstants::OutputParam].toObject();

  m_Ui->outputFilePathLE->blockSignals(true);

  m_Ui->outputFilePathLE->setText(outputParamObj[ioConstants::OutputImageFile].toString());

  m_Ui->outputFilePathLE->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::writeModuleSession(QJsonObject& obj)
{
  QJsonObject inputParamObj;
  writeInputParameters(inputParamObj);
  obj[ioConstants::InputParam] = inputParamObj;

  QJsonObject compParamObj;
  writeComputationalParameters(compParamObj);
  obj[ioConstants::CompParam] = compParamObj;

  QJsonObject outputParamObj;
  writeOutputParameters(outputParamObj);
  obj[ioConstants::OutputParam] = outputParamObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::writeInputParameters(QJsonObject& obj)
{
  obj[ioConstants::InputType] = m_Ui->inputTypeCB->currentIndex();

  m_Ui->hdf5SelectionWidget->writeParameters(obj);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::writeComputationalParameters(QJsonObject& obj)
{
  obj[ioConstants::PatternHeight] = m_Ui->patternHeightSB->value();
  obj[ioConstants::PatternWidth] = m_Ui->patternWidthSB->value();
  obj[ioConstants::UseROI] = m_Ui->roiCB->isChecked();
  obj[ioConstants::ROI_1] = m_Ui->roiSB_1->value();
  obj[ioConstants::ROI_2] = m_Ui->roiSB_2->value();
  obj[ioConstants::ROI_3] = m_Ui->roiSB_3->value();
  obj[ioConstants::ROI_4] = m_Ui->roiSB_4->value();
  obj[ioConstants::BinningFactor] = m_Ui->binningFactorSB->value();
  obj[ioConstants::BinningX] = m_Ui->binningXSB->value();
  obj[ioConstants::BinningY] = m_Ui->binningYSB->value();
  obj[ioConstants::IPFHeight] = m_Ui->ipfHeightSB->value();
  obj[ioConstants::IPFWidth] = m_Ui->ipfWidthSB->value();
  obj[ioConstants::MaskPattern] = m_Ui->maskPatternSB->value();
  obj[ioConstants::MaskRadius] = m_Ui->maskRadiusSB->value();
  obj[ioConstants::HipassFilter] = m_Ui->hipassSB->value();
  obj[ioConstants::NumberOfRegions] = m_Ui->numOfRegionsSB->value();
  obj[ioConstants::NumberOfThreads] = m_Ui->numOfThreadsSB->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMap_UI::writeOutputParameters(QJsonObject& obj)
{
  obj[ioConstants::OutputImageFile] = m_Ui->outputFilePathLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AverageDotProductMapController::ADPMapData AverageDotProductMap_UI::getData()
{
  AverageDotProductMapController::ADPMapData data;
  data.roi_1 = m_Ui->roiSB_1->value();
  data.roi_2 = m_Ui->roiSB_2->value();
  data.roi_3 = m_Ui->roiSB_3->value();
  data.roi_4 = m_Ui->roiSB_4->value();
  data.useROI = m_Ui->roiCB->isChecked();
  data.binningX = m_Ui->binningXSB->value();
  data.binningY = m_Ui->binningYSB->value();
  data.ipfWidth = m_Ui->ipfWidthSB->value();
  data.inputType = static_cast<AverageDotProductMapController::ADPMapData::InputType>(m_Ui->inputTypeCB->currentIndex() + 1);
  data.ipfHeight = m_Ui->ipfHeightSB->value();
  data.maskRadius = m_Ui->maskRadiusSB->value();
  data.maskPattern = m_Ui->maskPatternSB->value();
  data.hipassFilter = m_Ui->hipassSB->value();
  data.numOfRegions = m_Ui->numOfRegionsSB->value();
  data.numOfThreads = m_Ui->numOfThreadsSB->value();
  data.patternWidth = m_Ui->patternWidthSB->value();
  data.binningFactor = m_Ui->binningFactorSB->value();
  data.patternHeight = m_Ui->patternHeightSB->value();
  data.outputFilePath = m_Ui->outputFilePathLE->text();
  data.patternDataFile = m_Ui->hdf5SelectionWidget->getCurrentFile();

  QStringList selectedHDF5Paths = m_Ui->hdf5SelectionWidget->getSelectedHDF5Paths();
  if (m_Ui->hdf5SelectionWidget->isDatasetSelectionEnabled() && !selectedHDF5Paths.isEmpty())
  {
    QStringList hdfTokens = selectedHDF5Paths[0].split('/', QString::SplitBehavior::SkipEmptyParts);
    for (QString hdfToken : hdfTokens)
    {
      data.hdfStrings.push_back(hdfToken);
    }
  }

  return data;
}
