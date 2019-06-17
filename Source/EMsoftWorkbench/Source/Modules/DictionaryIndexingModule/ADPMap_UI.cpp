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

#include "ADPMap_UI.h"

#include <QtConcurrent>

#include "Modules/DictionaryIndexingModule/Constants.h"
#include "Modules/DictionaryIndexingModule/ChoosePatternsDatasetDialog.h"

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMap_UI::ADPMap_UI(QWidget *parent)
: QWidget(parent)
, m_Ui(new Ui::ADPMap_UI())
{
  m_Ui->setupUi(this);

  m_ADPController = new ADPMapController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMap_UI::~ADPMap_UI()
{
  delete m_ADPController;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::setupGui()
{
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::createValidators()
{
  m_Ui->maskRadiusLE->setValidator(new QDoubleValidator(m_Ui->maskRadiusLE));
  m_Ui->hipassLE->setValidator(new QDoubleValidator(m_Ui->hipassLE));

  m_Ui->patternHeightLE->setValidator(new QIntValidator(m_Ui->patternHeightLE));
  m_Ui->patternWidthLE->setValidator(new QIntValidator(m_Ui->patternWidthLE));
  m_Ui->roi1LE->setValidator(new QIntValidator(m_Ui->roi1LE));
  m_Ui->roi2LE->setValidator(new QIntValidator(m_Ui->roi2LE));
  m_Ui->roi3LE->setValidator(new QIntValidator(m_Ui->roi3LE));
  m_Ui->roi4LE->setValidator(new QIntValidator(m_Ui->roi4LE));
  m_Ui->binningFactorLE->setValidator(new QIntValidator(m_Ui->binningFactorLE));
  m_Ui->binningXLE->setValidator(new QIntValidator(m_Ui->binningXLE));
  m_Ui->binningYLE->setValidator(new QIntValidator(m_Ui->binningYLE));
  m_Ui->ipfWidthLE->setValidator(new QIntValidator(m_Ui->ipfWidthLE));
  m_Ui->ipfHeightLE->setValidator(new QIntValidator(m_Ui->ipfHeightLE));
  m_Ui->maskPatternLE->setValidator(new QIntValidator(m_Ui->maskPatternLE));
  m_Ui->numOfRegionsLE->setValidator(new QIntValidator(m_Ui->numOfRegionsLE));
  m_Ui->numOfThreadsLE->setValidator(new QIntValidator(m_Ui->numOfThreadsLE));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::createModificationConnections()
{
  // Line Edits
  connect(m_Ui->patternHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->patternWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->roi1LE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->roi2LE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->roi3LE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->roi4LE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->binningFactorLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->binningXLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->binningYLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->maskPatternLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numOfRegionsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numOfThreadsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->maskRadiusLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->hipassLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });

  // Checkboxes
  connect(m_Ui->roiCB, &QCheckBox::stateChanged, this, &ADPMap_UI::listenROICheckboxStateChanged);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::createWidgetConnections()
{
  connect(m_Ui->generateADPBtn, &QPushButton::clicked, this, &ADPMap_UI::listenADPGenerationStarted);

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_ADPController, &ADPMapController::errorMessageGenerated, this, &ADPMap_UI::errorMessageGenerated);
  connect(m_ADPController, &ADPMapController::warningMessageGenerated, this, &ADPMap_UI::warningMessageGenerated);
  connect(m_ADPController, &ADPMapController::stdOutputMessageGenerated, this, &ADPMap_UI::stdOutputMessageGenerated);
  connect(m_ADPController, &ADPMapController::adpMapCreated, m_Ui->adpViewer, &ADPMapImageViewer::loadImage);

  connect(m_Ui->adpViewer, &ADPMapImageViewer::errorMessageGenerated, this, &ADPMap_UI::errorMessageGenerated);
  connect(m_Ui->adpViewer, &ADPMapImageViewer::zoomFactorChanged, this, &ADPMap_UI::updateZoomFactor);
  connect(m_Ui->adpViewer, &ADPMapImageViewer::selectedPatternPixelChanged, this, &ADPMap_UI::selectedPatternPixelChanged);

  connect(m_Ui->adpMapZoomInBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::zoomIn);
  connect(m_Ui->adpMapZoomOutBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::zoomOut);
  connect(m_Ui->adpMapFitToScreenBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::fitToScreen);
  connect(m_Ui->adpMapZoomSB, QOverload<int>::of(&QSpinBox::valueChanged), [=] (int value) { m_Ui->adpViewer->setZoomFactor(value / 100.0f); });

  connect(m_Ui->adpMapSaveBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::saveImage);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::updateZoomFactor(float zoomFactor)
{
  m_Ui->adpMapZoomSB->blockSignals(true);
  m_Ui->adpMapZoomSB->setValue(zoomFactor * 100);
  m_Ui->adpMapZoomSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenROICheckboxStateChanged(int state)
{
  Qt::CheckState checkState = static_cast<Qt::CheckState>(state);
  m_Ui->roi1LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi2LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi3LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi4LE->setEnabled(checkState == Qt::Checked);

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenADPGenerationStarted()
{
  if(m_Ui->generateADPBtn->text() == "Cancel")
  {
    m_ADPController->setCancel(true);
    emit adpMapGenerationFinished();
    return;
  }

  ADPMapController::ADPMapData data = getADPMapData();

  m_Ui->generateADPBtn->setText("Cancel");
  m_Ui->adpParametersGroupBox->setDisabled(true);

  // Single-threaded for now, but we can multi-thread later if needed
  //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
  for(int i = 0; i < 1; i++)
  {
    m_ADPWatcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
    connect(m_ADPWatcher.data(), SIGNAL(finished()), this, SLOT(listenADPGenerationFinished()));

    QFuture<void> future = QtConcurrent::run(m_ADPController, &ADPMapController::createADPMap, data);
    m_ADPWatcher->setFuture(future);
  }

  emit adpMapGenerationStarted();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenADPGenerationFinished()
{
  m_ADPController->setCancel(false);

  m_Ui->adpMapZoomSB->setEnabled(true);
  m_Ui->adpMapSaveBtn->setEnabled(true);
  m_Ui->adpMapZoomInBtn->setEnabled(true);
  m_Ui->adpMapZoomOutBtn->setEnabled(true);
  m_Ui->adpMapFitToScreenBtn->setEnabled(true);

  m_Ui->generateADPBtn->setText("Generate");
  m_Ui->adpParametersGroupBox->setEnabled(true);

  emit adpMapGenerationFinished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ADPMap_UI::validateData()
{
  ADPMapController::ADPMapData adpData = getADPMapData();
  if(!m_ADPController->validateADPMapValues(adpData))
  {
    m_Ui->generateADPBtn->setDisabled(true);
    return false;
  }

  m_Ui->generateADPBtn->setEnabled(true);
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapController::ADPMapData ADPMap_UI::getADPMapData()
{
  ADPMapController::ADPMapData data;
  data.roi_1 = m_Ui->roi1LE->text().toInt();
  data.roi_2 = m_Ui->roi2LE->text().toInt();
  data.roi_3 = m_Ui->roi3LE->text().toInt();
  data.roi_4 = m_Ui->roi4LE->text().toInt();
  data.useROI = m_Ui->roiCB->isChecked();
  data.binningX = m_Ui->binningXLE->text().toInt();
  data.binningY = m_Ui->binningYLE->text().toInt();
  data.ipfWidth = m_Ui->ipfWidthLE->text().toInt();
  data.inputType = m_InputType;
  data.ipfHeight = m_Ui->ipfHeightLE->text().toInt();
  data.maskRadius = m_Ui->maskRadiusLE->text().toDouble();
  data.maskPattern = m_Ui->maskPatternLE->text().toInt();
  data.hipassFilter = m_Ui->hipassLE->text().toDouble();
  data.numOfRegions = m_Ui->numOfRegionsLE->text().toInt();
  data.numOfThreads = m_Ui->numOfThreadsLE->text().toInt();
  data.patternWidth = m_Ui->patternWidthLE->text().toInt();
  data.binningFactor = m_Ui->binningFactorLE->text().toInt();
  data.patternHeight = m_Ui->patternHeightLE->text().toInt();
  data.patternDataFile = m_PatternDataFile;
  data.hdfStrings = m_SelectedHDF5Path;
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::setInputType(ADPMapController::InputType inputType)
{
  m_InputType = inputType;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::setPatternDataFile(const QString &filePath)
{
  m_PatternDataFile = filePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::setSelectedHDF5Path(const QStringList &path)
{
  m_SelectedHDF5Path = path;
}
