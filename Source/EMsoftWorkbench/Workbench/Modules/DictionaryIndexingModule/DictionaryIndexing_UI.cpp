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

#include "DictionaryIndexing_UI.h"

#include <QtConcurrent>

#include <QtWidgets/QFileDialog>

#include "Modules/DictionaryIndexingModule/Constants.h"
#include "Modules/ModuleTools.hpp"

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;

using IndexingMode = DictionaryIndexingController::IndexingMode;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexing_UI::DictionaryIndexing_UI(QWidget *parent)
: QWidget(parent)
, m_Ui(new Ui::DictionaryIndexing_UI())
{
  m_Ui->setupUi(this);

  m_DIController = new DictionaryIndexingController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexing_UI::~DictionaryIndexing_UI()
{
  delete m_DIController;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setupGui()
{
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  validateData();

  listenIndexingModeChanged(0);

  m_Ui->numOfThreadsLabel->setToolTip(tr("Number of Threads must be between 1 and %1").arg(QThreadPool::globalInstance()->maxThreadCount()));
  m_Ui->numOfThreadsLE->setToolTip(tr("Number of Threads must be between 1 and %1").arg(QThreadPool::globalInstance()->maxThreadCount()));

  QStringList choices = ModuleTools::getPlatformInfo();
  m_Ui->gpuPlatformCB->insertItems(0, choices);
  if(m_Ui->gpuPlatformCB->count() > 0)
  {
    m_Ui->gpuPlatformCB->setCurrentIndex(0);
  }

  // Grab the first device as a default
  choices = ModuleTools::getDeviceInfo(1);
  m_Ui->gpuDeviceCB->insertItems(0, choices);
  if(m_Ui->gpuDeviceCB->count() > 0)
  {
    m_Ui->gpuDeviceCB->setCurrentIndex(0);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createValidators()
{
  // Double Validators
  m_Ui->maskRadiusLE->setValidator(new QDoubleValidator(m_Ui->maskRadiusLE));
  m_Ui->samplingStepSizeXLE->setValidator(new QDoubleValidator(m_Ui->samplingStepSizeXLE));
  m_Ui->samplingStepSizeYLE->setValidator(new QDoubleValidator(m_Ui->samplingStepSizeYLE));
  m_Ui->isangleLE->setValidator(new QDoubleValidator(m_Ui->isangleLE));
  m_Ui->LLE->setValidator(new QDoubleValidator(m_Ui->LLE));
  m_Ui->thetacLE->setValidator(new QDoubleValidator(m_Ui->thetacLE));
  m_Ui->deltaLE->setValidator(new QDoubleValidator(m_Ui->deltaLE));
  m_Ui->xpcLE->setValidator(new QDoubleValidator(m_Ui->xpcLE));
  m_Ui->ypcLE->setValidator(new QDoubleValidator(m_Ui->ypcLE));
  m_Ui->omegaLE->setValidator(new QDoubleValidator(m_Ui->omegaLE));
  m_Ui->energyMinLE->setValidator(new QDoubleValidator(m_Ui->energyMinLE));
  m_Ui->energyMaxLE->setValidator(new QDoubleValidator(m_Ui->energyMaxLE));
  m_Ui->beamCurrentLE->setValidator(new QDoubleValidator(m_Ui->beamCurrentLE));
  m_Ui->dwellTimeLE->setValidator(new QDoubleValidator(m_Ui->dwellTimeLE));
  m_Ui->gammaCorrectionFactorLE->setValidator(new QDoubleValidator(m_Ui->gammaCorrectionFactorLE));

  // Int Validators
  m_Ui->nnkLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->nnkLE));

  QIntValidator* intValidator = new QIntValidator(m_Ui->nnavLE);
  m_Ui->nnavLE->setValidator(intValidator);
  intValidator->setRange(1, std::numeric_limits<int>::max());
  connect(m_Ui->nnkLE, &QLineEdit::textChanged, [=] { intValidator->setRange(1, m_Ui->nnkLE->text().toInt()); });

  intValidator = new QIntValidator(m_Ui->nosmLE);
  m_Ui->nosmLE->setValidator(intValidator);
  intValidator->setRange(1, std::numeric_limits<int>::max());
  connect(m_Ui->nnkLE, &QLineEdit::textChanged, [=] { intValidator->setRange(1, m_Ui->nnkLE->text().toInt()); });

  intValidator = new QIntValidator(m_Ui->nismLE);
  m_Ui->nismLE->setValidator(intValidator);
  intValidator->setRange(1, std::numeric_limits<int>::max());
  connect(m_Ui->nnkLE, &QLineEdit::textChanged, [=] { intValidator->setRange(1, m_Ui->nnkLE->text().toInt()); });

  m_Ui->roi1LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi1LE));
  m_Ui->roi2LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi2LE));
  m_Ui->roi3LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi3LE));
  m_Ui->roi4LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi4LE));
  m_Ui->numdictsingleLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numdictsingleLE));
  m_Ui->ipfWidthLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfWidthLE));
  m_Ui->ipfHeightLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfHeightLE));
  m_Ui->numexptsingleLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numexptsingleLE));
  m_Ui->cubochoricPointsLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->cubochoricPointsLE));
  m_Ui->numOfThreadsLE->setValidator(new QIntValidator(1, QThreadPool::globalInstance()->maxThreadCount(), m_Ui->numOfThreadsLE));
  m_Ui->numsxLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numsxLE));
  m_Ui->numsyLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numsyLE));

  m_Ui->diZoomSB->setMaximum(std::numeric_limits<int>::max());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createModificationConnections()
{
  // Line Edits
  connect(m_Ui->maskRadiusLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->samplingStepSizeXLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->roi1LE, &QLineEdit::textChanged, [=] { listenROIChanged(); });
  connect(m_Ui->roi2LE, &QLineEdit::textChanged, [=] { listenROIChanged(); });
  connect(m_Ui->roi3LE, &QLineEdit::textChanged, [=] { listenROIChanged(); });
  connect(m_Ui->roi4LE, &QLineEdit::textChanged, [=] { listenROIChanged(); });
  connect(m_Ui->samplingStepSizeYLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->isangleLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->LLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->thetacLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->deltaLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numOfThreadsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->xpcLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ypcLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->omegaLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->energyMinLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->energyMaxLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->beamCurrentLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->dwellTimeLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->gammaCorrectionFactorLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->nnkLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->nnavLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->nosmLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->nismLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numdictsingleLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numexptsingleLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->cubochoricPointsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numsxLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numsyLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->eulerAngleFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->dictionaryFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->masterFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->outputDataFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->outputCtfFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->outputAngFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->outputAvgCtfFileLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createWidgetConnections()
{
  connect(m_Ui->generateDIBtn, &QPushButton::clicked, this, &DictionaryIndexing_UI::listenDIGenerationStarted);

  connect(m_Ui->eulerAngleFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectOpenFilePath("Select Euler Angle File", "Text Files (*.txt);;All Files(*.*)");
    m_Ui->eulerAngleFileLE->setText(filePath);
  });
  connect(m_Ui->dictionaryFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectOpenFilePath("Select Dictionary File", "HDF5 Files (*.hdf5 *.h5);;All Files(*.*)");
    m_Ui->dictionaryFileLE->setText(filePath);
  });
  connect(m_Ui->masterFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectOpenFilePath("Select Master File", "HDF5 Files (*.hdf5 *.h5);;All Files(*.*)");
    m_Ui->masterFileLE->setText(filePath);
  });
  connect(m_Ui->maskFilePathBtn, &QPushButton::clicked, [=] {
    QString filePath = selectSaveFilePath("Select Mask File", "Text Files (*.txt);;All Files(*.*)");
    m_Ui->maskFilePathLE->setText(filePath);
  });
  connect(m_Ui->outputDataFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectSaveFilePath("Select Data File", "HDF5 Files (*.hdf5 *.h5);;All Files(*.*)");
    m_Ui->outputDataFileLE->setText(filePath);
  });
  connect(m_Ui->outputCtfFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectSaveFilePath("Select Ctf File", "Ctf Files (*.ctf);;All Files(*.*)");
    m_Ui->outputCtfFileLE->setText(filePath);
  });
  connect(m_Ui->outputAngFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectSaveFilePath("Select Ang File", "Ang Files (*.ang);;All Files(*.*)");
    m_Ui->outputAngFileLE->setText(filePath);
  });
  connect(m_Ui->outputAvgCtfFileBtn, &QPushButton::clicked, [=] {
    QString filePath = selectSaveFilePath("Select Average Ctf File", "Ctf Files (*.ctf);;All Files(*.*)");
    m_Ui->outputAvgCtfFileLE->setText(filePath);
  });

  connect(m_Ui->diViewer, &GLImageViewer::errorMessageGenerated, this, &DictionaryIndexing_UI::errorMessageGenerated);
  connect(m_Ui->diViewer, &GLImageViewer::zoomFactorChanged, this, &DictionaryIndexing_UI::updateZoomFactor);

  connect(m_Ui->diZoomInBtn, &QPushButton::clicked, m_Ui->diViewer, &GLImageViewer::zoomIn);
  connect(m_Ui->diZoomOutBtn, &QPushButton::clicked, m_Ui->diViewer, &GLImageViewer::zoomOut);
  connect(m_Ui->diFitToScreenBtn, &QPushButton::clicked, m_Ui->diViewer, &GLImageViewer::fitToScreen);
  connect(m_Ui->diZoomSB, QOverload<int>::of(&QSpinBox::valueChanged), [=] (int value) { m_Ui->diViewer->setZoomFactor(value / 100.0f); });

  connect(m_Ui->diSaveBtn, &QPushButton::clicked, m_Ui->diViewer, &GLImageViewer::saveImage);

  // Checkboxes
  connect(m_Ui->roiCB, &QCheckBox::stateChanged, this, &DictionaryIndexing_UI::listenROICheckboxStateChanged);
  connect(m_Ui->useCustomMaskCB, &QCheckBox::stateChanged, this, &DictionaryIndexing_UI::listenMaskCheckboxStateChanged);
  connect(m_Ui->indexingModeCB, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &DictionaryIndexing_UI::listenIndexingModeChanged);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString DictionaryIndexing_UI::selectOpenFilePath(const QString &caption, const QString &filter)
{
  QString file = QFileDialog::getOpenFileName(this, caption, m_OpenDialogLastDirectory, filter);

  if(file.isEmpty())
  {
    return {};
  }

  file = QDir::toNativeSeparators(file);

  // Store the last used directory into the private instance variable
  QFileInfo fi(file);
  m_OpenDialogLastDirectory = fi.path();

  return file;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString DictionaryIndexing_UI::selectSaveFilePath(const QString &caption, const QString &filter)
{
  QString file = QFileDialog::getSaveFileName(this, caption, m_OpenDialogLastDirectory, filter);

  if(file.isEmpty())
  {
    return {};
  }

  file = QDir::toNativeSeparators(file);

  // Store the last used directory into the private instance variable
  QFileInfo fi(file);
  m_OpenDialogLastDirectory = fi.path();

  return file;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::updateZoomFactor(float zoomFactor)
{
  m_Ui->diZoomSB->blockSignals(true);
  m_Ui->diZoomSB->setValue(zoomFactor * 100);
  m_Ui->diZoomSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenROIChanged()
{
  int x = m_Ui->roi1LE->text().toInt();
  int y = m_Ui->roi2LE->text().toInt();
  int w = m_Ui->roi3LE->text().toInt();
  int h = m_Ui->roi4LE->text().toInt();
  m_Ui->diViewer->setROI(x, y, w, h);

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenMaskCheckboxStateChanged(int state)
{
  Qt::CheckState checkState = static_cast<Qt::CheckState>(state);
  m_Ui->maskFilePathLE->setEnabled(checkState == Qt::Checked);
  m_Ui->maskFilePathBtn->setEnabled(checkState == Qt::Checked);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenROICheckboxStateChanged(int state)
{
  Qt::CheckState checkState = static_cast<Qt::CheckState>(state);
  m_Ui->roi1LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi2LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi3LE->setEnabled(checkState == Qt::Checked);
  m_Ui->roi4LE->setEnabled(checkState == Qt::Checked);

  if(checkState == Qt::Checked)
  {
    int x = m_Ui->roi1LE->text().toInt();
    int y = m_Ui->roi2LE->text().toInt();
    int w = m_Ui->roi3LE->text().toInt();
    int h = m_Ui->roi4LE->text().toInt();
    m_Ui->diViewer->setROI(x, y, w, h);
  }
  else
  {
    m_Ui->diViewer->clearROI();
  }

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenIndexingModeChanged(int index)
{
  IndexingMode mode = static_cast<IndexingMode>(index);
  switch(mode)
  {
//  case IndexingMode::Static:
//    setStaticIndexingMode();
//    break;
  case IndexingMode::Dynamic:
    setDynamicIndexingMode();
    break;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setStaticIndexingMode()
{
  m_Ui->cubochoricPointsLE->hide();
  m_Ui->cubochoricPointsLabel->hide();
  m_Ui->LLE->hide();
  m_Ui->LLabel->hide();
  m_Ui->thetacLE->hide();
  m_Ui->thetacLabel->hide();
  m_Ui->deltaLE->hide();
  m_Ui->deltaLabel->hide();
  m_Ui->numsxLE->hide();
  m_Ui->numsLabel->hide();
  m_Ui->numsyLE->hide();
  m_Ui->patternCenterLabel->hide();
  m_Ui->xpcLE->hide();
  m_Ui->ypcLE->hide();
  m_Ui->omegaLE->hide();
  m_Ui->omegaLabel->hide();
  m_Ui->energyMinLE->hide();
  m_Ui->energyMinLabel->hide();
  m_Ui->energyMaxLE->hide();
  m_Ui->energyMaxLabel->hide();
  m_Ui->energyAveragingMethodLabel->hide();
  m_Ui->energyAveragingMethodCB->hide();
  m_Ui->spatialAveragingCB->hide();
  m_Ui->beamCurrentLE->hide();
  m_Ui->beamCurrentLabel->hide();
  m_Ui->dwellTimeLE->hide();
  m_Ui->dwellTimeLabel->hide();
  m_Ui->binningLabel->hide();
  m_Ui->binningCB->hide();
  m_Ui->intensityScalingModeLabel->hide();
  m_Ui->intensityScalingModeCB->hide();
  m_Ui->gammaCorrectionFactorLabel->hide();
  m_Ui->gammaCorrectionFactorLE->hide();
  m_Ui->masterFileLabel->hide();
  m_Ui->masterFileLE->hide();
  m_Ui->masterFileBtn->hide();

  m_Ui->dictionaryFileLabel->show();
  m_Ui->dictionaryFileLE->show();
  m_Ui->dictionaryFileBtn->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setDynamicIndexingMode()
{
  m_Ui->cubochoricPointsLE->show();
  m_Ui->cubochoricPointsLabel->show();
  m_Ui->LLE->show();
  m_Ui->LLabel->show();
  m_Ui->thetacLE->show();
  m_Ui->thetacLabel->show();
  m_Ui->deltaLE->show();
  m_Ui->deltaLabel->show();
  m_Ui->numsxLE->show();
  m_Ui->numsLabel->show();
  m_Ui->numsyLE->show();
  m_Ui->patternCenterLabel->show();
  m_Ui->xpcLE->show();
  m_Ui->ypcLE->show();
  m_Ui->omegaLE->show();
  m_Ui->omegaLabel->show();
  m_Ui->energyMinLE->show();
  m_Ui->energyMinLabel->show();
  m_Ui->energyMaxLE->show();
  m_Ui->energyMaxLabel->show();
  m_Ui->energyAveragingMethodLabel->show();
  m_Ui->energyAveragingMethodCB->show();
  m_Ui->spatialAveragingCB->show();
  m_Ui->beamCurrentLE->show();
  m_Ui->beamCurrentLabel->show();
  m_Ui->dwellTimeLE->show();
  m_Ui->dwellTimeLabel->show();
  m_Ui->binningLabel->show();
  m_Ui->binningCB->show();
  m_Ui->intensityScalingModeLabel->show();
  m_Ui->intensityScalingModeCB->show();
  m_Ui->gammaCorrectionFactorLabel->show();
  m_Ui->gammaCorrectionFactorLE->show();
  m_Ui->masterFileLabel->show();
  m_Ui->masterFileLE->show();
  m_Ui->masterFileBtn->show();

  m_Ui->dictionaryFileLabel->hide();
  m_Ui->dictionaryFileLE->hide();
  m_Ui->dictionaryFileBtn->hide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenInputTypeChanged(EMsoftWorkbenchConstants::InputType inputType)
{
  setInputType(inputType);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenPatternDataFileChanged(const QString &filePath)
{
  m_Ui->patternDataFileLabel->setText(filePath);

  setPatternDataFile(filePath);

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenSelectedPatternDatasetChanged(QStringList patternDSetPaths)
{
  setSelectedHDF5Path({});

  if (patternDSetPaths.size() == 1)
  {
    m_Ui->patternDsetPathLabel->setText(patternDSetPaths[0]);

    if(m_InputType == InputType::TSLHDF || m_InputType == InputType::BrukerHDF || m_InputType == InputType::OxfordHDF)
    {
      QStringList hdfTokens = patternDSetPaths[0].trimmed().split('/', QString::SplitBehavior::SkipEmptyParts);
      setSelectedHDF5Path(hdfTokens);
    }
  }
  else
  {
    m_Ui->patternDsetPathLabel->setText("N/A");
  }

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenDIGenerationStarted()
{
  if(m_Ui->generateDIBtn->text() == "Cancel")
  {
    m_DIController->cancel();
    emit diGenerationFinished();
    return;
  }

  DictionaryIndexingController::InputDataType data = getDIData();

  m_Ui->generateDIBtn->setText("Cancel");
  m_Ui->ppSelectionsGB->setDisabled(true);
  m_Ui->inputParamsGB->setDisabled(true);
  m_Ui->paramsGB->setDisabled(true);
  m_Ui->outputParamsGB->setDisabled(true);

  // Clear out the previous (if any) controller instance
  if(m_DIController != nullptr)
  {
    delete m_DIController;
    m_DIController = nullptr;
  }

  // Create a new QThread to run the Controller class.
  m_WorkerThread = QSharedPointer<QThread>(new QThread());
  m_DIController = new DictionaryIndexingController;
  m_DIController->moveToThread(m_WorkerThread.data());
  m_DIController->setData(data); // Set the input data

  // Conncet Signals & Slots to get the thread started and quit
  connect(m_WorkerThread.data(), SIGNAL(started()), m_DIController, SLOT(executeWrapper()));
  connect(m_DIController, SIGNAL(finished()), m_WorkerThread.data(), SLOT(quit()));
  connect(m_WorkerThread.data(), SIGNAL(finished()), this, SLOT(processFinished()));

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_DIController, &DictionaryIndexingController::errorMessageGenerated, this, &DictionaryIndexing_UI::errorMessageGenerated);
  connect(m_DIController, &DictionaryIndexingController::warningMessageGenerated, this, &DictionaryIndexing_UI::warningMessageGenerated);
  connect(m_DIController, SIGNAL(stdOutputMessageGenerated(QString)), this, SIGNAL(stdOutputMessageGenerated(QString)));
  connect(m_DIController, &DictionaryIndexingController::diCreated, m_Ui->diViewer, &GLImageViewer::loadImage);

  m_WorkerThread->start();
  emit diGenerationStarted();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::processFinished()
{
  m_Ui->diZoomSB->setEnabled(true);
  m_Ui->diSaveBtn->setEnabled(true);
  m_Ui->diZoomInBtn->setEnabled(true);
  m_Ui->diZoomOutBtn->setEnabled(true);
  m_Ui->diFitToScreenBtn->setEnabled(true);

  m_Ui->generateDIBtn->setText("Generate");
  m_Ui->ppSelectionsGB->setEnabled(true);
  m_Ui->inputParamsGB->setEnabled(true);
  m_Ui->paramsGB->setEnabled(true);
  m_Ui->outputParamsGB->setEnabled(true);

  emit diGenerationFinished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool DictionaryIndexing_UI::validateData()
{
  if (m_SelectedHipassValue < 0)
  {
    QString errMsg = "The 'Selected Hipass Value' field is invalid.  Please double-click inside "
                     "the image generated in the 'Pattern Preprocessing' tab to "
                     "choose a hipass value and number of regions.";
    emit errorMessageGenerated(errMsg);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  if (m_SelectedNumOfRegions < 0)
  {
    QString errMsg = "The 'Selected Number of Regions' field is invalid.  Please double-click inside "
                     "the image generated in the 'Pattern Preprocessing' tab to "
                     "choose a hipass value and number of regions.";
    emit errorMessageGenerated(errMsg);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  if(m_InputType == DictionaryIndexingController::InputType::TSLHDF || m_InputType == DictionaryIndexingController::InputType::BrukerHDF ||
     m_InputType == DictionaryIndexingController::InputType::OxfordHDF)
  {
    if(m_PatternDataFile.isEmpty())
    {
      QString ss = QObject::tr("Pattern data file is empty.  Please select a pattern data file from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generateDIBtn->setDisabled(true);
      return false;
    }
    if(m_SelectedHDF5Path.isEmpty())
    {
      QString ss = QObject::tr("Pattern dataset not chosen.  Please select a pattern dataset from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generateDIBtn->setDisabled(true);
      return false;
    }
  }

  QString masterFilePath = m_Ui->masterFileLE->text();
  if (masterFilePath.isEmpty())
  {
    QString ss = QObject::tr("'%1' field is empty.").arg(m_Ui->masterFileLabel->text());
    emit errorMessageGenerated(ss);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  QString outputDataFilePath = m_Ui->outputDataFileLE->text();
  if (outputDataFilePath.isEmpty())
  {
    QString ss = QObject::tr("'%1' field is empty.").arg(m_Ui->outputDataFileLabel->text());
    emit errorMessageGenerated(ss);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  QString angFilePath = m_Ui->outputAngFileLE->text();
  if (angFilePath.isEmpty())
  {
    QString ss = QObject::tr("'%1' field is empty.").arg(m_Ui->outputAngFileLabel->text());
    emit errorMessageGenerated(ss);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  QString ctfFilePath = m_Ui->outputCtfFileLE->text();
  if (ctfFilePath.isEmpty())
  {
    QString ss = QObject::tr("'%1' field is empty.").arg(m_Ui->outputCtfFileLabel->text());
    emit errorMessageGenerated(ss);
    m_Ui->generateDIBtn->setDisabled(true);
    return false;
  }

  m_Ui->generateDIBtn->setEnabled(true);
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingController::InputDataType DictionaryIndexing_UI::getDIData()
{
  DictionaryIndexingController::InputDataType data;
  data.indexingMode = static_cast<IndexingMode>(m_Ui->indexingModeCB->currentIndex());
  data.inputType = m_InputType;
  data.patternDataFile = m_PatternDataFile;
  data.hdfStrings = m_SelectedHDF5Path;
  data.eulerAngleFile = m_Ui->eulerAngleFileLE->text();
  data.dictFile = m_Ui->dictionaryFileLE->text();
  data.masterFile = m_Ui->masterFileLE->text();
  data.ipfWidth = m_Ui->ipfWidthLE->text().toInt();
  data.ipfHeight = m_Ui->ipfHeightLE->text().toInt();
  data.roi_x = m_Ui->roi1LE->text().toInt();
  data.roi_y = m_Ui->roi2LE->text().toInt();
  data.roi_w = m_Ui->roi3LE->text().toInt();
  data.roi_h = m_Ui->roi4LE->text().toInt();
  data.useROI = m_Ui->roiCB->isChecked();
  data.samplingStepSizeX = m_Ui->samplingStepSizeXLE->text().toFloat();
  data.samplingStepSizeY = m_Ui->samplingStepSizeYLE->text().toFloat();
  data.nnk = m_Ui->nnkLE->text().toInt();
  data.nnav = m_Ui->nnavLE->text().toInt();
  data.nism = m_Ui->nismLE->text().toInt();
  data.nosm = m_Ui->nosmLE->text().toInt();
  data.isangle = m_Ui->isangleLE->text().toFloat();
  data.maskfile = m_Ui->maskFilePathLE->text();
  data.useMask = m_Ui->useCustomMaskCB->isChecked();
  data.maskRadius = m_Ui->maskRadiusLE->text().toDouble();
  data.hipassValue = m_SelectedHipassValue;
  data.numOfRegions = m_SelectedNumOfRegions;
  data.nCubochoric = m_Ui->cubochoricPointsLE->text().toInt();
  data.L = m_Ui->LLE->text().toFloat();
  data.thetac = m_Ui->thetacLE->text().toFloat();
  data.delta = m_Ui->deltaLE->text().toFloat();
  data.numsx = m_Ui->numsxLE->text().toInt();
  data.numsy = m_Ui->numsyLE->text().toInt();
  data.xpc = m_Ui->xpcLE->text().toFloat();
  data.ypc = m_Ui->ypcLE->text().toFloat();
  data.omega = m_Ui->omegaLE->text().toFloat();
  data.energymin = m_Ui->energyMinLE->text().toFloat();
  data.energymax = m_Ui->energyMaxLE->text().toFloat();
  data.averagingMethod = static_cast<DictionaryIndexingController::EnergyAveragingMethod>(m_Ui->energyAveragingMethodCB->currentIndex());
  data.useSpatialAveraging = m_Ui->spatialAveragingCB->isChecked();
  data.beamCurrent = m_Ui->beamCurrentLE->text().toFloat();
  data.dwellTime = m_Ui->dwellTimeLE->text().toFloat();
  data.binning = m_Ui->binningCB->currentText().toInt();
  data.scalingMode = static_cast<DictionaryIndexingController::IntensityScalingMode>(m_Ui->intensityScalingModeCB->currentIndex());
  data.gammaCorrectionFactor = m_Ui->gammaCorrectionFactorLE->text().toFloat();
  data.exptFile = m_PatternDataFile;
  data.outputDataFilePath = m_Ui->outputDataFileLE->text();
  data.outputCtfFilePath = m_Ui->outputCtfFileLE->text();
  data.outputAngFilePath = m_Ui->outputAngFileLE->text();
  data.outputAvgCtfFilePath = m_Ui->outputAvgCtfFileLE->text();
  data.numDictSingle = m_Ui->numdictsingleLE->text().toInt();
  data.numExptSingle = m_Ui->numexptsingleLE->text().toInt();
  data.numOfThreads = m_Ui->numOfThreadsLE->text().toInt();
  data.platId = m_Ui->gpuPlatformCB->currentIndex() + 1;
  data.devId = m_Ui->gpuDeviceCB->currentIndex() + 1;
  data.adpMap = m_ADPMap;
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::readSession(const QJsonObject &obj)
{
  QJsonObject diParamsObj = obj[ioConstants::DIParams].toObject();

  if(!diParamsObj.isEmpty())
  {
    m_Ui->indexingModeCB->setCurrentIndex(diParamsObj[ioConstants::IndexingMode].toInt());
    m_Ui->eulerAngleFileLE->setText(diParamsObj[ioConstants::EulerAngleFile].toString());
    m_Ui->dictionaryFileLE->setText(diParamsObj[ioConstants::DictionaryFile].toString());
    m_Ui->masterFileLE->setText(diParamsObj[ioConstants::MasterPatternFile].toString());
    m_Ui->ipfHeightLE->setText(QString::number(diParamsObj[ioConstants::IPFHeight].toInt()));
    m_Ui->ipfWidthLE->setText(QString::number(diParamsObj[ioConstants::IPFWidth].toInt()));
    m_Ui->roiCB->setChecked(diParamsObj[ioConstants::UseROI].toBool());
    m_Ui->roi1LE->setText(QString::number(diParamsObj[ioConstants::ROI_X].toInt()));
    m_Ui->roi2LE->setText(QString::number(diParamsObj[ioConstants::ROI_Y].toInt()));
    m_Ui->roi3LE->setText(QString::number(diParamsObj[ioConstants::ROI_W].toInt()));
    m_Ui->roi4LE->setText(QString::number(diParamsObj[ioConstants::ROI_H].toInt()));
    m_Ui->samplingStepSizeXLE->setText(QString::number(diParamsObj[ioConstants::SamplingStepSizeX].toInt()));
    m_Ui->samplingStepSizeYLE->setText(QString::number(diParamsObj[ioConstants::SamplingStepSizeY].toInt()));
    m_Ui->nnkLE->setText(QString::number(diParamsObj[ioConstants::ADPMatches].toInt()));
    m_Ui->nosmLE->setText(QString::number(diParamsObj[ioConstants::OrientationSimilarityMM].toInt()));
    m_Ui->nnavLE->setText(QString::number(diParamsObj[ioConstants::OrientationAveragingMM].toInt()));
    m_Ui->nismLE->setText(QString::number(diParamsObj[ioConstants::IndexingSuccessMM].toInt()));
    m_Ui->isangleLE->setText(QString::number(diParamsObj[ioConstants::IndexingSuccessThreshAngle].toDouble()));
    m_Ui->useCustomMaskCB->setChecked(diParamsObj[ioConstants::UseCustomMask].toBool());
    m_Ui->maskFilePathLE->setText(diParamsObj[ioConstants::MaskFile].toString());
    m_Ui->maskRadiusLE->setText(QString::number(diParamsObj[ioConstants::MaskRadius].toDouble()));
    m_Ui->numdictsingleLE->setText(QString::number(diParamsObj[ioConstants::NumDictSingle].toInt()));
    m_Ui->numexptsingleLE->setText(QString::number(diParamsObj[ioConstants::NumExptSingle].toInt()));
    m_Ui->numOfThreadsLE->setText(QString::number(diParamsObj[ioConstants::NumberOfThreads].toInt()));
    m_Ui->gpuPlatformCB->setCurrentIndex(diParamsObj[ioConstants::GPUPlatform].toInt());
    m_Ui->gpuDeviceCB->setCurrentIndex(diParamsObj[ioConstants::GPUDevice].toInt());
    m_Ui->LLE->setText(QString::number(diParamsObj[ioConstants::L].toDouble()));
    m_Ui->cubochoricPointsLE->setText(QString::number(diParamsObj[ioConstants::CubochoricPoints].toInt()));
    m_Ui->thetacLE->setText(QString::number(diParamsObj[ioConstants::CameraTiltAngle].toDouble()));
    m_Ui->deltaLE->setText(QString::number(diParamsObj[ioConstants::Delta].toDouble()));
    m_Ui->numsxLE->setText(QString::number(diParamsObj[ioConstants::NumsX].toInt()));
    m_Ui->numsyLE->setText(QString::number(diParamsObj[ioConstants::NumsY].toInt()));
    m_Ui->xpcLE->setText(QString::number(diParamsObj[ioConstants::PatternCenterX].toDouble()));
    m_Ui->ypcLE->setText(QString::number(diParamsObj[ioConstants::PatternCenterY].toDouble()));
    m_Ui->omegaLE->setText(QString::number(diParamsObj[ioConstants::Omega].toDouble()));
    m_Ui->energyMinLE->setText(QString::number(diParamsObj[ioConstants::EnergyMin].toDouble()));
    m_Ui->energyMaxLE->setText(QString::number(diParamsObj[ioConstants::EnergyMax].toDouble()));
    m_Ui->energyAveragingMethodCB->setCurrentIndex(diParamsObj[ioConstants::EnergyAveragingMethod].toInt());
    m_Ui->spatialAveragingCB->setChecked(diParamsObj[ioConstants::SpatialAveragingMethod].toBool());
    m_Ui->beamCurrentLE->setText(QString::number(diParamsObj[ioConstants::BeamCurrent].toDouble()));
    m_Ui->dwellTimeLE->setText(QString::number(diParamsObj[ioConstants::DwellTime].toDouble()));
    m_Ui->binningCB->setCurrentIndex(diParamsObj[ioConstants::BinningMode].toInt());
    m_Ui->intensityScalingModeCB->setCurrentIndex(diParamsObj[ioConstants::IntensityScalingMode].toInt());
    m_Ui->gammaCorrectionFactorLE->setText(QString::number(diParamsObj[ioConstants::GammaCorrectionFactor].toDouble()));
    m_Ui->outputDataFileLE->setText(diParamsObj[ioConstants::OutputDataFile].toString());
    m_Ui->outputCtfFileLE->setText(diParamsObj[ioConstants::OutputCtfFile].toString());
    m_Ui->outputAngFileLE->setText(diParamsObj[ioConstants::OutputAngFile].toString());
    m_Ui->outputAvgCtfFileLE->setText(diParamsObj[ioConstants::OutputAvgCtfFile].toString());

    m_Ui->diViewer->readSession(diParamsObj);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::writeSession(QJsonObject& obj) const
{
  QJsonObject diParamsObj;

  diParamsObj[ioConstants::IndexingMode] = m_Ui->indexingModeCB->currentIndex();
  diParamsObj[ioConstants::EulerAngleFile] = m_Ui->eulerAngleFileLE->text();
  diParamsObj[ioConstants::DictionaryFile] = m_Ui->dictionaryFileLE->text();
  diParamsObj[ioConstants::MasterPatternFile] = m_Ui->masterFileLE->text();
  diParamsObj[ioConstants::IPFHeight] = m_Ui->ipfHeightLE->text().toInt();
  diParamsObj[ioConstants::IPFWidth] = m_Ui->ipfWidthLE->text().toInt();
  diParamsObj[ioConstants::UseROI] = m_Ui->roiCB->isChecked();
  diParamsObj[ioConstants::ROI_X] = m_Ui->roi1LE->text().toInt();
  diParamsObj[ioConstants::ROI_Y] = m_Ui->roi2LE->text().toInt();
  diParamsObj[ioConstants::ROI_W] = m_Ui->roi3LE->text().toInt();
  diParamsObj[ioConstants::ROI_H] = m_Ui->roi4LE->text().toInt();
  diParamsObj[ioConstants::SamplingStepSizeX] = m_Ui->samplingStepSizeXLE->text().toInt();
  diParamsObj[ioConstants::SamplingStepSizeY] = m_Ui->samplingStepSizeYLE->text().toInt();
  diParamsObj[ioConstants::ADPMatches] = m_Ui->nnkLE->text().toInt();
  diParamsObj[ioConstants::OrientationSimilarityMM] = m_Ui->nosmLE->text().toInt();
  diParamsObj[ioConstants::OrientationAveragingMM] = m_Ui->nnavLE->text().toInt();
  diParamsObj[ioConstants::IndexingSuccessMM] = m_Ui->nismLE->text().toInt();
  diParamsObj[ioConstants::IndexingSuccessThreshAngle] = m_Ui->isangleLE->text().toDouble();
  diParamsObj[ioConstants::UseCustomMask] = m_Ui->useCustomMaskCB->isChecked();
  diParamsObj[ioConstants::MaskFile] = m_Ui->maskFilePathLE->text();
  diParamsObj[ioConstants::MaskRadius] = m_Ui->maskRadiusLE->text().toDouble();
  diParamsObj[ioConstants::NumDictSingle] = m_Ui->numdictsingleLE->text().toInt();
  diParamsObj[ioConstants::NumExptSingle] = m_Ui->numexptsingleLE->text().toInt();
  diParamsObj[ioConstants::NumberOfThreads] = m_Ui->numOfThreadsLE->text().toInt();
  diParamsObj[ioConstants::GPUPlatform] = m_Ui->gpuPlatformCB->currentIndex();
  diParamsObj[ioConstants::GPUDevice] = m_Ui->gpuDeviceCB->currentIndex();
  diParamsObj[ioConstants::L] = m_Ui->LLE->text().toDouble();
  diParamsObj[ioConstants::CubochoricPoints] = m_Ui->cubochoricPointsLE->text().toInt();
  diParamsObj[ioConstants::CameraTiltAngle] = m_Ui->thetacLE->text().toDouble();
  diParamsObj[ioConstants::Delta] = m_Ui->deltaLE->text().toDouble();
  diParamsObj[ioConstants::NumsX] = m_Ui->numsxLE->text().toInt();
  diParamsObj[ioConstants::NumsY] = m_Ui->numsyLE->text().toInt();
  diParamsObj[ioConstants::PatternCenterX] = m_Ui->xpcLE->text().toDouble();
  diParamsObj[ioConstants::PatternCenterY] = m_Ui->ypcLE->text().toDouble();
  diParamsObj[ioConstants::Omega] = m_Ui->omegaLE->text().toDouble();
  diParamsObj[ioConstants::EnergyMin] = m_Ui->energyMinLE->text().toDouble();
  diParamsObj[ioConstants::EnergyMax] = m_Ui->energyMaxLE->text().toDouble();
  diParamsObj[ioConstants::EnergyAveragingMethod] = m_Ui->energyAveragingMethodCB->currentIndex();
  diParamsObj[ioConstants::SpatialAveragingMethod] = m_Ui->spatialAveragingCB->isChecked();
  diParamsObj[ioConstants::BeamCurrent] = m_Ui->beamCurrentLE->text().toDouble();
  diParamsObj[ioConstants::DwellTime] = m_Ui->dwellTimeLE->text().toDouble();
  diParamsObj[ioConstants::BinningMode] = m_Ui->binningCB->currentIndex();
  diParamsObj[ioConstants::IntensityScalingMode] = m_Ui->intensityScalingModeCB->currentIndex();
  diParamsObj[ioConstants::GammaCorrectionFactor] = m_Ui->gammaCorrectionFactorLE->text().toDouble();
  diParamsObj[ioConstants::OutputDataFile] = m_Ui->outputDataFileLE->text();
  diParamsObj[ioConstants::OutputCtfFile] = m_Ui->outputCtfFileLE->text();
  diParamsObj[ioConstants::OutputAngFile] = m_Ui->outputAngFileLE->text();
  diParamsObj[ioConstants::OutputAvgCtfFile] = m_Ui->outputAvgCtfFileLE->text();

  m_Ui->diViewer->writeSession(diParamsObj);

  obj[ioConstants::DIParams] = diParamsObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setInputType(InputType inputType)
{
  m_InputType = inputType;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setPatternDataFile(const QString &filePath)
{
  m_PatternDataFile = filePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setSelectedHDF5Path(const QStringList &path)
{
  m_SelectedHDF5Path = path;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setSelectedHipassValue(float value)
{
  m_SelectedHipassValue = value;

  m_Ui->ppHipassValueLabel->setText(QString::number(m_SelectedHipassValue));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setSelectedNumberOfRegions(int value)
{
  m_SelectedNumOfRegions = value;

  m_Ui->ppNumOfRegionsLabel->setText(QString::number(m_SelectedNumOfRegions));
}

// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setADPMap(const QImage& adpMap)
{
  m_ADPMap = adpMap;
  m_Ui->diViewer->loadImage(adpMap);
}
