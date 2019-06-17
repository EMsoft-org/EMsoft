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

#include "PatternPreprocessing_UI.h"

#include <QtConcurrent>

#include "Modules/DictionaryIndexingModule/Constants.h"
#include "Modules/DictionaryIndexingModule/ChoosePatternsDatasetDialog.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternPreprocessing_UI::PatternPreprocessing_UI(QWidget *parent)
: QWidget(parent)
, m_Ui(new Ui::PatternPreprocessing_UI())
{
  m_Ui->setupUi(this);

  m_PPMatrixController = new PatternPreprocessingController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternPreprocessing_UI::~PatternPreprocessing_UI()
{
  delete m_PPMatrixController;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setupGui()
{
  // Add limits to all spinboxes
  initializeSpinBoxLimits();

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
void PatternPreprocessing_UI::initializeSpinBoxLimits()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::createValidators()
{
  m_Ui->hipassValueLE->setValidator(new QDoubleValidator(m_Ui->hipassValueLE));

  m_Ui->hipassNumOfStepsLE->setValidator(new QIntValidator(m_Ui->hipassNumOfStepsLE));
  m_Ui->minNumOfRegionsLE->setValidator(new QIntValidator(m_Ui->minNumOfRegionsLE));
  m_Ui->maxNumOfRegionsLE->setValidator(new QIntValidator(m_Ui->maxNumOfRegionsLE));
  m_Ui->numOfRegionsStepSizeLE->setValidator(new QIntValidator(m_Ui->numOfRegionsStepSizeLE));
  m_Ui->patternHeightLE->setValidator(new QIntValidator(m_Ui->patternHeightLE));
  m_Ui->patternWidthLE->setValidator(new QIntValidator(m_Ui->patternWidthLE));
  m_Ui->ipfHeightLE->setValidator(new QIntValidator(m_Ui->ipfHeightLE));
  m_Ui->ipfWidthLE->setValidator(new QIntValidator(m_Ui->ipfWidthLE));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::createModificationConnections()
{
  // Line Edits
  connect(m_Ui->patternHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->patternWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->hipassValueLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->hipassNumOfStepsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->minNumOfRegionsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->maxNumOfRegionsLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->numOfRegionsStepSizeLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::createWidgetConnections()
{
  connect(m_Ui->generatePPMatrixBtn, &QPushButton::clicked, this, &PatternPreprocessing_UI::listenPatternPreprocessingStarted);

  connect(m_PPMatrixController, &PatternPreprocessingController::errorMessageGenerated, this, &PatternPreprocessing_UI::errorMessageGenerated);
  connect(m_PPMatrixController, &PatternPreprocessingController::warningMessageGenerated, this, &PatternPreprocessing_UI::warningMessageGenerated);
  connect(m_PPMatrixController, &PatternPreprocessingController::stdOutputMessageGenerated, this, &PatternPreprocessing_UI::stdOutputMessageGenerated);
  connect(m_PPMatrixController, &PatternPreprocessingController::preprocessedPatternsMatrixCreated, [=] (QImage image) {
    auto matrixData = getPPMatrixData();
    m_Ui->ppMatrixViewer->loadImage(image, matrixData.hipassValue, matrixData.hipassNumSteps);
  });

  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::errorMessageGenerated, this, &PatternPreprocessing_UI::errorMessageGenerated);
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::zoomFactorChanged, this, &PatternPreprocessing_UI::updateZoomFactor);
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::selectedHipassNumOfStepsChanged, this, &PatternPreprocessing_UI::selectedHipassNumOfStepsChanged);
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::selectedHipassValueChanged, this, &PatternPreprocessing_UI::selectedHipassValueChanged);

  connect(m_Ui->ppMatrixZoomInBtn, &QPushButton::clicked, m_Ui->ppMatrixViewer, &PPMatrixImageViewer::zoomIn);
  connect(m_Ui->ppMatrixZoomOutBtn, &QPushButton::clicked, m_Ui->ppMatrixViewer, &PPMatrixImageViewer::zoomOut);
  connect(m_Ui->ppMatrixFitToScreenBtn, &QPushButton::clicked, m_Ui->ppMatrixViewer, &PPMatrixImageViewer::fitToScreen);
  connect(m_Ui->ppMatrixZoomSB, QOverload<int>::of(&QSpinBox::valueChanged), [=] (int value) { m_Ui->ppMatrixViewer->setZoomFactor(value / 100.0f); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::updateZoomFactor(float zoomFactor)
{
  m_Ui->ppMatrixZoomSB->blockSignals(true);
  m_Ui->ppMatrixZoomSB->setValue(zoomFactor * 100);
  m_Ui->ppMatrixZoomSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenPatternPreprocessingStarted()
{
  if(m_Ui->generatePPMatrixBtn->text() == "Cancel")
  {
    m_PPMatrixController->setCancel(true);
    emit patternPreprocessingFinished();
    return;
  }

  PatternPreprocessingController::PPMatrixData data = getPPMatrixData();

  m_Ui->generatePPMatrixBtn->setText("Cancel");
  m_Ui->ppParametersGroupBox->setDisabled(true);

  // Single-threaded for now, but we can multi-thread later if needed
  //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
  for(int i = 0; i < 1; i++)
  {
    m_PPMatrixWatcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
    connect(m_PPMatrixWatcher.data(), SIGNAL(finished()), this, SLOT(listenPatternPreprocessingFinished()));

    QFuture<void> future = QtConcurrent::run(m_PPMatrixController, &PatternPreprocessingController::createPreprocessedPatternsMatrix, data);
    m_PPMatrixWatcher->setFuture(future);
  }

  emit patternPreprocessingStarted();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenPatternPreprocessingFinished()
{
  m_PPMatrixController->setCancel(false);

  m_Ui->generatePPMatrixBtn->setText("Generate");
  m_Ui->ppParametersGroupBox->setEnabled(true);

  if (!m_Ui->ppMatrixViewer->getCurrentImage().isNull())
  {
    m_Ui->ppMatrixZoomSB->setEnabled(true);
    m_Ui->ppMatrixSaveBtn->setEnabled(true);
    m_Ui->ppMatrixZoomInBtn->setEnabled(true);
    m_Ui->ppMatrixZoomOutBtn->setEnabled(true);
    m_Ui->ppMatrixFitToScreenBtn->setEnabled(true);
  }

  emit patternPreprocessingFinished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternPreprocessing_UI::validateData()
{
  PatternPreprocessingController::PPMatrixData ppData = getPPMatrixData();
  if(!m_PPMatrixController->validatePPValues(ppData))
  {
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(m_Ui->hipassValueLE->text().isEmpty())
  {
    emit errorMessageGenerated("The 'Hipass Value' field is empty.");
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(m_Ui->hipassNumOfStepsLE->text().isEmpty())
  {
    emit errorMessageGenerated("The 'Hipass Number of Steps' field is empty.");
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(m_Ui->minNumOfRegionsLE->text().isEmpty())
  {
    emit errorMessageGenerated("The 'Min Number of Regions' field is empty.");
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(m_Ui->maxNumOfRegionsLE->text().isEmpty())
  {
    emit errorMessageGenerated("The 'Max Number of Regions' field is empty.");
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(m_Ui->numOfRegionsStepSizeLE->text().isEmpty())
  {
    emit errorMessageGenerated("The 'Number of Regions Step Size' field is empty.");
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  m_Ui->generatePPMatrixBtn->setEnabled(true);
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternPreprocessingController::PPMatrixData PatternPreprocessing_UI::getPPMatrixData()
{
  PatternPreprocessingController::PPMatrixData data;
  data.patternHeight = m_Ui->patternHeightLE->text().toInt();
  data.patternWidth = m_Ui->patternWidthLE->text().toInt();
  data.ipfHeight = m_Ui->ipfHeightLE->text().toInt();
  data.ipfWidth = m_Ui->ipfWidthLE->text().toInt();
  data.hipassValue = m_Ui->hipassValueLE->text().toFloat();
  data.hipassNumSteps = m_Ui->hipassNumOfStepsLE->text().toInt();
  data.minNumOfRegions = m_Ui->minNumOfRegionsLE->text().toInt();
  data.maxNumOfRegions = m_Ui->maxNumOfRegionsLE->text().toInt();
  data.numOfRegionsStepSize = m_Ui->numOfRegionsStepSizeLE->text().toInt();
  data.patternDataFile = m_PatternDataFile;
  data.inputType = m_InputType;
  data.hdfStrings = m_SelectedHDF5Path;
  data.patternCoordinateX = m_SelectedADPPatternPixel.x();
  data.patternCoordinateY = m_SelectedADPPatternPixel.y();

  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setInputType(ADPMapController::InputType inputType)
{
  m_InputType = inputType;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setPatternDataFile(const QString &filePath)
{
  m_PatternDataFile = filePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setSelectedHDF5Path(const QStringList &path)
{
  m_SelectedHDF5Path = path;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setSelectedADPPatternPixel(const QPoint &pixel)
{
  m_SelectedADPPatternPixel = pixel;

  m_Ui->ppPatternCoordLabel->setText(tr("(%1, %2)").arg(QString::number(m_SelectedADPPatternPixel.x()), QString::number(m_SelectedADPPatternPixel.y())));
}
