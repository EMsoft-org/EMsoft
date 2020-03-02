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

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;

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

  m_Ui->ppInstructionsLabel->hide();
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

  m_Ui->hipassNumOfStepsLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->hipassNumOfStepsLE));
  m_Ui->minNumOfRegionsLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->minNumOfRegionsLE));
  m_Ui->maxNumOfRegionsLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->maxNumOfRegionsLE));
  m_Ui->numOfRegionsStepSizeLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numOfRegionsStepSizeLE));
  m_Ui->patternHeightLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->patternHeightLE));
  m_Ui->patternWidthLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->patternWidthLE));
  m_Ui->ipfHeightLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfHeightLE));
  m_Ui->ipfWidthLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfWidthLE));

  m_Ui->ppMatrixZoomSB->setMaximum(std::numeric_limits<int>::max());
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

  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::errorMessageGenerated, this, &PatternPreprocessing_UI::errorMessageGenerated);
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::zoomFactorChanged, this, &PatternPreprocessing_UI::updateZoomFactor);
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::selectedHipassNumOfRegionsChanged, [=] (int numOfRegions) {
    m_Ui->selectedNumOfRegionsLabel->setText(tr("Selected Num of Regions: %1").arg(numOfRegions));
    emit selectedHipassNumOfRegionsChanged(numOfRegions);
  });
  connect(m_Ui->ppMatrixViewer, &PPMatrixImageViewer::selectedHipassValueChanged, [=] (float hipassValue) {
    m_Ui->selectedHipassValueLabel->setText(tr("Selected Hipass Value: %1").arg(hipassValue));
    emit selectedHipassValueChanged(hipassValue);
  });

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
void PatternPreprocessing_UI::listenInputTypeChanged(EMsoftWorkbenchConstants::InputType inputType)
{
  setInputType(inputType);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenPatternDataFileChanged(const QString &filePath)
{
  m_Ui->patternDataFileLabel->setText(filePath);

  setPatternDataFile(filePath);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenSelectedPatternDatasetChanged(QStringList patternDSetPaths)
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
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenPatternPreprocessingStarted()
{
  if(m_Ui->generatePPMatrixBtn->text() == "Cancel")
  {
    m_PPMatrixController->cancel();
    emit patternPreprocessingFinished();
    return;
  }

  PatternPreprocessingController::InputDataType data = getPPMatrixData();

  m_Ui->generatePPMatrixBtn->setText("Cancel");
  m_Ui->ppParametersGroupBox->setDisabled(true);

  // Clear out the previous (if any) controller instance
  if(m_PPMatrixController != nullptr)
  {
    delete m_PPMatrixController;
    m_PPMatrixController = nullptr;
  }

  // Create a new QThread to run the Controller class.
  m_WorkerThread = QSharedPointer<QThread>(new QThread());
  m_PPMatrixController = new PatternPreprocessingController;
  m_PPMatrixController->moveToThread(m_WorkerThread.data());
  m_PPMatrixController->setData(data); // Set the input data

  // Conncet Signals & Slots to get the thread started and quit
  connect(m_WorkerThread.data(), SIGNAL(started()), m_PPMatrixController, SLOT(execute()));
  connect(m_PPMatrixController, SIGNAL(finished()), m_WorkerThread.data(), SLOT(quit()));
  connect(m_WorkerThread.data(), SIGNAL(finished()), this, SLOT(processFinished()));

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_PPMatrixController, &PatternPreprocessingController::errorMessageGenerated, this, &PatternPreprocessing_UI::errorMessageGenerated);
  connect(m_PPMatrixController, &PatternPreprocessingController::warningMessageGenerated, this, &PatternPreprocessing_UI::warningMessageGenerated);
  connect(m_PPMatrixController, SIGNAL(stdOutputMessageGenerated(QString)), this, SIGNAL(stdOutputMessageGenerated(QString)));
  connect(m_PPMatrixController, SIGNAL(preprocessedPatternsMatrixCreated(QImage)), this, SLOT(listenMatrixCreated(QImage)));

  //  connect(m_DIController, SIGNAL(updateMCProgress(int, int, float)), this, SLOT(updateMCProgress(int, int, float)));

  m_WorkerThread->start();

  emit patternPreprocessingStarted();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::listenMatrixCreated(QImage image)
{
  PatternPreprocessingController::InputDataType matrixData = getPPMatrixData();
  m_Ui->ppMatrixViewer->loadImage(image, matrixData.hipassValue, matrixData.hipassNumSteps);
  m_Ui->ppInstructionsLabel->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::processFinished()
{
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
  PatternPreprocessingController::InputDataType ppData = getPPMatrixData();

  if (ppData.patternCoordinateX < 0 || ppData.patternCoordinateY < 0)
  {
    QString errMsg = "The 'Chosen ADP Coordinate' field is invalid.  Please double-click inside "
                     "the average dot product map generated in the 'Average Dot Product Map' tab to "
                     "choose a coordinate.";
    emit errorMessageGenerated(errMsg);
    m_Ui->generatePPMatrixBtn->setDisabled(true);
    return false;
  }

  if(ppData.inputType == InputType::TSLHDF || ppData.inputType == InputType::BrukerHDF ||
     ppData.inputType == InputType::OxfordHDF)
  {
    if(m_PatternDataFile.isEmpty())
    {
      QString ss = QObject::tr("Pattern data file is empty.  Please select a pattern data file from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generatePPMatrixBtn->setDisabled(true);
      return false;
    }
    if(m_SelectedHDF5Path.isEmpty())
    {
      QString ss = QObject::tr("Pattern dataset not chosen.  Please select a pattern dataset from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generatePPMatrixBtn->setDisabled(true);
      return false;
    }
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
PatternPreprocessingController::InputDataType PatternPreprocessing_UI::getPPMatrixData()
{
  PatternPreprocessingController::InputDataType data;
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
void PatternPreprocessing_UI::readSession(const QJsonObject &obj)
{
  QJsonObject ppParamsObj = obj[ioConstants::PPParameters].toObject();

  if(!ppParamsObj.isEmpty())
  {
    m_Ui->patternHeightLE->setText(QString::number(ppParamsObj[ioConstants::PatternHeight].toInt()));
    m_Ui->patternWidthLE->setText(QString::number(ppParamsObj[ioConstants::PatternWidth].toInt()));
    m_Ui->ipfHeightLE->setText(QString::number(ppParamsObj[ioConstants::IPFHeight].toInt()));
    m_Ui->ipfWidthLE->setText(QString::number(ppParamsObj[ioConstants::IPFWidth].toInt()));
    m_Ui->hipassValueLE->setText(QString::number(ppParamsObj[ioConstants::HipassValue].toDouble()));
    m_Ui->hipassNumOfStepsLE->setText(QString::number(ppParamsObj[ioConstants::HipassNumOfSteps].toInt()));
    m_Ui->minNumOfRegionsLE->setText(QString::number(ppParamsObj[ioConstants::MinNumOfRegions].toInt()));
    m_Ui->maxNumOfRegionsLE->setText(QString::number(ppParamsObj[ioConstants::MaxNumOfRegions].toInt()));
    m_Ui->numOfRegionsStepSizeLE->setText(QString::number(ppParamsObj[ioConstants::NumOfRegionsStepSize].toInt()));

    m_Ui->ppMatrixViewer->readSession(ppParamsObj);

    if(validateData())
    {
      listenPatternPreprocessingStarted();
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::writeSession(QJsonObject& obj) const
{
  QJsonObject ppParamsObj;

  ppParamsObj[ioConstants::PatternHeight] = m_Ui->patternHeightLE->text().toInt();
  ppParamsObj[ioConstants::PatternWidth] = m_Ui->patternWidthLE->text().toInt();
  ppParamsObj[ioConstants::IPFHeight] = m_Ui->ipfHeightLE->text().toInt();
  ppParamsObj[ioConstants::IPFWidth] = m_Ui->ipfWidthLE->text().toInt();
  ppParamsObj[ioConstants::HipassValue] = m_Ui->hipassValueLE->text().toDouble();
  ppParamsObj[ioConstants::HipassNumOfSteps] = m_Ui->hipassNumOfStepsLE->text().toInt();
  ppParamsObj[ioConstants::MinNumOfRegions] = m_Ui->minNumOfRegionsLE->text().toInt();
  ppParamsObj[ioConstants::MaxNumOfRegions] = m_Ui->maxNumOfRegionsLE->text().toInt();
  ppParamsObj[ioConstants::NumOfRegionsStepSize] = m_Ui->numOfRegionsStepSizeLE->text().toInt();
  m_Ui->ppMatrixViewer->writeSession(ppParamsObj);

  obj[ioConstants::PPParameters] = ppParamsObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessing_UI::setInputType(EMsoftWorkbenchConstants::InputType inputType)
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
