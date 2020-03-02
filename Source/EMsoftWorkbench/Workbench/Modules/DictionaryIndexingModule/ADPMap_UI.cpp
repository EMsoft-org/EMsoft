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

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;
using InputType = EMsoftWorkbenchConstants::InputType;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMap_UI::ADPMap_UI(QWidget *parent)
: QWidget(parent)
, m_Ui(new Ui::ADPMap_UI())
{
  m_Ui->setupUi(this);

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

  m_Ui->numOfThreadsLabel->setToolTip(tr("Number of Threads must be between 1 and %1").arg(QThreadPool::globalInstance()->maxThreadCount()));
  m_Ui->numOfThreadsLE->setToolTip(tr("Number of Threads must be between 1 and %1").arg(QThreadPool::globalInstance()->maxThreadCount()));

  m_Ui->adpMapInstructionsLabel->hide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::createValidators()
{
  m_Ui->maskRadiusLE->setValidator(new QDoubleValidator(m_Ui->maskRadiusLE));
  m_Ui->hipassLE->setValidator(new QDoubleValidator(m_Ui->hipassLE));

  m_Ui->patternHeightLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->patternHeightLE));
  m_Ui->patternWidthLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->patternWidthLE));
  m_Ui->roi1LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi1LE));
  m_Ui->roi2LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi2LE));
  m_Ui->roi3LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi3LE));
  m_Ui->roi4LE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->roi4LE));
//  m_Ui->binningFactorLE->setValidator(new QIntValidator(m_Ui->binningFactorLE));
//  m_Ui->binningXLE->setValidator(new QIntValidator(m_Ui->binningXLE));
//  m_Ui->binningYLE->setValidator(new QIntValidator(m_Ui->binningYLE));
  m_Ui->ipfWidthLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfWidthLE));
  m_Ui->ipfHeightLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->ipfHeightLE));
//  m_Ui->maskPatternLE->setValidator(new QIntValidator(m_Ui->maskPatternLE));
  m_Ui->numOfRegionsLE->setValidator(new QIntValidator(1, std::numeric_limits<int>::max(), m_Ui->numOfRegionsLE));
  m_Ui->numOfThreadsLE->setValidator(new QIntValidator(1, QThreadPool::globalInstance()->maxThreadCount(), m_Ui->numOfThreadsLE));

  //  m_Ui->adpMapZoomSB->setMaximum(std::numeric_limits<int>::max());
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
//  connect(m_Ui->binningFactorLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
//  connect(m_Ui->binningXLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
//  connect(m_Ui->binningYLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfWidthLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->ipfHeightLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
//  connect(m_Ui->maskPatternLE, &QLineEdit::textChanged, [=] { emit parametersChanged(); });
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

  connect(m_Ui->adpViewer, &ADPMapImageViewer::errorMessageGenerated, this, &ADPMap_UI::errorMessageGenerated);
  // connect(m_Ui->adpViewer, &ADPMapImageViewer::zoomFactorChanged, this, &ADPMap_UI::updateZoomFactor);
  connect(m_Ui->adpViewer, &ADPMapImageViewer::selectedADPCoordinateChanged, this, &ADPMap_UI::listenSelectedADPCoordinateChanged);

  connect(m_Ui->adpMapZoomInBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::zoomIn);
  connect(m_Ui->adpMapZoomOutBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::zoomOut);
  connect(m_Ui->adpMapFitToScreenBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::fitToScreen);
  //  connect(m_Ui->adpMapZoomSB, QOverload<int>::of(&QSpinBox::valueChanged), [=](int value) { m_Ui->adpViewer->setZoomFactor(value / 100.0f); });

  connect(m_Ui->adpMapSaveBtn, &QPushButton::clicked, m_Ui->adpViewer, &ADPMapImageViewer::saveImage);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::updateZoomFactor(float zoomFactor)
{
  //  m_Ui->adpMapZoomSB->blockSignals(true);
  //  m_Ui->adpMapZoomSB->setValue(zoomFactor * 100);
  //  m_Ui->adpMapZoomSB->blockSignals(false);
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
void ADPMap_UI::listenSelectedADPCoordinateChanged(const QPoint& pixel)
{
  m_Ui->adpMapSelectedPixelLabel->setText(tr("Selected Pixel: (%1, %2)").arg(QString::number(pixel.x()), QString::number(pixel.y())));
  emit selectedADPCoordinateChanged(pixel);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenInputTypeChanged(EMsoftWorkbenchConstants::InputType inputType)
{
  setInputType(inputType);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenPatternDataFileChanged(const QString &filePath)
{
  m_Ui->patternDataFileLabel->setText(filePath);

  setPatternDataFile(filePath);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenSelectedPatternDatasetChanged(const QStringList& patternDSetPaths)
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
void ADPMap_UI::listenADPMapCreated(const QImage& adpMap)
{
  m_Ui->adpViewer->loadImage(adpMap);
  emit adpMapCreated(adpMap);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::listenADPGenerationStarted()
{
  if(m_Ui->generateADPBtn->text() == "Cancel" && m_ADPController != nullptr)
  {
    m_ADPController->cancel();
    emit adpMapGenerationFinished();
    return;
  }

  ADPMapController::InputDataType data = getADPMapData();

  m_Ui->generateADPBtn->setText("Cancel");
  m_Ui->adpParametersGroupBox->setDisabled(true);

  if(m_ADPController != nullptr)
  {
    delete m_ADPController;
    m_ADPController = nullptr;
  }
  m_Thread = QSharedPointer<QThread>(new QThread());
  m_ADPController = new ADPMapController;
  m_ADPController->moveToThread(m_Thread.data());
  m_ADPController->setData(data);
  connect(m_Thread.data(), SIGNAL(started()), m_ADPController, SLOT(execute()));
  connect(m_ADPController, SIGNAL(finished()), m_Thread.data(), SLOT(quit()));
  connect(m_Thread.data(), SIGNAL(finished()), this, SLOT(processFinished()));

  connect(m_ADPController, SIGNAL(adpMapCreated(const QImage&)), this, SLOT(listenADPMapCreated(const QImage&)));
  connect(m_ADPController, SIGNAL(errorMessageGenerated(QString)), this, SIGNAL(errorMessageGenerated(QString)));
  connect(m_ADPController, SIGNAL(warningMessageGenerated(QString)), this, SIGNAL(warningMessageGenerated(QString)));
  connect(m_ADPController, SIGNAL(stdOutputMessageGenerated(QString)), this, SIGNAL(stdOutputMessageGenerated(QString)));

  m_Thread->start();

#if 0
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
#endif
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::processFinished()
{
  m_Ui->adpMapZoomSB->setEnabled(true);
  m_Ui->adpMapSaveBtn->setEnabled(true);
  m_Ui->adpMapZoomInBtn->setEnabled(true);
  m_Ui->adpMapZoomOutBtn->setEnabled(true);
  m_Ui->adpMapFitToScreenBtn->setEnabled(true);

  m_Ui->generateADPBtn->setText("Generate");
  m_Ui->adpParametersGroupBox->setEnabled(true);

  m_Ui->adpMapInstructionsLabel->show();

  emit adpMapGenerationFinished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ADPMap_UI::validateData()
{
  ADPMapController::InputDataType adpData = getADPMapData();

  if(adpData.inputType == InputType::TSLHDF || adpData.inputType == InputType::BrukerHDF ||
     adpData.inputType == InputType::OxfordHDF)
  {
    if(m_PatternDataFile.isEmpty())
    {
      QString ss = QObject::tr("Pattern data file is empty.  Please select a pattern data file from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generateADPBtn->setDisabled(true);
      return false;
    }
    if(m_SelectedHDF5Path.isEmpty())
    {
      QString ss = QObject::tr("Pattern dataset not chosen.  Please select a pattern dataset from the 'Choose Patterns' tab.");
      emit errorMessageGenerated(ss);
      m_Ui->generateADPBtn->setDisabled(true);
      return false;
    }
  }

  m_Ui->generateADPBtn->setEnabled(true);
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapController::InputDataType ADPMap_UI::getADPMapData()
{
  ADPMapController::InputDataType data;
  data.roi_1 = m_Ui->roi1LE->text().toInt();
  data.roi_2 = m_Ui->roi2LE->text().toInt();
  data.roi_3 = m_Ui->roi3LE->text().toInt();
  data.roi_4 = m_Ui->roi4LE->text().toInt();
  data.useROI = m_Ui->roiCB->isChecked();
//  data.binningX = m_Ui->binningXLE->text().toInt();
//  data.binningY = m_Ui->binningYLE->text().toInt();
  data.ipfWidth = m_Ui->ipfWidthLE->text().toInt();
  data.inputType = m_InputType;
  data.ipfHeight = m_Ui->ipfHeightLE->text().toInt();
  data.maskRadius = m_Ui->maskRadiusLE->text().toDouble();
//  data.maskPattern = m_Ui->maskPatternLE->text().toInt();
  data.hipassFilter = m_Ui->hipassLE->text().toDouble();
  data.numOfRegions = m_Ui->numOfRegionsLE->text().toInt();
  data.numOfThreads = m_Ui->numOfThreadsLE->text().toInt();
  data.patternWidth = m_Ui->patternWidthLE->text().toInt();
//  data.binningFactor = m_Ui->binningFactorLE->text().toInt();
  data.patternHeight = m_Ui->patternHeightLE->text().toInt();
  data.patternDataFile = m_PatternDataFile;
  data.hdfStrings = m_SelectedHDF5Path;
  data.inputType = m_InputType;
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::readSession(const QJsonObject &obj)
{
  QJsonObject adpMapParamsObj = obj[ioConstants::ADPMapParams].toObject();

  if(!adpMapParamsObj.isEmpty())
  {
    m_Ui->patternHeightLE->setText(QString::number(adpMapParamsObj[ioConstants::PatternHeight].toInt()));
    m_Ui->patternWidthLE->setText(QString::number(adpMapParamsObj[ioConstants::PatternWidth].toInt()));
    m_Ui->roiCB->setChecked(adpMapParamsObj[ioConstants::UseROI].toBool());
    m_Ui->roi1LE->setText(QString::number(adpMapParamsObj[ioConstants::ROI_X].toInt()));
    m_Ui->roi2LE->setText(QString::number(adpMapParamsObj[ioConstants::ROI_Y].toInt()));
    m_Ui->roi3LE->setText(QString::number(adpMapParamsObj[ioConstants::ROI_W].toInt()));
    m_Ui->roi4LE->setText(QString::number(adpMapParamsObj[ioConstants::ROI_H].toInt()));
    m_Ui->ipfHeightLE->setText(QString::number(adpMapParamsObj[ioConstants::IPFHeight].toInt()));
    m_Ui->ipfWidthLE->setText(QString::number(adpMapParamsObj[ioConstants::IPFWidth].toInt()));
    m_Ui->maskRadiusLE->setText(QString::number(adpMapParamsObj[ioConstants::MaskRadius].toDouble()));
    m_Ui->hipassLE->setText(QString::number(adpMapParamsObj[ioConstants::HipassFilter].toDouble()));
    m_Ui->numOfRegionsLE->setText(QString::number(adpMapParamsObj[ioConstants::NumberOfRegions].toInt()));
    m_Ui->numOfThreadsLE->setText(QString::number(adpMapParamsObj[ioConstants::NumberOfThreads].toInt()));

    m_Ui->adpViewer->readSession(adpMapParamsObj);

    if(validateData())
    {
      listenADPGenerationStarted();
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::writeSession(QJsonObject& obj) const
{
  QJsonObject adpMapParamsObj;

  adpMapParamsObj[ioConstants::PatternHeight] = m_Ui->patternHeightLE->text().toInt();
  adpMapParamsObj[ioConstants::PatternWidth] = m_Ui->patternWidthLE->text().toInt();
  adpMapParamsObj[ioConstants::UseROI] = m_Ui->roiCB->isChecked();
  adpMapParamsObj[ioConstants::ROI_X] = m_Ui->roi1LE->text().toInt();
  adpMapParamsObj[ioConstants::ROI_Y] = m_Ui->roi2LE->text().toInt();
  adpMapParamsObj[ioConstants::ROI_W] = m_Ui->roi3LE->text().toInt();
  adpMapParamsObj[ioConstants::ROI_H] = m_Ui->roi4LE->text().toInt();
  adpMapParamsObj[ioConstants::IPFHeight] = m_Ui->ipfHeightLE->text().toInt();
  adpMapParamsObj[ioConstants::IPFWidth] = m_Ui->ipfWidthLE->text().toInt();
  adpMapParamsObj[ioConstants::MaskRadius] = m_Ui->maskRadiusLE->text().toDouble();
  adpMapParamsObj[ioConstants::HipassFilter] = m_Ui->hipassLE->text().toDouble();
  adpMapParamsObj[ioConstants::NumberOfRegions] = m_Ui->numOfRegionsLE->text().toInt();
  adpMapParamsObj[ioConstants::NumberOfThreads] = m_Ui->numOfThreadsLE->text().toInt();
  m_Ui->adpViewer->writeSession(adpMapParamsObj);

  obj[ioConstants::ADPMapParams] = adpMapParamsObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMap_UI::setInputType(EMsoftWorkbenchConstants::InputType inputType)
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
