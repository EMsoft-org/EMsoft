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

#include "PatternDisplay_UI.h"

#include <QtCore/QDebug>

#include <QtGui/QScreen>

#include <QtWidgets/QFileDialog>

#include "EMsoftApplication.h"

#include "Modules/PatternDisplayModule/MPMCDisplayWidget.h"

#include "Common/Constants.h"
#include "Common/FileIOTools.h"

#include "QtSupport/QtSSettings.h"

#include "QtSupport/QtSSettings.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplay_UI::PatternDisplay_UI(QWidget* parent)
: IModuleUI(parent)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::setupGui()
{
  createWidgetConnections();

  connect(m_Controller.get(), SIGNAL(mpmcGenerationFinished()), this, SLOT(resetDisplayWidgets()));

  connect(m_SingleAngleWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_AngleReaderWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_SamplingRateWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_SampleCubochoricSpaceWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));

  // Set the combo box to the first widget
  on_angleTypeCB_currentIndexChanged(0);

  createValidators();

  createModificationConnections();

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::createValidators() const
{
  QDoubleValidator* dblValidator = new QDoubleValidator(scintillatorDist);
  scintillatorDist->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(detectorTiltAngle);
  detectorTiltAngle->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(detectorOmegaAngle);
  detectorOmegaAngle->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(scintillatorPixSize);
  scintillatorPixSize->setValidator(dblValidator);

  QIntValidator* intValidator = new QIntValidator(numOfPixelsX);
  numOfPixelsX->setValidator(intValidator);

  intValidator = new QIntValidator(numOfPixelsY);
  numOfPixelsY->setValidator(intValidator);

  dblValidator = new QDoubleValidator(patternCenterX);
  patternCenterX->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(patternCenterY);
  patternCenterY->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(pixelCoordinateX);
  pixelCoordinateX->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(pixelCoordinateY);
  pixelCoordinateY->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(samplingStepSizeX);
  samplingStepSizeX->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(samplingStepSizeY);
  samplingStepSizeY->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(beamCurrent);
  beamCurrent->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(dwellTime);
  dwellTime->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(barrelDistortion);
  barrelDistortion->setValidator(dblValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::unique_ptr<SimulatedPatternDisplayWidget> PatternDisplay_UI::createPatternDisplayWidget()
{
  std::unique_ptr<SimulatedPatternDisplayWidget> widget = std::make_unique<SimulatedPatternDisplayWidget>();

  connect(widget.get(), &SimulatedPatternDisplayWidget::generationStarted, [=] { setRunning(true); });

  connect(widget.get(), &SimulatedPatternDisplayWidget::generationFinished, [=] { setRunning(false); });

  m_Controller->setPatternDisplayWidget(widget.get());

  connect(m_Controller.get(), SIGNAL(newProgressBarMaximumValue(int)), widget.get(), SLOT(setProgressBarMaximum(int)));
  connect(m_Controller.get(), SIGNAL(newProgressBarValue(int)), widget.get(), SLOT(setProgressBarValue(int)), Qt::QueuedConnection);
  connect(m_Controller.get(), SIGNAL(patternGenerationFinished()), widget.get(), SLOT(patternGenerationFinished()));
  connect(widget.get(), SIGNAL(cancelRequested()), m_Controller.get(), SLOT(cancelGeneration()));
  connect(widget.get(), SIGNAL(patternNeedsPriority(size_t)), m_Controller.get(), SLOT(addPriorityIndex(size_t)));

  connect(widget.get(), SIGNAL(dataChanged(SimulatedPatternDisplayWidget::PatternDisplayData)), this, SLOT(generateEBSDPatternImage(SimulatedPatternDisplayWidget::PatternDisplayData)));

  return widget;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::createWidgetConnections() const
{
  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller.get(), &PatternDisplayController::errorMessageGenerated, this, &PatternDisplay_UI::notifyErrorMessage);
  connect(m_Controller.get(), &PatternDisplayController::warningMessageGenerated, this, &PatternDisplay_UI::notifyWarningMessage);
  connect(m_Controller.get(), SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  // Connections to display messages to the PatternDisplay_UI status bar.  These messages also go in the log.
  connect(masterPatternDisplayWidget, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));
  connect(monteCarloDisplayWidget, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  // Connections to display master pattern and monte carlo images in their respective image viewers
  connect(m_Controller.get(), SIGNAL(mpImageNeedsDisplayed(PatternImageViewer::ImageData)), masterPatternDisplayWidget, SLOT(loadImage(PatternImageViewer::ImageData)));
  connect(m_Controller.get(), SIGNAL(mcImageNeedsDisplayed(PatternImageViewer::ImageData)), monteCarloDisplayWidget, SLOT(loadImage(PatternImageViewer::ImageData)));

  // Connections to allow the controller to tell the image viewers that the image spin box range has changed
  connect(m_Controller.get(), SIGNAL(imageRangeChanged(int, int)), masterPatternDisplayWidget, SLOT(setEnergyBinSpinBoxRange(int, int)));
  connect(m_Controller.get(), SIGNAL(imageRangeChanged(int, int)), monteCarloDisplayWidget, SLOT(setEnergyBinSpinBoxRange(int, int)));

  // Connections to allow the controller to tell the workbench parameters section that it has new ekeVs values
  connect(m_Controller.get(), SIGNAL(minMaxEnergyLevelsChanged(std::vector<float>)), this, SLOT(setMinAndMaxEnergyLevelChoices(std::vector<float>)));

  // Connections to allow the image viewers to request a new image from the controller
  connect(masterPatternDisplayWidget, SIGNAL(controlsChanged(MPMCDisplayWidget::MPMCData)), m_Controller.get(), SLOT(updateMPImage(MPMCDisplayWidget::MPMCData)));
  connect(monteCarloDisplayWidget, SIGNAL(controlsChanged(MPMCDisplayWidget::MPMCData)), m_Controller.get(), SLOT(updateMCImage(MPMCDisplayWidget::MPMCData)));

  // Connection to allow the PatternDisplay_UI to tell the controller that it needs to generate new pattern images
  connect(this, SIGNAL(patternNeedsGenerated(SimulatedPatternDisplayWidget::PatternDisplayData, PatternDisplayController::DetectorData)), m_Controller.get(),
          SLOT(generatePatternImages(SimulatedPatternDisplayWidget::PatternDisplayData, PatternDisplayController::DetectorData)));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::createModificationConnections()
{
  // Line Edits
  connect(scintillatorDist, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(detectorTiltAngle, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(detectorOmegaAngle, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(scintillatorPixSize, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(numOfPixelsX, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(numOfPixelsY, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(patternCenterX, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(patternCenterY, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(pixelCoordinateX, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(pixelCoordinateY, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(samplingStepSizeX, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(samplingStepSizeY, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(beamCurrent, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(dwellTime, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(barrelDistortion, &QLineEdit::textEdited, [=] { parametersChanged(); });

  // Tab Widget
  connect(patternDisplayTabWidget, &QTabWidget::currentChanged, [=] { parametersChanged(); });

  // Combo Boxes
  connect(angleTypeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });
  connect(energyMinCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });
  connect(energyMaxCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });

  // Angle Widgets
  m_SingleAngleWidget->createModificationConnections(this);
  m_AngleReaderWidget->createModificationConnections(this);
  m_SamplingRateWidget->createModificationConnections(this);
  m_SampleCubochoricSpaceWidget->createModificationConnections(this);

  // Display Widgets
  masterPatternDisplayWidget->createConnections(this);
  monteCarloDisplayWidget->createConnections(this);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::setGenerateButtonAvailability(bool value) const
{
  generateBtn->setEnabled(value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::setMinAndMaxEnergyLevelChoices(const std::vector<float>& ekeVs) const
{
  energyMinCB->clear();
  energyMaxCB->clear();

  QStringList energies;
  for(float energyLevelFloat : ekeVs)
  {
    int energyLevel = static_cast<int>(energyLevelFloat);
    energyMinCB->addItem(QString::number(energyLevel));
    energyMaxCB->addItem(QString::number(energyLevel));
  }

  // Set the max combo box to the last energy level
  energyMaxCB->setCurrentIndex(energyMaxCB->count() - 1);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::generateEBSDPatternImage(SimulatedPatternDisplayWidget::PatternDisplayData patternData) const
{
  PatternDisplayController::DetectorData detectorData = getDetectorData();
  patternData.angles = m_CurrentAngleWidget->getEulerAngles();

  emit patternNeedsGenerated(patternData, detectorData);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplayController::DetectorData PatternDisplay_UI::getDetectorData() const
{
  PatternDisplayController::DetectorData detectorData;
  detectorData.barrelDistortion = barrelDistortion->text().toDouble();
  detectorData.beamCurrent = beamCurrent->text().toDouble();
  detectorData.detectorOmegaAngle = detectorOmegaAngle->text().toDouble();
  detectorData.detectorTiltAngle = detectorTiltAngle->text().toDouble();
  detectorData.dwellTime = dwellTime->text().toDouble();
  detectorData.energyMax = energyMaxCB->currentText().toInt();
  detectorData.energyMin = energyMinCB->currentText().toInt();
  detectorData.patternCenterX = patternCenterX->text().toDouble();
  detectorData.patternCenterY = patternCenterY->text().toDouble();
  detectorData.pixelCoordinateX = pixelCoordinateX->text().toDouble();
  detectorData.pixelCoordinateY = pixelCoordinateY->text().toDouble();
  detectorData.numOfPixelsX = numOfPixelsX->text().toInt();
  detectorData.numOfPixelsY = numOfPixelsY->text().toInt();
  detectorData.samplingStepSizeX = samplingStepSizeX->text().toDouble();
  detectorData.samplingStepSizeY = samplingStepSizeY->text().toDouble();
  detectorData.scintillatorDist = scintillatorDist->text().toDouble();
  detectorData.scintillatorPixelSize = scintillatorPixSize->text().toDouble();
  detectorData.masterFilePath = mpLabel->text();
  return detectorData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::on_mpSelectBtn_clicked()
{
  QString lastDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = FileIOTools::GetOpenPathFromDialog("Load Master File", "HDF5 File (*.h5);;All Files (*.*)", lastDir);

  if(!filePath.isEmpty())
  {
    mpLabel->setText(filePath);

    m_Controller->setMasterFilePath(filePath);
  }

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::on_generateBtn_clicked()
{
  if(mpLabel->text().isEmpty())
  {
    return;
  }

  m_PatternDisplayWidget = createPatternDisplayWidget();
  m_PatternDisplayWidget->setExpectedPatterns(m_CurrentAngleWidget->getEulerAngles());
  m_PatternDisplayWidget->generateImages();
  m_PatternDisplayWidget->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::on_angleTypeCB_currentIndexChanged(int index)
{
  PatternDisplay_UI::AngleTypeMode mode = static_cast<PatternDisplay_UI::AngleTypeMode>(index);

  QLayoutItem* item = angleWidgetLayout->takeAt(0);
  if(item != nullptr)
  {
    QWidget* w = item->widget();
    if(w != nullptr)
    {
      w->hide();
      w->setParent(nullptr);
    }
  }

  if(mode == PatternDisplay_UI::AngleTypeMode::SingleAngle)
  {
    angleWidgetLayout->addWidget(m_SingleAngleWidget.get());
    m_CurrentAngleWidget = m_SingleAngleWidget.get();
    m_SingleAngleWidget->show();
  }
  else if(mode == PatternDisplay_UI::AngleTypeMode::ReadFile)
  {
    angleWidgetLayout->addWidget(m_AngleReaderWidget.get());
    m_CurrentAngleWidget = m_AngleReaderWidget.get();
    m_AngleReaderWidget->show();
  }
  else if(mode == PatternDisplay_UI::AngleTypeMode::SamplingRate)
  {
    angleWidgetLayout->addWidget(m_SamplingRateWidget.get());
    m_CurrentAngleWidget = m_SamplingRateWidget.get();
    m_SamplingRateWidget->show();
  }
  else if(mode == PatternDisplay_UI::AngleTypeMode::SampleCubochoricSpace)
  {
    angleWidgetLayout->addWidget(m_SampleCubochoricSpaceWidget.get());
    m_CurrentAngleWidget = m_SampleCubochoricSpaceWidget.get();
    m_SampleCubochoricSpaceWidget->show();
  }
  else
  {
    // Error!
  }

  generateBtn->setEnabled(m_CurrentAngleWidget->hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::validateData()
{
  clearModuleIssues();

  PatternDisplayController::DetectorData data = getDetectorData();
  if(m_Controller->validateDetectorValues(data))
  {
    generateBtn->setEnabled(true);
  }
  else
  {
    generateBtn->setDisabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::resetDisplayWidgets() const
{
  monteCarloDisplayWidget->setEnergyValue(1);
  masterPatternDisplayWidget->setEnergyValue(1);
  monteCarloDisplayWidget->setProjectionMode(MPMCDisplayWidget::ProjectionMode::Lambert_Square);
  masterPatternDisplayWidget->setProjectionMode(MPMCDisplayWidget::ProjectionMode::Lambert_Square);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::readWindowSettings(QtSSettings* prefs)
{
  bool ok = false;
  prefs->beginGroup("WindowSettings");
  if(prefs->contains(QString("MainWindowGeometry")))
  {
    QByteArray geo_data = prefs->value("MainWindowGeometry", QByteArray());
    ok = restoreGeometry(geo_data);
    if(!ok)
    {
      qDebug() << "Error Restoring the Window Geometry"
               << "\n";
    }
  }

  if(prefs->contains(QString("MainWindowState")))
  {
    //    QByteArray layout_data = prefs->value("MainWindowState", QByteArray());
    //    restoreState(layout_data);
  }

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::writeWindowSettings(QtSSettings* prefs) const
{
  prefs->beginGroup("WindowSettings");
  QByteArray geo_data = saveGeometry();
  //  QByteArray layout_data = saveState();
  prefs->setValue(QString("MainWindowGeometry"), geo_data);
  //  prefs->setValue(QString("MainWindowState"), layout_data);

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::readModuleSession(QJsonObject& obj)
{
  QString masterFilePath = obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternFilePath].toString();
  mpLabel->setText(masterFilePath);

  m_Controller->moveToThread(m_Thread.get());
  connect(m_Thread.get(), &QThread::started, [=] { m_Controller->setMasterFilePath(masterFilePath); });
  connect(m_Controller.get(), SIGNAL(mpInitializationFinished()), m_Thread.get(), SLOT(quit()));
  connect(m_Thread.get(), &QThread::finished, [=] { masterPathInitializationFinished(obj); });

  m_Thread->start();
}

// -----------------------------------------------------------------------------
void PatternDisplay_UI::masterPathInitializationFinished(const QJsonObject& obj)
{
  QJsonObject mpDataObj = obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternWidget].toObject();
  QJsonObject mcDataObj = obj[EMsoftWorkbenchConstants::IOStrings::MonteCarloWidget].toObject();

  masterPatternDisplayWidget->readSession(mpDataObj);
  monteCarloDisplayWidget->readSession(mcDataObj);

  QJsonObject dmObj = obj[EMsoftWorkbenchConstants::IOStrings::DMParameters].toObject();
  readDetectorAndMicroscopeParameters(dmObj);

  angleTypeCB->setCurrentIndex(obj[EMsoftWorkbenchConstants::IOStrings::AngleType].toInt());

  QJsonObject angleModeObj = obj[EMsoftWorkbenchConstants::IOStrings::AngleModeParameters].toObject();
  m_CurrentAngleWidget->readSession(angleModeObj);

  patternDisplayTabWidget->setCurrentIndex(obj[EMsoftWorkbenchConstants::IOStrings::CurrentDisplayTab].toInt());

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::readDetectorAndMicroscopeParameters(QJsonObject& obj)
{
  scintillatorDist->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorDistance].toDouble()));
  detectorTiltAngle->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::DetectorTiltAngle].toDouble()));
  detectorOmegaAngle->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::DetectorOmegaAngle].toDouble()));
  scintillatorPixSize->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorPixelSize].toDouble()));

  QJsonObject numberOfPixelsObj = obj[EMsoftWorkbenchConstants::IOStrings::NumberOfPixels].toObject();
  numOfPixelsX->setText(QString::number(numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::X].toInt()));
  numOfPixelsY->setText(QString::number(numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::Y].toInt()));

  QJsonObject patternCenterObj = obj[EMsoftWorkbenchConstants::IOStrings::PatternCenter].toObject();
  patternCenterX->setText(QString::number(patternCenterObj[EMsoftWorkbenchConstants::IOStrings::X].toDouble()));
  patternCenterY->setText(QString::number(patternCenterObj[EMsoftWorkbenchConstants::IOStrings::Y].toDouble()));

  QJsonObject pixelCoordsObj = obj[EMsoftWorkbenchConstants::IOStrings::PixelCoordinates].toObject();
  pixelCoordinateX->setText(QString::number(pixelCoordsObj[EMsoftWorkbenchConstants::IOStrings::X].toDouble()));
  pixelCoordinateY->setText(QString::number(pixelCoordsObj[EMsoftWorkbenchConstants::IOStrings::Y].toDouble()));

  QJsonObject samplingStepSizeObj = obj[EMsoftWorkbenchConstants::IOStrings::SamplingStepSize].toObject();
  samplingStepSizeX->setText(QString::number(samplingStepSizeObj[EMsoftWorkbenchConstants::IOStrings::X].toDouble()));
  samplingStepSizeY->setText(QString::number(samplingStepSizeObj[EMsoftWorkbenchConstants::IOStrings::Y].toDouble()));

  beamCurrent->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::BeamCurrent].toDouble()));
  dwellTime->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::DwellTime].toDouble()));
  barrelDistortion->setText(QString::number(obj[EMsoftWorkbenchConstants::IOStrings::BarrelDistortion].toDouble()));

  QJsonObject energyObj = obj[EMsoftWorkbenchConstants::IOStrings::Energy].toObject();
  energyMinCB->setCurrentText(QString::number(energyObj[EMsoftWorkbenchConstants::IOStrings::Minimum].toInt()));
  energyMaxCB->setCurrentText(QString::number(energyObj[EMsoftWorkbenchConstants::IOStrings::Maximum].toInt()));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::writeModuleSession(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternFilePath] = mpLabel->text();

  QJsonObject mpDataObj;
  QJsonObject mcDataObj;
  QJsonObject dmObj;
  QJsonObject angleModeObj;

  masterPatternDisplayWidget->writeSession(mpDataObj);
  monteCarloDisplayWidget->writeSession(mcDataObj);

  obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternWidget] = mpDataObj;
  obj[EMsoftWorkbenchConstants::IOStrings::MonteCarloWidget] = mcDataObj;

  writeDetectorAndMicroscopeParameters(dmObj);
  obj[EMsoftWorkbenchConstants::IOStrings::DMParameters] = dmObj;

  obj[EMsoftWorkbenchConstants::IOStrings::AngleType] = angleTypeCB->currentIndex();

  m_CurrentAngleWidget->writeSession(angleModeObj);
  obj[EMsoftWorkbenchConstants::IOStrings::AngleModeParameters] = angleModeObj;

  obj[EMsoftWorkbenchConstants::IOStrings::CurrentDisplayTab] = patternDisplayTabWidget->currentIndex();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplay_UI::writeDetectorAndMicroscopeParameters(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorDistance] = scintillatorDist->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::DetectorTiltAngle] = detectorTiltAngle->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::DetectorOmegaAngle] = detectorOmegaAngle->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorPixelSize] = scintillatorPixSize->text().toDouble();

  QJsonObject numberOfPixelsObj;
  numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::X] = numOfPixelsX->text().toInt();
  numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::Y] = numOfPixelsY->text().toInt();
  obj[EMsoftWorkbenchConstants::IOStrings::NumberOfPixels] = numberOfPixelsObj;

  QJsonObject patternCenterObj;
  patternCenterObj[EMsoftWorkbenchConstants::IOStrings::X] = patternCenterX->text().toDouble();
  patternCenterObj[EMsoftWorkbenchConstants::IOStrings::Y] = patternCenterY->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::PatternCenter] = patternCenterObj;

  QJsonObject pixelCoordsObj;
  pixelCoordsObj[EMsoftWorkbenchConstants::IOStrings::X] = pixelCoordinateX->text().toDouble();
  pixelCoordsObj[EMsoftWorkbenchConstants::IOStrings::Y] = pixelCoordinateY->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::PixelCoordinates] = pixelCoordsObj;

  QJsonObject samplingStepSizeObj;
  samplingStepSizeObj[EMsoftWorkbenchConstants::IOStrings::X] = samplingStepSizeX->text().toDouble();
  samplingStepSizeObj[EMsoftWorkbenchConstants::IOStrings::Y] = samplingStepSizeY->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::SamplingStepSize] = samplingStepSizeObj;

  obj[EMsoftWorkbenchConstants::IOStrings::BeamCurrent] = beamCurrent->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::DwellTime] = dwellTime->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::BarrelDistortion] = barrelDistortion->text().toDouble();

  QJsonObject energyObj;
  energyObj[EMsoftWorkbenchConstants::IOStrings::Minimum] = energyMinCB->currentText().toInt();
  energyObj[EMsoftWorkbenchConstants::IOStrings::Maximum] = energyMaxCB->currentText().toInt();
  obj[EMsoftWorkbenchConstants::IOStrings::Energy] = energyObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplayController* PatternDisplay_UI::getController() const
{
  return m_Controller.get();
}
