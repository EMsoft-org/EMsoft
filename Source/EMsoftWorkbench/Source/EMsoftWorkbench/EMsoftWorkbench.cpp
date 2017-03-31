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

#include "EMsoftWorkbench.h"

#include <QtCore/QDebug>

#include <QtGui/QScreen>

#include <QtWidgets/QFileDialog>

#include "EMsoftWorkbench/EMsoftApplication.h"
#include "EMsoftWorkbench/MPMCDisplayWidget.h"
#include "EMsoftWorkbench/QtSSettings.h"
#include "EMsoftWorkbench/QtSRecentFileList.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench::EMsoftWorkbench(QString path, QWidget* parent) :
  QMainWindow(parent)
{
  setupUi(this);

  beginSplashScreen();

  m_PatternDisplayWidget = new PatternDisplayWidget();

  m_Controller = new EMsoftController(this);

  // Connection to generate splash screen messages
  connect(m_Controller, SIGNAL(splashScreenMsgGenerated(const QString &)), this, SLOT(showSplashScreenMessage(const QString &)));

  // Connections to display messages to the EMsoftWorkbench status bar.  These messages also go in the log.
  connect(m_Controller, SIGNAL(statusMsgGenerated(const QString &)), this, SLOT(showStatusMessage(const QString &)));
  connect(masterPatternDisplayWidget, SIGNAL(statusMsgGenerated(const QString &)), this, SLOT(showStatusMessage(const QString &)));
  connect(monteCarloDisplayWidget, SIGNAL(statusMsgGenerated(const QString &)), this, SLOT(showStatusMessage(const QString &)));

  // Connections to display master pattern and monte carlo images in their respective image viewers
  connect(m_Controller, SIGNAL(mpImageNeedsDisplayed(GLImageDisplayWidget::GLImageData)), masterPatternDisplayWidget, SLOT(loadImage(GLImageDisplayWidget::GLImageData)));
  connect(m_Controller, SIGNAL(mcImageNeedsDisplayed(GLImageDisplayWidget::GLImageData)), monteCarloDisplayWidget, SLOT(loadImage(GLImageDisplayWidget::GLImageData)));

  // Connections to display keV data in the master pattern and monte carlo viewers
  connect(m_Controller, SIGNAL(mpKeVNeedsDisplayed(float)), masterPatternDisplayWidget, SLOT(setKeV(float)));
  connect(m_Controller, SIGNAL(mcKeVNeedsDisplayed(float)), monteCarloDisplayWidget, SLOT(setKeV(float)));

  // Connections to allow the controller to tell the image viewers that the image spin box range has changed
  connect(m_Controller, SIGNAL(imageRangeChanged(int,int)), masterPatternDisplayWidget, SLOT(setEnergyBinSpinBoxRange(int, int)));
  connect(m_Controller, SIGNAL(imageRangeChanged(int,int)), monteCarloDisplayWidget, SLOT(setEnergyBinSpinBoxRange(int, int)));

  // Connections to allow the controller to tell the workbench parameters section that it has new ekeVs values
  connect(m_Controller, SIGNAL(updateEkeVs(FloatArrayType::Pointer)), this, SLOT(setMinAndMaxEnergyLevelChoices(FloatArrayType::Pointer)));

  // Connections to allow the image viewers to request a new image from the controller
  connect(masterPatternDisplayWidget, SIGNAL(controlsChanged(int, MPMCDisplayWidget::ProjectionMode)), m_Controller, SLOT(updateMPImage(int, MPMCDisplayWidget::ProjectionMode)));
  connect(monteCarloDisplayWidget, SIGNAL(controlsChanged(int, MPMCDisplayWidget::ProjectionMode)), m_Controller, SLOT(updateMCImage(int, MPMCDisplayWidget::ProjectionMode)));
  connect(m_PatternDisplayWidget, SIGNAL(dataChanged(PatternDisplayWidget::PatternDisplayData)), this, SLOT(generateEBSDPatternImage(PatternDisplayWidget::PatternDisplayData)));

  // Connection to allow the workbench to tell the controller that it needs to generate new pattern images
  connect(this, SIGNAL(patternNeedsGenerated(PatternDisplayWidget::PatternDisplayData, EMsoftController::DetectorData)), m_Controller, SLOT(generatePatternImages(PatternDisplayWidget::PatternDisplayData, EMsoftController::DetectorData)));

  m_Controller->setMasterFilePath(path);
  m_Controller->setPatternDisplayWidget(m_PatternDisplayWidget);
  connect(m_Controller, SIGNAL(newProgressBarMaximumValue(int)), m_PatternDisplayWidget, SLOT(setProgressBarMaximum(int)));
  connect(m_Controller, SIGNAL(newProgressBarValue(int)), m_PatternDisplayWidget, SLOT(setProgressBarValue(int)), Qt::QueuedConnection);
  connect(m_Controller, SIGNAL(generationFinished()), m_PatternDisplayWidget, SLOT(generationFinished()));
  connect(m_PatternDisplayWidget, SIGNAL(cancelRequested()), m_Controller, SLOT(cancelGeneration()));
  connect(m_PatternDisplayWidget, SIGNAL(patternNeedsPriority(size_t)), m_Controller, SLOT(addPriorityIndex(size_t)));

  m_SingleAngleWidget = SingleAngleWidget::New();
  m_AngleReaderWidget = AngleReaderWidget::New();
  m_SamplingRateWidget = SamplingRateWidget::New();
  m_SampleCubochoricSpaceWidget = SampleCubochoricSpaceWidget::New();

  connect(m_SingleAngleWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_AngleReaderWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_SamplingRateWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));
  connect(m_SampleCubochoricSpaceWidget.get(), SIGNAL(dataChanged(bool)), this, SLOT(setGenerateButtonAvailability(bool)));

  // Set the combo box to the first widget
  on_angleTypeCB_currentIndexChanged(0);

  // give GUI components time to update before the mainwindow is shown
  QApplication::instance()->processEvents();
  endSplashScreen();
  QApplication::instance()->processEvents();

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench::~EMsoftWorkbench()
{
  writeSettings();

  emSoftApp->unregisterEMsoftWorkbenchWindow(this);

  if (emSoftApp->activeWindow() == this)
  {
    emSoftApp->setActiveWindow(nullptr);
  }

  delete this->m_SplashScreen;
  this->m_SplashScreen = nullptr;
  delete m_PatternDisplayWidget;
  m_PatternDisplayWidget = nullptr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::setupGui()
{
  readSettings();

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

  dblValidator = new QDoubleValidator(pc1);
  pc1->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(pc2);
  pc2->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(pixelCoords1);
  pixelCoords1->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(pixelCoords2);
  pixelCoords2->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(samplingStepSize1);
  samplingStepSize1->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(samplingStepSize2);
  samplingStepSize2->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(beamCurrent);
  beamCurrent->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(dwellTime);
  dwellTime->setValidator(dblValidator);

  dblValidator = new QDoubleValidator(barrelDistortion);
  barrelDistortion->setValidator(dblValidator);

  masterPatternDisplayWidget->setProjectionMode(MPMCDisplayWidget::ProjectionMode::Lambert_Square);
  monteCarloDisplayWidget->setProjectionMode(MPMCDisplayWidget::ProjectionMode::Lambert_Square);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::setGenerateButtonAvailability(bool value)
{
  generateBtn->setEnabled(value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::beginSplashScreen()
{
  // Assume we are launching on the main screen.
//  float pixelRatio = qApp->screens().at(0)->devicePixelRatio();

  QString name (":/EMsoftlogo.png");
//  if (pixelRatio >= 2) {
//    name.append("@2x");
//  }

//  name.append(".jpg");

  // Create and show the splash screen as the main window is being created.
  QPixmap pixmap(name);

  m_SplashScreen = new QSplashScreen(pixmap);
  QFont font = m_SplashScreen->font();
  font.setPixelSize(14);
  m_SplashScreen->setFont(font);
  m_SplashScreen->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::showSplashScreenMessage(const QString &msg)
{
  if (m_SplashScreen)
  {
    m_SplashScreen->showMessage(msg, Qt::AlignVCenter | Qt::AlignBottom, Qt::black);
    QCoreApplication::processEvents();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::endSplashScreen()
{
  if (m_ShowSplash)
  {
    m_SplashScreen->finish(nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::showStatusMessage(const QString &msg)
{
  statusbar->showMessage(msg);
  logTextEdit->append(msg);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::setMinAndMaxEnergyLevelChoices(FloatArrayType::Pointer ekeVs)
{
  energyMinCB->clear();
  energyMaxCB->clear();

  QStringList energies;
  for (int i = 0; i < ekeVs->getNumberOfTuples(); i++)
  {
    int energyLevel = static_cast<int>(ekeVs->getValue(i));
    energyMinCB->addItem(QString::number(energyLevel));
    energyMaxCB->addItem(QString::number(energyLevel));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::generateEBSDPatternImage(PatternDisplayWidget::PatternDisplayData patternData)
{
  EMsoftController::DetectorData detectorData;
  detectorData.barrelDistortion = barrelDistortion->text().toDouble();
  detectorData.beamCurrent = beamCurrent->text().toDouble();
  detectorData.detectorOmegaAngle = detectorOmegaAngle->text().toDouble();
  detectorData.detectorTiltAngle = detectorTiltAngle->text().toDouble();
  detectorData.dwellTime = dwellTime->text().toDouble();
  detectorData.energyMax = energyMaxCB->currentText().toInt();
  detectorData.energyMin = energyMinCB->currentText().toInt();
  detectorData.pcPixelsX = pc1->text().toDouble();
  detectorData.pcPixelsY = pc2->text().toDouble();
  detectorData.pixelCoordinateX = pixelCoords1->text().toDouble();
  detectorData.pixelCoordinateY = pixelCoords2->text().toDouble();
  detectorData.pixelNumX = numOfPixelsX->text().toInt();
  detectorData.pixelNumY = numOfPixelsY->text().toInt();
  detectorData.samplingStepSizeX = samplingStepSize1->text().toDouble();
  detectorData.samplingStepSizeY = samplingStepSize2->text().toDouble();
  detectorData.scintillatorDist = scintillatorDist->text().toDouble();
  detectorData.scintillatorPixelSize = scintillatorPixSize->text().toDouble();
  patternData.angles = m_CurrentAngleWidget->getEulerAngles();

  emit patternNeedsGenerated(patternData, detectorData);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::on_saveLogBtn_pressed()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Log File"),
    proposedDir, tr("Log File (*.log);;Text File (*.txt);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if (filePath.isEmpty()) { return; }

  QFile file(filePath);
  if (file.open(QIODevice::WriteOnly))
  {
    QTextStream stream(&file);

    stream << logTextEdit->toPlainText();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::on_generateBtn_pressed()
{
  m_PatternDisplayWidget->setExpectedPatterns(m_CurrentAngleWidget->getEulerAngles());
  m_PatternDisplayWidget->generateImages();
  m_PatternDisplayWidget->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::on_angleTypeCB_currentIndexChanged(int index)
{
  EMsoftWorkbench::AngleTypeMode mode = static_cast<EMsoftWorkbench::AngleTypeMode>(index);

  QLayoutItem* item = angleWidgetLayout->takeAt(0);
  if (item)
  {
    QWidget* w = item->widget();
    if (w)
    {
      w->hide();
      w->setParent(nullptr);
    }
  }

  if (mode == EMsoftWorkbench::AngleTypeMode::SingleAngle)
  {
    angleWidgetLayout->addWidget(m_SingleAngleWidget.get());
    m_CurrentAngleWidget = m_SingleAngleWidget.get();
    m_SingleAngleWidget->show();
  }
  else if (mode == EMsoftWorkbench::AngleTypeMode::ReadFile)
  {
    angleWidgetLayout->addWidget(m_AngleReaderWidget.get());
    m_CurrentAngleWidget = m_AngleReaderWidget.get();
    m_AngleReaderWidget->show();
  }
  else if (mode == EMsoftWorkbench::AngleTypeMode::SamplingRate)
  {
    angleWidgetLayout->addWidget(m_SamplingRateWidget.get());
    m_CurrentAngleWidget = m_SamplingRateWidget.get();
    m_SamplingRateWidget->show();
  }
  else if (mode == EMsoftWorkbench::AngleTypeMode::SampleCubochoricSpace)
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
void EMsoftWorkbench::changeEvent(QEvent* event)
{
  if (event->type() == QEvent::ActivationChange)
  {
    emit emSoftWindowChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//  Read our settings from a file
// -----------------------------------------------------------------------------
void EMsoftWorkbench::readSettings()
{
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());

  // Have the pipeline builder read its settings from the prefs file
  readWindowSettings(prefs.data());

  prefs->beginGroup("Pattern Display");

  scintillatorDist->setText(QString::number(prefs->value("ScintillatorDistance", QVariant()).toDouble()));
  detectorTiltAngle->setText(QString::number(prefs->value("DetectorTiltAngle", QVariant()).toDouble()));
  detectorOmegaAngle->setText(QString::number(prefs->value("DetectorOmegaAngle", QVariant()).toDouble()));
  scintillatorPixSize->setText(QString::number(prefs->value("ScintillatorPixelSize", QVariant()).toDouble()));
  numOfPixelsX->setText(QString::number(prefs->value("NumOfCameraPixelsX", QVariant()).toInt()));
  numOfPixelsY->setText(QString::number(prefs->value("NumOfCameraPixelsY", QVariant()).toInt()));
  pc1->setText(QString::number(prefs->value("PatternCenterX", QVariant()).toDouble()));
  pc2->setText(QString::number(prefs->value("PatternCenterY", QVariant()).toDouble()));
  pixelCoords1->setText(QString::number(prefs->value("PixelCoordinatesX", QVariant()).toDouble()));
  pixelCoords2->setText(QString::number(prefs->value("PixelCoordinatesY", QVariant()).toDouble()));
  samplingStepSize1->setText(QString::number(prefs->value("SamplingStepSizeX", QVariant()).toDouble()));
  samplingStepSize2->setText(QString::number(prefs->value("SamplingStepSizeY", QVariant()).toDouble()));
  beamCurrent->setText(QString::number(prefs->value("BeamCurrent", QVariant()).toDouble()));
  dwellTime->setText(QString::number(prefs->value("DwellTime", QVariant()).toDouble()));
  barrelDistortion->setText(QString::number(prefs->value("BarrelDistortion", QVariant()).toDouble()));

  int energyMin = prefs->value("EnergyMin", QVariant()).toInt();
  int energyMax = prefs->value("EnergyMax", QVariant()).toInt();

  energyMinCB->setCurrentText(QString::number(energyMin));
  energyMaxCB->setCurrentText(QString::number(energyMax));

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::readWindowSettings(QtSSettings* prefs)
{
  bool ok = false;
  prefs->beginGroup("WindowSettings");
  if (prefs->contains(QString("MainWindowGeometry")))
  {
    QByteArray geo_data = prefs->value("MainWindowGeometry", QByteArray());
    ok = restoreGeometry(geo_data);
    if (!ok)
    {
      qDebug() << "Error Restoring the Window Geometry" << "\n";
    }
  }

  if (prefs->contains(QString("MainWindowState")))
  {
    QByteArray layout_data = prefs->value("MainWindowState", QByteArray());
    restoreState(layout_data);
  }

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//  Write our Prefs to file
// -----------------------------------------------------------------------------
void EMsoftWorkbench::writeSettings()
{
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());

  // Have the pipeline builder write its settings to the prefs file
  writeWindowSettings(prefs.data());

  prefs->beginGroup("Pattern Display");

  prefs->setValue("ScintillatorDistance", scintillatorDist->text().toDouble());
  prefs->setValue("DetectorTiltAngle", detectorTiltAngle->text().toDouble());
  prefs->setValue("DetectorOmegaAngle", detectorOmegaAngle->text().toDouble());
  prefs->setValue("ScintillatorPixelSize", scintillatorPixSize->text().toDouble());
  prefs->setValue("NumOfCameraPixelsX", numOfPixelsX->text().toInt());
  prefs->setValue("NumOfCameraPixelsY", numOfPixelsY->text().toInt());
  prefs->setValue("PatternCenterX", pc1->text().toDouble());
  prefs->setValue("PatternCenterY", pc2->text().toDouble());
  prefs->setValue("PixelCoordinatesX", pixelCoords1->text().toDouble());
  prefs->setValue("PixelCoordinatesY", pixelCoords2->text().toDouble());
  prefs->setValue("SamplingStepSizeX", samplingStepSize1->text().toDouble());
  prefs->setValue("SamplingStepSizeY", samplingStepSize2->text().toDouble());
  prefs->setValue("BeamCurrent", beamCurrent->text().toDouble());
  prefs->setValue("DwellTime", dwellTime->text().toDouble());
  prefs->setValue("BarrelDistortion", barrelDistortion->text().toDouble());
  prefs->setValue("EnergyMin", energyMinCB->currentText().toInt());
  prefs->setValue("EnergyMax", energyMaxCB->currentText().toInt());

  prefs->endGroup();

  QtSRecentFileList::instance()->writeList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench::writeWindowSettings(QtSSettings* prefs)
{
  prefs->beginGroup("WindowSettings");
  QByteArray geo_data = saveGeometry();
  QByteArray layout_data = saveState();
  prefs->setValue(QString("MainWindowGeometry"), geo_data);
  prefs->setValue(QString("MainWindowState"), layout_data);

  prefs->endGroup();
}

