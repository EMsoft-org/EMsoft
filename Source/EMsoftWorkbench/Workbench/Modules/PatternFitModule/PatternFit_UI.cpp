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

#include "PatternFit_UI.h"

#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>
#include <QtCore/QTimer>

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QSplashScreen>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "EMsoftApplication.h"

#include "EbsdLib/Core/Orientation.hpp"
#include "EbsdLib/Core/OrientationTransformation.hpp"
#include "EbsdLib/Core/Quaternion.hpp"

#include "Common/Constants.h"
#include "Common/FileIOTools.h"
#include "Common/PatternTools.h"

#include "QtSupport/QtSSettings.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFit_UI::PatternFit_UI(QWidget* parent)
: IModuleUI(parent)
, m_CurrentPatternChoice(PatternControlsWidget::PatternChoice::Experimental)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFit_UI::~PatternFit_UI()
{
  delete this->m_SplashScreen;
  this->m_SplashScreen = nullptr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::setupGui()
{
  m_Controller = new PatternFitController(this);
  m_Controller->setObserver(this);

  m_FlickerTimer = new QTimer();
  m_FlickerTimer->setSingleShot(true);
  connect(m_FlickerTimer, &QTimer::timeout, this, [=] {
    if(m_CurrentPatternChoice == PatternControlsWidget::PatternChoice::Experimental)
    {
      m_CurrentPatternChoice = PatternControlsWidget::PatternChoice::Simulated;
    }
    else
    {
      m_CurrentPatternChoice = PatternControlsWidget::PatternChoice::Experimental;
    }

    displayImage();

    if(m_FlickerIsChecked)
    {
      m_FlickerTimer->start(patternFitViewer->getFlickerInterval());
    }
    else
    {
      m_CurrentPatternChoice = m_BeforeFlickerChoice;
      displayImage();
    }
  });

  // Set the max range of the spin boxes to be the absolute max value
  scintillatorDist->setMaximum(std::numeric_limits<double>::max());
  omega->setMaximum(std::numeric_limits<double>::max());
  patternCenterX->setMaximum(std::numeric_limits<int>::max());
  patternCenterY->setMaximum(std::numeric_limits<int>::max());
  patternCenterX->setMinimum(std::numeric_limits<int>::min());
  patternCenterY->setMinimum(std::numeric_limits<int>::min());
  detectorTiltAngle->setMaximum(std::numeric_limits<double>::max());
  intensityGamma->setMaximum(std::numeric_limits<double>::max());
  phi1->setMaximum(std::numeric_limits<double>::max());
  phi->setMaximum(std::numeric_limits<double>::max());
  phi2->setMaximum(std::numeric_limits<double>::max());

  // Initialize the spin box single step properties to their proper values
  scintillatorDist->setSingleStep(scintillatorDistMStep->text().toDouble());
  omega->setSingleStep(omegaMStep->text().toDouble());
  patternCenterX->setSingleStep(centerXMStep->text().toDouble());
  patternCenterY->setSingleStep(centerYMStep->text().toDouble());
  detectorTiltAngle->setSingleStep(detectorTiltAngleMStep->text().toDouble());
  intensityGamma->setSingleStep(intensityGammaMStep->text().toDouble());
  phi1->setSingleStep(phi1MStep->text().toDouble());
  phi->setSingleStep(phiMStep->text().toDouble());
  phi2->setSingleStep(phi2MStep->text().toDouble());

  // Update Rotation Quaternions using the first values
  updateRotationQuaternions(patternControlsWidget->getRotationStepSize(), detectorTiltAngle->value());

  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createParametersChangedConnections();

  // Create all signal/slot connections that change the spin box single step property when the step line edits change
  createStepConnections();

  // Create all signal/slot connections that are used to set the widget window as modified.
  createParametersChangedConnections();

  nonRefinableParamsGB->setDisabled(true);
  fitParamsGB->setDisabled(true);
  refinableDetectorParamsGB->setDisabled(true);
  refinableSampleParamsGB->setDisabled(true);
  patternControlsGB->setDisabled(true);
  startFitBtn->setDisabled(true);
  patternFitViewer->setDisabled(true);
  hipassFilterLowCutOff->setDisabled(true);

  // This button should be hidden until we have an automatic fit algorithm in place
  startFitBtn->hide();

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::initializeHipassFilterValues()
{
  bool init = true;
  bool destroy = false;

  int32_t hiPassDims[2];
  hiPassDims[0] = numOfPixelsX->text().toInt();
  hiPassDims[1] = numOfPixelsY->text().toInt();

  m_HiPassData.resize(hiPassDims[0] * hiPassDims[1]);

  double cutOff = hipassFilterLowCutOff->text().toDouble();
  HiPassFilterC(nullptr, hiPassDims, &cutOff, &init, &destroy, m_HiPassData.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::destroyHipassFilterValues()
{
  bool init = false;
  bool destroy = true;

  int32_t hiPassDims[2];
  hiPassDims[0] = numOfPixelsX->text().toInt();
  hiPassDims[1] = numOfPixelsY->text().toInt();

  double cutOff = hipassFilterLowCutOff->text().toDouble();
  HiPassFilterC(nullptr, hiPassDims, &cutOff, &init, &destroy, m_HiPassData.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::createValidators() const
{
  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
  scintillatorPixelSize->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(beamCurrent);
  beamCurrent->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(dwellTime);
  dwellTime->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(hipassFilterLowCutOff);
  hipassFilterLowCutOff->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(scintillatorDistMStep);
  scintillatorDistMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(omegaMStep);
  omegaMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(centerXMStep);
  centerXMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(centerYMStep);
  centerYMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(detectorTiltAngleMStep);
  detectorTiltAngleMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(intensityGammaMStep);
  intensityGammaMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phi1MStep);
  phi1MStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phiMStep);
  phiMStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phi2MStep);
  phi2MStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(scintillatorDistFStep);
  scintillatorDistFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(omegaFStep);
  omegaFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(centerXFStep);
  centerXFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(centerYFStep);
  centerYFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(detectorTiltAngleFStep);
  detectorTiltAngleFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(intensityGammaFStep);
  intensityGammaFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phi1FStep);
  phi1FStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phiFStep);
  phiFStep->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(phi2FStep);
  phi2FStep->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::createParametersChangedConnections()
{
  // Line Edits
  connect(scintillatorPixelSize, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(beamCurrent, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(dwellTime, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(hipassFilterLowCutOff, &QLineEdit::textEdited, [=] {
    m_HiPassValuesChanged = true;
    parametersChanged();
  });
  connect(scintillatorDistMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(omegaMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(centerXMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(centerYMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(detectorTiltAngleMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(intensityGammaMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(phi1MStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(phiMStep, &QLineEdit::textEdited, [=] { parametersChanged(); });
  connect(phi2MStep, &QLineEdit::textEdited, [=] { parametersChanged(); });

  // Checkboxes
  connect(hipassFilter, &QCheckBox::stateChanged, [=](int state) {
    hipassFilterLowCutOff->setEnabled(state == Qt::Checked);
    parametersChanged();
  });
  connect(linearRampSubtraction, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(inverseGaussian, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(circularMask, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(scintillatorDistCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(omegaCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(centerXCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(centerYCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(detectorTiltAngleCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(intensityGammaCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(phi1CB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(phiCB, &QCheckBox::stateChanged, [=] { parametersChanged(); });
  connect(phi2CB, &QCheckBox::stateChanged, [=] { parametersChanged(); });

  // Spin Boxes
  connect(scintillatorDist, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(omega, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(detectorTiltAngle, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(intensityGamma, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(phi1, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(phi, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(phi2, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });

  // Combo Boxes
  connect(fitCriterionCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });
  connect(fitModeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });

  connect(patternFitViewer, &PatternFitViewer::controlsChanged, [=] { emit moduleParametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::parametersChanged()
{
  checkFitMode();
  validateData();

  if (hasValidValues())
  {
    generateSimulatedPatternImage();
  }

  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::checkFitMode() const
{
  fitModeCB->blockSignals(true);

  if(scintillatorDistCB->isChecked() && centerXCB->isChecked() && centerYCB->isChecked() && !omegaCB->isChecked() && !detectorTiltAngleCB->isChecked() && !intensityGammaCB->isChecked() &&
     !phi1CB->isChecked() && !phiCB->isChecked() && !phi2CB->isChecked())
  {
    fitModeCB->setCurrentIndex(static_cast<int>(PatternFit_UI::FitMode::Detector));
  }
  else if(!scintillatorDistCB->isChecked() && centerXCB->isChecked() && centerYCB->isChecked() && !omegaCB->isChecked() && !detectorTiltAngleCB->isChecked() && !intensityGammaCB->isChecked() &&
          phi1CB->isChecked() && phiCB->isChecked() && phi2CB->isChecked())
  {
    fitModeCB->setCurrentIndex(static_cast<int>(PatternFit_UI::FitMode::Orientation));
  }
  else
  {
    fitModeCB->setCurrentIndex(static_cast<int>(PatternFit_UI::FitMode::Free));
  }

  fitModeCB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::createStepConnections() const
{
  connect(scintillatorDistMStep, &QLineEdit::textChanged, [=] { scintillatorDist->setSingleStep(scintillatorDistMStep->text().toDouble()); });
  connect(scintillatorDistMStep, &QLineEdit::textChanged, [=] { scintillatorDist->setSingleStep(scintillatorDistMStep->text().toDouble()); });
  connect(omegaMStep, &QLineEdit::textChanged, [=] { omega->setSingleStep(omegaMStep->text().toDouble()); });
  connect(centerXMStep, &QLineEdit::textChanged, [=] { patternCenterX->setSingleStep(centerXMStep->text().toDouble()); });
  connect(centerYMStep, &QLineEdit::textChanged, [=] { patternCenterY->setSingleStep(centerYMStep->text().toDouble()); });
  connect(detectorTiltAngleMStep, &QLineEdit::textChanged, [=] { detectorTiltAngle->setSingleStep(detectorTiltAngleMStep->text().toDouble()); });
  connect(intensityGammaMStep, &QLineEdit::textChanged, [=] { intensityGamma->setSingleStep(intensityGammaMStep->text().toDouble()); });
  connect(phi1MStep, &QLineEdit::textChanged, [=] { phi1->setSingleStep(phi1MStep->text().toDouble()); });
  connect(phiMStep, &QLineEdit::textChanged, [=] { phi->setSingleStep(phiMStep->text().toDouble()); });
  connect(phi2MStep, &QLineEdit::textChanged, [=] { phi2->setSingleStep(phi2MStep->text().toDouble()); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::createWidgetConnections()
{
  connect(m_Controller, &PatternFitController::errorMessageGenerated, this, &PatternFit_UI::notifyErrorMessage);
  connect(m_Controller, &PatternFitController::warningMessageGenerated, this, &PatternFit_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  connect(patternControlsWidget, &PatternControlsWidget::patternChoiceChanged, this, &PatternFit_UI::slot_patternChoiceChanged);
  connect(patternControlsWidget, &PatternControlsWidget::rotationStepSizeChanged, this, [=](double rot) { updateRotationQuaternions(rot, detectorTiltAngle->text().toDouble()); });
  connect(patternControlsWidget, &PatternControlsWidget::controlsChoicePressed, this, &PatternFit_UI::slot_controlsChoicePressed);
  connect(patternControlsWidget, &PatternControlsWidget::opacityChanged, this, [=](double opacity) {
    m_Opacity = opacity;
    displayImage();
  });

  connect(patternFitViewer, &PatternFitViewer::flickerBoxChecked, this, [=](int state) {
    m_FlickerIsChecked = static_cast<bool>(state);

    if(m_FlickerIsChecked)
    {
      m_BeforeFlickerChoice = m_CurrentPatternChoice;

      m_CurrentPatternChoice = PatternControlsWidget::PatternChoice::Experimental;
      displayImage();

      m_FlickerTimer->start(patternFitViewer->getFlickerInterval());
    }
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::slot_controlsChoicePressed(PatternControlsWidget::ControlsChoice choice)
{
  if(choice == PatternControlsWidget::ControlsChoice::ZERO)
  {
    phi1->setValue(0.0);
    phi->setValue(0.0);
    phi2->setValue(0.0);
  }
  else
  {
    Quaternion<double> choiceQuat;
    if(choice == PatternControlsWidget::ControlsChoice::CW)
    {
      choiceQuat = m_NavigationQuatZ;
    }
    else if(choice == PatternControlsWidget::ControlsChoice::CCW)
    {
      choiceQuat = m_NavigationQuatZ;
      choiceQuat.x() *= -1;
      choiceQuat.y() *= -1;
      choiceQuat.z() *= -1;
    }
    else if(choice == PatternControlsWidget::ControlsChoice::UP)
    {
      choiceQuat = m_NavigationQuatX;
    }
    else if(choice == PatternControlsWidget::ControlsChoice::DOWN)
    {
      choiceQuat = m_NavigationQuatX;
      choiceQuat.x() *= -1;
      choiceQuat.y() *= -1;
      choiceQuat.z() *= -1;
    }
    else if(choice == PatternControlsWidget::ControlsChoice::LEFT)
    {
      choiceQuat = m_NavigationQuatY;
      choiceQuat.x() *= -1;
      choiceQuat.y() *= -1;
      choiceQuat.z() *= -1;
    }
    else if(choice == PatternControlsWidget::ControlsChoice::RIGHT)
    {
      choiceQuat = m_NavigationQuatY;
    }
    else
    {
      // Error!
      return;
    }

    using QuatType = QuatD;
    OrientationD euler(phi1->value() * EbsdLib::Constants::k_PiOver180D, phi->value() * EbsdLib::Constants::k_PiOver180D, phi2->value() * EbsdLib::Constants::k_PiOver180D);
    QuatType quat = OrientationTransformation::eu2qu<OrientationD, QuatType>(euler, QuatType::Order::ScalarVector);

    QuatType resultQuat = quat * choiceQuat;

    euler = OrientationTransformation::qu2eu<QuatType, OrientationD>(resultQuat, QuatType::Order::VectorScalar);

    phi1->blockSignals(true);
    phi->blockSignals(true);
    phi2->blockSignals(true);
    phi1->setValue(euler[0] * EbsdLib::Constants::k_180OverPiD);
    phi->setValue(euler[1] * EbsdLib::Constants::k_180OverPiD);
    phi2->setValue(euler[2] * EbsdLib::Constants::k_180OverPiD);
    phi1->blockSignals(false);
    phi->blockSignals(false);
    phi2->blockSignals(false);
  }

  generateSimulatedPatternImage();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::slot_patternChoiceChanged(int index)
{
  PatternControlsWidget::PatternChoice choice = static_cast<PatternControlsWidget::PatternChoice>(index);

  if(m_FlickerIsChecked)
  {
    m_BeforeFlickerChoice = choice;
  }
  else
  {
    m_CurrentPatternChoice = choice;
    displayImage();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_mpSelectBtn_clicked()
{
  QString lastDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = FileIOTools::GetOpenPathFromDialog("Load Master File", "HDF5 File (*.h5);;All Files (*.*)", lastDir);

  if(!filePath.isEmpty())
  {
    setMasterFilePath(filePath);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_expPatternSelectBtn_clicked()
{
  QString lastDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = FileIOTools::GetOpenPathFromDialog("Load Experimental Pattern File", "PNG File (*.png);;TIF File (*.tif);;JPEG File (*.jpeg);;All Files (*.*)", lastDir);

  if(!filePath.isEmpty())
  {
    setExperimentalPatternFilePath(filePath);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::validateData()
{
  clearModuleIssues();

  if(hasValidValues())
  {
    startFitBtn->setEnabled(true);
  }
  else
  {
    startFitBtn->setDisabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternFit_UI::hasValidValues() const
{
  PatternFitController::SimulationData data = getSimulationData();
  return m_Controller->validateSimulationValues(data);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::setExperimentalPatternFilePath(const QString& filePath)
{
  expPatternLabel->setText(filePath);

  // Load the reference pattern into the image viewer
  QImage refPattern = QImage(filePath);
  numOfPixelsX->setText(QString::number(refPattern.width()));
  numOfPixelsY->setText(QString::number(refPattern.height()));

  if(refPattern.format() != QImage::Format_Grayscale8)
  {
    refPattern = refPattern.convertToFormat(QImage::Format_Grayscale8);
  }

  m_ReferencePattern.image = refPattern;

  if(!mpLabel->text().isEmpty())
  {
    nonRefinableParamsGB->setEnabled(true);
    fitParamsGB->setEnabled(true);
    refinableDetectorParamsGB->setEnabled(true);
    refinableSampleParamsGB->setEnabled(true);
    patternControlsGB->setEnabled(true);
    startFitBtn->setEnabled(true);
    patternFitViewer->setEnabled(true);

    generateSimulatedPatternImage();
  }

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::setMasterFilePath(const QString& filePath)
{
  mpLabel->setText(filePath);
  m_Controller->setMasterFilePath(filePath);

  if(!expPatternLabel->text().isEmpty())
  {
    nonRefinableParamsGB->setEnabled(true);
    fitParamsGB->setEnabled(true);
    refinableDetectorParamsGB->setEnabled(true);
    refinableSampleParamsGB->setEnabled(true);
    patternControlsGB->setEnabled(true);
    startFitBtn->setEnabled(true);
    patternFitViewer->setEnabled(true);

    generateSimulatedPatternImage();
  }

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_detectorTiltAngle_valueChanged(double value)
{
  updateRotationQuaternions(patternControlsWidget->getRotationStepSize(), value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::updateRotationQuaternions(double rot, double detValue)
{
  // Update the rotation quaternions for navigation
  MasterPatternFileReader::MasterPatternData mpData = m_Controller->getMPFileData();

  double ang = rot * 0.5 * EbsdLib::Constants::k_PiOver180D;
  double cang = cos(ang);
  double sang = sin(ang);
  double eta = (mpData.sigma - detValue) * EbsdLib::Constants::k_PiOver180D;
  double delta = EbsdLib::Constants::k_PiD * 0.5 - eta;
  double ceta = cos(eta);
  double seta = sin(eta);
  double cdelta = cos(delta);
  double sdelta = sin(delta);

  m_NavigationQuatX.w() = cang;
  m_NavigationQuatX.x() = 0.0;
  m_NavigationQuatX.y() = sang;
  m_NavigationQuatX.z() = 0.0;

  m_NavigationQuatY.w() = cang;
  m_NavigationQuatY.x() = sang * cdelta;
  m_NavigationQuatY.y() = 0.0;
  m_NavigationQuatY.z() = -sang * sdelta;

  m_NavigationQuatZ.w() = cang;
  m_NavigationQuatZ.x() = sang * ceta;
  m_NavigationQuatZ.y() = 0.0;
  m_NavigationQuatZ.z() = sang * seta;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_fitModeCB_currentIndexChanged(int index) const
{
  PatternFit_UI::FitMode fitMode = static_cast<PatternFit_UI::FitMode>(index);
  scintillatorDistCB->blockSignals(true);
  omegaCB->blockSignals(true);
  centerXCB->blockSignals(true);
  centerYCB->blockSignals(true);
  detectorTiltAngleCB->blockSignals(true);
  intensityGammaCB->blockSignals(true);
  phi1CB->blockSignals(true);
  phiCB->blockSignals(true);
  phi2CB->blockSignals(true);
  if(fitMode == PatternFit_UI::FitMode::Free)
  {
    scintillatorDistCB->setChecked(false);
    omegaCB->setChecked(false);
    centerXCB->setChecked(false);
    centerYCB->setChecked(false);
    detectorTiltAngleCB->setChecked(false);
    intensityGammaCB->setChecked(false);
    phi1CB->setChecked(false);
    phiCB->setChecked(false);
    phi2CB->setChecked(false);
  }
  else if(fitMode == PatternFit_UI::FitMode::Detector)
  {
    scintillatorDistCB->setChecked(true);
    omegaCB->setChecked(false);
    centerXCB->setChecked(true);
    centerYCB->setChecked(true);
    detectorTiltAngleCB->setChecked(false);
    intensityGammaCB->setChecked(false);
    phi1CB->setChecked(false);
    phiCB->setChecked(false);
    phi2CB->setChecked(false);
  }
  else if(fitMode == PatternFit_UI::FitMode::Orientation)
  {
    scintillatorDistCB->setChecked(false);
    omegaCB->setChecked(false);
    centerXCB->setChecked(true);
    centerYCB->setChecked(true);
    detectorTiltAngleCB->setChecked(false);
    intensityGammaCB->setChecked(false);
    phi1CB->setChecked(true);
    phiCB->setChecked(true);
    phi2CB->setChecked(true);
  }
  scintillatorDistCB->blockSignals(false);
  omegaCB->blockSignals(false);
  centerXCB->blockSignals(false);
  centerYCB->blockSignals(false);
  detectorTiltAngleCB->blockSignals(false);
  intensityGammaCB->blockSignals(false);
  phi1CB->blockSignals(false);
  phiCB->blockSignals(false);
  phi2CB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_circularMask_toggled(bool checked)
{
  displayImage();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_inverseGaussian_toggled(bool checked)
{
  displayImage();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_hipassFilter_toggled(bool checked)
{
  displayImage();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::on_linearRampSubtraction_toggled(bool checked)
{
  displayImage();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::displayImage()
{
  PatternImageViewer::ImageData processedPattern;
  if(m_CurrentPatternChoice == PatternControlsWidget::PatternChoice::Experimental && !m_ReferencePattern.image.isNull())
  {
    processedPattern = m_ReferencePattern;

    QPair<int, int> minMax = PatternTools::CalculateMinMaxValue(processedPattern.image);
    processedPattern.minValue = minMax.first;
    processedPattern.maxValue = minMax.second;

    patternFitViewer->loadImage(processedPattern);
  }
  else if(!m_SimulatedPatternData.empty())
  {
    std::vector<float> processedPatternData = m_SimulatedPatternData;

    if(inverseGaussian->isChecked())
    {
      processedPatternData = PatternTools::InverseGaussian(processedPatternData, m_SimulatedPatternDims);
    }

    if(hipassFilter->isChecked())
    {
      if(!m_HiPassDataInitialized)
      {
        initializeHipassFilterValues();
        m_HiPassDataInitialized = true;
      }

      processedPatternData = PatternTools::ApplyHipassFilter(processedPatternData, m_SimulatedPatternDims, hipassFilterLowCutOff->text().toDouble(), m_HiPassData);
    }

    processedPattern = m_Controller->generatePatternImage(processedPatternData, m_SimulatedPatternDims[0], m_SimulatedPatternDims[1]);

    if(m_CurrentPatternChoice == PatternControlsWidget::PatternChoice::Difference)
    {
      processedPattern.image = PatternTools::CalculateDifference(m_ReferencePattern.image, processedPattern.image);
    }
    else if(m_CurrentPatternChoice == PatternControlsWidget::PatternChoice::Composite)
    {
      QVector<QRgb> colorTable;
      for(int i = 0; i <= 255; i++)
      {
        colorTable.push_back(qRgb(0, i, 0));
      }

      processedPattern.image.setColorTable(colorTable);

      processedPattern.image = PatternTools::CalculateComposite(m_ReferencePattern.image, processedPattern.image, m_Opacity);
    }
    else if(m_CurrentPatternChoice == PatternControlsWidget::PatternChoice::Color)
    {
      processedPattern.image = PatternTools::CalculateColorChannelBlend(m_ReferencePattern.image, processedPattern.image);
    }

    if(linearRampSubtraction->isChecked())
    {
      processedPattern.image = PatternTools::RemoveRamp(processedPattern.image);
    }

    if(circularMask->isChecked())
    {
      processedPattern.image = PatternTools::ApplyCircularMask(processedPattern.image);
    }

    QPair<int, int> minMax = PatternTools::CalculateMinMaxValue(processedPattern.image);
    processedPattern.minValue = minMax.first;
    processedPattern.maxValue = minMax.second;

    patternFitViewer->loadImage(processedPattern);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::generateSimulatedPatternImage()
{
  if(mpLabel->text().isEmpty())
  {
    // We haven't loaded a master file yet, so bail
    return;
  }

  m_SimulatedPatternDims.clear();

  // Add values to the simulation data object
  PatternFitController::SimulationData data = getSimulationData();

  m_SimulatedPatternData = m_Controller->generatePattern(data);
  m_SimulatedPatternDims.push_back(data.numOfPixelsX);
  m_SimulatedPatternDims.push_back(data.numOfPixelsY);

  if(m_HiPassValuesChanged)
  {
    if(m_HiPassDataInitialized)
    {
      destroyHipassFilterValues();
    }
    initializeHipassFilterValues();
    m_HiPassValuesChanged = false;
    m_HiPassDataInitialized = true;
  }

  if(!m_FlickerIsChecked)
  {
    displayImage();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::readModuleSession(QJsonObject& obj)
{
  QString expPatternFilePath = obj[EMsoftWorkbenchConstants::IOStrings::ExperimentalPatternFilePath].toString();
  if(!expPatternFilePath.isEmpty())
  {
    setExperimentalPatternFilePath(expPatternFilePath);
  }

  QString masterFilePath = obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternFilePath].toString();
  if(!masterFilePath.isEmpty())
  {
    setMasterFilePath(masterFilePath);
  }

  QJsonObject controlsObj = obj[EMsoftWorkbenchConstants::IOStrings::PatternControls].toObject();
  QJsonObject patternFitViewerObj = obj[EMsoftWorkbenchConstants::IOStrings::PatternFitViewer].toObject();

  if(!controlsObj.isEmpty())
  {
    patternControlsWidget->readSession(controlsObj);
  }

  if(!patternFitViewerObj.isEmpty())
  {
    patternFitViewer->readSession(patternFitViewerObj);
  }

  readNonRefinableParameters(obj);
  readRefinableDetectorParameters(obj);
  readRefinableSampleParameters(obj);
  readFitParameters(obj);

  validateData();

  if(hasValidValues())
  {
    generateSimulatedPatternImage();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::readNonRefinableParameters(QJsonObject& obj)
{
  QJsonObject nonRefParamObj = obj[EMsoftWorkbenchConstants::IOStrings::NonRefinableParameters].toObject();

  if(!nonRefParamObj.isEmpty())
  {
    scintillatorPixelSize->setText(QString::number(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::ScintillatorPixelSize].toDouble()));
    beamCurrent->setText(QString::number(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::BeamCurrent].toDouble()));

    QJsonObject numberOfPixelsObj = nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::NumberOfPixels].toObject();
    if(!numberOfPixelsObj.isEmpty())
    {
      numOfPixelsX->setText(QString::number(numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::X].toInt()));
      numOfPixelsY->setText(QString::number(numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::Y].toInt()));
    }

    dwellTime->setText(QString::number(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::DwellTime].toDouble()));
    hipassFilter->setChecked(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::HipassFilter].toBool());
    hipassFilterLowCutOff->setText(QString::number(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::HipassFilterLCF].toDouble()));
    linearRampSubtraction->setChecked(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::LinearRampSubtraction].toBool());
    inverseGaussian->setChecked(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::InverseGaussian].toBool());
    circularMask->setChecked(nonRefParamObj[EMsoftWorkbenchConstants::IOStrings::CircularMask].toBool());
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::readRefinableDetectorParameters(QJsonObject& obj)
{
  QJsonObject refDetectorParamObj = obj[EMsoftWorkbenchConstants::IOStrings::RefinableDetectorParameters].toObject();

  if(!refDetectorParamObj.isEmpty())
  {
    QJsonObject scintillatorDistObj = refDetectorParamObj[EMsoftWorkbenchConstants::IOStrings::ScintillatorDistance].toObject();
    if(!scintillatorDistObj.isEmpty())
    {
      scintillatorDistCB->setChecked(scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      scintillatorDist->setValue(scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      scintillatorDistMStep->setText(QString::number(scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      scintillatorDistFStep->setText(QString::number(scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject omegaObj = refDetectorParamObj[EMsoftWorkbenchConstants::IOStrings::SampleOmegaAngle].toObject();
    if(!omegaObj.isEmpty())
    {
      omegaCB->setChecked(omegaObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      omega->setValue(omegaObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      omegaMStep->setText(QString::number(omegaObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      omegaFStep->setText(QString::number(omegaObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject centerXObj = refDetectorParamObj[EMsoftWorkbenchConstants::IOStrings::PatternCenterX].toObject();
    if(!centerXObj.isEmpty())
    {
      centerXCB->setChecked(centerXObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      patternCenterX->setValue(centerXObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      centerXMStep->setText(QString::number(centerXObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      centerXFStep->setText(QString::number(centerXObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject centerYObj = refDetectorParamObj[EMsoftWorkbenchConstants::IOStrings::PatternCenterY].toObject();
    if(!centerYObj.isEmpty())
    {
      centerYCB->setChecked(centerYObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      patternCenterY->setValue(centerYObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      centerYMStep->setText(QString::number(centerYObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      centerYFStep->setText(QString::number(centerYObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject detectorTiltAngleObj = refDetectorParamObj[EMsoftWorkbenchConstants::IOStrings::DetectorTiltAngle].toObject();
    if(!detectorTiltAngleObj.isEmpty())
    {
      detectorTiltAngleCB->setChecked(detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      detectorTiltAngle->setValue(detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      detectorTiltAngleMStep->setText(QString::number(detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      detectorTiltAngleFStep->setText(QString::number(detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::readRefinableSampleParameters(QJsonObject& obj)
{
  QJsonObject refSampleParamObj = obj[EMsoftWorkbenchConstants::IOStrings::RefinableSampleParameters].toObject();

  if(!refSampleParamObj.isEmpty())
  {
    QJsonObject intensityGammaObj = refSampleParamObj[EMsoftWorkbenchConstants::IOStrings::IntensityGamma].toObject();
    if(!intensityGammaObj.isEmpty())
    {
      intensityGammaCB->setChecked(intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      intensityGamma->setValue(intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      intensityGammaMStep->setText(QString::number(intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      intensityGammaFStep->setText(QString::number(intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject phi1Obj = refSampleParamObj[EMsoftWorkbenchConstants::IOStrings::Phi1].toObject();
    if(!phi1Obj.isEmpty())
    {
      phi1CB->setChecked(phi1Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      phi1->setValue(phi1Obj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      phi1MStep->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      phi1FStep->setText(QString::number(phi1Obj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject phiObj = refSampleParamObj[EMsoftWorkbenchConstants::IOStrings::Phi].toObject();
    if(!phiObj.isEmpty())
    {
      phiCB->setChecked(phiObj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      phi->setValue(phiObj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      phiMStep->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      phiFStep->setText(QString::number(phiObj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }

    QJsonObject phi2Obj = refSampleParamObj[EMsoftWorkbenchConstants::IOStrings::Phi2].toObject();
    if(!phi2Obj.isEmpty())
    {
      phi2CB->setChecked(phi2Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked].toBool());
      phi2->setValue(phi2Obj[EMsoftWorkbenchConstants::IOStrings::Value].toDouble());
      phi2MStep->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::MStepSize].toDouble()));
      phi2FStep->setText(QString::number(phi2Obj[EMsoftWorkbenchConstants::IOStrings::FStepSize].toDouble()));
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::readFitParameters(QJsonObject& obj)
{
  QJsonObject fitParamObj = obj[EMsoftWorkbenchConstants::IOStrings::FitParameters].toObject();

  if(!fitParamObj.isEmpty())
  {
    fitCriterionCB->setCurrentText(fitParamObj[EMsoftWorkbenchConstants::IOStrings::FitCriterion].toString());
    fitModeCB->setCurrentText(fitParamObj[EMsoftWorkbenchConstants::IOStrings::FitMode].toString());
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::writeModuleSession(QJsonObject& obj) const
{
  QJsonObject nonRefParamObj;
  QJsonObject refDetectorParamObj;
  QJsonObject refSampleParamObj;
  QJsonObject fitParamObj;
  QJsonObject controlsObj;
  QJsonObject patternFitViewerObj;

  writeNonRefinableParameters(nonRefParamObj);
  writeRefinableDetectorParameters(refDetectorParamObj);
  writeRefinableSampleParameters(refSampleParamObj);
  writeFitParameters(fitParamObj);

  patternControlsWidget->writeSession(controlsObj);
  patternFitViewer->writeSession(patternFitViewerObj);

  obj[EMsoftWorkbenchConstants::IOStrings::MasterPatternFilePath] = mpLabel->text();
  obj[EMsoftWorkbenchConstants::IOStrings::ExperimentalPatternFilePath] = expPatternLabel->text();
  obj[EMsoftWorkbenchConstants::IOStrings::NonRefinableParameters] = nonRefParamObj;
  obj[EMsoftWorkbenchConstants::IOStrings::RefinableDetectorParameters] = refDetectorParamObj;
  obj[EMsoftWorkbenchConstants::IOStrings::RefinableSampleParameters] = refSampleParamObj;
  obj[EMsoftWorkbenchConstants::IOStrings::FitParameters] = fitParamObj;
  obj[EMsoftWorkbenchConstants::IOStrings::PatternControls] = controlsObj;
  obj[EMsoftWorkbenchConstants::IOStrings::PatternFitViewer] = patternFitViewerObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::writeNonRefinableParameters(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorPixelSize] = scintillatorPixelSize->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::BeamCurrent] = beamCurrent->text().toDouble();

  QJsonObject numberOfPixelsObj;
  numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::X] = numOfPixelsX->text().toInt();
  numberOfPixelsObj[EMsoftWorkbenchConstants::IOStrings::Y] = numOfPixelsY->text().toInt();
  obj[EMsoftWorkbenchConstants::IOStrings::NumberOfPixels] = numberOfPixelsObj;

  obj[EMsoftWorkbenchConstants::IOStrings::DwellTime] = dwellTime->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::HipassFilter] = hipassFilter->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::HipassFilterLCF] = hipassFilterLowCutOff->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::LinearRampSubtraction] = linearRampSubtraction->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::InverseGaussian] = inverseGaussian->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::CircularMask] = circularMask->isChecked();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::writeRefinableDetectorParameters(QJsonObject& obj) const
{
  QJsonObject scintillatorDistObj;
  scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = scintillatorDistCB->isChecked();
  scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::Value] = scintillatorDist->text().toDouble();
  scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = scintillatorDistMStep->text().toDouble();
  scintillatorDistObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = scintillatorDistFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::ScintillatorDistance] = scintillatorDistObj;

  QJsonObject omegaObj;
  omegaObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = omegaCB->isChecked();
  omegaObj[EMsoftWorkbenchConstants::IOStrings::Value] = omega->text().toDouble();
  omegaObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = omegaMStep->text().toDouble();
  omegaObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = omegaFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::SampleOmegaAngle] = omegaObj;

  QJsonObject centerXObj;
  centerXObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = centerXCB->isChecked();
  centerXObj[EMsoftWorkbenchConstants::IOStrings::Value] = patternCenterX->text().toDouble();
  centerXObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = centerXMStep->text().toDouble();
  centerXObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = centerXFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::PatternCenterX] = centerXObj;

  QJsonObject centerYObj;
  centerYObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = centerYCB->isChecked();
  centerYObj[EMsoftWorkbenchConstants::IOStrings::Value] = patternCenterY->text().toDouble();
  centerYObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = centerYMStep->text().toDouble();
  centerYObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = centerYFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::PatternCenterY] = centerYObj;

  QJsonObject detectorTiltAngleObj;
  detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = detectorTiltAngleCB->isChecked();
  detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::Value] = detectorTiltAngle->text().toDouble();
  detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = detectorTiltAngleMStep->text().toDouble();
  detectorTiltAngleObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = detectorTiltAngleFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::DetectorTiltAngle] = detectorTiltAngleObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::writeRefinableSampleParameters(QJsonObject& obj) const
{
  QJsonObject intensityGammaObj;
  intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = intensityGammaCB->isChecked();
  intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::Value] = intensityGamma->text().toDouble();
  intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = intensityGammaMStep->text().toDouble();
  intensityGammaObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = intensityGammaFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::IntensityGamma] = intensityGammaObj;

  QJsonObject phi1Obj;
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phi1CB->isChecked();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::Value] = phi1->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = phi1MStep->text().toDouble();
  phi1Obj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = phi1FStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi1] = phi1Obj;

  QJsonObject phiObj;
  phiObj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phiCB->isChecked();
  phiObj[EMsoftWorkbenchConstants::IOStrings::Value] = phi->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = phiMStep->text().toDouble();
  phiObj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = phiFStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi] = phiObj;

  QJsonObject phi2Obj;
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::IsChecked] = phi2CB->isChecked();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::Value] = phi2->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::MStepSize] = phi2MStep->text().toDouble();
  phi2Obj[EMsoftWorkbenchConstants::IOStrings::FStepSize] = phi2FStep->text().toDouble();
  obj[EMsoftWorkbenchConstants::IOStrings::Phi2] = phi2Obj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::writeFitParameters(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::FitCriterion] = fitCriterionCB->currentText();
  obj[EMsoftWorkbenchConstants::IOStrings::FitMode] = fitModeCB->currentText();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitController::SimulationData PatternFit_UI::getSimulationData() const
{
  PatternFitController::SimulationData data;
  data.scintillatorDist = scintillatorDist->value();
  data.detectorTiltAngle = detectorTiltAngle->value();
  data.scintillatorPixelSize = scintillatorPixelSize->text().toDouble();
  data.numOfPixelsX = numOfPixelsX->text().toDouble();
  data.numOfPixelsY = numOfPixelsY->text().toDouble();
  data.patternCenterX = patternCenterX->value();
  data.patternCenterY = patternCenterY->value();
  data.beamCurrent = beamCurrent->text().toDouble();
  data.dwellTime = dwellTime->text().toDouble();
  data.sampleOmegaAngle = omega->value();

  data.angles.resize(3);
  data.angles[0] = phi1->value() * EbsdLib::Constants::k_PiOver180D;
  data.angles[1] = phi->value() * EbsdLib::Constants::k_PiOver180D;
  data.angles[2] = phi2->value() * EbsdLib::Constants::k_PiOver180D;

  data.gammaValue = intensityGamma->value();

  data.useCircularMask = circularMask->isChecked();
  data.useHipassFilter = hipassFilter->isChecked();
  data.useInverseGaussian = inverseGaussian->isChecked();
  data.useLinearRampSubtraction = linearRampSubtraction->isChecked();

  data.hipassFilterLowCutOff = hipassFilterLowCutOff->text().toDouble();
  data.masterFilePath = mpLabel->text();
  data.expPatternFilePath = expPatternLabel->text();

  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::setCurrentPatternChoice(const PatternControlsWidget::PatternChoice& value)
{
  m_CurrentPatternChoice = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternControlsWidget::PatternChoice PatternFit_UI::getCurrentPatternChoice() const
{
  return m_CurrentPatternChoice;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFit_UI::setController(PatternFitController* value)
{
  m_Controller = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitController* PatternFit_UI::getController() const
{
  return m_Controller;
}

