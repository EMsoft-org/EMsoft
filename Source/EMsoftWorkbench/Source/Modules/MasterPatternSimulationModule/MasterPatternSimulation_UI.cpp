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

#include "MasterPatternSimulation_UI.h"
#if defined (_MSC_VER)
#define _MATH_DEFINES_DEFINED
#endif
#include <initializer_list>


#include <QtCore/QJsonDocument>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QThreadPool>
#include <QtWidgets/QFileDialog>
#include <QtConcurrent>

#include "EMsoftLib/EMsoftLib.h"

#include "EMsoftWorkbench/EMsoftApplication.h"

#include "Common/Constants.h"
#include "Common/QtSSettings.h"
#include "Common/PatternTools.h"
#include "Common/FileIOTools.h"

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

const QString csTooltipStr = "<font>When the <b>Generate</b> button is pressed, the crystal structure file with this file name will be "
                             "loaded from the folder <b>%1</b>.  This is because the <b>EMXtalFolderpathname</b> variable in your "
                             "configuration settings is set to that path.  Choose the <b>Edit EMsoft Configuration...</b> option in the "
                             "menus to change this variable.</font>";

const QString mcSelectBtnDisabledTooltipStr = "<font>The <b>Select</b> button is disabled because the <b>EMdatapathname</b> variable in "
                                              "your configuration settings is set to <b>%1</b>.  The monte carlo file will be accessed from "
                                              "this path.  If you want to access the monte carlo file from an absolute file path instead, choose the "
                                              "<b>Edit EMsoft Configuration...</b> option in the menus and set the variable to empty.</font>";

const QString mcSelectBtnEnabledTooltipStr = "<font>Since the <b>EMdatapathname</b> variable in your configuration settings is currently "
                                             "empty, we are using absolute paths to access the monte carlo file.  If you want to access the "
                                             "monte carlo file using a prepended directory instead, choose the <b>Edit EMsoft Configuration...</b> "
                                             "option in the menus and set the <b>EMdatapathname</b> variable to the preferred path.</font>";

const QString mcAbsPathTooltipStr = "<font>When the <b>Generate</b> button is pressed, a monte carlo file will be accessed from this "
                                    "file path.  To access the file using a prepended directory instead, set the <b>EMdatapathname</b> variable in "
                                    "your configuration settings to the preferred path.  Choose the <b>Edit EMsoft Configuration...</b> "
                                    "option in the menus to change the <b>EMdatapathname</b> variable.</font>";

const QString mcRelativePathTooltipStr =  "<font>When the <b>Generate</b> button is pressed, a monte carlo file will be accessed from "
                                          "the directory specified by the <b>EMdatapathname</b> variable in your configuration "
                                          "settings.  To access the file using an absolute file path instead, set the <b>EMdatapathname</b> variable "
                                          "in your configuration settings to empty.  Choose the <b>Edit EMsoft Configuration...</b> option "
                                          "in the menus to change the <b>EMdatapathname</b> variable.</font>";

const QString mpSelectBtnDisabledTooltipStr = "<font>The <b>Select</b> button is disabled because the <b>EMdatapathname</b> variable in "
                                              "your configuration settings is set to <b>%1</b>.  The master pattern file will be saved to "
                                              "this path.  If you want to save the master pattern file to an absolute file path instead, choose the "
                                              "<b>Edit EMsoft Configuration...</b> option in the menus and set the variable to empty.</font>";

const QString mpSelectBtnEnabledTooltipStr = "<font>Since the <b>EMdatapathname</b> variable in your configuration settings is currently "
                                             "empty, we are using absolute paths to save the master pattern file.  If you want to save the "
                                             "master pattern file using a prepended directory instead, choose the <b>Edit EMsoft Configuration...</b> "
                                             "option in the menus and set the <b>EMdatapathname</b> variable to the preferred path.</font>";

const QString mpAbsPathTooltipStr = "<font>When the <b>Generate</b> button is pressed, a master pattern file will be written out to this "
                                    "file path.  To write the file using a prepended directory instead, set the <b>EMdatapathname</b> variable in "
                                    "your configuration settings to the preferred path.  Choose the <b>Edit EMsoft Configuration...</b> "
                                    "option in the menus to change the <b>EMdatapathname</b> variable.</font>";

const QString mpRelativePathTooltipStr =  "<font>When the <b>Generate</b> button is pressed, a master pattern file will be written out "
                                          "to the directory specified by the <b>EMdatapathname</b> variable in your configuration "
                                          "settings.  To write the file to an absolute file path instead, set the <b>EMdatapathname</b> variable "
                                          "in your configuration settings to empty.  Choose the <b>Edit EMsoft Configuration...</b> option "
                                          "in the menus to change the <b>EMdatapathname</b> variable.</font>";

const QString generateTooltipStr = "<font>Press the <b>Generate</b> button to create a master pattern file "
                                   "with the specified settings at <b>%1</b>.</font>";

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulation_UI::MasterPatternSimulation_UI(QWidget* parent) :
  IModuleUI(parent)
{
  setupUi(this);

  m_Controller = new MasterPatternSimulationController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulation_UI::~MasterPatternSimulation_UI()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::setupGui()
{  
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  QString emDataPathName = m_Controller->getEMDataPathName();
  if (emDataPathName.isEmpty())
  {
    mcSelectBtn->setEnabled(true);
    mpSelectBtn->setEnabled(true);
    mcFileLabel->setText("Monte Carlo File Path");
    outputFileNameLabel->setText("Master Pattern File Path");
    mcSelectBtn->setToolTip(tr(mcSelectBtnEnabledTooltipStr.toStdString().c_str()));
    mpSelectBtn->setToolTip(tr(mpSelectBtnEnabledTooltipStr.toStdString().c_str()));
    mcFilePathLE->setToolTip(tr(mcAbsPathTooltipStr.toStdString().c_str()));
    mpFilePathLE->setToolTip(tr(mpAbsPathTooltipStr.toStdString().c_str()));
    mcAbsolutePathLabel->setText(mcFilePathLE->text());
    mpAbsolutePathLabel->setText(mpFilePathLE->text());
    simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(mpFilePathLE->text()));
  }
  else
  {
    mcSelectBtn->setDisabled(true);
    mpSelectBtn->setDisabled(true);
    mcFileLabel->setText("Monte Carlo File Name");
    outputFileNameLabel->setText("Master Pattern File Name");
    mcSelectBtn->setToolTip(tr(mcSelectBtnDisabledTooltipStr.toStdString().c_str()).arg(emDataPathName));
    mpSelectBtn->setToolTip(tr(mpSelectBtnDisabledTooltipStr.toStdString().c_str()).arg(emDataPathName));
    mcFilePathLE->setToolTip(tr(mcRelativePathTooltipStr.toStdString().c_str()));
    mpFilePathLE->setToolTip(tr(mpRelativePathTooltipStr.toStdString().c_str()));
    mcAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mcFilePathLE->text());
    mpAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mpFilePathLE->text());
    simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(emDataPathName + QDir::separator() + mpFilePathLE->text()));
  }

  validateData();

  int numOfCores = m_Controller->getNumCPUCores();
  numOfOpenMPThreadsSB->setMaximum(numOfCores);
  numOfOpenMPThreadsSB->setValue(numOfCores);
  numOfOpenMPThreadsSB->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createValidators()
{
//  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
//  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createModificationConnections()
{
  // Spin Boxes
  connect(smallestDSpacingSB, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(numOfMPPixelsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersXSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersYSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(betheParametersZSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });
  connect(numOfOpenMPThreadsSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });

  // Line Edits
  connect(mpFilePathLE, &QLineEdit::textChanged, [=] {
    QString emDataPathName = m_Controller->getEMDataPathName();
    if (emDataPathName.isEmpty())
    {
      simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(mpFilePathLE->text()));
      mpAbsolutePathLabel->setText(mpFilePathLE->text());
    }
    else
    {
      simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(emDataPathName + QDir::separator() + mpFilePathLE->text()));
      mpAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mpFilePathLE->text());
    }
    parametersChanged();
  });
  connect(mcFilePathLE, &QLineEdit::textChanged, [=] {
    QString emDataPathName = m_Controller->getEMDataPathName();
    if (emDataPathName.isEmpty())
    {
      mcAbsolutePathLabel->setText(mcFilePathLE->text());
    }
    else
    {
      mcAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mcFilePathLE->text());
    }
    parametersChanged();
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::createWidgetConnections()
{
  connect(simulateBtn, &QPushButton::clicked, this, &MasterPatternSimulation_UI::slot_simulateBtn_clicked);

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller, &MasterPatternSimulationController::errorMessageGenerated, this, &MasterPatternSimulation_UI::notifyErrorMessage);
  connect(m_Controller, &MasterPatternSimulationController::warningMessageGenerated, this, &MasterPatternSimulation_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(const QString &)), this, SLOT(appendToStdOut(const QString &)));

  connect(mcSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.h5";
    if (mcFilePathLE->text().isEmpty() == false)
    {
      proposedFile = mcFilePathLE->text();
    }

    QString filePath = FileIOTools::GetSavePathFromDialog("Select Monte Carlo File", "Monte Carlo File (*.h5);;All Files (*.*)", proposedFile);
    if(true == filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    mcFilePathLE->setText(filePath);
  });

  connect(mpSelectBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.h5";
    if (mpFilePathLE->text().isEmpty() == false)
    {
      proposedFile = mpFilePathLE->text();
    }

    QString filePath = FileIOTools::GetSavePathFromDialog("Select Master Pattern File", "Master Pattern File (*.h5);;All Files (*.*)", proposedFile);
    if(true == filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    emSoftApp->setOpenDialogLastDirectory(filePath);

    mpFilePathLE->setText(filePath);
  });

  connect(emSoftApp, &EMsoftApplication::emSoftConfigurationChanged, [=] {
    QString emDataPathName = m_Controller->getEMDataPathName();
    if (emDataPathName.isEmpty())
    {
      mcSelectBtn->setEnabled(true);
      mpSelectBtn->setEnabled(true);
      mcFileLabel->setText("Monte Carlo File Path");
      outputFileNameLabel->setText("Master Pattern File Path");
      mcSelectBtn->setToolTip(tr(mcSelectBtnEnabledTooltipStr.toStdString().c_str()));
      mpSelectBtn->setToolTip(tr(mpSelectBtnEnabledTooltipStr.toStdString().c_str()));
      mcFilePathLE->setToolTip(tr(mcAbsPathTooltipStr.toStdString().c_str()));
      mpFilePathLE->setToolTip(tr(mpAbsPathTooltipStr.toStdString().c_str()));
      mcAbsolutePathLabel->setText(mcFilePathLE->text());
      mpAbsolutePathLabel->setText(mpFilePathLE->text());
      simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(mpFilePathLE->text()));
    }
    else
    {
      mcSelectBtn->setDisabled(true);
      mpSelectBtn->setDisabled(true);
      mcFileLabel->setText("Monte Carlo File Name");
      outputFileNameLabel->setText("Master Pattern File Name");
      mcSelectBtn->setToolTip(tr(mcSelectBtnDisabledTooltipStr.toStdString().c_str()).arg(emDataPathName));
      mpSelectBtn->setToolTip(tr(mpSelectBtnDisabledTooltipStr.toStdString().c_str()).arg(emDataPathName));
      mcFilePathLE->setToolTip(tr(mcRelativePathTooltipStr.toStdString().c_str()));
      mpFilePathLE->setToolTip(tr(mpRelativePathTooltipStr.toStdString().c_str()));
      mcAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mcFilePathLE->text());
      mpAbsolutePathLabel->setText(emDataPathName + QDir::separator() + mpFilePathLE->text());
      simulateBtn->setToolTip(tr(generateTooltipStr.toStdString().c_str()).arg(emDataPathName + QDir::separator() + mpFilePathLE->text()));
    }
    validateData();
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::slot_simulateBtn_clicked()
{
  if (simulateBtn->text() == "Cancel")
  {
    m_Controller->setCancel(true);
    setRunning(false);
    return;
  }
  else
  {
    setRunning(true);
    clearModuleIssues();

    MasterPatternSimulationController::MasterPatternSimulationData data = getSimulationData();

    simulateBtn->setText("Cancel");
    inputGrpBox->setDisabled(true);
    compParamGrpBox->setDisabled(true);
    outputGrpBox->setDisabled(true);

    // Single-threaded for now, but we can multi-thread later if needed
    //  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
    for (int i = 0; i < 1; i++)
    {
      m_Watcher = QSharedPointer<QFutureWatcher<void>>(new QFutureWatcher<void>());
      connect(m_Watcher.data(), SIGNAL(finished()), this, SLOT(threadFinished()));

      QFuture<void> future = QtConcurrent::run(m_Controller, &MasterPatternSimulationController::createMasterPattern, data);
      m_Watcher->setFuture(future);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::threadFinished()
{
  m_Controller->setCancel(false);

  simulateBtn->setText("Simulate");
  inputGrpBox->setEnabled(true);
  compParamGrpBox->setEnabled(true);
  outputGrpBox->setEnabled(true);

  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MasterPatternSimulation_UI::validateData()
{
  clearModuleIssues();

  MasterPatternSimulationController::MasterPatternSimulationData data = getSimulationData();
  if (m_Controller->validateMasterPatternValues(data) == true)
  {
    simulateBtn->setEnabled(true);
    return true;
  }
  else
  {
    simulateBtn->setDisabled(true);
    return false;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::changeEvent(QEvent* event)
{
  if (event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::readModuleSession(QJsonObject &obj)
{
  readComputationalParameters(obj);

  mcFilePathLE->setText(obj[ioConstants::InputMonteCarloFileName].toString());
  mpFilePathLE->setText(obj[ioConstants::OutputFileName].toString());

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::readComputationalParameters(QJsonObject &obj)
{
  QJsonObject compParamObj = obj[ioConstants::CompParam].toObject();

  if (compParamObj.isEmpty() == false)
  {
    smallestDSpacingSB->blockSignals(true);
    numOfMPPixelsSB->blockSignals(true);
    betheParametersXSB->blockSignals(true);
    betheParametersYSB->blockSignals(true);
    betheParametersZSB->blockSignals(true);
    numOfOpenMPThreadsSB->blockSignals(true);

    smallestDSpacingSB->setValue(compParamObj[ioConstants::SmallestDSpacing].toDouble());
    numOfMPPixelsSB->setValue(compParamObj[ioConstants::NumOfMasterPatternPxls].toInt());

    QJsonObject betheParamObj = compParamObj[ioConstants::BetheParameters].toObject();
    betheParametersXSB->setValue(betheParamObj[ioConstants::Bethe_X].toInt());
    betheParametersYSB->setValue(betheParamObj[ioConstants::Bethe_Y].toInt());
    betheParametersZSB->setValue(betheParamObj[ioConstants::Bethe_Z].toInt());

    numOfOpenMPThreadsSB->setValue(compParamObj[ioConstants::NumOfOpenMPThreads].toInt());

    smallestDSpacingSB->blockSignals(false);
    numOfMPPixelsSB->blockSignals(false);
    betheParametersXSB->blockSignals(false);
    betheParametersYSB->blockSignals(false);
    betheParametersZSB->blockSignals(false);
    numOfOpenMPThreadsSB->blockSignals(false);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::writeModuleSession(QJsonObject &obj)
{ 
  QJsonObject compParamObj;

  writeComputationalParameters(compParamObj);

  obj[ioConstants::CompParam] = compParamObj;
  obj[ioConstants::OutputFileName] = mpFilePathLE->text();
  obj[ioConstants::InputMonteCarloFileName] = mcFilePathLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulation_UI::writeComputationalParameters(QJsonObject &obj)
{  
  obj[ioConstants::SmallestDSpacing] = smallestDSpacingSB->value();
  obj[ioConstants::NumOfMasterPatternPxls] = numOfMPPixelsSB->value();

  QJsonObject betheParamObj;
  betheParamObj[ioConstants::Bethe_X] = betheParametersXSB->value();
  betheParamObj[ioConstants::Bethe_Y] = betheParametersYSB->value();
  betheParamObj[ioConstants::Bethe_Z] = betheParametersZSB->value();
  obj[ioConstants::BetheParameters] = betheParamObj;

  obj[ioConstants::NumOfOpenMPThreads] = numOfOpenMPThreadsSB->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationData MasterPatternSimulation_UI::getSimulationData()
{
  MasterPatternSimulationController::MasterPatternSimulationData data;
  data.smallestDSpacing = smallestDSpacingSB->value();
  data.numOfMPPixels = numOfMPPixelsSB->value();
  data.betheParametersX = betheParametersXSB->value();
  data.betheParametersY = betheParametersYSB->value();
  data.betheParametersZ = betheParametersZSB->value();
  data.numOfOpenMPThreads = numOfOpenMPThreadsSB->value();
  data.inputMonteCarloFileName = mcFilePathLE->text();
  data.outputFileName = mpFilePathLE->text();
  return data;
}

