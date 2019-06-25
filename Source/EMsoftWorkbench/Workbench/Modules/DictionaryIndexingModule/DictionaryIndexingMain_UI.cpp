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

#include "DictionaryIndexingMain_UI.h"

#include <QtConcurrent>

#include "Modules/DictionaryIndexingModule/Constants.h"
#include "Modules/DictionaryIndexingModule/ChoosePatternsDatasetDialog.h"

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingMain_UI::DictionaryIndexingMain_UI(QWidget *parent)
: IModuleUI(parent)
, m_Ui(new Ui::DictionaryIndexingMain_UI())
{
  m_Ui->setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingMain_UI::~DictionaryIndexingMain_UI() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::setupGui()
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
void DictionaryIndexingMain_UI::initializeSpinBoxLimits()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::createValidators()
{
  //  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
  //  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::createModificationConnections()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::createWidgetConnections()
{
  connect(m_Ui->tabWidget, &QTabWidget::currentChanged, [=] { validateData(); });

  connect(m_Ui->adpMapUI, &ADPMap_UI::selectedADPCoordinateChanged, m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::setSelectedADPPatternPixel);
  connect(m_Ui->adpMapUI, &ADPMap_UI::adpMapGenerationStarted, this, &DictionaryIndexingMain_UI::listenADPMapGenerationStarted);
  connect(m_Ui->adpMapUI, &ADPMap_UI::adpMapGenerationFinished, this, &DictionaryIndexingMain_UI::listenADPMapGenerationFinished);
  connect(m_Ui->adpMapUI, &ADPMap_UI::errorMessageGenerated, this, &DictionaryIndexingMain_UI::notifyErrorMessage);
  connect(m_Ui->adpMapUI, &ADPMap_UI::warningMessageGenerated, this, &DictionaryIndexingMain_UI::notifyWarningMessage);
  connect(m_Ui->adpMapUI, &ADPMap_UI::stdOutputMessageGenerated, this, &DictionaryIndexingMain_UI::appendToStdOut);
  connect(m_Ui->adpMapUI, &ADPMap_UI::parametersChanged, this, &DictionaryIndexingMain_UI::listenParametersChanged);

  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::selectedHipassValueChanged, m_Ui->dictionaryIndexingUI, &DictionaryIndexing_UI::setSelectedHipassValue);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::selectedHipassNumOfStepsChanged, m_Ui->dictionaryIndexingUI, &DictionaryIndexing_UI::setSelectedNumberOfRegions);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::patternPreprocessingStarted, this, &DictionaryIndexingMain_UI::listenPatternPreprocessingStarted);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::patternPreprocessingFinished, this, &DictionaryIndexingMain_UI::listenPatternPreprocessingFinished);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::errorMessageGenerated, this, &DictionaryIndexingMain_UI::notifyErrorMessage);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::warningMessageGenerated, this, &DictionaryIndexingMain_UI::notifyWarningMessage);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::stdOutputMessageGenerated, this, &DictionaryIndexingMain_UI::appendToStdOut);
  connect(m_Ui->patternPreprocessingUI, &PatternPreprocessing_UI::parametersChanged, this, &DictionaryIndexingMain_UI::listenParametersChanged);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::listenADPMapGenerationStarted()
{
  setRunning(true);
  clearModuleIssues();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::listenADPMapGenerationFinished()
{
  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::listenPatternPreprocessingStarted()
{
  setRunning(true);
  clearModuleIssues();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::listenPatternPreprocessingFinished()
{
  emit validationOfOtherModulesNeeded(this);
  setRunning(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::listenParametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::validateData()
{
  clearModuleIssues();

  ModuleTab tab = static_cast<ModuleTab>(m_Ui->tabWidget->currentIndex());
  switch(tab)
  {
  case ModuleTab::AvgDotProductMap:
    m_Ui->adpMapUI->validateData();
    break;
  case ModuleTab::PatternPreprocessing:
    m_Ui->patternPreprocessingUI->validateData();
    break;
  case ModuleTab::DictionaryIndexing:
    break;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::readModuleSession(QJsonObject& obj)
{
  m_Ui->adpMapUI->readSession(obj);
  m_Ui->patternPreprocessingUI->readSession(obj);
  m_Ui->dictionaryIndexingUI->readSession(obj);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingMain_UI::writeModuleSession(QJsonObject& obj) const
{
  QJsonObject diModuleObj;

  m_Ui->adpMapUI->writeSession(diModuleObj);
  m_Ui->patternPreprocessingUI->writeSession(diModuleObj);
  m_Ui->dictionaryIndexingUI->writeSession(diModuleObj);

  obj[ioConstants::DIModule] = diModuleObj;
}
