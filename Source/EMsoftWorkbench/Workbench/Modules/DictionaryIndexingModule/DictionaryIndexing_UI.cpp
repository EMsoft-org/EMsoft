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

#include "Modules/DictionaryIndexingModule/Constants.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexing_UI::DictionaryIndexing_UI(QWidget *parent)
: QWidget(parent)
, m_Ui(new Ui::DictionaryIndexing_UI())
{
  m_Ui->setupUi(this);

  m_DIController = new PatternPreprocessingController(this);

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
void DictionaryIndexing_UI::initializeSpinBoxLimits()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createValidators()
{
  //  QDoubleValidator* doubleValidator = new QDoubleValidator(scintillatorPixelSize);
  //  scintillatorPixelSize->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createModificationConnections()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::createWidgetConnections()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::listenGenerateDIBtnPressed()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::generateDIThreadFinished()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::parametersChanged()
{
  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::validateData()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapController::ADPMapData DictionaryIndexing_UI::getDIData()
{
  return ADPMapController::ADPMapData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::readSession(const QJsonObject &obj)
{
  //  QJsonObject adpMapParamsObj = obj[ioConstants::ADPMapParams].toObject();

  //  if(!adpMapParamsObj.isEmpty())
  //  {
  //    m_Ui->patternHeightLE->blockSignals(true);
  //    m_Ui->patternWidthLE->blockSignals(true);
  //    m_Ui->roiCB->blockSignals(true);
  //    m_Ui->roi1LE->blockSignals(true);
  //    m_Ui->roi2LE->blockSignals(true);
  //    m_Ui->roi3LE->blockSignals(true);
  //    m_Ui->roi4LE->blockSignals(true);
  //    m_Ui->binningFactorLE->blockSignals(true);
  //    m_Ui->binningXLE->blockSignals(true);
  //    m_Ui->binningYLE->blockSignals(true);
  //    m_Ui->ipfHeightLE->blockSignals(true);
  //    m_Ui->ipfWidthLE->blockSignals(true);
  //    m_Ui->maskPatternLE->blockSignals(true);
  //    m_Ui->maskRadiusLE->blockSignals(true);
  //    m_Ui->hipassLE->blockSignals(true);
  //    m_Ui->numOfRegionsLE->blockSignals(true);
  //    m_Ui->numOfThreadsLE->blockSignals(true);

  //    m_Ui->patternHeightLE->setText(adpMapParamsObj[ioConstants::PatternHeight].toString());
  //    m_Ui->patternWidthLE->setText(adpMapParamsObj[ioConstants::PatternWidth].toString());
  //    m_Ui->roiCB->setChecked(adpMapParamsObj[ioConstants::UseROI].toBool());
  //    m_Ui->roi1LE->setText(adpMapParamsObj[ioConstants::ROI_1].toString());
  //    m_Ui->roi2LE->setText(adpMapParamsObj[ioConstants::ROI_2].toString());
  //    m_Ui->roi3LE->setText(adpMapParamsObj[ioConstants::ROI_3].toString());
  //    m_Ui->roi4LE->setText(adpMapParamsObj[ioConstants::ROI_4].toString());
  //    m_Ui->binningFactorLE->setText(adpMapParamsObj[ioConstants::BinningFactor].toString());
  //    m_Ui->binningXLE->setText(adpMapParamsObj[ioConstants::BinningX].toString());
  //    m_Ui->binningYLE->setText(adpMapParamsObj[ioConstants::BinningY].toString());
  //    m_Ui->ipfHeightLE->setText(adpMapParamsObj[ioConstants::IPFHeight].toString());
  //    m_Ui->ipfWidthLE->setText(adpMapParamsObj[ioConstants::IPFWidth].toString());
  //    m_Ui->maskPatternLE->setText(adpMapParamsObj[ioConstants::MaskPattern].toString());
  //    m_Ui->maskRadiusLE->setText(adpMapParamsObj[ioConstants::MaskRadius].toString());
  //    m_Ui->hipassLE->setText(adpMapParamsObj[ioConstants::HipassFilter].toString());
  //    m_Ui->numOfRegionsLE->setText(adpMapParamsObj[ioConstants::NumberOfRegions].toString());
  //    m_Ui->numOfThreadsLE->setText(adpMapParamsObj[ioConstants::NumberOfThreads].toString());

  //    m_Ui->patternHeightLE->blockSignals(false);
  //    m_Ui->patternWidthLE->blockSignals(false);
  //    m_Ui->roiCB->blockSignals(false);
  //    m_Ui->roi1LE->blockSignals(false);
  //    m_Ui->roi2LE->blockSignals(false);
  //    m_Ui->roi3LE->blockSignals(false);
  //    m_Ui->roi4LE->blockSignals(false);
  //    m_Ui->binningFactorLE->blockSignals(false);
  //    m_Ui->binningXLE->blockSignals(false);
  //    m_Ui->binningYLE->blockSignals(false);
  //    m_Ui->ipfHeightLE->blockSignals(false);
  //    m_Ui->ipfWidthLE->blockSignals(false);
  //    m_Ui->maskPatternLE->blockSignals(false);
  //    m_Ui->maskRadiusLE->blockSignals(false);
  //    m_Ui->hipassLE->blockSignals(false);
  //    m_Ui->numOfRegionsLE->blockSignals(false);
  //    m_Ui->numOfThreadsLE->blockSignals(false);

  //    m_Ui->adpViewer->readSession(adpMapParamsObj);
  //  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::writeSession(QJsonObject& obj) const
{
  //  QJsonObject adpMapParamsObj;
  //  adpMapParamsObj[ioConstants::PatternHeight] = m_Ui->patternHeightLE->text().toInt();
  //  adpMapParamsObj[ioConstants::PatternWidth] = m_Ui->patternWidthLE->text().toInt();
  //  adpMapParamsObj[ioConstants::UseROI] = m_Ui->roiCB->isChecked();
  //  adpMapParamsObj[ioConstants::ROI_1] = m_Ui->roi1LE->text().toInt();
  //  adpMapParamsObj[ioConstants::ROI_2] = m_Ui->roi2LE->text().toInt();
  //  adpMapParamsObj[ioConstants::ROI_3] = m_Ui->roi3LE->text().toInt();
  //  adpMapParamsObj[ioConstants::ROI_4] = m_Ui->roi4LE->text().toInt();
  //  adpMapParamsObj[ioConstants::BinningFactor] = m_Ui->binningFactorLE->text().toInt();
  //  adpMapParamsObj[ioConstants::BinningX] = m_Ui->binningXLE->text().toInt();
  //  adpMapParamsObj[ioConstants::BinningY] = m_Ui->binningYLE->text().toInt();
  //  adpMapParamsObj[ioConstants::IPFHeight] = m_Ui->ipfHeightLE->text().toInt();
  //  adpMapParamsObj[ioConstants::IPFWidth] = m_Ui->ipfWidthLE->text().toInt();
  //  adpMapParamsObj[ioConstants::MaskPattern] = m_Ui->maskPatternLE->text().toInt();
  //  adpMapParamsObj[ioConstants::MaskRadius] = m_Ui->maskRadiusLE->text().toDouble();
  //  adpMapParamsObj[ioConstants::HipassFilter] = m_Ui->hipassLE->text().toDouble();
  //  adpMapParamsObj[ioConstants::NumberOfRegions] = m_Ui->numOfRegionsLE->text().toInt();
  //  adpMapParamsObj[ioConstants::NumberOfThreads] = m_Ui->numOfThreadsLE->text().toInt();
  //  m_Ui->adpViewer->writeSession(adpMapParamsObj);
  //  obj[ioConstants::ADPMapParams] = adpMapParamsObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setHipassValue(float hipassValue)
{
  m_HipassValue = hipassValue;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexing_UI::setHipassNumberOfRegions(int numOfRegions)
{
  m_HipassNumOfRegions = numOfRegions;
}
