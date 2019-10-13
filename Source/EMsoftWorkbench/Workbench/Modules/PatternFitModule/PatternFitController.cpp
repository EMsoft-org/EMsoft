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

#include "PatternFitController.h"

#include <initializer_list>

#include <QtConcurrent>
#include <QtCore/QFileInfo>
#include <QtCore/QThreadPool>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "Common/ImageGenerator.hpp"
#include "Common/PatternTools.h"
#include "Common/ProjectionConversions.hpp"

#include "Modules/PatternDisplayModule/PatternListModel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitController::PatternFitController(QObject* parent)
: QObject(parent)
, m_Observer(nullptr)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitController::~PatternFitController() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitController::setMasterFilePath(const QString& masterFilePath)
{
  m_MasterFilePath = masterFilePath;

  QFileInfo fi(masterFilePath);
  emit stdOutputMessageGenerated("Full Path: " + masterFilePath);
  emit stdOutputMessageGenerated("Path: " + fi.path());
  emit stdOutputMessageGenerated("Data File: " + fi.fileName());
  emit stdOutputMessageGenerated("Suffix: " + fi.completeSuffix() + "\n");

  MasterPatternFileReader reader(masterFilePath, m_Observer);
  m_MPFileData = reader.readMasterPatternData();

  if(m_MPFileData.ekevs.empty())
  {
    return;
  }
  emit updateEkeVs(m_MPFileData.ekevs);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternFitController::generatePattern(PatternFitController::SimulationData detectorData)
{
  // Build up the iParValues object
  PatternTools::IParValues iParValues;
  iParValues.numsx = m_MPFileData.numsx;
  iParValues.numset = m_MPFileData.numset;
  iParValues.incidentBeamVoltage = static_cast<float>(m_MPFileData.incidentBeamVoltage);
  iParValues.minEnergy = static_cast<float>(m_MPFileData.minEnergy);
  iParValues.energyBinSize = static_cast<float>(m_MPFileData.energyBinSize);
  iParValues.npx = m_MPFileData.npx;
  iParValues.numOfPixelsX = detectorData.numOfPixelsX;
  iParValues.numOfPixelsY = detectorData.numOfPixelsY;
  iParValues.detectorBinningValue = 1;
  iParValues.numberOfOrientations = 1;

  // Build up the fParValues object
  PatternTools::FParValues fParValues;
  fParValues.omega = static_cast<float>(m_MPFileData.omega);
  fParValues.sigma = static_cast<float>(m_MPFileData.sigma);
  fParValues.pcPixelsX = detectorData.patternCenterX;
  fParValues.pcPixelsY = detectorData.patternCenterY;
  fParValues.scintillatorPixelSize = detectorData.scintillatorPixelSize;
  fParValues.scintillatorDist = detectorData.scintillatorDist;
  fParValues.detectorTiltAngle = detectorData.detectorTiltAngle;
  fParValues.beamCurrent = detectorData.beamCurrent;
  fParValues.dwellTime = detectorData.dwellTime;
  fParValues.gammaValue = detectorData.gammaValue;

  std::vector<float> pattern =
      PatternTools::GeneratePattern(iParValues, fParValues, m_MPFileData.masterLPNHData, m_MPFileData.masterLPSHData, m_MPFileData.monteCarloSquareData, detectorData.angles, 0, m_Cancel);
  return pattern;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternImageViewer::ImageData PatternFitController::generatePatternImage(PatternFitController::SimulationData detectorData)
{
  std::vector<float> patternData = generatePattern(detectorData);
  PatternImageViewer::ImageData imageData = generatePatternImage(patternData, static_cast<size_t>(detectorData.numOfPixelsX), static_cast<size_t>(detectorData.numOfPixelsY));

  return imageData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternImageViewer::ImageData PatternFitController::generatePatternImage(const std::vector<float>& patternData, size_t xDim, size_t yDim, size_t zValue)
{
  PatternImageViewer::ImageData imageData;

  AbstractImageGenerator::Pointer imgGen = ImageGenerator<float>::New(patternData, xDim, yDim, static_cast<int32_t>(zValue), false, true);
  imgGen->createImage();

  imageData.image = imgGen->getGeneratedImage();
  VariantPair variantPair = imgGen->getMinMaxPair();
  imageData.minValue = variantPair.first.toFloat();
  imageData.maxValue = variantPair.second.toFloat();

  return imageData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternFitController::validateSimulationValues(PatternFitController::SimulationData data) const
{
  if(data.masterFilePath.isEmpty())
  {
    QString ss = QObject::tr("The master file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }
  QFileInfo fi(data.masterFilePath);
  QString suffix = fi.completeSuffix();
  if(suffix != "h5" && suffix != "dream3d")
  {
    QString ss = QObject::tr("The master file path '%1' is not an HDF5 file.").arg(data.masterFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if(!fi.exists())
  {
    QString ss = QObject::tr("The master file path '%1' does not exist.").arg(data.masterFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  if(data.expPatternFilePath.isEmpty())
  {
    QString ss = QObject::tr("The experimental pattern file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }
  fi.setFile(data.expPatternFilePath);
  suffix = fi.completeSuffix();
  if(suffix != "png" && suffix != "tif" && suffix != "jpeg")
  {
    QString ss = QObject::tr("The experimental pattern file path '%1' is not an image file.").arg(data.masterFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if(!fi.exists())
  {
    QString ss = QObject::tr("The experimental pattern file path '%1' does not exist.").arg(data.masterFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitController::setObserver(IObserver* value)
{
  m_Observer = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IObserver* PatternFitController::getObserver() const
{
  return m_Observer;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitController::setMPFileData(const MasterPatternFileReader::MasterPatternData& value)
{
  m_MPFileData = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternFileReader::MasterPatternData PatternFitController::getMPFileData() const
{
  return m_MPFileData;
}

