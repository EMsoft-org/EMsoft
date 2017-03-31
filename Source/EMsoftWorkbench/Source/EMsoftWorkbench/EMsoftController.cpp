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

#include "EMsoftController.h"

#include <initializer_list>



#include "EMsoftLib/EMsoftLib.h"

#include "OrientationLib/OrientationMath/OrientationTransforms.hpp"

#include "EMsoftWorkbench/ProjectionConversions.hpp"
#include "EMsoftWorkbench/PatternListModel.h"

#include <QtCore/QFileInfo>
#include <QtCore/QThreadPool>
#include <QtConcurrent>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftController::EMsoftController(QObject* parent) :
  QObject(parent),
  m_NumOfFinishedPatternsLock(1),
  m_CurrentOrderLock(1),
  m_EkeVs(FloatArrayType::NullPointer())
{
  // Connection to allow the pattern list to redraw itself
  PatternListModel* model = PatternListModel::Instance();
  connect(this, SIGNAL(rowDataChanged(const QModelIndex &, const QModelIndex &)), model, SIGNAL(dataChanged(const QModelIndex &, const QModelIndex &)), Qt::QueuedConnection);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftController::~EMsoftController()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::setMasterFilePath(QString masterFilePath)
{
  m_MasterFilePath = masterFilePath;

  QFileInfo fi(masterFilePath);
  emit statusMsgGenerated("Full Path: " + masterFilePath);
  emit statusMsgGenerated("Path: " + fi.path());
  emit statusMsgGenerated("Data File: " + fi.fileName());
  emit statusMsgGenerated("Suffix: " + fi.completeSuffix() + "\n");

  readMasterFile();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::readHeaderData(hid_t fileId)
{
  hid_t ebsdMasterHeaderId = H5Utilities::openHDF5Object(fileId, "EMheader/EBSDmaster");
  if (ebsdMasterHeaderId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(ebsdMasterHeaderId));
  }
  HDF5ScopedGroupSentinel sentinel(&ebsdMasterHeaderId, true);

  m_HeaderData.mpProgramName = readStringDataset(ebsdMasterHeaderId, "ProgramName");
  m_HeaderData.mpVersionId = readStringDataset(ebsdMasterHeaderId, "Version");

  hid_t mcOpenCLHeaderId = H5Utilities::openHDF5Object(fileId, "EMheader/MCOpenCL");
  if (mcOpenCLHeaderId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(mcOpenCLHeaderId));
  }
  sentinel.addGroupId(&mcOpenCLHeaderId);

  m_HeaderData.mcProgramName = readStringDataset(mcOpenCLHeaderId, "ProgramName");
  m_HeaderData.mcVersionId = readStringDataset(mcOpenCLHeaderId, "Version");

  hid_t ebsdMasterDataId = H5Utilities::openHDF5Object(fileId, "EMData/EBSDmaster");
  if (ebsdMasterDataId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(ebsdMasterDataId));
  }
  sentinel.addGroupId(&ebsdMasterDataId);

  m_HeaderData.numMPEnergyBins = readScalarDataset<int>(ebsdMasterDataId, "numEbins");

  hid_t mcOpenCLDataId = H5Utilities::openHDF5Object(fileId, "EMData/MCOpenCL");
  if (mcOpenCLDataId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(mcOpenCLDataId));
  }
  sentinel.addGroupId(&mcOpenCLDataId);

  m_HeaderData.numDepthBins = readScalarDataset<int>(mcOpenCLDataId, "numzbins");
  m_HeaderData.numMCEnergyBins = readScalarDataset<int>(mcOpenCLDataId, "numEbins");

  hid_t mcclNameListId = H5Utilities::openHDF5Object(fileId, "NMLparameters/MCCLNameList");
  if (mcclNameListId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(mcclNameListId));
  }
  sentinel.addGroupId(&mcclNameListId);

  m_HeaderData.numsx = readScalarDataset<float>(mcclNameListId, "numsx");
  m_HeaderData.mcStructureFileName = readStringDataset(mcclNameListId, "xtalname");
  m_HeaderData.incidentBeamVoltage = readScalarDataset<float>(mcclNameListId, "EkeV");
  m_HeaderData.mcMode = readStringDataset(mcclNameListId, "MCmode");
  m_HeaderData.omega = readScalarDataset<float>(mcclNameListId, "omega");
  m_HeaderData.sigma = readScalarDataset<float>(mcclNameListId, "sig");
  m_HeaderData.minEnergy = readScalarDataset<float>(mcclNameListId, "Ehistmin");
  m_HeaderData.maxEnergy = readScalarDataset<float>(mcclNameListId, "EkeV");
  m_HeaderData.energyBinSize = readScalarDataset<float>(mcclNameListId, "Ebinsize");
  m_HeaderData.maxDepth = readScalarDataset<float>(mcclNameListId, "depthmax");
  m_HeaderData.depthStep = readScalarDataset<float>(mcclNameListId, "depthstep");
  m_HeaderData.totalNumIncidentEl = readScalarDataset<int>(mcclNameListId, "totnum_el");

  hid_t ebsdMasterNameListId = H5Utilities::openHDF5Object(fileId, "NMLparameters/EBSDMasterNameList");
  if (ebsdMasterNameListId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(ebsdMasterNameListId));
  }
  sentinel.addGroupId(&ebsdMasterNameListId);

  m_HeaderData.npx = readScalarDataset<int>(ebsdMasterNameListId, "npx");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::readMasterPatternData(hid_t fileId, size_t &currentCount, size_t &totalItems)
{
  QString ebsdMasterPath = "EMData/EBSDmaster";
  hid_t ebsdMasterId = H5Utilities::openHDF5Object(fileId, ebsdMasterPath.toStdString());
  if (ebsdMasterId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(ebsdMasterPath));
    return;
  }
  HDF5ScopedGroupSentinel sentinel(&ebsdMasterId, true);

  // Read energy value data
  {
    m_EkeVs = readArrayDataset<float>(ebsdMasterId, "EkeVs");
    if (m_EkeVs == FloatArrayType::NullPointer()) { return; }
    emit updateEkeVs(m_EkeVs);
  }

  // Read numset data
  QString numsetName = "numset";
  int err = QH5Lite::readScalarDataset(ebsdMasterId, numsetName, m_HeaderData.numset);
  if(err < 0)
  {
    emit statusMsgGenerated(tr("Error: Could not read object '%1'").arg(numsetName));
    return;
  }

  // Read Master Pattern lambert square projection data
  emit statusMsgGenerated(tr("Reading Master Pattern data sets..."));
  emit splashScreenMsgGenerated(tr("Reading Master Pattern data sets (%1/%2)...").arg(currentCount).arg(totalItems));
  emit statusMsgGenerated(tr("File generated by program '%1'").arg(m_HeaderData.mpProgramName));
  emit statusMsgGenerated(tr("Version Identifier: %1").arg(m_HeaderData.mpVersionId));
  emit statusMsgGenerated(tr("Number Of Energy Bins: %1\n").arg(QString::number(m_HeaderData.numMPEnergyBins)));

  m_MasterLPNHData = readArrayDataset<float>(ebsdMasterId, "mLPNH");
  if (m_MasterLPNHData == FloatArrayType::NullPointer()) { return; }

  std::vector<hsize_t> mLPNH_dims = readDatasetDimensions(ebsdMasterId, "mLPNH");
  if (mLPNH_dims.size() <= 0) { return; }

  m_MasterLPSHData = readArrayDataset<float>(ebsdMasterId, "mLPSH");
  if (m_MasterLPSHData == FloatArrayType::NullPointer()) { return; }

  std::vector<hsize_t> mLPSH_dims = readDatasetDimensions(ebsdMasterId, "mLPSH");
  if (mLPSH_dims.size() <= 0) { return; }

  QString mpDimStr = "";
  for (int i = 0; i < mLPNH_dims.size(); i++)
  {
    mpDimStr.append(QString::number(mLPNH_dims[i]));
    if (i < mLPNH_dims.size() - 1)
    {
      mpDimStr.append(" x ");
    }
  }

  emit statusMsgGenerated(tr("Size of mLPNH data array: %1").arg(mpDimStr));

  m_MasterLPNH = createImages<float>(m_MasterLPNHData, mLPNH_dims[3], mLPNH_dims[2], mLPNH_dims[1], m_MasterLPNHPairs);
  m_MasterLPSH = createImages<float>(m_MasterLPSHData, mLPSH_dims[3], mLPSH_dims[2], mLPSH_dims[1], m_MasterLPSHPairs);

  currentCount++;
  emit splashScreenMsgGenerated(tr("Reading Master Pattern data sets (%1/%2)...").arg(currentCount).arg(totalItems));

  // Generate Master Pattern Lambert Circle projection data
  size_t zDim = mLPNH_dims[1];

  for (int z = 0; z < zDim; z++)
  {
    ProjectionConversions projConversion(this);
    FloatArrayType::Pointer circularProj = projConversion.convertLambertSquareData<float>(m_MasterLPNHData, mLPNH_dims[3], ModifiedLambertProjection::ProjectionType::Circular, z, ModifiedLambertProjection::Square::NorthSquare);
    FloatPair minMaxPair;
    m_MasterCircle.push_back(createImage<float>(circularProj, mLPNH_dims[3], mLPNH_dims[2], 0, minMaxPair));
    m_MasterCirclePairs.push_back(minMaxPair);
  }

  currentCount++;
  emit splashScreenMsgGenerated(tr("Reading Master Pattern data sets (%1/%2)...").arg(currentCount).arg(totalItems));

  // Read Master Pattern stereographic projection data
  m_MasterSPNHData = readArrayDataset<float>(ebsdMasterId, "masterSPNH");
  if (m_MasterSPNHData == FloatArrayType::NullPointer()) { return; }

  std::vector<hsize_t> masterSPNH_dims = readDatasetDimensions(ebsdMasterId, "masterSPNH");
  if (masterSPNH_dims.size() <= 0) { return; }

  m_MasterSPNH = createImages<float>(m_MasterSPNHData, masterSPNH_dims[2], masterSPNH_dims[1], masterSPNH_dims[0], m_MasterSPNHPairs);

  currentCount++;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::readMonteCarloData(hid_t fileId, size_t &currentCount, size_t &totalItems)
{
  QString mcOpenCLPath = "EMData/MCOpenCL";
  hid_t mcOpenCLId = H5Utilities::openHDF5Object(fileId, mcOpenCLPath.toStdString());
  if (mcOpenCLId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open object at path '%1'").arg(mcOpenCLPath));
    return;
  }
  HDF5ScopedGroupSentinel sentinel(&mcOpenCLId, true);

  // Read Monte Carlo lambert square projection data
  emit statusMsgGenerated(tr("Reading Monte Carlo data sets..."));
  emit splashScreenMsgGenerated(tr("Reading Monte Carlo data sets (%1/%2)...").arg(currentCount).arg(totalItems));
  emit statusMsgGenerated(tr("File generated by program '%1'").arg(m_HeaderData.mcProgramName));
  emit statusMsgGenerated(tr("Version Identifier: %1").arg(m_HeaderData.mcVersionId));

  m_MonteCarloSquareData = readArrayDataset<int32_t>(mcOpenCLId, "accum_e");
  if (m_MonteCarloSquareData == Int32ArrayType::NullPointer()) { return; }

  std::vector<hsize_t> monteCarlo_dims = readDatasetDimensions(mcOpenCLId, "accum_e");
  if (monteCarlo_dims.size() <= 0) { return; }

  Int32ArrayType::Pointer monteCarloSquare_data = deHyperSlabData<int32_t>(m_MonteCarloSquareData, monteCarlo_dims[0], monteCarlo_dims[1], monteCarlo_dims[2]);

  m_MonteCarloSquare = createImages<int32_t>(monteCarloSquare_data, monteCarlo_dims[0], monteCarlo_dims[1], monteCarlo_dims[2], m_MonteCarloSquarePairs);

  currentCount++;
  emit splashScreenMsgGenerated(tr("Reading Monte Carlo data sets (%1/%2)...").arg(currentCount).arg(totalItems));

  size_t zDim = monteCarlo_dims[2];

  // Generate Monte Carlo stereographic and circular projection data
  for (int z = 0; z < zDim; z++)
  {
    ProjectionConversions projConversion(this);
    FloatArrayType::Pointer circularProj = projConversion.convertLambertSquareData<int32_t>(monteCarloSquare_data, monteCarlo_dims[0], ModifiedLambertProjection::ProjectionType::Circular, z, ModifiedLambertProjection::Square::NorthSquare);

    FloatPair minMaxPair;
    m_MonteCarloCircle.push_back(createImage<float>(circularProj, monteCarlo_dims[0], monteCarlo_dims[1], 0, minMaxPair).mirrored(false, true));
    m_MonteCarloCirclePairs.push_back(minMaxPair);

    FloatArrayType::Pointer stereoProj = projConversion.convertLambertSquareData<int32_t>(monteCarloSquare_data, monteCarlo_dims[0], ModifiedLambertProjection::ProjectionType::Stereographic, z, ModifiedLambertProjection::Square::NorthSquare);
    m_MonteCarloStereo.push_back(createImage<float>(stereoProj, monteCarlo_dims[0], monteCarlo_dims[1], 0, minMaxPair));
    m_MonteCarloStereoPairs.push_back(minMaxPair);
  }

  currentCount++;
  emit splashScreenMsgGenerated(tr("Reading Monte Carlo data sets (%1/%2)...").arg(currentCount).arg(totalItems));

  currentCount++;

  QString mcDimStr = "";
  for (int i = 0; i < monteCarlo_dims.size(); i++)
  {
    mcDimStr.append(QString::number(monteCarlo_dims[i]));
    if (i < monteCarlo_dims.size() - 1)
    {
      mcDimStr.append(" x ");
    }
  }

  emit statusMsgGenerated(tr("Structure File Name: %1").arg(m_HeaderData.mcStructureFileName));
  emit statusMsgGenerated(tr("Lambert Dimensions: %1").arg(QString::number(m_HeaderData.numsx) + " x " + QString::number(m_HeaderData.numsx)));
  emit statusMsgGenerated(tr("Incident Beam Voltage (kV): %1").arg(QString::number(m_HeaderData.incidentBeamVoltage, 'f', 2)));
  emit statusMsgGenerated(tr("Monte Carlo Mode: %1").arg(m_HeaderData.mcMode));
  emit statusMsgGenerated(tr("Sample Tilt Angles Omega/Sigma (degrees): %1 / %2").arg(QString::number(m_HeaderData.omega, 'f', 2)).arg(QString::number(m_HeaderData.sigma, 'f', 2)));
  emit statusMsgGenerated(tr("Min/Max Energy (keV): %1 / %2").arg(QString::number(m_HeaderData.minEnergy, 'f', 2)).arg(QString::number(m_HeaderData.maxEnergy, 'f', 2)));
  emit statusMsgGenerated(tr("Energy Bin Size (keV): %1").arg(QString::number(m_HeaderData.energyBinSize, 'f', 2)));
  emit statusMsgGenerated(tr("Number Of Energy Bins: %1").arg(QString::number(m_HeaderData.numMCEnergyBins)));
  emit statusMsgGenerated(tr("Maximum Integration Depth (nm): %1").arg(QString::number(m_HeaderData.maxDepth, 'f', 2)));
  emit statusMsgGenerated(tr("Integration Depth Step Size (nm): %1").arg(QString::number(m_HeaderData.depthStep, 'f', 2)));
  emit statusMsgGenerated(tr("Number Of Depth Bins: %1").arg(QString::number(m_HeaderData.numDepthBins)));
  emit statusMsgGenerated(tr("Total Number Of Incident Electrons: %1").arg(QString::number(m_HeaderData.totalNumIncidentEl)));
  emit statusMsgGenerated(tr("Size of accum_e data array: %1").arg(mcDimStr));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::readMasterFile()
{
  QFileInfo fi(m_MasterFilePath);

  hid_t fileId = H5Utilities::openFile(m_MasterFilePath.toStdString(), true);
  if (fileId < 0)
  {
    emit statusMsgGenerated(tr("Error: Unable to open data file '%1'").arg(fi.fileName()));
    return;
  }
  else
  {
    emit statusMsgGenerated(tr("Reading data file '%1'...").arg(fi.fileName()));

    double fileSize = fi.size();      // This is in bytes
    fileSize = fileSize / 1000000;    // Convert to MB
    emit statusMsgGenerated(tr("File Size: %1 MB\n").arg(QString::number(fileSize, 'f', 2)));
  }
  HDF5ScopedFileSentinel sentinel(&fileId, true);

  // Read the header data
  readHeaderData(fileId);

  size_t currentCount = 1;
  size_t totalItems = 6;

  // Read the Master Pattern Data
  readMasterPatternData(fileId, currentCount, totalItems);

  // Read the Monte Carlo Data
  readMonteCarloData(fileId, currentCount, totalItems);

  if ((m_MasterLPNH.size() == m_MasterSPNH.size()
       && m_MasterLPNH.size() == m_MasterCircle.size()
      && m_MasterLPNH.size() == m_MonteCarloSquare.size()
       && m_MasterLPNH.size() == m_MonteCarloCircle.size()
       && m_MasterLPNH.size() == m_MonteCarloStereo.size()))
  {
    if (m_MasterLPNH.size() > 0)
    {
      emit imageRangeChanged(1, m_MasterLPNH.size());
    }
    else
    {
      emit statusMsgGenerated(tr("Error: No data images were generated."));
    }
  }
  else
  {
    emit statusMsgGenerated(tr("Error: Data Images have different sizes."));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<hsize_t> EMsoftController::readDatasetDimensions(hid_t parentId, QString objectName)
{
  std::vector<hsize_t> dims;
  H5T_class_t classType;
  size_t size;
  hid_t err = H5Lite::getDatasetInfo(parentId, objectName.toStdString(), dims, classType, size);
  if (err < 0)
  {
    emit statusMsgGenerated(tr("Error: Could not read dimensions of object '%1'").arg(objectName));
    return std::vector<hsize_t>();
  }

  return dims;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::generatePatternImagesUsingThread(PatternDisplayWidget::PatternDisplayData patternData, EMsoftController::DetectorData detectorData)
{
  Int32ArrayType::Pointer genericIParPtr = Int32ArrayType::CreateArray(40, QVector<size_t>(1, 1), "IPar");
  genericIParPtr->initializeWithZeros();

  FloatArrayType::Pointer  genericFParPtr = FloatArrayType::CreateArray(40, QVector<size_t>(1, 1), "FPar");
  genericFParPtr->initializeWithZeros();

  int32_t* genericIPar = genericIParPtr->getPointer(0);
  float* genericFPar = genericFParPtr->getPointer(0);

  genericIPar[0] = (m_HeaderData.numsx - 1) / 2;
  genericIPar[8] = m_HeaderData.numset;
  genericIPar[11] = static_cast<int>((m_HeaderData.incidentBeamVoltage - m_HeaderData.minEnergy) / m_HeaderData.energyBinSize) + 1;
  genericIPar[16] = m_HeaderData.npx;

  genericIPar[18] = static_cast<size_t>(detectorData.pixelNumX);
  genericIPar[19] = static_cast<size_t>(detectorData.pixelNumY);

  int binningIndex = 0;
  if (patternData.detectorBinningValue == 2)
  {
    binningIndex = 1;
  }
  else if (patternData.detectorBinningValue == 4)
  {
    binningIndex = 2;
  }
  else if (patternData.detectorBinningValue == 8)
  {
    binningIndex = 3;
  }

  //genericIPar[20] = static_cast<size_t>(genericQuaternionsPtr->getNumberOfTuples()); // number of orientations
  genericIPar[20] = static_cast<size_t>(1); // number of orientations
  genericIPar[21] = binningIndex;
  genericIPar[22] = static_cast<size_t>(detectorData.pixelNumX / patternData.detectorBinningValue);
  genericIPar[23] = static_cast<size_t>(detectorData.pixelNumY / patternData.detectorBinningValue);

  // and set all the float input parameters for the EMsoftCgetEBSDPatterns routine
  // some of these have been set in previous filters
  genericFPar[0] = m_HeaderData.sigma;
  genericFPar[1] = m_HeaderData.omega;

  genericFPar[14] = detectorData.pcPixelsX;         // pattern center x component (in pixel units)
  genericFPar[15] = detectorData.pcPixelsY;         // pattern center y component (in pixel units)
  genericFPar[16] = detectorData.scintillatorPixelSize;       // pixel size (microns) on scintillator surface
  genericFPar[17] = detectorData.detectorTiltAngle;      // detector tilt angle (degrees) from horizontal (positive for detector looking upwards)
  genericFPar[18] = detectorData.scintillatorDist;           // sample-scintillator distance (microns)
  genericFPar[19] = detectorData.beamCurrent; // beam current [nA]
  genericFPar[20] = detectorData.dwellTime;   // beam dwell time per pattern [micro-seconds]
  genericFPar[21] = patternData.gammaValue;  // intensity scaling gamma value

  QVector<size_t> cDims(2);
  cDims[0] = genericIPar[22];
  cDims[1] = genericIPar[23];

  FloatArrayType::Pointer genericEBSDPatternsPtr = FloatArrayType::CreateArray(1, cDims, "ebsdPatterns");
  FloatArrayType::Pointer eulerAngles = patternData.angles;
  PatternListModel* model = PatternListModel::Instance();

  while (m_CurrentOrder.size() > 0)
  {
    if (m_Cancel == true) { return; }

    // Load the next image
    if (m_CurrentOrderLock.tryAcquire() == true)
    {
      int index;
      if (m_PriorityOrder.size() > 0)
      {
        // An index in this thread has been given priority
        index = m_PriorityOrder.front();
        m_PriorityOrder.pop_front();
        m_CurrentOrder.removeAll(index);
      }
      else
      {
        index = m_CurrentOrder.front();
        m_CurrentOrder.pop_front();
      }
      m_CurrentOrderLock.release();

      QModelIndex modelIndex = model->index(index, PatternListItem::DefaultColumn);
      model->setPatternStatus(index, PatternListItem::PatternStatus::Loading);
      emit rowDataChanged(modelIndex, modelIndex);

      bool success = generatePatternImage(index, eulerAngles, m_MasterLPNHData, m_MasterLPSHData, m_MonteCarloSquareData, genericEBSDPatternsPtr, genericIParPtr, genericFParPtr, patternData.patternOrigin);

      if (success == true)
      {
        model->setPatternStatus(index, PatternListItem::PatternStatus::Loaded);
      }
      else
      {
        model->setPatternStatus(index, PatternListItem::PatternStatus::Error);
      }

      m_NumOfFinishedPatternsLock.acquire();
      m_NumOfFinishedPatterns++;
      emit newProgressBarValue(m_NumOfFinishedPatterns);
      m_NumOfFinishedPatternsLock.release();

      emit rowDataChanged(modelIndex, modelIndex);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftController::generatePatternImage(size_t index, FloatArrayType::Pointer eulerAngles, FloatArrayType::Pointer genericLPNHPtr, FloatArrayType::Pointer genericLPSHPtr, Int32ArrayType::Pointer genericAccum_ePtr, FloatArrayType::Pointer genericEBSDPatternsPtr, Int32ArrayType::Pointer genericIParPtr, FloatArrayType::Pointer genericFParPtr, QString patternOrigin)
{
  int32_t* genericIPar = genericIParPtr->getPointer(0);
  float* genericFPar = genericFParPtr->getPointer(0);
  float* genericEBSDPatterns = genericEBSDPatternsPtr->getPointer(0);
  int32_t* genericAccum_e = genericAccum_ePtr->getPointer(0);
  float* genericLPNH = genericLPNHPtr->getPointer(0);
  float* genericLPSH = genericLPSHPtr->getPointer(0);

  FloatArrayType::Pointer genericQuaternionsPtr = FloatArrayType::CreateArray(1, QVector<size_t>(1, 4), "Quats");
  float* genericQuaternions = genericQuaternionsPtr->getPointer(0);

  QVector<float> eulerAngle;
  eulerAngle.push_back(eulerAngles->getComponent(index, 0));
  eulerAngle.push_back(eulerAngles->getComponent(index, 1));
  eulerAngle.push_back(eulerAngles->getComponent(index, 2));

  QVector<float> quat(4);
  OrientationTransforms<QVector<float>,float>::eu2qu(eulerAngle, quat, QuaternionMath<float>::QuaternionScalarVector);
  genericQuaternionsPtr->setComponent(0, 0, quat[0]);
  genericQuaternionsPtr->setComponent(0, 1, quat[1]);
  genericQuaternionsPtr->setComponent(0, 2, quat[2]);
  genericQuaternionsPtr->setComponent(0, 3, quat[3]);

  EMsoftCgetEBSDPatterns(genericIPar, genericFPar, genericEBSDPatterns, genericQuaternions, genericAccum_e, genericLPNH, genericLPSH, nullptr, 0, &m_Cancel);

  QVector<size_t> cDims(2);
  cDims[0] = genericIPar[22];
  cDims[1] = genericIPar[23];

  FloatPair minMaxPair;
  QImage patternImage = createImage<float>(genericEBSDPatternsPtr, cDims[0], cDims[1], 0, minMaxPair);

  GLImageDisplayWidget::GLImageData imageData;
  imageData.image = patternImage;
  imageData.minValue = minMaxPair.first;
  imageData.maxValue = minMaxPair.second;

  m_PatternDisplayWidget->loadImage(index, imageData);

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::generatePatternImages(PatternDisplayWidget::PatternDisplayData patternData, EMsoftController::DetectorData detectorData)
{ 
  m_NumOfFinishedPatterns = 0;
  m_NumOfFinishedThreads = 0;
  m_Watchers.clear();

  FloatArrayType::Pointer eulerAngles = patternData.angles;
  size_t angleCount = eulerAngles->getNumberOfTuples();
  emit newProgressBarMaximumValue(angleCount);

  PatternListModel* model = PatternListModel::Instance();
  for (int i = 0; i < angleCount; i++)
  {
    model->setPatternStatus(i, PatternListItem::PatternStatus::WaitingToLoad);
    if (i == patternData.currentRow)
    {
      // We want to render the current index first
      m_CurrentOrder.push_front(i);
    }
    else
    {
      m_CurrentOrder.push_back(i);
    }
  }

  size_t threads = QThreadPool::globalInstance()->maxThreadCount();
  for (int i = 0; i < threads; i++)
  {
    QSharedPointer<QFutureWatcher<void>> watcher(new QFutureWatcher<void>());
    connect(watcher.data(), SIGNAL(finished()), this, SLOT(threadFinished()));

    QFuture<void> future = QtConcurrent::run(this, &EMsoftController::generatePatternImagesUsingThread, patternData, detectorData);
    watcher->setFuture(future);

    m_Watchers.push_back(watcher);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::addPriorityIndex(size_t index)
{
  m_PriorityOrder.push_back(index);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::updateMPImage(int value, MPMCDisplayWidget::ProjectionMode mode)
{
  QImage image;
  FloatPair minMaxPair;
  if (mode == MPMCDisplayWidget::ProjectionMode::Lambert_Square)
  {
    image = m_MasterLPNH[value - 1];
    minMaxPair = m_MasterLPNHPairs[value - 1];
  }
  else if (mode == MPMCDisplayWidget::ProjectionMode::Lambert_Circle)
  {
    image = m_MasterCircle[value - 1];
    minMaxPair = m_MasterCirclePairs[value - 1];
  }
  else if (mode == MPMCDisplayWidget::ProjectionMode::Stereographic)
  {
    image = m_MasterSPNH[value - 1];
    minMaxPair = m_MasterSPNHPairs[value - 1];
  }

  float keV = m_EkeVs->getValue(value - 1);

  GLImageDisplayWidget::GLImageData imageData;
  imageData.image = image;
  imageData.minValue = minMaxPair.first;
  imageData.maxValue = minMaxPair.second;

  emit mpImageNeedsDisplayed(imageData);
  emit mpKeVNeedsDisplayed(keV);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::updateMCImage(int value, MPMCDisplayWidget::ProjectionMode mode)
{
  QImage image;
  IntPair minMaxPair;
  if (mode == MPMCDisplayWidget::ProjectionMode::Lambert_Square)
  {
    image = m_MonteCarloSquare[value - 1];
    minMaxPair = m_MonteCarloSquarePairs[value - 1];
  }
  else if (mode == MPMCDisplayWidget::ProjectionMode::Lambert_Circle)
  {
    image = m_MonteCarloCircle[value - 1];
    minMaxPair = m_MonteCarloCirclePairs[value - 1];
  }
  else if (mode == MPMCDisplayWidget::ProjectionMode::Stereographic)
  {
    image = m_MonteCarloStereo[value - 1];
    minMaxPair = m_MonteCarloStereoPairs[value - 1];
  }

  float keV = m_EkeVs->getValue(value - 1);

  GLImageDisplayWidget::GLImageData imageData;
  imageData.image = image;
  imageData.minValue = minMaxPair.first;
  imageData.maxValue = minMaxPair.second;

  emit mcImageNeedsDisplayed(imageData);
  emit mcKeVNeedsDisplayed(keV);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::threadFinished()
{
  m_NumOfFinishedThreads++;
  if (m_NumOfFinishedThreads == QThreadPool::globalInstance()->maxThreadCount())
  {
    m_Cancel = false;
    emit generationFinished();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftController::cancelGeneration()
{
  m_Cancel = true;
}

