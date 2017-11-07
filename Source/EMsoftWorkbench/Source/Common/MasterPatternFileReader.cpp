/* ============================================================================
* Copyright (c) 2009-2016 BlueQuartz Software, LLC
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

#include "MasterPatternFileReader.h"

#include <QtCore/QFileInfo>

#include "Common/Constants.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternFileReader::MasterPatternFileReader(const QString &filePath, IObserver* obs) :
  m_Observer(obs)
{
  QFileInfo fi(filePath);

  m_FileId = H5Utilities::openFile(filePath.toStdString(), true);
  if (m_FileId < 0)
  {
    if (obs != nullptr)
    {
      obs->processObserverMessage(QObject::tr("Error: Unable to open data file '%1'").arg(fi.fileName()));
    }
    return;
  }
  else
  {
    if (obs != nullptr)
    {
      obs->processObserverMessage(QObject::tr("Reading data file '%1'...").arg(fi.fileName()));
    }

    double fileSize = fi.size();      // This is in bytes
    fileSize = fileSize / 1000000;    // Convert to MB

    if (obs != nullptr)
    {
      obs->processObserverMessage(QObject::tr("File Size: %1 MB\n").arg(QString::number(fileSize, 'f', 2)));
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternFileReader::~MasterPatternFileReader()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternFileReader::MasterPatternData MasterPatternFileReader::readMasterPatternData()
{
  MasterPatternFileReader::MasterPatternData mpData;

  QString objName = EMsoft::Constants::EMheader + "/" + EMsoft::Constants::EBSDmaster;
  hid_t ebsdMasterHeaderId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (ebsdMasterHeaderId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  HDF5ScopedGroupSentinel sentinel(&ebsdMasterHeaderId, true);

  mpData.mpProgramName = readStringDataset(ebsdMasterHeaderId, EMsoft::Constants::ProgramName);
  mpData.mpVersionId = readStringDataset(ebsdMasterHeaderId, EMsoft::Constants::Version);

  objName = EMsoft::Constants::EMheader + "/" + EMsoft::Constants::MCOpenCL;
  hid_t mcOpenCLHeaderId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (mcOpenCLHeaderId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  sentinel.addGroupId(&mcOpenCLHeaderId);

  mpData.mcProgramName = readStringDataset(mcOpenCLHeaderId, EMsoft::Constants::ProgramName);
  mpData.mcVersionId = readStringDataset(mcOpenCLHeaderId, EMsoft::Constants::Version);

  objName = EMsoft::Constants::EMData + "/" + EMsoft::Constants::EBSDmaster;
  hid_t ebsdMasterDataId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (ebsdMasterDataId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  sentinel.addGroupId(&ebsdMasterDataId);

  mpData.masterLPNHData = readArrayDataset<float>(ebsdMasterDataId, EMsoft::Constants::mLPNH);

  mpData.mLPNH_dims = readDatasetDimensions(ebsdMasterDataId, EMsoft::Constants::mLPNH);

  mpData.masterLPSHData = readArrayDataset<float>(ebsdMasterDataId, EMsoft::Constants::mLPSH);

  mpData.mLPSH_dims = readDatasetDimensions(ebsdMasterDataId, EMsoft::Constants::mLPSH);

  mpData.masterSPNHData = readArrayDataset<float>(ebsdMasterDataId, EMsoft::Constants::masterSPNH);

  mpData.masterSPNH_dims = readDatasetDimensions(ebsdMasterDataId, EMsoft::Constants::masterSPNH);

  mpData.numset = readScalarDataset<int>(ebsdMasterDataId, EMsoft::Constants::numset);

  mpData.ekevs = readArrayDataset<float>(ebsdMasterDataId, EMsoft::Constants::EkeVs);

  mpData.numMPEnergyBins = readScalarDataset<int>(ebsdMasterDataId, EMsoft::Constants::numEbins);

  objName = EMsoft::Constants::EMData + "/" + EMsoft::Constants::MCOpenCL;
  hid_t mcOpenCLDataId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (mcOpenCLDataId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  sentinel.addGroupId(&mcOpenCLDataId);

  mpData.monteCarloSquareData = readArrayDataset<int32_t>(mcOpenCLDataId, EMsoft::Constants::accume);

  mpData.monteCarlo_dims = readDatasetDimensions(mcOpenCLDataId, EMsoft::Constants::accume);

  mpData.numDepthBins = readScalarDataset<int>(mcOpenCLDataId, EMsoft::Constants::numzbins);
  mpData.numMCEnergyBins = readScalarDataset<int>(mcOpenCLDataId, EMsoft::Constants::numzbins);

  objName = EMsoft::Constants::NMLparameters + "/" + EMsoft::Constants::MCCLNameList;
  hid_t mcclNameListId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (mcclNameListId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  sentinel.addGroupId(&mcclNameListId);

  mpData.numsx = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::numsx);
  mpData.mcStructureFileName = readStringDataset(mcclNameListId, EMsoft::Constants::xtalname);
  mpData.incidentBeamVoltage = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::EkeV);
  mpData.mcMode = readStringDataset(mcclNameListId, EMsoft::Constants::MCmode);
  mpData.omega = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::omega);
  mpData.sigma = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::sig);
  mpData.minEnergy = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::Ehistmin);
  mpData.maxEnergy = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::EkeV);
  mpData.energyBinSize = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::Ebinsize);
  mpData.maxDepth = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::depthmax);
  mpData.depthStep = readScalarDataset<float>(mcclNameListId, EMsoft::Constants::depthstep);
  mpData.totalNumIncidentEl = readScalarDataset<int>(mcclNameListId, EMsoft::Constants::totnumel);

  objName = EMsoft::Constants::NMLparameters + "/" + EMsoft::Constants::EBSDMasterNameList;
  hid_t ebsdMasterNameListId = H5Utilities::openHDF5Object(m_FileId, objName.toStdString());
  if (ebsdMasterNameListId < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Unable to open object at path '%1'").arg(objName));
    }
  }
  sentinel.addGroupId(&ebsdMasterNameListId);

  mpData.npx = readScalarDataset<int>(ebsdMasterNameListId, EMsoft::Constants::npx);

  return mpData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<hsize_t> MasterPatternFileReader::readDatasetDimensions(hid_t parentId, QString objectName)
{
  std::vector<hsize_t> dims;
  H5T_class_t classType;
  size_t size;
  hid_t err = H5Lite::getDatasetInfo(parentId, objectName.toStdString(), dims, classType, size);
  if (err < 0)
  {
    if (m_Observer != nullptr)
    {
      m_Observer->processObserverMessage(QObject::tr("Error: Could not read dimensions of object '%1'").arg(objectName));
    }
    return std::vector<hsize_t>();
  }

  return dims;
}
