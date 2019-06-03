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

#include "XtalFileReader.h"

#include <iostream>

#include <QtCore/QFileInfo>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Common/Constants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
XtalFileReader::XtalFileReader()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
XtalFileReader::~XtalFileReader()
{
  H5Utilities::closeFile(m_FileId);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::openFile(const QString &filePath)
{
  QFileInfo fi(filePath);
  m_FileId = H5Utilities::openFile(filePath.toStdString(), true);
  if (m_FileId < 0)
  {
    QString ss = QObject::tr("Error opening HDF5 Data File '%1'").arg(fi.fileName());
    emit errorMessageGenerated(ss, -10000);
    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void XtalFileReader::initializeData()
{
  m_StartTime.clear();
  m_Atompos.clear();
  m_Atomtypes.clear();
  m_Latparm.clear();
  m_CreationDate.clear();
  m_CreationTime.clear();
  m_Creator.clear();
  m_ProgramName.clear();

  m_CrystalSystem = -1;
  m_Natomtypes = -1;
  m_SpaceGroupNumber = -1;
  m_SpaceGroupSetting = -1;

  m_IParVector.clear();
  m_FParVector.clear();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::closeFile()
{
  if (QH5Utilities::closeFile(m_FileId))
  {
    initializeData();
    m_FileId = -1;
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hid_t XtalFileReader::getFileId()
{
  return m_FileId;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> XtalFileReader::getIParPtr()
{
  if (m_IParVector.empty())
  {
    int crystalSystem = 0;
    int natomTypes = 0;
    int spaceGroupNumber = 0;
    int spaceGroupSetting = 0;

    if (!getCrystalSystem(crystalSystem) || !getNatomTypes(natomTypes) ||
        !getSpaceGroupNumber(spaceGroupNumber) ||
        !getSpaceGroupSetting(spaceGroupSetting)) {
      return std::vector<int32_t>();
    }

    m_IParVector.resize(EMsoftWorkbenchConstants::Constants::IParSize);
    std::fill(m_IParVector.begin(), m_IParVector.end(), 0);

    m_IParVector.at(7) = crystalSystem;      // crystal system ID
    m_IParVector.at(8) = natomTypes;         // # atoms in asymmetric unit
    m_IParVector.at(9) = spaceGroupNumber;   // # space group
    m_IParVector.at(10) = spaceGroupSetting; // space group origin setting
  }

  return m_IParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> XtalFileReader::getFParPtr()
{
  if (m_FParVector.empty())
  {
    QVector<size_t> cDims = { EMsoftWorkbenchConstants::Constants::FParSize };

    m_FParVector.resize(EMsoftWorkbenchConstants::Constants::FParSize);
    std::fill(m_FParVector.begin(), m_FParVector.end(), 0.0f);

    // Fill the fPar vector with values
  }

  return m_FParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getAtomPos(std::vector<float> &atomPos) {
  if (m_Atompos.empty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::AtomData);
    herr_t err = QH5Lite::readVectorDataset(m_FileId, path, m_Atompos);
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10001);
      atomPos = std::vector<float>();
      return false;
    }
  }

  atomPos = m_Atompos;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getAtomTypes(std::vector<int32_t> &atomTypes) {
  if (m_Atomtypes.empty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::Atomtypes);
    herr_t err = QH5Lite::readVectorDataset(m_FileId, path, m_Atomtypes);
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10002);
      atomTypes = std::vector<int32_t>();
      return false;
    }
  }

  atomTypes = m_Atomtypes;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getLatticeParameters(std::vector<float> &latParm) {
  if (m_Latparm.empty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::LatticeParameters);
    herr_t err = QH5Lite::readVectorDataset(m_FileId, path, m_Latparm);
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10003);
      latParm = std::vector<float>();
      return false;
    }
  }

  latParm = m_Latparm;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getCreationDate(QString &creationDate) {
  if (m_CreationDate.isEmpty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::CreationDate);
    herr_t err = QH5Lite::readStringDataset(m_FileId, path, m_CreationDate );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10004);
      creationDate = QString();
      return false;
    }
  }

  creationDate = m_CreationDate;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getCreationTime(QString &creationTime) {
  if (m_CreationTime.isEmpty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::CreationTime);
    herr_t err = QH5Lite::readStringDataset(m_FileId, path, m_CreationTime );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10005);
      creationTime = QString();
      return false;
    }
  }

  creationTime = m_CreationTime;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getCreator(QString &creator) {
  if (m_Creator.isEmpty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::Creator);
    herr_t err = QH5Lite::readStringDataset(m_FileId, path, m_Creator );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10006);
      creator = QString();
      return false;
    }
  }

  creator = m_Creator;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getProgramName(QString &programName) {
  if (m_ProgramName.isEmpty())
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::ProgramName);
    herr_t err = QH5Lite::readStringDataset(m_FileId, path, m_ProgramName );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10007);
      programName = QString();
      return false;
    }
  }

  programName = m_ProgramName;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getCrystalSystem(int &crystalSystem) {
  if (m_CrystalSystem < 0)
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::CrystalSystem);
    herr_t err = QH5Lite::readScalarDataset(m_FileId, path, m_CrystalSystem );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10008);
      crystalSystem = -1;
      return false;
    }
  }

  crystalSystem = m_CrystalSystem;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getNatomTypes(int &natomTypes) {
  if (m_Natomtypes < 0)
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::Natomtypes);
    herr_t err = QH5Lite::readScalarDataset(m_FileId, path, m_Natomtypes );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10009);
      natomTypes = -1;
      return false;
    }
  }

  natomTypes = m_Natomtypes;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getSpaceGroupNumber(int &spaceGroupNumber) {
  if (m_SpaceGroupNumber < 0)
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::SpaceGroupNumber);
    herr_t err = QH5Lite::readScalarDataset(m_FileId, path, m_SpaceGroupNumber );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10010);
      spaceGroupNumber = -1;
      return false;
    }
  }

  spaceGroupNumber = m_SpaceGroupNumber;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool XtalFileReader::getSpaceGroupSetting(int &spaceGroupSetting) {
  if (m_SpaceGroupSetting < 0)
  {
    QString path = QString("%1/%2").arg(EMsoft::Constants::CrystalData).arg(EMsoft::Constants::SpaceGroupSetting);
    herr_t err = QH5Lite::readScalarDataset(m_FileId, path, m_SpaceGroupSetting );
    if(err < 0)
    {
      QString ss = tr("Error reading data set %1").arg(path);
      emit errorMessageGenerated(ss, -10011);
      spaceGroupSetting = -1;
      return false;
    }
  }

  spaceGroupSetting = m_SpaceGroupSetting;
  return true;
}
