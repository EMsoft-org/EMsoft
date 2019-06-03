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

#include "MonteCarloFileReader.h"

#include <iostream>

#include <QtCore/QFileInfo>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloFileReader::MonteCarloFileReader() :
  XtalFileReader()
{
  initializeData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloFileReader::~MonteCarloFileReader()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloFileReader::initializeData()
{
  m_SampleTiltAngleSig = -1.0;
  m_SampleRotAngleOmega = -1.0;
  m_SampleStartTiltAngle = -1.0;
  m_SampleEndTiltAngle = -1.0;
  m_SampleTiltStepSize = -1.0;
  m_AcceleratingVoltage = -1.0;
  m_MinEnergyConsider = -1.0;
  m_EnergyBinSize = -1.0;
  m_MaxDepthConsider = -1.0;
  m_DepthStepSize = -1.0;
  m_NumOfPixelsN = -1;
  m_NumOfEPerWorkitem = -1;
  m_TotalNumOfEConsidered = -1;
  m_MultiplierForTotalNumOfE = -1;
  m_GPUPlatformID = -1;
  m_GPUDeviceID = -1;
  m_GlobalWorkGroupSize = -1;
  m_MonteCarloMode = -1;
  m_XtalFileName = "";
  m_AccumzPtr.clear();
  m_IParVector.clear();
  m_FParVector.clear();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::closeFile()
{
  if (XtalFileReader::closeFile())
  {
    initializeData();
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> MonteCarloFileReader::getIParPtr()
{
  if (m_IParVector.empty())
  {
    int numOfPixelsN = 0;
    int globalWorkGrpSize = 0;
    int numOfEPerWorkitem = 0;
    int totEConsidered = 0;
    int multiplier = 0;
    int gpuDeviceID = 0;
    int gpuPlatformID = 0;
    int mcMode = 0;
    double maxDepth = 0.0;
    double depthStepSize = 0.0;
    double accelVoltage = 0.0;
    double minEnergy = 0.0;
    double energyBinSize = 0.0;
    double sampleEndTiltAngle = 0.0;
    double sampleStartTiltAngle = 0.0;
    double sampleTiltStepSize = 0.0;

    // If we couldn't get all of these values, then bail
    if (!getNumberOfPixelsN(numOfPixelsN) ||
        !getGlobalWorkgroupSize(globalWorkGrpSize) ||
        !getNumberOfElectronsPerWorkitem(numOfEPerWorkitem) ||
        !getTotalNumberOfElectronsConsidered(totEConsidered) ||
        !getMultiplierForTotalNumberOfElectrons(multiplier) ||
        !getGPUDeviceID(gpuDeviceID) || !getGPUPlatformID(gpuPlatformID) ||
        !getMaximumDepthToConsider(maxDepth) ||
        !getDepthStepSize(depthStepSize) || !getMonteCarloMode(mcMode))
    {
      return std::vector<int32_t>();
    }

    // Get 'full' mode values, otherwise bail
    if (mcMode == 1 && (!getAcceleratingVoltage(accelVoltage) || !getMinimumEnergyToConsider(minEnergy) || !getEnergyBinSize(energyBinSize)))
    {
      return std::vector<int32_t>();
    }

    // Get 'bse1' mode values, otherwise bail
    if (mcMode != 1 && (!getSampleEndTiltAngle(sampleEndTiltAngle) || !getSampleStartTiltAngle(sampleStartTiltAngle) ||
                        !getSampleTiltStepSize(sampleTiltStepSize)))
    {
      return std::vector<int32_t>();
    }

    m_IParVector = XtalFileReader::getIParPtr();
    if (m_IParVector.empty())
    {
      return std::vector<int32_t>();
    }

    m_IParVector.at(0) = static_cast<int>((numOfPixelsN - 1) / 2); // number of pixels along x
    m_IParVector.at(1) = globalWorkGrpSize;                  // global work group size
    m_IParVector.at(2) = numOfEPerWorkitem; // number of electrons in work group
    m_IParVector.at(3) = totEConsidered;    // total number of electrons in single MCstep
    m_IParVector.at(4) = multiplier;        // multiplier for # of electrons
    m_IParVector.at(5) = gpuDeviceID + 1;   // OpenCL device ID
    m_IParVector.at(6) = gpuPlatformID + 1; // OpenCL platform ID
    m_IParVector.at(12) = static_cast<int>(maxDepth / depthStepSize + 1); // num z bins
    m_IParVector.at(13) = mcMode; // simulation mode (1=full, 2=bse1)

    // this next pair of values is a bit tricky since we use the accum_e and accum_z arrays for
    // two different cases, 'full' and 'bse1'
    if (m_IParVector.at(13) == 1)
    {
      m_IParVector.at(11) = static_cast<int>((accelVoltage - minEnergy) / energyBinSize + 1); // num E bins
      m_IParVector.at(14) = 1;    // only one major loop to be executed
    }
    else
    {
      m_IParVector.at(11) = static_cast<int>((sampleEndTiltAngle - sampleStartTiltAngle) / sampleTiltStepSize + 1); // number of bse1 angles
      m_IParVector.at(14) = m_IParVector.at(11);
    }
    m_IParVector.at(15) = static_cast<int>((numOfPixelsN - 1) / 20); // number of depth bins along x
  }

  return m_IParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> MonteCarloFileReader::getFParPtr()
{
  if (m_FParVector.empty()) {
    double maxDepth = 0.0;
    double depthStepSize = 0.0;
    double accelVoltage = 0.0;
    double minEnergy = 0.0;
    double energyBinSize = 0.0;
    double sampleEndTiltAngle = 0.0;
    double sampleStartTiltAngle = 0.0;
    double sampleTiltStepSize = 0.0;
    double sampleTiltAngleSigma = 0.0;
    double sampleRotAngleOmega = 0.0;

    if (!getSampleTiltAngleSigma(sampleTiltAngleSigma) ||
        !getMaximumDepthToConsider(maxDepth) ||
        !getDepthStepSize(depthStepSize) ||
        !getAcceleratingVoltage(accelVoltage) ||
        !getMinimumEnergyToConsider(minEnergy) ||
        !getEnergyBinSize(energyBinSize) ||
//        !getSampleEndTiltAngle(sampleEndTiltAngle) ||
//        !getSampleStartTiltAngle(sampleStartTiltAngle) ||
//        !getSampleTiltStepSize(sampleTiltStepSize) ||
        !getSampleRotationalAngleOmega(sampleRotAngleOmega))
    {
      return std::vector<float>();
    }

    m_FParVector = XtalFileReader::getFParPtr();

    // fill the m_GenericFPar array
    m_FParVector.at(0) = sampleTiltAngleSigma; // sample tilt angle
    m_FParVector.at(1) = sampleRotAngleOmega;  // omega sample tilt angle
    m_FParVector.at(2) = accelVoltage;         // accelerating voltage
    m_FParVector.at(3) = minEnergy;            // Energy minimum in histogram
    m_FParVector.at(4) = energyBinSize;        // Energy histogram bin size
    m_FParVector.at(5) = maxDepth;             // maximum depth to store
    m_FParVector.at(6) = depthStepSize;        // depth step size
//    m_FParVector.at(7) = sampleStartTiltAngle; // get starting angle
//    m_FParVector.at(8) = sampleEndTiltAngle;   // end angle
//    m_FParVector.at(9) = sampleTiltStepSize;   // angle step size
  }

  return m_FParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getSampleTiltAngleSigma(double &sampleTiltAngleSigma)
{
  if (m_SampleTiltAngleSig < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::sig);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_SampleTiltAngleSig);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20000);
      sampleTiltAngleSigma = -1.0;
      return false;
    }
  }

  sampleTiltAngleSigma = m_SampleTiltAngleSig;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getSampleRotationalAngleOmega(double &sampleRotAngleOmega)
{
  if (m_SampleRotAngleOmega < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::omega);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_SampleRotAngleOmega);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20001);
      sampleRotAngleOmega = -1.0;
      return false;
    }
  }

  sampleRotAngleOmega = m_SampleRotAngleOmega;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getSampleStartTiltAngle(double &sampleStartTiltAngle)
{
  if (m_SampleStartTiltAngle < 0.0)
  {
    // Implement this!
    QString str;
    QTextStream ss(&str);
    ss << "Could not get sample start tilt angle";
    emit errorMessageGenerated(str, -20002);
    sampleStartTiltAngle = -1;
    return false;
  }

  sampleStartTiltAngle = m_SampleStartTiltAngle;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getSampleEndTiltAngle(double &sampleEndTiltAngle)
{
  if (m_SampleEndTiltAngle < 0.0)
  {
    // Implement this!
    QString str;
    QTextStream ss(&str);
    ss << "Could not get sample end tilt angle";
    emit errorMessageGenerated(str, -20003);
    sampleEndTiltAngle = -1;
    return false;
  }

  sampleEndTiltAngle = m_SampleEndTiltAngle;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getSampleTiltStepSize(double &sampleTiltStepSize)
{
  if (m_SampleTiltStepSize < 0)
  {
    // Implement this!
    QString str;
    QTextStream ss(&str);
    ss << "Could not get sample tilt step size";
    emit errorMessageGenerated(str, -20004);
    sampleTiltStepSize = -1;
    return false;
  }

  sampleTiltStepSize = m_SampleTiltStepSize;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getAcceleratingVoltage(double &accelVoltage)
{
  if (m_AcceleratingVoltage < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::EkeV);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_AcceleratingVoltage);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20005);
      accelVoltage = -1.0;
      return false;
    }
  }

  accelVoltage = m_AcceleratingVoltage;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getMinimumEnergyToConsider(double &minEnergy)
{
  if (m_MinEnergyConsider < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::Ehistmin);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_MinEnergyConsider);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20006);
      minEnergy = -1.0;
      return false;
    }
  }

  minEnergy = m_MinEnergyConsider;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getEnergyBinSize(double &energyBinSize)
{
  if (m_EnergyBinSize < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::Ebinsize);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_EnergyBinSize);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20007);
      energyBinSize = -1.0;
      return false;
    }
  }

  energyBinSize = m_EnergyBinSize;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getMaximumDepthToConsider(double &maxDepth)
{
  if (m_MaxDepthConsider < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::depthmax);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_MaxDepthConsider);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20008);
      maxDepth = -1.0;
      return false;
    }
  }

  maxDepth = m_MaxDepthConsider;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getDepthStepSize(double &depthStepSize)
{
  if (m_DepthStepSize < 0.0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::depthstep);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_DepthStepSize);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20009);
      depthStepSize = -1.0;
      return false;
    }
  }

  depthStepSize = m_DepthStepSize;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getNumberOfPixelsN(int &numOfPixelsN)
{
  if (m_NumOfPixelsN < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::numsx);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_NumOfPixelsN);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20010);
      numOfPixelsN = -1;
      return false;
    }
  }

  numOfPixelsN = m_NumOfPixelsN;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getNumberOfElectronsPerWorkitem(int &numOfEPerWorkitem)
{
  if (m_NumOfEPerWorkitem < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::num_el);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_NumOfEPerWorkitem);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20011);
      numOfEPerWorkitem = -1;
      return false;
    }
  }

  numOfEPerWorkitem = m_NumOfEPerWorkitem;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getTotalNumberOfElectronsConsidered(int &totEConsidered)
{
  if (m_TotalNumOfEConsidered < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::totnumel);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_TotalNumOfEConsidered);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20012);
      totEConsidered = -1;
      return false;
    }
  }

  totEConsidered = m_TotalNumOfEConsidered;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getMultiplierForTotalNumberOfElectrons(int &multiplier)
{
  if (m_MultiplierForTotalNumOfE < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::multiplier);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_MultiplierForTotalNumOfE);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20013);
      multiplier = -1;
      return false;
    }
  }

  multiplier = m_MultiplierForTotalNumOfE;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getGPUPlatformID(int &gpuPlatformID)
{
  if (m_GPUPlatformID < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::platid);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_GPUPlatformID);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20014);
      gpuPlatformID = -1;
      return false;
    }
  }

  gpuPlatformID = m_GPUPlatformID;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getGPUDeviceID(int &gpuDeviceID)
{
  if (m_GPUDeviceID < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::devid);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_GPUDeviceID);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20015);
      gpuDeviceID = -1;
      return false;
    }
  }

  gpuDeviceID = m_GPUDeviceID;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getGlobalWorkgroupSize(int &globalWorkgrpSize)
{
  if (m_GlobalWorkGroupSize < 0)
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::globalworkgrpsz);
    herr_t err = QH5Lite::readScalarDataset(getFileId(), path, m_GlobalWorkGroupSize);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20016);
      globalWorkgrpSize = -1;
      return false;
    }
  }

  globalWorkgrpSize = m_GlobalWorkGroupSize;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getMonteCarloMode(int &mcMode)
{
  if (m_MonteCarloMode < 0)
  {
    QString modeStr = "";
    QString path = QString("%1/%2/%3")
                       .arg(EMsoft::Constants::NMLparameters)
                       .arg(EMsoft::Constants::MCCLNameList)
                       .arg(EMsoft::Constants::mode);
    herr_t err = QH5Lite::readStringDataset(getFileId(), path, modeStr);
    if (err < 0) {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20017);
      mcMode = -1;
      return false;
    }

    if (modeStr == "full") {
      m_MonteCarloMode = 1;
    } else if (modeStr == "bse1") {
      m_MonteCarloMode = 2;
    }
  }

  mcMode = m_MonteCarloMode;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloFileReader::getXtalFileName(QString &xtalFileName)
{
  if (m_XtalFileName.isEmpty())
  {
    QString path = QString("%1/%2/%3").arg(EMsoft::Constants::NMLparameters).arg(EMsoft::Constants::MCCLNameList).arg(EMsoft::Constants::xtalname);
    herr_t err = QH5Lite::readStringDataset(getFileId(), path, m_XtalFileName);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20018);
      xtalFileName = QString();
      return false;
    }
  }

  xtalFileName = m_XtalFileName;
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> MonteCarloFileReader::getAccumePtr()
{
  if (m_AccumePtr.empty())
  {
    //--------------------------
    // read a bunch of data from the Monte Carlo group (used to be a separate file in previous version)
    std::vector<int32_t> accum_e;

    QString path = QString("/%1/%2/%3").arg(EMsoft::Constants::EMData).arg(EMsoft::Constants::MCOpenCL).arg(EMsoft::Constants::accume);
    herr_t err = QH5Lite::readVectorDataset(getFileId(), path, accum_e);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20019);
      return std::vector<int32_t>();
    }

    // get the array dimensions
    QVector<hsize_t> dims;
    H5T_class_t type_class;
    size_t type_size = 0;
    err = QH5Lite::getDatasetInfo(getFileId(), path, dims, type_class, type_size);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20020);
      return std::vector<int32_t>();
    }

    // Create a new GenericAccumz Array
    int size = std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<hsize_t>());
    m_AccumePtr.resize(size);
    std::fill(m_AccumePtr.begin(), m_AccumePtr.end(), 0);

    for(size_t i = 0; i < m_AccumePtr.size(); i++)
    {
      m_AccumePtr.at(i) = accum_e.at(i);
    }
  }

  return m_AccumePtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> MonteCarloFileReader::getAccumzPtr()
{
  if (m_AccumzPtr.empty())
  {
    //--------------------------
    // read a bunch of data from the Monte Carlo group (used to be a separate file in previous version)
    std::vector<int32_t> accum_z;

    QString path = QString("/%1/%2/%3").arg(EMsoft::Constants::EMData).arg(EMsoft::Constants::MCOpenCL).arg(EMsoft::Constants::accumz);
    herr_t err = QH5Lite::readVectorDataset(getFileId(), path, accum_z);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20021);
      return std::vector<int32_t>();
    }

    // get the array dimensions
    QVector<hsize_t> dims;
    H5T_class_t type_class;
    size_t type_size = 0;
    err = QH5Lite::getDatasetInfo(getFileId(), path, dims, type_class, type_size);
    if(err < 0)
    {
      QString str;
      QTextStream ss(&str);
      ss << "Error reading data set " << path;
      emit errorMessageGenerated(str, -20022);
      return std::vector<int32_t>();
    }

    // Create a new GenericAccumz Array
    int size = std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<hsize_t>());
    m_AccumzPtr.resize(size);
    std::fill(m_AccumzPtr.begin(), m_AccumzPtr.end(), 0);

    for(size_t i = 0; i < accum_z.size(); i++)
    {
      m_AccumzPtr[i] = accum_z[i];
    }
  }

  return m_AccumzPtr;
}
