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

#pragma once

#include <QtCore/QObject>
#include <QtCore/QString>

#include <H5Support/QH5Lite.h>
#include <H5Support/QH5Utilities.h>


#include "Common/IObserver.h"

#include <hdf5.h>

class MasterPatternFileReader
{
  public:
    MasterPatternFileReader(const QString &filePath, IObserver* obs);
    virtual ~MasterPatternFileReader();

    /**
    * @brief Setter property for Observer
    */
    void setObserver(IObserver *value);

    /**
    * @brief Getter property for Observer
    * @return Value of Observer
    */
    IObserver* getObserver() const;

    struct MasterPatternData
    {
      // EMheader/EBSDmaster
      QString mpProgramName;
      QString mpVersionId;

      // EMheader/MCOpenCL
      QString mcProgramName;
      QString mcVersionId;

      // EMData/EBSDmaster
      int numMPEnergyBins;
      int numset;
      std::vector<float> ekevs;
      std::vector<float> masterLPNHData;
      std::vector<hsize_t>    mLPNH_dims;
      std::vector<float> masterLPSHData;
      std::vector<hsize_t>    mLPSH_dims;
      std::vector<float> masterSPNHData;
      std::vector<hsize_t>    masterSPNH_dims;

      // EMData/MCOpenCL
      int numDepthBins;
      int numMCEnergyBins;
      std::vector<int32_t> monteCarloSquareData;
      std::vector<hsize_t>    monteCarlo_dims;

      // NMLparameters/EBSDMasterNameList
      int npx;

      // NMLparameters/MCCLNameList
      QString mcStructureFileName;
      double incidentBeamVoltage;
      QString mcMode;
      double omega;
      double sigma;
      double minEnergy;
      double maxEnergy;
      double energyBinSize;
      double maxDepth;
      double depthStep;
      int totalNumIncidentEl;
      int numsx;
    };

    /**
     * @brief MasterPatternFileReader::readMasterPatternData
     * @return
     */
    MasterPatternFileReader::MasterPatternData readMasterPatternData() const;

  private:
    IObserver* m_Observer = nullptr;

    hid_t m_FileId = -1;

    /**
     * @brief readDatasetDimensions
     * @param parentId
     * @param objectName
     * @return
     */
    std::vector<hsize_t> readDatasetDimensions(hid_t parentId, const QString &objectName) const;

    /**
     * @brief readStringDataset
     * @param parentId
     * @param objectName
     * @return
     */
    QString readStringDataset(const hid_t &parentId, const QString &objectName) const
    {
      QString value = "";
      if (QH5Lite::readStringDataset(parentId, objectName, value) < 0)
      {
//        QString objPath = QH5Utilities::getObjectPath(parentId);
//        emit stdOutputMessageGenerated(tr("Error: Unable to read string dataset '%1' at path '%2'").arg(objectName).arg(objPath));
      }

      return value;
    }

    template <typename T>
    /**
     * @brief readScalarDataset
     * @param parentId
     * @param objectName
     * @return
     */
    T readScalarDataset(const hid_t &parentId, const QString &objectName) const
    {
      T value = -1;
      if (QH5Lite::readScalarDataset(parentId, objectName, value) < 0)
      {
//        QString objPath = QH5Utilities::getObjectPath(parentId);
//        emit stdOutputMessageGenerated(tr("Error: Unable to read scalar dataset '%1' at path '%2'").arg(objectName).arg(objPath));
      }

      return value;
    }

    /**
     * @brief readDataset
     * @param parentId
     * @param objectName
     * @return
     */
    template <typename T>
    std::vector<T> readArrayDataset(hid_t parentId, QString objectName) const
    {
      std::vector<hsize_t> dims = readDatasetDimensions(parentId, objectName);
      if (dims.empty())
      {
        return std::vector<T>();
      }

      size_t numTuples = dims[0];
      for (int i = 1; i < dims.size(); i++)
      {
        numTuples = numTuples * dims[i];
      }

      std::vector<T> dataArray = std::vector<T>(numTuples);
      herr_t mLPNH_id = QH5Lite::readPointerDataset(parentId, objectName, dataArray.data());
      if (mLPNH_id < 0)
      {
//        emit stdOutputMessageGenerated(tr("Error: Could not read object '%1'").arg(objectName));
        return std::vector<T>();
      }

      return dataArray;
    }

  public:
    MasterPatternFileReader(const MasterPatternFileReader&) = delete; // Copy Constructor Not Implemented
    MasterPatternFileReader(MasterPatternFileReader&&) = delete;      // Move Constructor Not Implemented
    MasterPatternFileReader& operator=(const MasterPatternFileReader&) = delete; // Copy Assignment Not Implemented
    MasterPatternFileReader& operator=(MasterPatternFileReader&&) = delete;      // Move Assignment Not Implemented
};
