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

#ifndef _masterpatternfilereader_h_
#define _masterpatternfilereader_h_

#include <QtCore/QObject>
#include <QtCore/QString>

#include <H5Support/QH5Lite.h>
#include <H5Support/QH5Utilities.h>

#include "SIMPLib/DataArrays/DataArray.hpp"
#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include "Common/IObserver.h"

#include <hdf5.h>

class MasterPatternFileReader
{
  public:
    MasterPatternFileReader(const QString &filePath, IObserver* obs);
    ~MasterPatternFileReader();

    SIMPL_INSTANCE_PROPERTY(IObserver*, Observer)

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
      FloatArrayType::Pointer ekevs;
      FloatArrayType::Pointer masterLPNHData;
      std::vector<hsize_t>    mLPNH_dims;
      FloatArrayType::Pointer masterLPSHData;
      std::vector<hsize_t>    mLPSH_dims;
      FloatArrayType::Pointer masterSPNHData;
      std::vector<hsize_t>    masterSPNH_dims;

      // EMData/MCOpenCL
      int numDepthBins;
      int numMCEnergyBins;
      Int32ArrayType::Pointer monteCarloSquareData;
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
    MasterPatternFileReader::MasterPatternData readMasterPatternData();

  private:
    hid_t                           m_FileId = -1;

    QString                         m_ErrorMessage = "";
    int                             m_ErrorCode = 0;

    /**
     * @brief readDatasetDimensions
     * @param parentId
     * @param objectName
     * @return
     */
    std::vector<hsize_t> readDatasetDimensions(hid_t parentId, QString objectName);

    /**
     * @brief readStringDataset
     * @param parentId
     * @param objectName
     * @return
     */
    QString readStringDataset(const hid_t &parentId, const QString &objectName)
    {
      QString value = "";
      if (QH5Lite::readStringDataset(parentId, objectName, value) < 0)
      {
        QString objPath = QH5Utilities::getObjectPath(parentId);
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
    T readScalarDataset(const hid_t &parentId, const QString &objectName)
    {
      T value = -1;
      if (QH5Lite::readScalarDataset(parentId, objectName, value) < 0)
      {
        QString objPath = QH5Utilities::getObjectPath(parentId);
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
    typename DataArray<T>::Pointer readArrayDataset(hid_t parentId, QString objectName)
    {
      std::vector<hsize_t> dims = readDatasetDimensions(parentId, objectName);
      if (dims.size() <= 0) { return DataArray<T>::NullPointer(); }

      size_t numTuples = dims[0];
      for (int i = 1; i < dims.size(); i++)
      {
        numTuples = numTuples * dims[i];
      }

      typename DataArray<T>::Pointer dataArray = DataArray<T>::CreateArray(numTuples, QVector<size_t>(1, 1), objectName);
      herr_t mLPNH_id = QH5Lite::readPointerDataset(parentId, objectName, dataArray->getPointer(0));
      if (mLPNH_id < 0)
      {
//        emit stdOutputMessageGenerated(tr("Error: Could not read object '%1'").arg(objectName));
        return DataArray<T>::NullPointer();
      }

      return dataArray;
    }

    MasterPatternFileReader(const MasterPatternFileReader&);    // Copy Constructor Not Implemented
    void operator=(const MasterPatternFileReader&);  // Operator '=' Not Implemented
};

#endif /* _masterpatternfilereader_h_ */
