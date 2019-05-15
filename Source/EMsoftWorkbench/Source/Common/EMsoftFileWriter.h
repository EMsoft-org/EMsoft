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

#ifndef _emsoftfilewriter_h_
#define _emsoftfilewriter_h_

#include <QtCore/QObject>
#include <QtCore/QString>
#include <QtCore/QStack>

#include <H5Support/QH5Lite.h>
#include <H5Support/QH5Utilities.h>

#include "SIMPLib/DataArrays/DataArray.hpp"
#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include <hdf5.h>

class EMsoftFileWriter : public QObject
{
    Q_OBJECT

  public:
    EMsoftFileWriter();
    ~EMsoftFileWriter() override;

    /**
     * @brief openFile
     * @param filePath
     * @return
     */
    bool openFile(const QString &filePath);

    /**
     * @brief closeFile
     * @return
     */
    bool closeFile();

    /**
     * @brief openGroup
     * @param groupName
     * @return
     */
    bool openGroup(const QString &groupName);

    /**
     * @brief closeGroup
     * @return
     */
    bool closeGroup();

    hid_t getFileId();
    hid_t getCurrentLocId();

    /**
     * @brief writeScalarDataset
     * @param dsetName
     * @param value
     * @return
     */
    template <typename T>
    bool writeScalarDataset(const QString &dsetName, T value)
    {
      hid_t locId = getCurrentLocId();
      herr_t err = QH5Lite::writeScalarDataset(locId, dsetName, value);
      if(err < 0)
      {
        QString str = QObject::tr("Error writing data set %1/%2").arg(QH5Utilities::getObjectPath(locId)).arg(dsetName);
        emit errorMessageGenerated(str, -10005);
        return false;
      }

      return true;
    }

    bool writeStringDataset(const QString &dsetName, const QString &value);

    /**
     * @brief writeVectorDataset
     * @param dsetName
     * @param value
     * @param dims
     * @return
     */
    template <typename T>
    bool writeVectorDataset(const QString &dsetName, QVector<T> value, QVector<hsize_t> dims)
    {
      hid_t locId = getCurrentLocId();
      herr_t err = QH5Lite::writeVectorDataset(locId, dsetName, dims, value);
      if(err < 0)
      {
        QString str = QObject::tr("Error writing data set %1/%2").arg(QH5Utilities::getObjectPath(locId)).arg(dsetName);
        emit errorMessageGenerated(str, -10007);
        return false;
      }

      return true;
    }

    /**
     * @brief writeScalarDataset
     * @param dsetName
     * @param value
     * @return
     */
    template <typename T>
    bool writePointerDataset(const QString &dsetName, T* value, QVector<hsize_t> dims)
    {
      hid_t locId = getCurrentLocId();
      int32_t rank = dims.size();
      int err = QH5Lite::writePointerDataset(locId, dsetName, rank, dims.data(), value);
      if(err < 0)
      {
        QString str = QObject::tr("Error writing data set %1/%2").arg(QH5Utilities::getObjectPath(locId)).arg(dsetName);
        emit errorMessageGenerated(str, -20019);
        return false;
      }

      return true;
    }

  signals:
    void errorMessageGenerated(const QString &msg, int code);

  private:
    hid_t                           m_FileId = -1;
    QStack<hid_t>                   m_IdStack;

    EMsoftFileWriter(const EMsoftFileWriter&);    // Copy Constructor Not Implemented
    void operator=(const EMsoftFileWriter&);  // Operator '=' Not Implemented
};

#endif /* _emsoftfilewriter_h_ */
