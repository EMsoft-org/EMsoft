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

#ifndef _xtalfilereader_h_
#define _xtalfilereader_h_

#include <QtCore/QObject>
#include <QtCore/QString>

#include <H5Support/QH5Lite.h>
#include <H5Support/QH5Utilities.h>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include <hdf5.h>

class XtalFileReader : public QObject
{
    Q_OBJECT

  public:
    XtalFileReader();
    ~XtalFileReader() override;

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
    virtual bool closeFile();

    hid_t getFileId();
    bool getAtomPos(std::vector<float> &atomPos);
    bool getAtomTypes(std::vector<int32_t> &atomTypes);
    bool getLatticeParameters(std::vector<float> &latParam);
    bool getCreationDate(QString &creationDate);
    bool getCreationTime(QString &creationTime);
    bool getCreator(QString &creator);
    bool getProgramName(QString &programName);
    bool getCrystalSystem(int &crystalSystem);
    bool getNatomTypes(int &natomTypes);
    bool getSpaceGroupNumber(int &spaceGroupNumber);
    bool getSpaceGroupSetting(int &spaceGroupSetting);
    std::vector<int32_t> getIParPtr();
    std::vector<float> getFParPtr();

  signals:
    void errorMessageGenerated(const QString &msg, int code);

  private:
    hid_t                           m_FileId = -1;
    QString                         m_StartTime = "";

    std::vector<float>              m_Atompos;
    std::vector<int32_t>            m_Atomtypes;
    std::vector<float>              m_Latparm;
    QString                         m_CreationDate;
    QString                         m_CreationTime;
    QString                         m_Creator;
    QString                         m_ProgramName;

    int                             m_CrystalSystem = -1;
    int                             m_Natomtypes = -1;
    int                             m_SpaceGroupNumber = -1;
    int                             m_SpaceGroupSetting = -1;

    std::vector<int32_t> m_IParVector;
    std::vector<float> m_FParVector;

    /**
     * @brief initializeData
     */
    void initializeData();

    XtalFileReader(const XtalFileReader&);    // Copy Constructor Not Implemented
    void operator=(const XtalFileReader&);  // Operator '=' Not Implemented
};

#endif /* _xtalfilereader_h_ */
