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

#include "EMsoftFileWriter.h"

#include <iostream>

#include <QtCore/QFileInfo>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftFileWriter::EMsoftFileWriter() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftFileWriter::~EMsoftFileWriter()
{
  while (!m_IdStack.isEmpty())
  {
    closeGroup();
  }

  if (m_FileId >= 0)
  {
    closeFile();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftFileWriter::openFile(const QString &filePath)
{
  QFileInfo fi(filePath);
  if (fi.exists())
  {
    m_FileId = QH5Utilities::openFile(filePath);
  }
  else
  {
    m_FileId = QH5Utilities::createFile(filePath);
  }

  if (m_FileId < 0)
  {
    QString ss = QObject::tr("Error creating HDF5 Data File '%1'").arg(filePath);
    emit errorMessageGenerated(ss, -10001);
    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftFileWriter::closeFile()
{
  if (QH5Utilities::closeFile(m_FileId) < 0)
  {
    QString ss = QObject::tr("Error closing HDF5 file.'");
    emit errorMessageGenerated(ss, -10002);
    return false;
  }

  m_IdStack.clear();
  m_FileId = -1;

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftFileWriter::openGroup(const QString &groupName)
{
  hid_t groupId;
  hid_t locId;
  if (m_IdStack.isEmpty())
  {
    locId = m_FileId;
  }
  else
  {
    locId = m_IdStack.top();
  }

  if (static_cast<bool>(H5Lexists(locId, groupName.toStdString().c_str(), H5P_DEFAULT)))
  {
    groupId = QH5Utilities::openHDF5Object(locId, groupName);
  }
  else
  {
    groupId = QH5Utilities::createGroup(locId, groupName);
  }

  if (groupId < 0)
  {
    QString parentPath = QH5Utilities::getObjectPath(m_IdStack.top());
    QString ss = QObject::tr("Error opening HDF5 group at path '%1/%2'").arg(parentPath, groupName);
    emit errorMessageGenerated(ss, -10005);
    return false;
  }

  m_IdStack.push(groupId);
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftFileWriter::closeGroup()
{
  if (m_IdStack.isEmpty())
  {
    QString ss = QObject::tr("Error closing HDF5 group: no groups are open.'");
    emit errorMessageGenerated(ss, -10006);
    return false;
  }

  hid_t id = m_IdStack.pop();
  herr_t err = QH5Utilities::closeHDF5Object(id);
  if (err < 0)
  {
    QString ss = QObject::tr("Error closing HDF5 group at path '%1'").arg(QH5Utilities::getObjectPath(id));
    emit errorMessageGenerated(ss, -10007);
    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hid_t EMsoftFileWriter::getFileId() const
{
  return m_FileId;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hid_t EMsoftFileWriter::getCurrentLocId() const
{
  if (m_IdStack.isEmpty())
  {
    return m_FileId;
  }

  return m_IdStack.top();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftFileWriter::writeStringDataset(const QString &dsetName, const QString &value) const
{
  hid_t locId = getCurrentLocId();
  herr_t err = QH5Lite::writeStringDataset(locId, dsetName, value );
  if(err < 0)
  {
    QString str = QObject::tr("Error writing data set %1/%2").arg(QH5Utilities::getObjectPath(locId), dsetName);
    emit errorMessageGenerated(str, -10008);
    return false;
  }

  return true;
}
