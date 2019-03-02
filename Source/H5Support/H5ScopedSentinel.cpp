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

#include "H5ScopedSentinel.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedFileSentinel::H5ScopedFileSentinel(hid_t* fileId, bool turnOffErrors)
: m_FileId(fileId)
, m_TurnOffErrors(turnOffErrors)
{
  if(m_TurnOffErrors)
  {
    H5Eget_auto(H5E_DEFAULT, &_oldHDF_error_func, &_oldHDF_error_client_data);
    H5Eset_auto(H5E_DEFAULT, nullptr, nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedFileSentinel::~H5ScopedFileSentinel()
{
  if(m_TurnOffErrors)
  {
    H5Eset_auto(H5E_DEFAULT, _oldHDF_error_func, _oldHDF_error_client_data);
  }
  for(auto temp : m_Groups)
  {
    if(*temp > 0)
    {
      H5Gclose(*temp);
      *temp = -1;
    }
  }

  if(*m_FileId > 0)
  {
    H5Utilities::closeFile(*m_FileId);
    *m_FileId = -1;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void H5ScopedFileSentinel::setFileId(hid_t* fileId)
{
  m_FileId = fileId;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hid_t* H5ScopedFileSentinel::getFileId()
{
  return m_FileId;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void H5ScopedFileSentinel::addGroupId(hid_t* gid)
{
  m_Groups.push_back(gid);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedGroupSentinel::H5ScopedGroupSentinel(hid_t* gid, bool turnOffErrors)
: m_TurnOffErrors(turnOffErrors)
{
  m_Groups.push_back(gid);
  if(m_TurnOffErrors)
  {
    H5Eget_auto(H5E_DEFAULT, &_oldHDF_error_func, &_oldHDF_error_client_data);
    H5Eset_auto(H5E_DEFAULT, nullptr, nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedGroupSentinel::~H5ScopedGroupSentinel()
{
  if(m_TurnOffErrors)
  {
    H5Eset_auto(H5E_DEFAULT, _oldHDF_error_func, _oldHDF_error_client_data);
  }
  for(auto temp : m_Groups)
  {
    if(*temp > 0)
    {
      H5Gclose(*temp);
      *temp = -1;
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void H5ScopedGroupSentinel::addGroupId(hid_t* gid)
{
  m_Groups.push_back(gid);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedObjectSentinel::H5ScopedObjectSentinel(hid_t* gid, bool turnOffErrors)
: m_TurnOffErrors(turnOffErrors)
{
  m_Groups.push_back(gid);
  if(m_TurnOffErrors)
  {
    H5Eget_auto(H5E_DEFAULT, &_oldHDF_error_func, &_oldHDF_error_client_data);
    H5Eset_auto(H5E_DEFAULT, nullptr, nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5ScopedObjectSentinel::~H5ScopedObjectSentinel()
{
  if(m_TurnOffErrors)
  {
    H5Eset_auto(H5E_DEFAULT, _oldHDF_error_func, _oldHDF_error_client_data);
  }
  for(auto temp : m_Groups)
  {
    if(*temp > 0)
    {
      H5Utilities::closeHDF5Object(*temp);
      *temp = -1;
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void H5ScopedObjectSentinel::addGroupId(hid_t* gid)
{
  m_Groups.push_back(gid);
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5GroupAutoCloser::H5GroupAutoCloser(hid_t* groupId)
: gid(groupId)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
H5GroupAutoCloser::~H5GroupAutoCloser()
{
  if(*gid > 0)
  {
    H5Gclose(*gid);
  }
}

