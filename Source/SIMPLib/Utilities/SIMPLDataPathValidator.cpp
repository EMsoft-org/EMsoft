/* ============================================================================
 * Copyright (c) 2017 BlueQuartz Software, LLC
 * All rights reserved.
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
 * Neither the names of any of the BlueQuartz Software contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
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
 *
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include "SIMPLDataPathValidator.h"

#include <QtCore/QDir>
#include <QtCore/QCoreApplication>

SIMPLDataPathValidator* SIMPLDataPathValidator::m_Self = nullptr;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SIMPLDataPathValidator::SIMPLDataPathValidator()
{
  // Default data directory for Macs with choosable data directory turned on (generally for Release builds)
  m_SIMPLDataDirectory = QDir::homePath() + QDir::separator() + tr("%1Data").arg(QCoreApplication::applicationName());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SIMPLDataPathValidator::~SIMPLDataPathValidator() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SIMPLDataPathValidator* SIMPLDataPathValidator::Instance()
{
  if(m_Self == nullptr)
  {
    m_Self = new SIMPLDataPathValidator();
  }

  return m_Self;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SIMPLDataPathValidator::convertToAbsolutePath(const QString &path)
{
  QString absolutePath = path;
  QFileInfo fi(absolutePath);
  if (fi.isRelative())
  {
    QDir dir = QDir(qApp->applicationDirPath());

	  QString parentPath;

#if defined(SIMPL_RELATIVE_PATH_CHECK)
	  parentPath = m_SIMPLDataDirectory;
#else
  #if defined (Q_OS_MAC)
    if(dir.dirName() == "MacOS")
    {
      dir.cdUp();
      dir.cdUp();
      dir.cdUp();
    }
  #elif defined(Q_OS_LINUX)
    dir.cdUp();
  #endif

    parentPath = dir.absolutePath();
#endif

    if(!path.startsWith(QDir::separator()) && !parentPath.endsWith(QDir::separator()))
    {
      absolutePath.prepend(QDir::separator());
    }
    absolutePath.prepend(parentPath);
    absolutePath = QDir::toNativeSeparators(absolutePath);
    //macOS and Linux do not like to have a ":" character in the path names
    #if !defined (Q_OS_WIN)
    absolutePath.replace(":", "");
    #endif
  }

  return absolutePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SIMPLDataPathValidator::setSIMPLDataDirectory(const QString &path)
{
  m_SIMPLDataDirectory = path;

  emit dataDirectoryChanged(path);
}
