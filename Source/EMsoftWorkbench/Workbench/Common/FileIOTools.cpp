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

#include "FileIOTools.h"

#include <QtCore/QCoreApplication>

#include <QtWidgets/QFileDialog>

#include <iostream>
#include <sstream>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FileIOTools::FileIOTools() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FileIOTools::~FileIOTools() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString FileIOTools::GetOpenPathFromDialog(const QString &title, const QString &filters, QString &openDialogLastDirectory)
{
  QString proposedDir = openDialogLastDirectory;
  QString filePath = QFileDialog::getOpenFileName(nullptr, title, proposedDir, filters);
  if (!filePath.isEmpty()) { openDialogLastDirectory = filePath; }

  return filePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString FileIOTools::GetSavePathFromDialog(const QString &title, const QString &filters, QString &openDialogLastDirectory)
{
  QString proposedDir = openDialogLastDirectory;
  QString filePath = QFileDialog::getSaveFileName(nullptr, title, proposedDir, filters);
  if (!filePath.isEmpty()) { openDialogLastDirectory = filePath; }

  return filePath;
}

// -----------------------------------------------------------------------------
QString FileIOTools::GetAbsolutePath(const QString& path)
{
  QString absolutePath = path;
  QFileInfo fi(absolutePath);
  if(fi.isRelative())
  {
    QDir dir = QDir(qApp->applicationDirPath());

    QString parentPath;

#if defined(SIMPL_RELATIVE_PATH_CHECK)
    parentPath = m_SIMPLDataDirectory;
#else
#if defined(Q_OS_MAC)
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
// macOS and Linux do not like to have a ":" character in the path names
#if !defined(Q_OS_WIN)
    absolutePath.replace(":", "");
#endif
  }

  return absolutePath;
}

// -----------------------------------------------------------------------------
std::string FileIOTools::CreateNMLEntry(const QString& key, const QString& value, bool last)
{
  std::stringstream out;
  out << " " << key.toStdString() << " = '" << value.toStdString() << "'";
  if(!last)
  {
    out << ",";
  }
  return out.str();
}
// -----------------------------------------------------------------------------
std::string FileIOTools::CreateNMLEntry(const QString& key, double value, bool last)
{
  std::stringstream out;
  out << " " << key.toStdString() << " = " << std::fixed << value << "D0";

  if(!last)
  {
    out << ",";
  }
  return out.str();
}
// -----------------------------------------------------------------------------
std::string FileIOTools::CreateNMLEntry(const QString& key, float value, bool last)
{
  std::stringstream out;
  out << " " << key.toStdString() << " = " << std::fixed << value;
  if(!last)
  {
    out << ",";
  }
  return out.str();
}
// -----------------------------------------------------------------------------
std::string FileIOTools::CreateNMLEntry(const QString& key, int32_t value, bool last)
{
  std::stringstream out;
  out << " " << key.toStdString() << " = " << value;
  if(!last)
  {
    out << ",";
  }
  return out.str();
}

// -----------------------------------------------------------------------------
std::string FileIOTools::CreateNMLEntry(const QString& key, bool value, bool last)
{
  std::stringstream out;

  out << " " << key.toStdString() << " = ";
  if(value)
  {
    out << ".TRUE.";
  }
  else
  {
    out << ".FALSE.";
  }

  if(!last)
  {
    out << ",";
  }
  return out.str();
}

// -----------------------------------------------------------------------------
std::string FileIOTools::GetEMsoftPathName()
{
  QDir workingDirectory = QDir(QCoreApplication::applicationDirPath());
#if defined(Q_OS_MAC)
  // Let's assume we start out in the .app package.
  if(workingDirectory.dirName() == "MacOS")
  {
    workingDirectory.cdUp();
    if(workingDirectory.exists("bin") && workingDirectory.cd("bin"))
    {
      return workingDirectory.absolutePath().toStdString();
    }
    workingDirectory.cdUp();
    workingDirectory.cdUp();
    // We should now be out of the app package and this assumes we are running from an IDE
    // so we are in the build folder.
    if(workingDirectory.exists("opencl"))
    {
      return workingDirectory.absolutePath().toStdString();
    }
  }
#endif
  return workingDirectory.absolutePath().toStdString();
}

// -----------------------------------------------------------------------------
std::pair<QString, QString> FileIOTools::GetExecutablePath(const QString& name)
{

  QString exePath;
  QString errMsg;

  QDir workingDirectory = QDir(QCoreApplication::applicationDirPath());

  QString ext = {""};
#if defined(Q_OS_WIN)
  ext = ".exe";
#endif
  QString completeName = name + ext;

  if(workingDirectory.exists(completeName))
  {
    return {workingDirectory.absolutePath() + QDir::separator() + completeName, QString("")};
  }

#if defined(Q_OS_MAC)

  // Look to see if we are inside an .app package or inside the 'tools' directory
  if(workingDirectory.dirName() == "MacOS")
  {
    workingDirectory.cdUp();
    if(workingDirectory.cd("bin"))
    {
      if(workingDirectory.exists(completeName))
      {
        return {workingDirectory.absolutePath() + QDir::separator() + completeName, QString("")};
      }
      workingDirectory.cdUp();
    }

    workingDirectory.cdUp();
    workingDirectory.cdUp();

    if(workingDirectory.dirName() == "Bin" && workingDirectory.exists(completeName))
    {
      return {workingDirectory.absolutePath() + QDir::separator() + completeName, QString("")};
    }
  }
#endif

#if defined(Q_OS_LINUX)
  // We are on Linux - I think
  // Try the current location of where the application was launched from which is
  // typically the case when debugging from a build tree
  if(workingDirectory.cd("bin"))
  {
    if(workingDirectory.exists(completeName))
    {
      return {workingDirectory.absolutePath() + QDir::separator() + completeName, QString("")};
    }
    workingDirectory.cdUp();
  }

  // Now try moving up a directory which is what should happen when running from a
  // proper distribution of EMsoft
  workingDirectory = QDir(QCoreApplication::applicationDirPath());
  workingDirectory.cdUp();
  if(workingDirectory.cd("bin"))
  {
    if(workingDirectory.exists(completeName))
    {
      return {workingDirectory.absolutePath() + QDir::separator() + completeName, QString("")};
    }
    workingDirectory.cdUp();
  }
#endif

  if(exePath.isEmpty())
  {
    errMsg = QString("Could not find executable: %1").arg(completeName);
  }

  return {exePath, errMsg};
}
