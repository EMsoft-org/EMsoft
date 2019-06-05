/* ============================================================================
 * Copyright (c) 2017 BlueQuartz Softwae, LLC
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

#include "QtSFileUtils.h"

#include <QtCore/QUrl>
#include <QtCore/QDir>
#include <QtCore/QProcess>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QApplication>
#include <QtGui/QDesktopServices>


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSFileUtils::QtSFileUtils() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSFileUtils::~QtSFileUtils() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSFileUtils::GenerateFileSystemPath(const QString &pathEnding)
{
  QString appPath = QApplication::applicationDirPath();

  QDir dir = QDir(appPath);

#if defined(Q_OS_WIN)

#elif defined(Q_OS_MAC)
  if(dir.dirName() == "MacOS")
  {
    dir.cdUp();
    dir.cdUp();
    dir.cdUp();
  }
#else
  // We are on Linux - I think
  QFileInfo fi(dir.absolutePath() + pathEnding);
  if(!fi.exists())
  {
    // The help file does not exist at the default location because we are probably running from the build tree.
    // Try up one more directory
    dir.cdUp();
  }
#endif

#if defined(Q_OS_WIN) || defined(Q_OS_MAC)
  QFileInfo fi(dir.absolutePath() + pathEnding);
  if(!fi.exists())
  {
    // Try up one more directory
    dir.cdUp();
  }
#endif

  QString filePath = dir.absolutePath() + pathEnding;
  filePath = QDir::toNativeSeparators(filePath);
  return filePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSFileUtils::GetPathSeperator()
{
  QString sep(":"); // Assume : on Linux and macOS and unix
  if (QSysInfo::windowsVersion() != QSysInfo::WV_None)
  {
    sep = QString(";");
  }
  return sep;
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList QtSFileUtils::GetEnvVar(const QString &envVar)
{
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  if(env.contains(envVar))
  {
    QString path = env.value(envVar, "");
    return path.split(GetPathSeperator());
  }
  return QStringList();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSFileUtils::FindInPath(const QString &exe)
{
  QStringList paths = QtSFileUtils::GetEnvVar("PATH");
  foreach (const QString &p, paths)
  {
    QFileInfo fi(p + QDir::separator() + exe);
    if(fi.exists())
    {
      return fi.absoluteFilePath();
    }
  }
  return QString("");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSFileUtils::ShowPathInGui(QWidget* parent, const QString &pathIn)
{
  const QFileInfo fileInfo(pathIn);
  if (QSysInfo::windowsVersion() != QSysInfo::WV_None)
  {
    const QString explorer = FindInPath(QLatin1String("explorer.exe"));
    if (explorer.isEmpty()) {
      QMessageBox::warning(parent,
                           QString("Launching Windows Explorer Failed"),
                           QString("Could not find explorer.exe in path to launch Windows Explorer."));
      return;
    }
    QStringList param;
    if (!fileInfo.isDir())
    {
      param += QLatin1String("/select,");
    }
    param += QDir::toNativeSeparators(fileInfo.canonicalFilePath());
    QProcess::startDetached(explorer, param);
  }
  else if (QSysInfo::MacVersion() != QSysInfo::MV_None)
  {
    QStringList scriptArgs;
    scriptArgs << QLatin1String("-e")
               << QString::fromLatin1(R"(tell application "Finder" to reveal POSIX file "%1")")
                  .arg(fileInfo.absoluteFilePath());
    QProcess::execute(QLatin1String("/usr/bin/osascript"), scriptArgs);
    scriptArgs.clear();
    scriptArgs << QLatin1String("-e")
               << QLatin1String(R"(tell application "Finder" to activate)");
    QProcess::execute(QLatin1String("/usr/bin/osascript"), scriptArgs);
  } else {
    // we cannot select a file here, because no file browser really supports it...
    QString s("file://");
    s = s + pathIn;
    QDesktopServices::openUrl(s);
  }
}

