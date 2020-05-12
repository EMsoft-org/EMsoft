/* ============================================================================
 * Copyright (c) 2009-2017 BlueQuartz Software, LLC
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

#if !defined(_MSC_VER)
#include <unistd.h>
#endif

#include <iostream>

#include <QtCore/QDebug>
#include <QtCore/QDir>
#include <QtGui/QSurfaceFormat>

#include <QtWidgets/QApplication>

#include "EMsoftApplication.h"

QString findEMsoftPathName()
{
  QDir aPathDir = QDir(QCoreApplication::applicationDirPath());

#if defined(Q_OS_WIN)
  if(aPathDir.cd("opencl"))
  {
    aPathDir.cdUp();
    return aPathDir.absolutePath();
  }
#elif defined(Q_OS_MAC)
  // Look to see if we are inside a .app package
  if(aPathDir.dirName() == "MacOS")
  {
    aPathDir.cdUp();
    if(aPathDir.cd("bin"))
    {
      return aPathDir.absolutePath();
    }
    aPathDir.cdUp();
    aPathDir.cdUp();
    return aPathDir.absolutePath();
  }
#else
  // We are on Linux - I think
  // Try the current location of where the application was launched from which is
  // typically the case when debugging from a build tree
  if(aPathDir.cd("opencl"))
  {
    aPathDir.cdUp();
    return aPathDir.absolutePath();
  }

  // Now try moving up a directory which is what should happen when running from a
  // proper distribution of SIMPLView
  aPathDir.cdUp();
  if(aPathDir.cd("opencl"))
  {
    aPathDir.cdUp();
    return aPathDir.absolutePath();
  }
#endif

  return {};
}

int main(int argc, char* argv[])
{
  QCoreApplication::setApplicationName("EMsoftWorkbench");
  QCoreApplication::setOrganizationDomain("bluequartz.net");
  QCoreApplication::setOrganizationName("BlueQuartz Software");
  QCoreApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);

#if QT_VERSION >= QT_VERSION_CHECK(5, 6, 0)
  QGuiApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
#endif

  EMsoftApplication app(argc, argv);

  QString envVarName = "EMSOFTPATHNAME";
  QString envPath = findEMsoftPathName();
  if(!envPath.isEmpty())
  {
    qputenv(envVarName.toStdString().c_str(), envPath.toStdString().c_str());
  }
  else
  {
    std::cout << "Could not set " << envVarName.toStdString() << " environment variable - Unable to find EMsoft path." << std::endl;
  }

  QDir aPluginDir = QDir(qApp->applicationDirPath());
  qDebug() << aPluginDir;

  if(!app.initialize(argc, argv))
  {
    return 1;
  }

  return app.exec();
}
