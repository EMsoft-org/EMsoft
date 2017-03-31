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
#include "EMsoftApplication.h"

#include <iostream>

#include <QtCore/QPluginLoader>
#include <QtCore/QProcess>

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QSplashScreen>
#include <QtGui/QIcon>
#include <QtGui/QBitmap>
#include <QtGui/QDesktopServices>
#include <QtGui/QBitmap>
#include <QtGui/QClipboard>

#include "EMsoftWorkbench/EMsoftWorkbench.h"
#include "EMsoftWorkbench/EMsoftMenuItems.h"
#include "EMsoftWorkbench/LandingWidget.h"
#include "EMsoftWorkbench/QtSRecentFileList.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftApplication::EMsoftApplication(int& argc, char** argv) :
  QApplication(argc, argv),
  m_OpenDialogLastDirectory(""),
  m_ActiveWindow(nullptr)
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  // Menu Items Connections
  connect(menuItems->getActionOpen(), SIGNAL(triggered()), this, SLOT(on_actionOpen_triggered()));
  connect(menuItems->getActionSave(), SIGNAL(triggered()), this, SLOT(on_actionSave_triggered()));
  connect(menuItems->getActionSaveAs(), SIGNAL(triggered()), this, SLOT(on_actionSaveAs_triggered()));
  connect(menuItems->getActionExit(), SIGNAL(triggered()), this, SLOT(on_actionExit_triggered()));
  connect(menuItems->getActionClearRecentFiles(), SIGNAL(triggered()), this, SLOT(on_actionClearRecentFiles_triggered()));
  connect(menuItems->getActionAboutEMsoftWorkbench(), SIGNAL(triggered()), this, SLOT(on_actionAboutEMsoftWorkbench_triggered()));

  m_LandingWidget = new LandingWidget;
  connect(m_LandingWidget, SIGNAL(landingWidgetWindowChangedState()), this, SLOT(landingWidgetWindowChanged()));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftApplication::~EMsoftApplication()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftApplication::initialize(int argc, char* argv[])
{
  Q_UNUSED(argc)
  Q_UNUSED(argv)

  // Open pipeline if SIMPLView was opened from a compatible file
  if (argc == 2)
  {
    char* two = argv[1];
    QString filePath = QString::fromLatin1(two);
    if (!filePath.isEmpty())
    {
      newInstanceFromFile(filePath, true, true);
    }
  }
  else
  {
    m_LandingWidget->show();
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionClearRecentFiles_triggered()
{
  // This should never be executed
  return;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionOpen_triggered()
{
  QString proposedDir = m_OpenDialogLastDirectory;
  QString filePath = QFileDialog::getOpenFileName(nullptr, tr("Open Master File"),
    proposedDir, tr("HDF5 File (*.h5);;All Files (*.*)"));
  if (filePath.isEmpty()) { return; }

  if (m_ActiveWindow != nullptr)
  {
    // Write the active window's settings so that the newly opened instance can read them
    m_ActiveWindow->writeSettings();
  }

  // Cache the last directory on old instance
  m_OpenDialogLastDirectory = filePath;

  openMasterFile(filePath);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::openMasterFile(const QString &path)
{
  newInstanceFromFile(path, true, true);

  // Add file path to the recent files list for both instances
  QtSRecentFileList* list = QtSRecentFileList::instance();
  list->addFile(path);

  if (m_LandingWidget->isVisible())
  {
    m_LandingWidget->hide();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionSave_triggered()
{
  if (nullptr != m_ActiveWindow)
  {
//    m_ActiveWindow->savePipeline();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionSaveAs_triggered()
{
  if (nullptr != m_ActiveWindow)
  {
//    m_ActiveWindow->savePipelineAs();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionAboutEMsoftWorkbench_triggered()
{
//  AboutSIMPLView d(nullptr);
//  d.exec();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionCloseWindow_triggered()
{
  m_ActiveWindow->close();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionExit_triggered()
{
  bool shouldReallyClose = true;
  for (int i = 0; i<m_EMsoftWorkbenchInstances.size(); i++)
  {
    EMsoftWorkbench* dream3dWindow = m_EMsoftWorkbenchInstances[i];
    if (nullptr != dream3dWindow)
    {
      if (dream3dWindow->close() == false)
      {
        shouldReallyClose = false;
      }
    }
  }

  if (shouldReallyClose == true)
  {
    quit();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::emSoftWindowChanged(EMsoftWorkbench* instance)
{
  // This should never be executed
  return;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::landingWidgetWindowChanged()
{
  // This should never be executed
  return;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench *EMsoftApplication::getActiveWindow()
{
  return m_ActiveWindow;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QList<EMsoftWorkbench*> EMsoftApplication::getEMsoftWorkbenchInstances()
{
  return m_EMsoftWorkbenchInstances;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::registerEMsoftWorkbenchWindow(EMsoftWorkbench* window)
{
  m_EMsoftWorkbenchInstances.push_back(window);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::unregisterEMsoftWorkbenchWindow(EMsoftWorkbench* window)
{
  // This should never be executed
  return;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::newInstanceFromFile(const QString& filePath, const bool& setOpenedFilePath, const bool& addToRecentFiles)
{
  QString nativeFilePath = QDir::toNativeSeparators(filePath);
  EMsoftWorkbench* ui = getNewEMsoftWorkbenchInstance(nativeFilePath);

  QtSRecentFileList* list = QtSRecentFileList::instance();
  if (addToRecentFiles == true)
  {
    // Add file to the recent files list
    list->addFile(filePath);
  }
  ui->show();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench* EMsoftApplication::getNewEMsoftWorkbenchInstance(QString filePath)
{
  // Create new EMsoftWorkbench instance
  EMsoftWorkbench* newInstance = new EMsoftWorkbench(filePath, nullptr);
  newInstance->setAttribute(Qt::WA_DeleteOnClose);
  newInstance->setWindowTitle("[*]Untitled - EBSD, ECP, and Kossel Pattern Display Program");

  registerEMsoftWorkbenchWindow(newInstance);

  if (nullptr != m_ActiveWindow)
  {
    newInstance->move(m_ActiveWindow->x() + 45, m_ActiveWindow->y() + 45);
  }

  m_ActiveWindow = newInstance;

  connect(newInstance, SIGNAL(emSoftWindowChangedState(EMsoftWorkbench*)), this, SLOT(emSoftWindowChanged(EMsoftWorkbench*)));

  return newInstance;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::setActiveWindow(EMsoftWorkbench* instance)
{
  m_ActiveWindow = instance;
}






