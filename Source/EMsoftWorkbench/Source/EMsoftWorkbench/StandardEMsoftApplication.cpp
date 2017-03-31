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
#include "StandardEMsoftApplication.h"

#include "EMsoftWorkbench/QtSRecentFileList.h"
#include "EMsoftWorkbench/EMsoftWorkbench.h"
#include "EMsoftWorkbench/EMsoftMenuItems.h"
#include "EMsoftWorkbench/LandingWidget.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StandardEMsoftApplication::StandardEMsoftApplication(int& argc, char** argv) :
  EMsoftApplication(argc, argv)
{
  // Connection to update the recent files list on all windows when it changes
  QtSRecentFileList* recents = QtSRecentFileList::instance();
  connect(recents, SIGNAL(fileListChanged(const QString&)),
          this, SLOT(updateRecentFileList(const QString&)));

  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());
  recents->readList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StandardEMsoftApplication::~StandardEMsoftApplication()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::updateRecentFileList(const QString& file)
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenu* recentFilesMenu = menuItems->getMenuRecentFiles();
  QAction* clearRecentFilesAction = menuItems->getActionClearRecentFiles();

  // Clear the Recent Items Menu
  recentFilesMenu->clear();

  // Get the list from the static object
  QStringList files = QtSRecentFileList::instance()->fileList();
  foreach(QString file, files)
  {
    QAction* action = recentFilesMenu->addAction(QtSRecentFileList::instance()->parentAndFileName(file));
    action->setData(file);
    action->setVisible(true);
    connect(action, SIGNAL(triggered()), this, SLOT(openRecentFile()));
  }

  recentFilesMenu->addSeparator();
  recentFilesMenu->addAction(clearRecentFilesAction);

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::on_actionClearRecentFiles_triggered()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenu* recentFilesMenu = menuItems->getMenuRecentFiles();
  QAction* clearRecentFilesAction = menuItems->getActionClearRecentFiles();

  // Clear the Recent Items Menu
  recentFilesMenu->clear();
  recentFilesMenu->addSeparator();
  recentFilesMenu->addAction(clearRecentFilesAction);

  // Clear the actual list
  QtSRecentFileList* recents = QtSRecentFileList::instance();
  recents->clear();

  // Write out the empty list
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());
  recents->writeList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::emSoftWindowChanged(EMsoftWorkbench* instance)
{
  if (instance->isActiveWindow())
  {
    m_ActiveWindow = instance;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::landingWidgetWindowChanged()
{
  if (getLandingWidget()->isActiveWindow())
  {
    m_ActiveWindow = nullptr;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::unregisterEMsoftWorkbenchWindow(EMsoftWorkbench* window)
{
  m_EMsoftWorkbenchInstances.removeAll(window);

  if (m_EMsoftWorkbenchInstances.size() <= 0)
  {
    quit();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenuBar* StandardEMsoftApplication::getSIMPLViewMenuBar(EMsoftWorkbench* instance)
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenuBar* menuBar = new QMenuBar();
  QMenu* menuFile = new QMenu("File", menuBar);
  QMenu* menuEdit = new QMenu("Edit", menuBar);
  QMenu* menuView = new QMenu("View", menuBar);
  QAction* actionOpen = menuItems->getActionOpen();
  QAction* actionSave = menuItems->getActionSave();
  QAction* actionSaveAs = menuItems->getActionSaveAs();
  QMenu* menuRecentFiles = menuItems->getMenuRecentFiles();
  QAction* actionClearRecentFiles = menuItems->getActionClearRecentFiles();
  QAction* actionExit = menuItems->getActionExit();

  // Create File Menu
  menuBar->addMenu(menuFile);
  menuFile->addAction(actionOpen);
  menuFile->addSeparator();
  menuFile->addAction(actionSave);
  menuFile->addAction(actionSaveAs);
  menuFile->addSeparator();
  menuFile->addAction(menuRecentFiles->menuAction());
  menuFile->addSeparator();
  menuFile->addAction(actionExit);
  menuRecentFiles->addSeparator();
  menuRecentFiles->addAction(actionClearRecentFiles);

  // Create Edit Menu
  menuBar->addMenu(menuEdit);
  menuEdit->addSeparator();

  // Create View Menu
  menuBar->addMenu(menuView);
  menuView->addSeparator();

  return menuBar;
}

