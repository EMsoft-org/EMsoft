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
#include "MacEMsoftApplication.h"

#include "EMsoftWorkbench/QtSRecentFileList.h"
#include "EMsoftWorkbench/EMsoftWorkbench.h"
#include "EMsoftWorkbench/EMsoftMenuItems.h"
#include "EMsoftWorkbench/LandingWidget.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MacEMsoftApplication::MacEMsoftApplication(int& argc, char** argv) :
EMsoftApplication(argc, argv),
m_GlobalMenu(nullptr)
{
  // Create the global menu
  createGlobalMenu();

  // Add custom actions to a dock menu
  m_DockMenu = QSharedPointer<QMenu>(createCustomDockMenu());
  m_DockMenu.data()->setAsDockMenu();

  // Connection to update the recent files list on all windows when it changes
  QtSRecentFileList* recents = QtSRecentFileList::instance();
  connect(recents, SIGNAL(fileListChanged(const QString&)), this, SLOT(updateRecentFileList(const QString&)));
  connect(recents, SIGNAL(fileListChanged(const QString&)), getLandingWidget(), SLOT(updateRecentFiles(const QString&)));

  m_Mapper = new QSignalMapper(this);

  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());
  recents->readList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MacEMsoftApplication::~MacEMsoftApplication()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::updateRecentFileList(const QString& file)
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
    m_Mapper->setMapping(action, file);
    connect(action, SIGNAL(triggered()), m_Mapper, SLOT(map()));
    connect(m_Mapper, SIGNAL(mapped(const QString &)), this, SLOT(openMasterFile(const QString &)));
  }

  recentFilesMenu->addSeparator();
  recentFilesMenu->addAction(clearRecentFilesAction);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::on_actionClearRecentFiles_triggered()
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
void MacEMsoftApplication::unregisterEMsoftWorkbenchWindow(EMsoftWorkbench* window)
{
  m_EMsoftWorkbenchInstances.removeAll(window);

  if (m_EMsoftWorkbenchInstances.size() <= 0)
  {
    getLandingWidget()->show();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::emSoftWindowChanged(EMsoftWorkbench* instance)
{
  if (instance->isActiveWindow())
  {
    m_ActiveWindow = instance;
    toWorkbenchMenuState();
  }
  else if (m_EMsoftWorkbenchInstances.size() == 1)
  {
    /* If the inactive signal got fired and there are no more windows,
     * this means that the last window has been closed. */
    m_ActiveWindow = nullptr;
    toEmptyMenuState();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::landingWidgetWindowChanged()
{
  if (getLandingWidget()->isActiveWindow())
  {
    toEmptyMenuState();
    m_ActiveWindow = nullptr;
  }
  else if (m_EMsoftWorkbenchInstances.size() <= 0)
  {
    toEmptyMenuState();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::toWorkbenchMenuState()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  menuItems->getActionSave()->setEnabled(true);
  menuItems->getActionSaveAs()->setEnabled(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::toEmptyMenuState()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  menuItems->getActionSave()->setDisabled(true);
  menuItems->getActionSaveAs()->setDisabled(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenu* MacEMsoftApplication::createCustomDockMenu()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenu* dockMenu = new QMenu();
  dockMenu->addAction(menuItems->getActionOpen());

  return dockMenu;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MacEMsoftApplication::createGlobalMenu()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenu* menuFile = new QMenu("File", m_GlobalMenu.data());
  m_MenuEdit = new QMenu("Edit", m_GlobalMenu.data());
  QMenu* menuView = new QMenu("View", m_GlobalMenu.data());
  QAction* actionOpen = menuItems->getActionOpen();
  QAction* actionSave = menuItems->getActionSave();
  QAction* actionSaveAs = menuItems->getActionSaveAs();
  QMenu* menuRecentFiles = menuItems->getMenuRecentFiles();
  QAction* actionClearRecentFiles = menuItems->getActionClearRecentFiles();
  QAction* actionExit = menuItems->getActionExit();

  m_GlobalMenu = QSharedPointer<QMenuBar>(new QMenuBar());

  // Create File Menu
  m_GlobalMenu->addMenu(menuFile);
  menuFile->addAction(actionOpen);
  menuFile->addSeparator();
  menuFile->addAction(actionSave);
  menuFile->addAction(actionSaveAs);
  menuFile->addSeparator();
  menuFile->addAction(menuRecentFiles->menuAction());
  menuRecentFiles->addSeparator();
  menuRecentFiles->addAction(actionClearRecentFiles);
  menuFile->addSeparator();
  menuFile->addAction(actionExit);

  // Create Edit Menu
  m_GlobalMenu->addMenu(m_MenuEdit);
  m_EditSeparator = m_MenuEdit->addSeparator();

  // Create View Menu
  m_GlobalMenu->addMenu(menuView);
  m_EditSeparator = m_MenuEdit->addSeparator();
}
