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

#include "Common/EMsoftMenuItems.h"
#include "Common/QtSSettings.h"
#include "Common/QtSRecentFileList.h"

#include "Modules/ModuleManager.h"

#include "EMsoftWorkbench/EMsoftWorkbench_UI.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MacEMsoftApplication::MacEMsoftApplication(int& argc, char** argv) :
EMsoftApplication(argc, argv),
m_GlobalMenu(nullptr)
{
  // Create the global menu
  createGlobalMenu();

#if defined (Q_OS_MAC)
  // Add custom actions to a dock menu
  m_DockMenu = QSharedPointer<QMenu>(createCustomDockMenu());
  m_DockMenu.data()->setAsDockMenu();
#endif

  connect(this, &MacEMsoftApplication::lastWindowClosed, [=]
  {
    m_ActiveWindow = nullptr;
    toEmptyMenuState();
  });
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
void MacEMsoftApplication::unregisterWorkbenchInstance(EMsoftWorkbench_UI* instance)
{
  m_WorkbenchInstances.removeAll(instance);
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
void MacEMsoftApplication::emSoftWindowChanged(EMsoftWorkbench_UI* instance)
{
  if (instance->isActiveWindow())
  {
    m_ActiveWindow = instance;
    toWorkbenchMenuState();
  }
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
  QAction* actionNew = menuItems->getActionNew();
  QAction* actionOpen = menuItems->getActionOpen();
  QAction* actionSave = menuItems->getActionSave();
  QAction* actionSaveAs = menuItems->getActionSaveAs();
  QMenu* menuRecentFiles = menuItems->getMenuRecentFiles();
  QAction* actionClearRecentFiles = menuItems->getActionClearRecentFiles();
  QAction* actionExit = menuItems->getActionExit();
  QAction* actionEditStyle = menuItems->getActionEditStyle();
  QAction* actionEditConfig = menuItems->getActionEditConfig();

  m_GlobalMenu = QSharedPointer<QMenuBar>(new QMenuBar());

  // Create File Menu
  m_GlobalMenu->addMenu(menuFile);
  menuFile->addAction(actionNew);
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
  m_MenuEdit->addAction(actionEditConfig);
  m_MenuEdit->addAction(actionEditStyle);

  // Create View Menu
  m_GlobalMenu->addMenu(menuView);
}

