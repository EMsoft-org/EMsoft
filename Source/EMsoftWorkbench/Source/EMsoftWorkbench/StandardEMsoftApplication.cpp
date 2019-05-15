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

#include "Common/EMsoftMenuItems.h"
#include "Common/QtSSettings.h"
#include "Common/QtSRecentFileList.h"

#include "Modules/ModuleManager.h"

#include "EMsoftWorkbench/EMsoftWorkbench_UI.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StandardEMsoftApplication::StandardEMsoftApplication(int& argc, char** argv) :
  EMsoftApplication(argc, argv)
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StandardEMsoftApplication::~StandardEMsoftApplication() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::emSoftWindowChanged(EMsoftWorkbench_UI* instance)
{
  if (instance->isActiveWindow())
  {
    m_ActiveWindow = instance;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StandardEMsoftApplication::unregisterWorkbenchInstance(EMsoftWorkbench_UI *instance)
{
  m_WorkbenchInstances.removeAll(instance);

  if (m_WorkbenchInstances.empty())
  {
    quit();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenuBar* StandardEMsoftApplication::getSIMPLViewMenuBar()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenuBar* menuBar = new QMenuBar();
  QMenu* menuFile = new QMenu("File", menuBar);
  QMenu* menuEdit = new QMenu("Edit", menuBar);
  QMenu* menuView = new QMenu("View", menuBar);
  QAction* menuNew = menuItems->getActionNew();
  QAction* actionOpen = menuItems->getActionOpen();
  QAction* actionSave = menuItems->getActionSave();
  QAction* actionSaveAs = menuItems->getActionSaveAs();
  QMenu* menuRecentFiles = menuItems->getMenuRecentFiles();
  QAction* actionClearRecentFiles = menuItems->getActionClearRecentFiles();
  QAction* actionExit = menuItems->getActionExit();
  QAction* actionEditStyle = menuItems->getActionEditStyle();

  // Create File Menu
  menuBar->addMenu(menuFile);
  menuFile->addAction(menuNew);
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
  menuBar->addAction(actionEditStyle);

  // Create View Menu
  menuBar->addMenu(menuView);
  menuView->addSeparator();

  return menuBar;
}

