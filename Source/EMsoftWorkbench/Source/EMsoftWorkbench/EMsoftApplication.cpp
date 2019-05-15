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

#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QPluginLoader>
#include <QtCore/QProcess>

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QSplashScreen>

#include <QtGui/QBitmap>
#include <QtGui/QClipboard>
#include <QtGui/QDesktopServices>
#include <QtGui/QIcon>

#include "Modules/ModuleManager.h"

#include "Common/Constants.h"
#include "Common/EMsoftMenuItems.h"
#include "Common/FileIOTools.h"
#include "Common/QtSFileUtils.h"
#include "Common/QtSRecentFileList.h"
#include "Common/QtSSettings.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "EMsoftWorkbench/EMsoftWorkbench_UI.h"
#include "EMsoftWorkbench/StyleSheetEditor.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftApplication::EMsoftApplication(int& argc, char** argv)
: QApplication(argc, argv)
, m_OpenDialogLastDirectory("")
, m_ActiveWindow(nullptr)
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  connect(menuItems->getActionNew(), SIGNAL(triggered()), this, SLOT(on_actionNew_triggered()));
  connect(menuItems->getActionOpen(), SIGNAL(triggered()), this, SLOT(on_actionOpen_triggered()));
  connect(menuItems->getActionSave(), SIGNAL(triggered()), this, SLOT(on_actionSave_triggered()));
  connect(menuItems->getActionSaveAs(), SIGNAL(triggered()), this, SLOT(on_actionSaveAs_triggered()));
  connect(menuItems->getActionExit(), SIGNAL(triggered()), this, SLOT(on_actionExit_triggered()));
  connect(menuItems->getActionClearRecentFiles(), SIGNAL(triggered()), this, SLOT(on_actionClearRecentFiles_triggered()));
  connect(menuItems->getActionAboutEMsoftWorkbench(), SIGNAL(triggered()), this, SLOT(on_actionAboutEMsoftWorkbench_triggered()));
  connect(menuItems->getActionEditStyle(), SIGNAL(triggered()), this, SLOT(on_actionEditStyle_triggered()));

  // Connection to update the recent files list on all windows when it changes
  QtSRecentFileList* recentsList = QtSRecentFileList::instance();
  connect(recentsList, &QtSRecentFileList::fileListChanged, this, [=] { updateRecentFileList(); });

  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());
  recentsList->readList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftApplication::~EMsoftApplication()
{
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());

  QtSRecentFileList* recentsList = QtSRecentFileList::instance();
  recentsList->writeList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftApplication::initialize(int argc, char* argv[])
{
  if(argc == 2)
  {
    // Open EMsoftWorkbench from a compatible file
    char* two = argv[1];
    QString filePath = QString::fromLatin1(two);
    if(!filePath.isEmpty())
    {
      EMsoftWorkbench_UI* instance = newInstanceFromFile(filePath);
      if(instance != nullptr)
      {
        instance->show();
      }
    }
  }
  else
  {
    // Open blank EMsoftWorkbench
    EMsoftWorkbench_UI* workbench = getNewWorkbenchInstance();
    workbench->show();
  }

  // Create the style sheet editor and apply the style sheet
  styleSheetEditor = new StyleSheetEditor();

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionNew_triggered()
{
  EMsoftWorkbench_UI* ui = getNewWorkbenchInstance();
  if(ui != nullptr)
  {
    ui->show();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionOpen_triggered()
{
  QString proposedDir = m_OpenDialogLastDirectory;
  QString filePath = QFileDialog::getOpenFileName(nullptr, tr("Open Module From File"), proposedDir, tr("JSON File (*.json);;All Files (*.*)"));
  if(filePath.isEmpty())
  {
    return;
  }

  filePath = QDir::toNativeSeparators(filePath);

  EMsoftWorkbench_UI* instance = newInstanceFromFile(filePath);
  if(instance != nullptr)
  {
    instance->show();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI* EMsoftApplication::newInstanceFromFile(const QString& filePath)
{
  QFileInfo fi(filePath);
  QFile inputFile(filePath);
  if(!inputFile.open(QIODevice::ReadOnly))
  {
    QMessageBox::critical(nullptr, "JSON Read Error", tr("The JSON file\n\n\"%1\"\n\ncould not be opened.").arg(filePath), QMessageBox::Ok);
    return nullptr;
  }

  QJsonParseError parseError;
  QByteArray byteArray = inputFile.readAll();
  QJsonDocument doc = QJsonDocument::fromJson(byteArray, &parseError);
  if(parseError.error != QJsonParseError::NoError)
  {
    QMessageBox::critical(nullptr, "JSON Read Error", tr("The contents of the JSON file\n\n\"%1\"\n\nis not valid JSON.").arg(filePath), QMessageBox::Ok);
    return nullptr;
  }

  QJsonObject root = doc.object();

  if(!root.contains(EMsoftWorkbenchConstants::StringConstants::Modules))
  {
    QMessageBox::critical(nullptr, "JSON Read Error", tr("The contents of the JSON file\n\n\"%1\"\n\nis not formatted correctly for %2.").arg(filePath, QCoreApplication::applicationName()),
                          QMessageBox::Ok);
    return nullptr;
  }

  QJsonObject modulesObj = root[EMsoftWorkbenchConstants::StringConstants::Modules].toObject();

  if(modulesObj.empty())
  {
    QMessageBox::warning(nullptr, "JSON Read Warning", tr("The JSON file\n\n\"%1\"\n\ndoes not contain any modules.").arg(filePath), QMessageBox::Ok);
    return nullptr;
  }

  EMsoftWorkbench_UI* instance = getNewWorkbenchInstance();

  instance->openSession(modulesObj);
  instance->setOpenedFilePath(filePath);
  instance->setWindowModified(false);
  instance->setWindowTitle(QObject::tr("[*]%1 - %2").arg(fi.baseName(), QCoreApplication::applicationName()));

  QtSRecentFileList* list = QtSRecentFileList::instance();
  list->addFile(filePath);

  registerWorkbenchInstance(instance);

  // Cache the last directory on old instance
  m_OpenDialogLastDirectory = filePath;

  return instance;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::updateRecentFileList()
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
    QAction* action = recentFilesMenu->addAction(QtSRecentFileList::parentAndFileName(file));
    action->setData(file);
    action->setVisible(true);
    connect(action, &QAction::triggered, this, [=] {
      EMsoftWorkbench_UI* ui = newInstanceFromFile(file);
      ui->show();
    });
  }

  recentFilesMenu->addSeparator();
  recentFilesMenu->addAction(clearRecentFilesAction);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionSave_triggered()
{
  if(nullptr != m_ActiveWindow)
  {
    m_ActiveWindow->saveSession();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionSaveAs_triggered()
{
  if(nullptr != m_ActiveWindow)
  {
    m_ActiveWindow->saveSessionAs();
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
void EMsoftApplication::on_actionEditStyle_triggered()
{
  styleSheetEditor->setGeometry(40, 40, 500, 800);
  styleSheetEditor->show();
  styleSheetEditor->activateWindow();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionClearRecentFiles_triggered()
{
  EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();

  QMenu* recentFilesMenu = menuItems->getMenuRecentFiles();
  QAction* clearRecentFilesAction = menuItems->getActionClearRecentFiles();

  // Clear the Recent Items Menu
  recentFilesMenu->clear();
  recentFilesMenu->addSeparator();
  recentFilesMenu->addAction(clearRecentFilesAction);

  // Clear the actual list
  QtSRecentFileList* recentsList = QtSRecentFileList::instance();
  recentsList->clear();

  // Write out the empty list
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());
  recentsList->writeList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::on_actionExit_triggered()
{
  bool shouldReallyClose = true;
  for(EMsoftWorkbench_UI* workbench : m_WorkbenchInstances)
  {
    if(nullptr != workbench)
    {
      if(!workbench->close())
      {
        shouldReallyClose = false;
      }
    }
  }

  if(shouldReallyClose)
  {
    quit();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI* EMsoftApplication::getActiveWindow()
{
  return m_ActiveWindow;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::registerWorkbenchInstance(EMsoftWorkbench_UI* instance)
{
  m_WorkbenchInstances.push_back(instance);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI* EMsoftApplication::getNewWorkbenchInstance()
{
  // Create new EMsoftWorkbench instance
  EMsoftWorkbench_UI* workbench = new EMsoftWorkbench_UI();
  workbench->setAttribute(Qt::WA_DeleteOnClose);
  workbench->setWindowTitle(QObject::tr("[*]%1 - %2").arg("Untitled", QCoreApplication::applicationName()));

  connect(workbench, &EMsoftWorkbench_UI::workbenchWindowChangedState, this, &EMsoftApplication::emSoftWindowChanged);

  if(m_ActiveWindow != nullptr)
  {
    workbench->move(m_ActiveWindow->x() + 25, m_ActiveWindow->y() + 25);
  }

  registerWorkbenchInstance(workbench);
  return workbench;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::setActiveWindow(EMsoftWorkbench_UI* workbench)
{
  m_ActiveWindow = workbench;
}
