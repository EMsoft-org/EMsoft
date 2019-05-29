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
#include "Common/FileIOTools.h"
#include "Common/SVStyle.h"

#include "QtSupport/QtSFileUtils.h"
#include "QtSupport/QtSRecentFileList.h"
#include "QtSupport/QtSSettings.h"

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
  // Create the default menu bar that gets displayed if there is no EMsoftWorkbench_UI instance (MacOS only)
  createDefaultMenuBar();

  // If on Mac, add custom actions to a dock menu
#if defined(Q_OS_MAC)
  createCustomDockMenu();

  setQuitOnLastWindowClosed(false);
#endif

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
void EMsoftApplication::emSoftWindowChanged(EMsoftWorkbench_UI* instance)
{
  if (instance->isActiveWindow())
  {
    m_ActiveWindow = instance;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::listenNewInstanceTriggered()
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
void EMsoftApplication::listenOpenTriggered()
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
  // Clear the Recent Items Menu
  m_MenuRecentFiles->clear();

  // Get the list from the static object
  QStringList files = QtSRecentFileList::instance()->fileList();
  foreach(QString file, files)
  {
    QAction* action = m_MenuRecentFiles->addAction(QtSRecentFileList::parentAndFileName(file));
    action->setData(file);
    action->setVisible(true);
    connect(action, &QAction::triggered, this, [=] {
      EMsoftWorkbench_UI* ui = newInstanceFromFile(file);
      ui->show();
    });
  }

  m_MenuRecentFiles->addSeparator();
  m_MenuRecentFiles->addAction(m_ActionClearRecentFiles);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::listenAboutEMsoftWorkbenchTriggered()
{
  //  AboutSIMPLView d(nullptr);
  //  d.exec();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::listenEditStyleTriggered() const
{
  styleSheetEditor->setGeometry(40, 40, 500, 800);
  styleSheetEditor->show();
  styleSheetEditor->activateWindow();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::listenClearRecentFilesTriggered() const
{
  // Clear the Recent Items Menu
  m_MenuRecentFiles->clear();
  m_MenuRecentFiles->addSeparator();
  m_MenuRecentFiles->addAction(m_ActionClearRecentFiles);

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
void EMsoftApplication::listenExitApplicationTriggered() const
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
EMsoftWorkbench_UI* EMsoftApplication::getActiveWindow() const
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
void EMsoftApplication::unregisterWorkbenchInstance(EMsoftWorkbench_UI* instance)
{
  m_WorkbenchInstances.removeAll(instance);

  if (m_WorkbenchInstances.isEmpty())
  {
    m_ActiveWindow = nullptr;
  }

#if defined(Q_OS_MAC)
#else
  if (m_WorkbenchInstances.size() <= 0)
  {
    quit();
  }
#endif
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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenu* EMsoftApplication::createCustomDockMenu() const
{
  QMenu* dockMenu = new QMenu();
  dockMenu->addAction(m_ActionOpen);

  return dockMenu;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::createDefaultMenuBar()
{
  m_DefaultMenuBar = new QMenuBar();

  m_MenuFile = new QMenu("File", m_DefaultMenuBar);
  m_MenuEdit = new QMenu("Edit", m_DefaultMenuBar);
  m_MenuView = new QMenu("View", m_DefaultMenuBar);
  m_MenuRecentFiles = new QMenu("Recent Files", m_DefaultMenuBar);
  m_MenuHelp = new QMenu("Help", m_DefaultMenuBar);

  m_ActionNew = new QAction("New...", m_DefaultMenuBar);
  m_ActionNew->setShortcut(QKeySequence::New);

  m_ActionOpen = new QAction("Open...", m_DefaultMenuBar);
  m_ActionOpen->setShortcut(QKeySequence::Open);

  m_ActionSave = new QAction("Save", m_DefaultMenuBar);
  m_ActionSave->setShortcut(QKeySequence::Save);

  m_ActionSaveAs = new QAction("Save As...", m_DefaultMenuBar);
  m_ActionSaveAs->setShortcut(QKeySequence::SaveAs);

  m_ActionClearRecentFiles = new QAction("Clear Recent Files", m_DefaultMenuBar);

  m_ActionAboutEMsoftWorkbench = new QAction("About " + QApplication::applicationName(), m_DefaultMenuBar);

  m_ActionExit = new QAction("Exit " + QApplication::applicationName(), m_DefaultMenuBar);
  m_ActionExit->setShortcut(QKeySequence::Quit);

  m_ActionEditStyle = new QAction("Edit Style...", this);

  connect(m_ActionNew, SIGNAL(triggered()), this, SLOT(listenNewInstanceTriggered()));
  connect(m_ActionOpen, SIGNAL(triggered()), this, SLOT(listenOpenTriggered()));
//  connect(m_ActionSave, SIGNAL(triggered()), this, SLOT(on_actionSave_triggered()));
//  connect(m_ActionSaveAs, SIGNAL(triggered()), this, SLOT(on_actionSaveAs_triggered()));
  connect(m_ActionExit, SIGNAL(triggered()), this, SLOT(listenExitApplicationTriggered()));
  connect(m_ActionClearRecentFiles, SIGNAL(triggered()), this, SLOT(listenClearRecentFilesTriggered()));
//  connect(m_ActionAboutEMsoftWorkbench, SIGNAL(triggered()), this, SLOT(on_actionAboutEMsoftWorkbench_triggered()));
  connect(m_ActionEditStyle, SIGNAL(triggered()), this, SLOT(listenEditStyleTriggered()));

  m_ActionSave->setDisabled(true);
  m_ActionSaveAs->setDisabled(true);

  // Create File Menu
  m_DefaultMenuBar->addMenu(m_MenuFile);
  m_MenuFile->addAction(m_ActionNew);
  m_MenuFile->addAction(m_ActionOpen);
  m_MenuFile->addSeparator();
  m_MenuFile->addAction(m_ActionSave);
  m_MenuFile->addAction(m_ActionSaveAs);
  m_MenuFile->addSeparator();
  m_MenuFile->addAction(m_MenuRecentFiles->menuAction());
  m_MenuRecentFiles->addSeparator();
  m_MenuRecentFiles->addAction(m_ActionClearRecentFiles);
  m_MenuFile->addSeparator();
  m_MenuFile->addAction(m_ActionExit);

  // Create Edit Menu
  m_DefaultMenuBar->addMenu(m_MenuEdit);
  m_MenuEdit->addAction(m_ActionEditStyle);

  // Create View Menu
  m_DefaultMenuBar->addMenu(m_MenuView);

  // Create Help Menu
//  m_MenuHelp->addAction(m_ActionAboutEMsoftWorkbench);
//  m_DefaultMenuBar->addMenu(m_MenuHelp);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftApplication::setOpenDialogLastDirectory(const QString& value)
{
  m_OpenDialogLastDirectory = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString EMsoftApplication::getOpenDialogLastDirectory() const
{
  return m_OpenDialogLastDirectory;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenu* EMsoftApplication::createThemeMenu(QActionGroup* actionGroup, QWidget* parent)
{
  SVStyle* style = SVStyle::Instance();

  QMenu* menuThemes = new QMenu("Themes", parent);

  QString themePath = ":/SIMPL/StyleSheets/Default.json";
  QAction* action = menuThemes->addAction("Default", [=] { style->loadStyleSheet(themePath); });
  action->setCheckable(true);
  if(themePath == style->getCurrentThemeFilePath())
  {
    action->setChecked(true);
  }
  actionGroup->addAction(action);

  QStringList themeNames = BrandedStrings::LoadedThemeNames;
  for(int i = 0; i < themeNames.size(); i++)
  {
    QString themePath = BrandedStrings::DefaultStyleDirectory + QDir::separator() + themeNames[i] + ".json";
    QAction* action = menuThemes->addAction(themeNames[i], [=] { style->loadStyleSheet(themePath); });
    action->setCheckable(true);
    if(themePath == style->getCurrentThemeFilePath())
    {
      action->setChecked(true);
    }
    actionGroup->addAction(action);
  }

  return menuThemes;
}
