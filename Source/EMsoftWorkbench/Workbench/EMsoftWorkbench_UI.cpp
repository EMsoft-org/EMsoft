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

#include "EMsoftWorkbench_UI.h"

#include <iostream>

#include <QtCore/QJsonDocument>
#include <QtCore/QPropertyAnimation>
#include <QtCore/QDebug>

#include <QtGui/QResizeEvent>

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QToolButton>

#include "Modules/DictionaryIndexingModule/Constants.h"
#include "Modules/CrystalStructureCreationModule/CrystalStructureCreation_UI.h"
#include "Modules/ModuleManager.h"

#include "Common/Constants.h"
#include "Common/FileIOTools.h"

#include "QtSupport/QtSRecentFileList.h"
#include "QtSupport/QtSSettings.h"

#include "EMsoftApplication.h"
#include "StatusBarWidget.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI::EMsoftWorkbench_UI(QWidget* parent)
: QMainWindow(parent)
{
  setupUi(this);

  setupGui();

  // Read various settings
  readSettings();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI::~EMsoftWorkbench_UI()
{
  writeSettings();

  emSoftApp->unregisterWorkbenchInstance(this);

  if(emSoftApp->activeWindow() == this)
  {
    emSoftApp->setActiveWindow(nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setupGui()
{
  createWorkbenchMenuSystem();

  setupIssuesTable();

  setupStatusBar();

  setupMainToolbarAndStackedWidget();

  createWidgetConnections();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setupStatusBar()
{
  m_StatusBar = new StatusBarWidget();
  this->statusBar()->insertPermanentWidget(0, m_StatusBar, 0);

  m_StatusBar->setButtonAction(issuesDockWidget, StatusBarWidget::Button::Issues);
  m_StatusBar->setButtonAction(stdOutDockWidget, StatusBarWidget::Button::StandardOutput);
  m_StatusBar->setButtonAction(mainToolbar, StatusBarWidget::Button::ModuleNavigator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setupMainToolbarAndStackedWidget()
{
  // Move the toolbar to the left side
  addToolBar(Qt::LeftToolBarArea, mainToolbar);

  // If the current index in the module stacked widget changes: clear the issues table and standard output window,
  // disconnect all signals/slots from old module, connect all signals/slots for new module, and populate
  // the issues table and standard output window with the new module's existing issues and messages
  connect(moduleStackedWidget, &QStackedWidget::currentChanged, [=](int index) {
    clearIssuesTable();
    stdOutTE->clear();

    IModuleUI* oldModule = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(m_CurrentStackedWidgetIdx));
    IModuleUI* newModule = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(index));

    if(oldModule != nullptr)
    {
      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::issuesCleared, nullptr, nullptr);

      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::moduleParametersChanged, nullptr, nullptr);

      // Connect the module's error messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::errorMessageGenerated, nullptr, nullptr);

      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::warningMessageGenerated, nullptr, nullptr);

      // Connect the module's std output messages so that they can be displayed in the std output pane.
      // This must be done using the old-style signal/slot connection because it may cross threads
      disconnect(oldModule, SIGNAL(stdOutputMessageGenerated(const QString&)), this, SLOT(slot_StdOutputMsgReceived(const QString&)));
    }

    if(newModule != nullptr)
    {
      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::issuesCleared, [=] { clearIssuesTable(); });

      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::moduleParametersChanged, [=] { setWindowModified(true); });

      // Connect the module's error messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::errorMessageGenerated, [=](const QString& msg) { addIssue(msg, IModuleUI::IssueType::Error); });

      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::warningMessageGenerated, [=](const QString& msg) { addIssue(msg, IModuleUI::IssueType::Warning); });

      // Connect the module's std output messages so that they can be displayed in the std output pane.
      // This must be done using the old-style signal/slot connection because it may cross threads
      connect(newModule, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(slot_StdOutputMsgReceived(QString)));

      // Set the widget into the frame
      QList<IModuleUI::ModuleIssue> issues = newModule->getModuleIssues();
      for(const IModuleUI::ModuleIssue& issue : issues)
      {
        addIssue(issue.msg, issue.msgType);
      }

      stdOutTE->setText(newModule->getStdOutput());
    }
  }); // End connect

  // Create the order that the modules will be displayed in the user interface.  This order
  // gives the user an idea of what order to use the modules in.
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::CrystalStructureCreation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::MonteCarloSimulation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::MasterPatternSimulation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::PatternDisplay);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::PatternFit);
  m_ModuleNamesOrder.append(DictionaryIndexingModuleConstants::ModuleName);

  m_ToolbarButtonGroup = new QActionGroup(this);

  for(int i = 0; i < m_ModuleNamesOrder.size(); i++)
  {
    // Create a toolbar action labeled with the module name
    QString moduleName = m_ModuleNamesOrder[i];
    QAction* toolbarAction = mainToolbar->addAction(tr("%1. %2").arg(QString::number(i + 1), moduleName)); // Create toolbar action
    toolbarAction->setCheckable(true);
    m_ToolbarButtonGroup->addAction(toolbarAction);

    // If a user launches the program and immediately selects the fourth toolbar action, QStackedWidget must have slots 0, 1, and 2
    // filled with widgets to create and insert the module widget in the 3rd slot.  We will insert dummy widgets for the widgets that haven't been shown
    // yet so that we can keep the stacked widgets and their corresponding toolbar actions in the same order.
    moduleStackedWidget->addWidget(new QWidget());

    // Connection that handles when a toolbar action has been triggered.  If the corresponding module widget for the triggered toolbar action
    // has not been created yet, this lambda slot creates it and then inserts it into the stacked widget in the proper place.
    connect(toolbarAction, &QAction::triggered, [=] {
      QList<QAction*> toolbarActions = mainToolbar->actions();
      int idx = toolbarActions.indexOf(toolbarAction);
      updateModuleWidgetSelection(idx);
    }); // End connect
  }

  // Trigger the first toolbar action so that it displays first by default
  QList<QAction*> toolbarActions = mainToolbar->actions();
  if(!toolbarActions.empty())
  {
    toolbarActions[0]->trigger();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::updateModuleWidgetSelection(int index)
{
  ModuleManager* manager = ModuleManager::Instance();
  QList<QAction*> toolbarActions = mainToolbar->actions();

  IModuleUI* module_ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(index));
  if(module_ui == nullptr)
  {
    // This module doesn't exist yet, so create it
    module_ui = manager->getModuleFromName(m_ModuleNamesOrder[index], QJsonObject(), this);
    if(module_ui != nullptr)
    {
      connect(module_ui, &IModuleUI::validationOfOtherModulesNeeded, [=](IModuleUI* module_ui) {
        for(int i = 0; i < moduleStackedWidget->count(); i++)
        {
          IModuleUI* ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
          if(ui != nullptr && ui != module_ui)
          {
            ui->validateData();
          }
        }
      });

      // Delete the placeholder
      QWidget* placeholder = moduleStackedWidget->widget(index);
      moduleStackedWidget->removeWidget(placeholder);
      delete placeholder;

      // Insert the module widget into its proper place in the stacked widget
      moduleStackedWidget->insertWidget(index, module_ui);
      moduleStackedWidget->setCurrentIndex(index);
      m_CurrentStackedWidgetIdx = index;
    }
    else
    {
      // The user didn't give the module the information it needed to create itself, or there was an error.
      // Restore the toolbar back to its previous selection
      toolbarActions[m_CurrentStackedWidgetIdx]->setChecked(true);
    }
  }
  else
  {
    // The module widget already exists in the stacked widget, so set its index as the current index
    moduleStackedWidget->setCurrentIndex(index);
    m_CurrentStackedWidgetIdx = index;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setupIssuesTable()
{
  errorTableWidget->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
  errorTableWidget->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);

  errorTableWidget->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::createWidgetConnections()
{
}

// -----------------------------------------------------------------------------
//  Called when the main window is closed.
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::closeEvent(QCloseEvent* event)
{
  for(int i = 0; i < moduleStackedWidget->count(); i++)
  {
    IModuleUI* module_ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
    if(module_ui != nullptr && module_ui->isRunning())
    {
      QString moduleName = m_ModuleNamesOrder[i];
      QMessageBox runningPipelineBox;
      runningPipelineBox.setWindowTitle("Module is Running");
      runningPipelineBox.setText(tr("The '%1' module is currently running.\nPlease cancel the running module and try again.").arg(moduleName));
      runningPipelineBox.setStandardButtons(QMessageBox::Ok);
      runningPipelineBox.setIcon(QMessageBox::Warning);
      runningPipelineBox.exec();
      event->ignore();
      return;
    }
  }

  QMessageBox::StandardButton choice = checkDirtyDocument();
  if(choice == QMessageBox::Cancel)
  {
    event->ignore();
    return;
  }

  event->accept();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMessageBox::StandardButton EMsoftWorkbench_UI::checkDirtyDocument()
{
  QMessageBox::StandardButton stdBtn = QMessageBox::Ignore;

  if(this->isWindowModified())
  {
    int r = QMessageBox::warning(this, QCoreApplication::applicationName(), tr("The Workbench has been modified.\nDo you want to save your changes?"), QMessageBox::Save | QMessageBox::Default,
                                 QMessageBox::Discard, QMessageBox::Cancel | QMessageBox::Escape);

    if(r == QMessageBox::Save)
    {
      if(saveSession())
      {
        stdBtn = QMessageBox::Save;
      }
      else
      {
        stdBtn = QMessageBox::Cancel;
      }
    }
    else if(r == QMessageBox::Discard)
    {
      stdBtn = QMessageBox::Discard;
    }
    else if(r == QMessageBox::Cancel)
    {
      stdBtn = QMessageBox::Cancel;
    }
  }

  return stdBtn;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::on_saveBtn_clicked()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Standard Output"), proposedDir, tr("Log File (*.log);;Text File (*.txt);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if(filePath.isEmpty())
  {
    return;
  }

  QFile file(filePath);
  if(file.open(QIODevice::WriteOnly))
  {
    QTextStream stream(&file);

    stream << stdOutTE->toPlainText();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::openSession(QJsonObject obj)
{
  ModuleManager* manager = ModuleManager::Instance();

  for(int i = 0; i < m_ModuleNamesOrder.size(); i++)
  {
    QString moduleName = m_ModuleNamesOrder[i];
    QJsonObject moduleObj = obj[moduleName].toObject();
    if(!moduleObj.isEmpty())
    {
      IModuleUI* module_ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
      if(module_ui == nullptr)
      {
        module_ui = manager->getModuleFromName(moduleName, moduleObj, this);
        connect(module_ui, &IModuleUI::validationOfOtherModulesNeeded, [=](IModuleUI* module_ui) {
          for(int i = 0; i < moduleStackedWidget->count(); i++)
          {
            IModuleUI* ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
            if(ui != nullptr && ui != module_ui)
            {
              ui->validateData();
            }
          }
        });

        moduleStackedWidget->insertWidget(i, module_ui); // Add module to stacked widget
      }

      module_ui->readModuleSession(moduleObj);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::listenSaveSessionTriggered()
{
  saveSession();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftWorkbench_UI::saveSession()
{
  if(isWindowModified())
  {
    QString filePath;
    if(m_OpenedFilePath.isEmpty())
    {
      // When the file hasn't been saved before, the same functionality as a "Save As" occurs...
      bool didSave = saveSessionAs();
      return didSave;
    }

    filePath = m_OpenedFilePath;

    // Fix the separators
    filePath = QDir::toNativeSeparators(filePath);

    // Write the modules to the file
    writeModulesToFile(filePath);

    // Set window title and save flag
    QFileInfo prefFileInfo = QFileInfo(filePath);
    setWindowTitle("[*]" + prefFileInfo.baseName() + " - " + QCoreApplication::applicationName());
    setWindowModified(false);

    // Add file to the recent files list
    QtSRecentFileList* list = QtSRecentFileList::instance();
    list->addFile(filePath);
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::listenSaveSessionAsTriggered()
{
  saveSessionAs();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftWorkbench_UI::saveSessionAs()
{
  QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.json";
  QString filePath = FileIOTools::GetSavePathFromDialog("Save Session", "Json File (*.json);;All Files (*.*)", proposedFile);
  if(filePath.isEmpty())
  {
    return false;
  }

  filePath = QDir::toNativeSeparators(filePath);

  // If the filePath already exists - delete it so that we get a clean write to the file
  QFileInfo fi(filePath);
  if(fi.suffix().isEmpty())
  {
    filePath.append(".json");
    fi.setFile(filePath);
  }

  // Write the modules to the file
  bool success = writeModulesToFile(filePath);

  if(success)
  {
    // Set window title and save flag
    setWindowTitle(QObject::tr("[*]%1 - %2").arg(fi.baseName(), QCoreApplication::applicationName()));
    setWindowModified(false);

    // Add file to the recent files list
    QtSRecentFileList* list = QtSRecentFileList::instance();
    list->addFile(filePath);

    m_OpenedFilePath = filePath;
  }
  else
  {
    return false;
  }

  // Cache the last directory
  emSoftApp->setOpenDialogLastDirectory(filePath);

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftWorkbench_UI::writeModulesToFile(const QString& filePath)
{
  // Write the contents
  if(!filePath.isEmpty())
  {
    QFile outputFile(filePath);
    QFileInfo info(outputFile);
    QString parentPath = info.absolutePath();
    QDir parentDir(parentPath);

    if(!parentDir.exists())
    {
      parentDir.mkpath(parentPath);
    }

    QJsonObject root;
    QJsonObject modulesObj;

    for(int i = 0; i < moduleStackedWidget->count(); i++)
    {
      IModuleUI* module = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
      if(module != nullptr)
      {
        QString moduleName = m_ModuleNamesOrder[i];

        QJsonObject moduleObj;
        module->writeModuleSession(moduleObj);

        modulesObj[moduleName] = moduleObj;
      }
    }

    root[EMsoftWorkbenchConstants::StringConstants::Modules] = modulesObj;

    QJsonDocument doc(root);

    if(outputFile.exists())
    {
      outputFile.remove();
    }
    if(outputFile.open(QIODevice::WriteOnly))
    {
      outputFile.write(doc.toJson());
      outputFile.close();
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::addIssue(const QString& msg, IModuleUI::IssueType type)
{
  int row = errorTableWidget->rowCount();
  errorTableWidget->insertRow(row);

  QTableWidgetItem* msgItem = new QTableWidgetItem(msg);
  QTableWidgetItem* typeItem;
  if(type == IModuleUI::IssueType::Error)
  {
    typeItem = new QTableWidgetItem("Error");
    typeItem->setData(Qt::BackgroundColorRole, QColor(255, 191, 193));
    msgItem->setData(Qt::BackgroundColorRole, QColor(255, 191, 193));
    m_StatusBar->issuesTableHasErrors(true);
  }
  else
  {
    typeItem = new QTableWidgetItem("Warning");
    typeItem->setData(Qt::BackgroundColorRole, QColor(251, 254, 137));
    msgItem->setData(Qt::BackgroundColorRole, QColor(251, 254, 137));
  }

  errorTableWidget->setItem(row, 0, typeItem);
  errorTableWidget->setItem(row, 1, msgItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::slot_StdOutputMsgReceived(const QString& msg)
{
  // Only add the standard output message to the standard output pane if the module
  // that sent the std output message is active
  stdOutTE->append(msg);
  statusbar->showMessage(msg);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit workbenchWindowChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::clearIssuesTable()
{
  errorTableWidget->clearContents();
  while(errorTableWidget->rowCount() > 0)
  {
    errorTableWidget->removeRow(0);
  }
  m_StatusBar->issuesTableHasErrors(false);
}

// -----------------------------------------------------------------------------
//  Read our settings from a file
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::readSettings()
{
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());

  // Have the pipeline builder read its settings from the prefs file
  readWindowSettings(prefs.data());

  // Read dock widget settings
  prefs->beginGroup("DockWidgetSettings");

  prefs->beginGroup("Issues Dock Widget");
  readDockWidgetSettings(prefs.data(), issuesDockWidget);
  prefs->endGroup();

  prefs->beginGroup("Standard Output Dock Widget");
  readDockWidgetSettings(prefs.data(), stdOutDockWidget);
  prefs->endGroup();

  prefs->endGroup();

  QtSRecentFileList::instance()->readList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::readWindowSettings(QtSSettings* prefs)
{
  bool ok = false;
  prefs->beginGroup("WindowSettings");
  if(prefs->contains(QString("MainWindowGeometry")))
  {
    QByteArray geo_data = prefs->value("MainWindowGeometry", QByteArray());
    ok = restoreGeometry(geo_data);
    if(!ok)
    {
      qDebug() << "Error Restoring the Window Geometry"
               << "\n";
    }
  }

  if(prefs->contains(QString("MainWindowState")))
  {
    QByteArray layout_data = prefs->value("MainWindowState", QByteArray());
    restoreState(layout_data);
  }

  if(prefs->contains(QString("NavigatorState")))
  {
    bool b = prefs->value("NavigatorState", QVariant(false)).toBool();
    mainToolbar->setHidden(b);
  }

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::readDockWidgetSettings(QtSSettings* prefs, QDockWidget* dw)
{
  restoreDockWidget(dw);

  bool b = prefs->value(dw->objectName(), QVariant(false)).toBool();
  dw->setHidden(b);
}

// -----------------------------------------------------------------------------
//  Write our Prefs to file
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::writeSettings()
{
  QSharedPointer<QtSSettings> prefs = QSharedPointer<QtSSettings>(new QtSSettings());

  // Have the pipeline builder write its settings to the prefs file
  writeWindowSettings(prefs.data());

  prefs->beginGroup("DockWidgetSettings");

  prefs->beginGroup("Issues Dock Widget");
  writeDockWidgetSettings(prefs.data(), issuesDockWidget);
  prefs->endGroup();

  prefs->beginGroup("Standard Output Dock Widget");
  writeDockWidgetSettings(prefs.data(), stdOutDockWidget);
  prefs->endGroup();

  prefs->endGroup();

  QtSRecentFileList::instance()->writeList(prefs.data());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::writeWindowSettings(QtSSettings* prefs)
{
  prefs->beginGroup("WindowSettings");
  QByteArray geo_data = saveGeometry();
  QByteArray layout_data = saveState();
  prefs->setValue(QString("MainWindowGeometry"), geo_data);
  prefs->setValue(QString("MainWindowState"), layout_data);
  prefs->setValue(QString("NavigatorState"), mainToolbar->isHidden());

  prefs->endGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::writeDockWidgetSettings(QtSSettings* prefs, QDockWidget* dw)
{
  prefs->setValue(dw->objectName(), dw->isHidden());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::createWorkbenchMenuSystem()
{
  m_MenuBar = new QMenuBar();

  m_MenuFile = new QMenu("File", m_MenuBar);
//  m_MenuEdit = new QMenu("Edit", m_MenuBar);
  m_MenuView = new QMenu("View", m_MenuBar);
  m_MenuThemes = emSoftApp->createThemeMenu(m_MenuView);
  m_MenuRecentFiles = new QMenu("Recent Files", m_MenuBar);
  m_MenuHelp = new QMenu("Help", m_MenuBar);

  m_ActionNew = new QAction("New...", m_MenuBar);
  m_ActionNew->setShortcut(QKeySequence::New);

  m_ActionOpen = new QAction("Open...", m_MenuBar);
  m_ActionOpen->setShortcut(QKeySequence::Open);

  m_ActionSave = new QAction("Save", m_MenuBar);
  m_ActionSave->setShortcut(QKeySequence::Save);

  m_ActionSaveAs = new QAction("Save As...", m_MenuBar);
  m_ActionSaveAs->setShortcut(QKeySequence::SaveAs);

  m_ActionClearRecentFiles = new QAction("Clear Recent Files", m_MenuBar);

  m_ActionAboutEMsoftWorkbench = new QAction("About " + QApplication::applicationName(), m_MenuBar);

  m_ActionExit = new QAction("Exit " + QApplication::applicationName(), m_MenuBar);
  m_ActionExit->setShortcut(QKeySequence::Quit);

//  m_ActionEditStyle = new QAction("Edit Style...", this);

  connect(m_ActionNew, &QAction::triggered, emSoftApp, &EMsoftApplication::listenNewInstanceTriggered);
  connect(m_ActionOpen, &QAction::triggered, emSoftApp, &EMsoftApplication::listenOpenTriggered);
  connect(m_ActionSave, &QAction::triggered, this, &EMsoftWorkbench_UI::listenSaveSessionTriggered);
  connect(m_ActionSaveAs, &QAction::triggered, this, &EMsoftWorkbench_UI::listenSaveSessionAsTriggered);
  connect(m_ActionExit, &QAction::triggered, emSoftApp, &EMsoftApplication::listenExitApplicationTriggered);
  connect(m_ActionClearRecentFiles, &QAction::triggered, emSoftApp, &EMsoftApplication::listenClearRecentFilesTriggered);
//  connect(m_ActionAboutEMsoftWorkbench, &QAction::triggered, emSoftApp, &EMsoftApplication::listenAboutEMsoftWorkbenchTriggered);
//  connect(m_ActionEditStyle, &QAction::triggered, emSoftApp, &EMsoftApplication::listenEditStyleTriggered);

  // Create File Menu
  m_MenuBar->addMenu(m_MenuFile);
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
//  m_MenuBar->addMenu(m_MenuEdit);
//  m_MenuEdit->addAction(m_ActionEditStyle);

  // Create View Menu
  m_MenuView->addMenu(m_MenuThemes);
  m_MenuBar->addMenu(m_MenuView);

  // Create Help Menu
  //  m_MenuHelp->addAction(m_ActionAboutEMsoftWorkbench);
  //  m_MenuBar->addMenu(m_MenuHelp);

  setMenuBar(m_MenuBar);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setOpenedFilePath(const QString& value)
{
  m_OpenedFilePath = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString EMsoftWorkbench_UI::getOpenedFilePath() const
{
  return m_OpenedFilePath;
}

