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

#include <QtGui/QResizeEvent>

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QToolButton>

#include "Modules/ModuleManager.h"
#include "Modules/CrystalStructureCreationModule/CrystalStructureCreation_UI.h"

#include "Common/Constants.h"
#include "Common/EMsoftMenuItems.h"
#include "Common/FileIOTools.h"
#include "Common/QtSSettings.h"
#include "Common/QtSRecentFileList.h"

#include "EMsoftWorkbench/StandardEMsoftApplication.h"
#include "EMsoftWorkbench/StatusBarWidget.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftWorkbench_UI::EMsoftWorkbench_UI(QWidget* parent) :
  QMainWindow(parent)
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

  if (emSoftApp->activeWindow() == this)
  {
    emSoftApp->setActiveWindow(nullptr);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::setupGui()
{  
#if defined (Q_OS_WIN)
  setMenuBar(standardApp->getSIMPLViewMenuBar());
#endif

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
  connect(moduleStackedWidget, &QStackedWidget::currentChanged, [=] (int index) {
    clearIssuesTable();
    stdOutTE->clear();

    IModuleUI* oldModule = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(m_CurrentStackedWidgetIdx));
    IModuleUI* newModule = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(index));

    if (oldModule != nullptr)
    {
      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::issuesCleared, 0, 0);

      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::moduleParametersChanged, 0, 0);

      // Connect the module's error messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::errorMessageGenerated, 0, 0);

      // Connect the module's warning messages so that they can be displayed in the issues table
      disconnect(oldModule, &IModuleUI::warningMessageGenerated, 0, 0);

      // Connect the module's std output messages so that they can be displayed in the std output pane.
      // This must be done using the old-style signal/slot connection because it may cross threads
      disconnect(oldModule, SIGNAL(stdOutputMessageGenerated(const QString &)), this, SLOT(slot_StdOutputMsgReceived(const QString &)));
    }

    if (newModule != nullptr)
    {
      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::issuesCleared, [=] {
        clearIssuesTable();
      });

      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::moduleParametersChanged, [=] {
        setWindowModified(true);
      });

      // Connect the module's error messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::errorMessageGenerated, [=] (const QString &msg) {
          addIssue(msg, IModuleUI::IssueType::Error);
      });

      // Connect the module's warning messages so that they can be displayed in the issues table
      connect(newModule, &IModuleUI::warningMessageGenerated, [=] (const QString &msg) {
        addIssue(msg, IModuleUI::IssueType::Warning);
      });

      // Connect the module's std output messages so that they can be displayed in the std output pane.
      // This must be done using the old-style signal/slot connection because it may cross threads
      connect(newModule, SIGNAL(stdOutputMessageGenerated(const QString &)), this, SLOT(slot_StdOutputMsgReceived(const QString &)));

      // Set the widget into the frame
      QList<IModuleUI::ModuleIssue> issues = newModule->getModuleIssues();
      for (int i = 0; i < issues.size(); i++)
      {
        IModuleUI::ModuleIssue issue = issues[i];
        addIssue(issue.msg, issue.msgType);
      }

      stdOutTE->setText(newModule->getStdOutput());
    }

  }); // End connect

  // Create the order that the modules will be displayed in the user interface.  This order
  // gives the user an idea of what order to use the modules in.
  ModuleManager* manager = ModuleManager::Instance();
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::CrystalStructureCreation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::MonteCarloSimulation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::MasterPatternSimulation);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::PatternDisplay);
  m_ModuleNamesOrder.append(EMsoftWorkbenchConstants::ModuleNames::PatternFit);

  m_ToolbarButtonGroup = new QActionGroup(this);

  for (int i = 0; i < m_ModuleNamesOrder.size(); i++)
  {
    // Create a toolbar action labeled with the module name
    QString moduleName = m_ModuleNamesOrder[i];
    QAction* toolbarAction = mainToolbar->addAction(tr("%1. %2").arg(QString::number(i + 1)).arg(moduleName));   // Create toolbar action
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

      if (dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(idx)) == nullptr)
      {
        // This module doesn't exist yet, so create it
        IModuleUI* module_ui = manager->getModuleFromName(m_ModuleNamesOrder[idx], QJsonObject(), this);
        if (module_ui != nullptr)
        {
          connect(module_ui, &IModuleUI::validationOfOtherModulesNeeded, [=] (IModuleUI* module_ui) {
            for (int i = 0; i < moduleStackedWidget->count(); i++)
            {
              IModuleUI* ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
              if (ui != nullptr && ui != module_ui)
              {
                ui->validateData();
              }
            }
          });

          // Delete the placeholder
          QWidget* placeholder = moduleStackedWidget->widget(idx);
          moduleStackedWidget->removeWidget(placeholder);
          delete placeholder;

          // Insert the module widget into its proper place in the stacked widget
          moduleStackedWidget->insertWidget(idx, module_ui);
          moduleStackedWidget->setCurrentIndex(idx);
          m_CurrentStackedWidgetIdx = idx;
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
        moduleStackedWidget->setCurrentIndex(idx);
        m_CurrentStackedWidgetIdx = idx;
      }
    });  // End connect
  }

  // Trigger the first toolbar action so that it displays first by default
  QList<QAction*> toolbarActions = mainToolbar->actions();
  if (toolbarActions.size() > 0)
  {
    toolbarActions[0]->trigger();
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
  for (int i = 0; i < moduleStackedWidget->count(); i++)
  {
    IModuleUI* module_ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
    if (module_ui != nullptr && module_ui->isRunning() == true)
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
  if(this->isWindowModified() == true)
  {
    int r = QMessageBox::warning(this, QCoreApplication::applicationName(), tr("The Workbench has been modified.\nDo you want to save your changes?"), QMessageBox::Save | QMessageBox::Default,
                                 QMessageBox::Discard, QMessageBox::Cancel | QMessageBox::Escape);
    if(r == QMessageBox::Save)
    {
      if(saveSession() == true)
      {
        return QMessageBox::Save;
      }
      else
      {
        return QMessageBox::Cancel;
      }
    }
    else if(r == QMessageBox::Discard)
    {
      return QMessageBox::Discard;
    }
    else if(r == QMessageBox::Cancel)
    {
      return QMessageBox::Cancel;
    }
  }

  return QMessageBox::Ignore;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::on_saveBtn_clicked()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Standard Output"),
    proposedDir, tr("Log File (*.log);;Text File (*.txt);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if (filePath.isEmpty()) { return; }

  QFile file(filePath);
  if (file.open(QIODevice::WriteOnly))
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

  for (int i = 0; i < m_ModuleNamesOrder.size(); i++)
  {
    QString moduleName = m_ModuleNamesOrder[i];
    QJsonObject moduleObj = obj[moduleName].toObject();
    if (moduleObj.isEmpty() == false)
    {
      IModuleUI* module_ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
      if (module_ui == nullptr)
      {
        module_ui = manager->getModuleFromName(moduleName, moduleObj, this);
        connect(module_ui, &IModuleUI::validationOfOtherModulesNeeded, [=] (IModuleUI* module_ui) {
          for (int i = 0; i < moduleStackedWidget->count(); i++)
          {
            IModuleUI* ui = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
            if (ui != nullptr && ui != module_ui)
            {
              ui->validateData();
            }
          }
        });

        moduleStackedWidget->insertWidget(i, module_ui);     // Add module to stacked widget
      }

      module_ui->readModuleSession(moduleObj);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool EMsoftWorkbench_UI::saveSession()
{
  if(isWindowModified() == true)
  {
    QString filePath;
    if(m_OpenedFilePath.isEmpty())
    {
      // When the file hasn't been saved before, the same functionality as a "Save As" occurs...
      bool didSave = saveSessionAs();
      return didSave;
    }
    else
    {
      filePath = m_OpenedFilePath;
    }

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
bool EMsoftWorkbench_UI::saveSessionAs()
{
  QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.json";
  QString filePath = FileIOTools::GetSavePathFromDialog("Save Session", "Json File (*.json);;All Files (*.*)", proposedFile);
  if(true == filePath.isEmpty())
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

  if(success == true)
  {
    // Set window title and save flag
    setWindowTitle(QObject::tr("[*]%1 - %2").arg(fi.baseName()).arg(QCoreApplication::applicationName()));
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
bool EMsoftWorkbench_UI::writeModulesToFile(const QString &filePath)
{
  // Write the contents
  if(filePath.isEmpty() == false)
  {
    QFile outputFile(filePath);
    QFileInfo info(outputFile);
    QString parentPath = info.absolutePath();
    QDir parentDir(parentPath);

    if(parentDir.exists() == false)
    {
      parentDir.mkpath(parentPath);
    }

    QJsonObject root;
    QJsonObject modulesObj;

    for (int i = 0; i < moduleStackedWidget->count(); i++)
    {
      IModuleUI* module = dynamic_cast<IModuleUI*>(moduleStackedWidget->widget(i));
      if (module != nullptr)
      {
        QString moduleName = m_ModuleNamesOrder[i];

        QJsonObject moduleObj;
        module->writeModuleSession(moduleObj);

        modulesObj[moduleName] = moduleObj;
      }
    }

    root[EMsoftWorkbenchConstants::StringConstants::Modules] = modulesObj;

    QJsonDocument doc(root);

    if(outputFile.exists() == true)
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
void EMsoftWorkbench_UI::addIssue(const QString &msg, IModuleUI::IssueType type)
{
  int row = errorTableWidget->rowCount();
  errorTableWidget->insertRow(row);

  QTableWidgetItem* msgItem = new QTableWidgetItem(msg);
  QTableWidgetItem* typeItem;
  if (type == IModuleUI::IssueType::Error)
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
void EMsoftWorkbench_UI::slot_StdOutputMsgReceived(const QString &msg)
{
  // Only add the standard output message to the standard output pane if the module
  // that sent the std output message is active
  stdOutTE->append("\n" + msg);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftWorkbench_UI::changeEvent(QEvent* event)
{
  if (event->type() == QEvent::ActivationChange)
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
  while (errorTableWidget->rowCount() > 0)
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

  QString name = dw->objectName();
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



