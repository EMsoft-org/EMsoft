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

#pragma once

#include <QtCore/QObject>
#include <QtCore/QPropertyAnimation>

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMessageBox>

#include "Modules/IModuleUI.h"

#include "ui_EMsoftWorkbench_UI.h"

class IModuleUI;
class QtSSettings;
class StatusBarWidget;

class EMsoftWorkbench_UI : public QMainWindow, public Ui::EMsoftWorkbench_UI
{
  Q_OBJECT

public:
  EMsoftWorkbench_UI(QWidget* parent = nullptr);
  ~EMsoftWorkbench_UI() override;

  SIMPL_INSTANCE_PROPERTY(QString, OpenedFilePath)

  /**
   * @brief openSession
   * @param filePath
   * @return
   */
  void openSession(QJsonObject obj);

  /**
   * @brief saveSession
   */
  bool saveSession();

  /**
   * @brief saveSessionAs
   */
  bool saveSessionAs();

  /**
   * @brief readSettings
   */
  void readSettings();

  /**
   * @brief writeSettings
   */
  void writeSettings();

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

  /**
   * @brief changeEvent
   * @param event
   */
  void changeEvent(QEvent* event) override;

  /**
   * @brief closeEvent
   * @param event
   */
  void closeEvent(QCloseEvent* event) override;

  /**
   * @brief checkDirtyDocument
   * @return
   */
  QMessageBox::StandardButton checkDirtyDocument();

protected slots:
  void on_saveBtn_clicked();

  void slot_StdOutputMsgReceived(const QString& msg);

signals:
  /**
   * @brief workbenchWindowChangedState
   */
  void workbenchWindowChangedState(EMsoftWorkbench_UI* instance);

private:
  int m_CurrentStackedWidgetIdx = -1;
  QActionGroup* m_ToolbarButtonGroup = nullptr;
  QStringList m_ModuleNamesOrder;
  StatusBarWidget* m_StatusBar;

  /**
   * @brief setupStatusBar
   */
  void setupStatusBar();

  /**
   * @brief setupMainToolbarAndStackedWidget
   */
  void setupMainToolbarAndStackedWidget();

  /**
   * @brief setupIssuesTable
   */
  void setupIssuesTable();

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections();

  /**
   * @brief clearIssuesTable
   */
  void clearIssuesTable();

  /**
   * @brief addMessage
   * @param msg
   * @param msgType
   */
  void addIssue(const QString& msg, IModuleUI::IssueType type);

  /**
   * @brief readWindowSettings
   * @param prefs
   */
  void readWindowSettings(QtSSettings* prefs);

  /**
   * @brief readDockWidgetSettings
   * @param prefs
   * @param dw
   */
  void readDockWidgetSettings(QtSSettings* prefs, QDockWidget* dw);

  /**
   * @brief writeWindowSettings
   * @param prefs
   */
  void writeWindowSettings(QtSSettings* prefs);

  /**
   * @brief writeDockWidgetSettings
   * @param prefs
   * @param dw
   */
  void writeDockWidgetSettings(QtSSettings* prefs, QDockWidget* dw);

  /**
   * @brief writeModulesToFile
   */
  bool writeModulesToFile(const QString& filePath);

public:
  EMsoftWorkbench_UI(const EMsoftWorkbench_UI&) = delete;            // Copy Constructor Not Implemented
  EMsoftWorkbench_UI(EMsoftWorkbench_UI&&) = delete;                 // Move Constructor Not Implemented
  EMsoftWorkbench_UI& operator=(const EMsoftWorkbench_UI&) = delete; // Copy Assignment Not Implemented
  EMsoftWorkbench_UI& operator=(EMsoftWorkbench_UI&&) = delete;      // Move Assignment Not Implemented
};
