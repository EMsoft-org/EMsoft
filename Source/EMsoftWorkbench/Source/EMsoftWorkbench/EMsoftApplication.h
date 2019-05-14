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

#pragma once

#include <QtCore/QJsonObject>
#include <QtCore/QSet>
#include <QtCore/QSharedPointer>

#include <QtWidgets/QApplication>
#include <QtWidgets/QMenuBar>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include "Modules/IModuleUI.h"

#define emSoftApp (static_cast<EMsoftApplication*>(qApp))

class EMsoftWorkbench_UI;
class StyleSheetEditor;

class EMsoftApplication : public QApplication
{
  Q_OBJECT

public:
  EMsoftApplication(int& argc, char** argv);

  ~EMsoftApplication() override;

  SIMPL_INSTANCE_PROPERTY(QString, OpenDialogLastDirectory)

  bool initialize(int argc, char* argv[]);

  void registerWorkbenchInstance(EMsoftWorkbench_UI* instance);

  void unregisterWorkbenchInstance(EMsoftWorkbench_UI* instance);

  EMsoftWorkbench_UI* getNewWorkbenchInstance();

  EMsoftWorkbench_UI* getActiveWindow();
  void setActiveWindow(EMsoftWorkbench_UI* workbench);

public slots:
  void listenNewInstanceTriggered();
  void listenOpenTriggered();
  void listenEditStyleTriggered();
  void listenAboutEMsoftWorkbenchTriggered();
  void listenClearRecentFilesTriggered();
  void listenExitApplicationTriggered();

protected:
  EMsoftWorkbench_UI* m_ActiveWindow;

  // This is a set of all EMsoftWorkbench_UI instances currently available
  QList<EMsoftWorkbench_UI*> m_WorkbenchInstances;

protected slots:
  void updateRecentFileList();

  void emSoftWindowChanged(EMsoftWorkbench_UI* instance);

signals:
  void emSoftConfigurationChanged();

private:
  StyleSheetEditor* styleSheetEditor;

  QMenuBar* m_DefaultMenuBar = nullptr;

  QMenu* m_MenuFile = nullptr;
  QMenu* m_MenuEdit = nullptr;
  QMenu* m_MenuView = nullptr;
  QMenu* m_MenuRecentFiles = nullptr;
  QMenu* m_MenuHelp = nullptr;

  QAction* m_ActionNew = nullptr;
  QAction* m_ActionOpen = nullptr;
  QAction* m_ActionSave = nullptr;
  QAction* m_ActionSaveAs = nullptr;
  QAction* m_ActionClearRecentFiles = nullptr;
  QAction* m_ActionAboutEMsoftWorkbench = nullptr;
  QAction* m_ActionExit = nullptr;
  QAction* m_ActionEditStyle = nullptr;

  /**
   * @brief Creates the default menu bar that gets shown if there is no EMsoftWorkbench_UI window.
   */
  void createDefaultMenuBar();

  /**
   * @brief createCustomDockMenu
   * @return
   */
  QMenu* createCustomDockMenu();

  /**
   * @brief newInstanceFromFile
   * @param filePath
   * @return
   */
  EMsoftWorkbench_UI* newInstanceFromFile(const QString &filePath);

public:
  EMsoftApplication(const EMsoftApplication&) = delete;            // Copy Constructor Not Implemented
  EMsoftApplication(EMsoftApplication&&) = delete;                 // Move Constructor Not Implemented
  EMsoftApplication& operator=(const EMsoftApplication&) = delete; // Copy Assignment Not Implemented
  EMsoftApplication& operator=(EMsoftApplication&&) = delete;      // Move Assignment Not Implemented
};
