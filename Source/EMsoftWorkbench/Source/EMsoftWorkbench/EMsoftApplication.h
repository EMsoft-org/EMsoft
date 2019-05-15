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
  ~EMsoftApplication() override;

  SIMPL_INSTANCE_PROPERTY(QString, OpenDialogLastDirectory)

  bool initialize(int argc, char* argv[]);

  void registerWorkbenchInstance(EMsoftWorkbench_UI* instance);

  virtual void unregisterWorkbenchInstance(EMsoftWorkbench_UI* instance) = 0;

  EMsoftWorkbench_UI* getNewWorkbenchInstance();

  EMsoftWorkbench_UI* getActiveWindow();
  void setActiveWindow(EMsoftWorkbench_UI* workbench);

protected:
  EMsoftApplication(int& argc, char** argv);

  EMsoftWorkbench_UI* m_ActiveWindow;

  // This is a set of all EMsoftWorkbench_UI instances currently available
  QList<EMsoftWorkbench_UI*> m_WorkbenchInstances;

protected slots:
  void on_actionNew_triggered();
  void on_actionOpen_triggered();
  void on_actionSave_triggered();
  void on_actionSaveAs_triggered();

  void on_actionEditStyle_triggered();

  void on_actionClearRecentFiles_triggered();
  void on_actionCloseWindow_triggered();
  void on_actionExit_triggered();
  void on_actionAboutEMsoftWorkbench_triggered();

  void updateRecentFileList();

  virtual void emSoftWindowChanged(EMsoftWorkbench_UI* instance) = 0;

signals:
  void emSoftConfigurationChanged();

private:
  StyleSheetEditor* styleSheetEditor;

  EMsoftWorkbench_UI* newInstanceFromFile(const QString& filePath);

public:
  EMsoftApplication(const EMsoftApplication&) = delete;            // Copy Constructor Not Implemented
  EMsoftApplication(EMsoftApplication&&) = delete;                 // Move Constructor Not Implemented
  EMsoftApplication& operator=(const EMsoftApplication&) = delete; // Copy Assignment Not Implemented
  EMsoftApplication& operator=(EMsoftApplication&&) = delete;      // Move Assignment Not Implemented
};
