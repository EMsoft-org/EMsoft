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

#include <QtWidgets/QFrame>
#include <QtWidgets/QToolBar>

#include "ui_StatusBarWidget.h"

class QDockWidget;

class StatusBarWidget : public QFrame, private Ui::StatusBarWidget
{

  Q_OBJECT

public:
  StatusBarWidget(QWidget* parent = 0);
  virtual ~StatusBarWidget() override;

  using EnumType = unsigned int;

  enum class Button : EnumType
  {
    Issues = 0,
    StandardOutput = 1,
    ModuleNavigator = 2
  };

  /**
   * @brief setButtonAction
   * @param action
   * @param btn
   */
  void setButtonAction(QDockWidget* dock, Button btn);

  /**
   * @brief setButtonAction
   * @param toolBar
   * @param btn
   */
  void setButtonAction(QToolBar* toolBar, Button btn);

  /**
   * @brief updateStyle
   */
  void updateStyle();

  /**
   * @brief generateStyleSheet
   * @param error
   * @return
   */
  QString generateStyleSheet(bool error);

public slots:
  /**
   * @brief issuesVisibilityChanged
   * @param b
   */
  void issuesVisibilityChanged(bool b);
  /**
   * @brief stdOutputVisibilityChanged
   * @param b
   */
  void stdOutputVisibilityChanged(bool b);

  /**
   * @brief navigatorVisibilityChanged
   * @param b
   */
  void navigatorVisibilityChanged(bool b);

  /**
   * @brief toolboxVisibilityChanged
   * @param b
   */
  void toolboxVisibilityChanged(bool b);

  /**
   * @brief issuesTableHasErrors
   * @param b
   */
  void issuesTableHasErrors(bool b);

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

private:
public:
  StatusBarWidget(const StatusBarWidget&) = delete;            // Copy Constructor Not Implemented
  StatusBarWidget(StatusBarWidget&&) = delete;                 // Move Constructor Not Implemented
  StatusBarWidget& operator=(const StatusBarWidget&) = delete; // Copy Assignment Not Implemented
  StatusBarWidget& operator=(StatusBarWidget&&) = delete;      // Move Assignment Not Implemented
};
