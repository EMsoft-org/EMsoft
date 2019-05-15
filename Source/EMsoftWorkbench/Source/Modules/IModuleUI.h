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

#include <QtWidgets/QWidget>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

class IModuleUI : public QWidget
{
  Q_OBJECT

public:
  SIMPL_SHARED_POINTERS(IModuleUI)

  SIMPL_BOOL_PROPERTY(Running)

  ~IModuleUI() override;

  using EnumType = unsigned int;

  enum class IssueType : EnumType
  {
    Error,
    Warning
  };

  struct ModuleIssue
  {
    QString msg;
    IssueType msgType;
  };

  /**
   * @brief readModuleSession
   * @param obj
   */
  virtual void readModuleSession(QJsonObject& obj) = 0;

  /**
   * @brief writeModuleSession
   * @param obj
   */
  virtual void writeModuleSession(QJsonObject& obj) = 0;

  /**
   * @brief validateData
   */
  virtual bool validateData() = 0;

  /**
   * @brief getModuleIssues
   */
  QList<IModuleUI::ModuleIssue> getModuleIssues();

  /**
   * @brief getStdOutput
   * @return
   */
  QString getStdOutput();

protected:
  QList<ModuleIssue> m_ModuleIssues;
  QString m_StdOutput;

  IModuleUI(QWidget* parent = nullptr);

  /**
   * @brief getOpenedFilePath
   * @return
   */
  QString getOpenedFilePath();

  /**
   * @brief setOpenedFilePath
   * @param val
   * @return
   */
  void setOpenedFilePath(const QString& val);

  /**
   * @brief clearModuleIssues
   */
  void clearModuleIssues();

protected slots:
  void appendToStdOut(const QString& msg);

  void notifyErrorMessage(const QString& msg);
  void notifyWarningMessage(const QString& msg);

signals:
  /**
   * @brief moduleChangedState
   * @param instance
   */
  void moduleChangedState(IModuleUI* instance);

  void issuesCleared();

  void validationOfOtherModulesNeeded(IModuleUI* self);

  void moduleParametersChanged();

  void errorMessageGenerated(const QString& msg);
  void warningMessageGenerated(const QString& msg);
  void stdOutputMessageGenerated(const QString& msg);

private:
  QString m_OpenedFilePath = "";

public:
  IModuleUI(const IModuleUI&) = delete;            // Copy Constructor Not Implemented
  IModuleUI(IModuleUI&&) = delete;                 // Move Constructor Not Implemented
  IModuleUI& operator=(const IModuleUI&) = delete; // Copy Assignment Not Implemented
  IModuleUI& operator=(IModuleUI&&) = delete;      // Move Assignment Not Implemented
};
