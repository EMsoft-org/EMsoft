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
#include <QtCore/QProcess>
#include <QtCore/QSharedPointer>

class IProcessController : public QObject
{
  Q_OBJECT

public:
  ~IProcessController() override;

public slots:
  /**
   * @brief execute
   */
  void execute();

  /**
   * @brief cancelProcess
   */
  void cancelProcess();

protected:
  IProcessController(const QString& exeName, const QString& nmlName, QObject* parent = nullptr);

  bool m_Cancel = false;
  bool m_Executing = false;

protected slots:
  void processFinished(int exitCode, QProcess::ExitStatus exitStatus);

signals:
  void warningMessageGenerated(const QString& msg) const;
  void errorMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;
  void finished();

private:
  QString m_ExeName;
  QString m_NMLName;

  QString m_StartTime = "";
  QSharedPointer<QProcess> m_CurrentProcess;

  /**
   * @brief DictionaryIndexingController::generateNMLFile
   * @param path
   */
  virtual void generateNMLFile(const QString& path) = 0;

  /**
   * @brief processFinished
   */
  virtual void processFinished() = 0;

public:
  IProcessController(const IProcessController&) = delete;            // Copy Constructor Not Implemented
  IProcessController(IProcessController&&) = delete;                 // Move Constructor Not Implemented
  IProcessController& operator=(const IProcessController&) = delete; // Copy Assignment Not Implemented
  IProcessController& operator=(IProcessController&&) = delete;      // Move Assignment Not Implemented
};
