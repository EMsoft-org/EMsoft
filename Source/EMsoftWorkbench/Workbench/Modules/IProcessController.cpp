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

#include "IProcessController.h"

#include <QtCore/QDateTime>
#include <QtCore/QTemporaryDir>
#include <QtCore/QTextStream>

#include "Workbench/Common/FileIOTools.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IProcessController::IProcessController(const QString& exeName, const QString& nmlName, QObject* parent)
: QObject(parent)
, m_ExeName(exeName)
, m_NMLName(nmlName)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IProcessController::~IProcessController()
{
}

// -----------------------------------------------------------------------------
void IProcessController::cancel()
{
  if(m_CurrentProcess != nullptr)
  {
    m_CurrentProcess->kill();
  }

  m_Cancel = true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void IProcessController::execute()
{
  QString dtFormat("yyyy:MM:dd hh:mm:ss.zzz");

  QTemporaryDir tempDir;
  // Set the start time for this run (m_StartTime)
  QString str;
  QTextStream out(&str);

  m_CurrentProcess = QSharedPointer<QProcess>(new QProcess());
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit stdOutputMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardOutput().toStdString())); });
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardError, [=] { emit errorMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardError().toStdString())); });
  connect(m_CurrentProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { processFinished(exitCode, exitStatus); });
  std::pair<QString, QString> result = FileIOTools::GetExecutablePath(m_ExeName);
  if(!result.first.isEmpty())
  {
    QProcessEnvironment env = m_CurrentProcess->processEnvironment();
    env.insert("EMSOFTPATHNAME", QString::fromStdString(FileIOTools::GetEMsoftPathName()));
    m_CurrentProcess->setProcessEnvironment(env);
    out << m_ExeName << ": Executable Path:" << result.first << "\n";
    out << m_ExeName << ": Start Time: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
    out << m_ExeName << ": Insert EMSOFTPATHNAME=" << QString::fromStdString(FileIOTools::GetEMsoftPathName()) << "\n";
    out << m_ExeName << ": Output from " << m_ExeName << " follows next...."
        << "\n";
    out << "===========================================================\n";

    emit stdOutputMessageGenerated(str);

    QString nmlFilePath = tempDir.path() + QDir::separator() + m_NMLName;
    generateNMLFile(nmlFilePath);
    QStringList parameters = {nmlFilePath};
    m_CurrentProcess->start(result.first, parameters);
    m_Executing = true;

    // Wait until the QProcess is finished to exit this thread.
    m_CurrentProcess->waitForFinished(-1);
  }
  else
  {
    emit errorMessageGenerated(result.second);
  }

  str = "";
  out << "===========================================================\n";
  out << m_ExeName << ": Finished: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
  //  out << k_ExeName << ": Output File Location: " << m_InputData.inputFilePath << "\n";
  emit stdOutputMessageGenerated(str);

  emit finished();
}

// -----------------------------------------------------------------------------
void IProcessController::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  processFinished();

  m_Executing = false;

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(m_Cancel)
  {
    emit stdOutputMessageGenerated(QString("%1 was canceled.").arg(m_ExeName));
    m_Cancel = false;
    return;
  }

  if(exitStatus == QProcess::ExitStatus::CrashExit)
  {
    emit stdOutputMessageGenerated(QString("%1n process crashed with exit code %2").arg(m_ExeName).arg(exitCode));
  }

  if(exitStatus == QProcess::ExitStatus::NormalExit)
  {
    emit stdOutputMessageGenerated(QString("%1 Completed").arg(m_ExeName));
  }
}
