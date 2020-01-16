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

#include "MasterPatternSimulationController.h"

#include <cmath>
#include <functional>
#include <sstream>
#include <utility>

#include <QtCore/QCoreApplication>
#include <QtCore/QDateTime>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QTemporaryDir>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Workbench/Common/Constants.h"
#include "Workbench/Common/EMsoftFileWriter.h"
#include "Workbench/Common/FileIOTools.h"
#include "Workbench/Common/MonteCarloFileReader.h"
#include "Workbench/Common/ProjectionConversions.hpp"

#include "EMsoftWorkbenchVersion.h"

#define CL_VECTOR std::vector

static size_t s_InstanceKey = 0;
static QMap<size_t, MasterPatternSimulationController*> s_ControllerInstances;

/**
 * @brief MasterPatternSimulationControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void MasterPatternSimulationControllerProgress(size_t instance, int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops)
{
  MasterPatternSimulationController* obj = s_ControllerInstances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops, EloopCompleted, totalEloops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationController(QObject* parent)
: QObject(parent)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::~MasterPatternSimulationController()
{
  s_InstanceKey--;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setData(const InputDataType& data)
{
  m_InputData = data;
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::cancelProcess()
{
  if(m_CurrentProcess != nullptr)
  {
    m_CurrentProcess->kill();
  }
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::execute()
{
  QString dtFormat("yyyy:MM:dd hh:mm:ss.zzz");

  QTemporaryDir tempDir;
  // Set the start time for this run (m_StartTime)
  QString str;
  QTextStream out(&str);

  m_CurrentProcess = QSharedPointer<QProcess>(new QProcess());
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit stdOutputMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardOutput().toStdString())); });
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardError, [=] { emit stdOutputMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardError().toStdString())); });
  connect(m_CurrentProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { processFinished(exitCode, exitStatus); });
  std::pair<QString, QString> result = FileIOTools::GetExecutablePath(k_ExeName);
  if(!result.first.isEmpty())
  {
    QProcessEnvironment env = m_CurrentProcess->processEnvironment();
    env.insert("EMSOFTPATHNAME", QString::fromStdString(FileIOTools::GetEMsoftPathName()));
    m_CurrentProcess->setProcessEnvironment(env);
    out << k_ExeName << ": Executable Path:" << result.first << "\n";
    out << k_ExeName << ": Start Time: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
    out << k_ExeName << ": Insert EMSOFTPATHNAME=" << QString::fromStdString(FileIOTools::GetEMsoftPathName()) << "\n";
    out << k_ExeName << ": Output from " << k_ExeName << " follows next...."
        << "\n";
    out << "===========================================================\n";

    emit stdOutputMessageGenerated(str);

    QString nmlFilePath = tempDir.path() + QDir::separator() + k_NMLName;
    generateNMLFile(nmlFilePath);
    QStringList parameters = {nmlFilePath};
    m_CurrentProcess->start(result.first, parameters);

    // Wait until the QProcess is finished to exit this thread.
    m_CurrentProcess->waitForFinished(-1);
  }
  else
  {
    emit errorMessageGenerated(result.second);
  }

  str = "";
  out << "===========================================================\n";
  out << k_ExeName << ": Finished: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
  out << k_ExeName << ": Output File Location: " << m_InputData.inputFilePath << "\n";
  emit stdOutputMessageGenerated(str);

  emit finished();
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::generateNMLFile(const QString& path)
{
  QFileInfo fi(path);
  QString betheParametersFilePath = fi.path() + QDir::separator() + "BetheParameters.nml";
  generateBetheParametersFile(betheParametersFilePath);

  std::vector<std::string> nml;

  m_InputData.inputFilePath = FileIOTools::GetAbsolutePath(m_InputData.inputFilePath);

  nml.emplace_back(std::string(" &EBSDmastervars"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dmin, static_cast<float>(m_InputData.smallestDSpacing)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::npx, m_InputData.numOfMPPixels));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nthreads, m_InputData.numOfOpenMPThreads));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energyfile, m_InputData.inputFilePath));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::BetheParametersFile, betheParametersFilePath));
  nml.emplace_back(FileIOTools::CreateNMLEntry(QString("Notify"), QString("Off")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::copyfromenergyfile, QString("undefined")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::h5copypath, QString("undefined")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::restart, false));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::uniform, false));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::useEnergyWeighting, false));

  nml.emplace_back(std::string(" /"));

  QFile outputFile(path);
  if(outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    for(const auto& entry : nml)
    {
      out << QString::fromStdString(entry) << "\n";
    }
    outputFile.close();

    outputFile.copy("/tmp/EMEBSDmaster.nml");
  }
  else
  {
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(path));
  }
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::generateBetheParametersFile(const QString& path)
{
  std::vector<std::string> nml;

  m_InputData.inputFilePath = FileIOTools::GetAbsolutePath(m_InputData.inputFilePath);

  nml.emplace_back(std::string(" &Bethelist"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c1, static_cast<float>(m_InputData.betheParametersX)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c2, m_InputData.betheParametersY));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c3, m_InputData.betheParametersZ));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sgdbdiff, m_InputData.sgdbdiff));

  nml.emplace_back(std::string(" /"));

  QFile outputFile(path);
  if(outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    for(const auto& entry : nml)
    {
      out << QString::fromStdString(entry) << "\n";
    }
    outputFile.close();

    outputFile.copy("/tmp/BetheParameters.nml");
  }
  else
  {
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(path));
  }
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  m_Executing = false;
  s_ControllerInstances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(m_Cancel)
  {
    emit stdOutputMessageGenerated(QString("%1 was canceled.").arg(k_ExeName));
  }

  if(exitStatus == QProcess::CrashExit)
  {
    emit stdOutputMessageGenerated(QString("%1n process crashed with exit code %2").arg(k_ExeName).arg(exitCode));
  }

  if(exitStatus == QProcess::NormalExit)
  {
    emit stdOutputMessageGenerated(QString("%1 Completed").arg(k_ExeName));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MasterPatternSimulationController::validateInput() const
{
  bool valid = true;

  QString inputPath = m_InputData.inputFilePath;
  QFileInfo inFi(inputPath);
  if(inFi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The input Monte Carlo file at path '%1' needs a '.h5' suffix.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if(!inFi.exists())
  {
    QString ss = QObject::tr("The input Monte Carlo file with path '%1' does not exist.").arg(inputPath);
    emit errorMessageGenerated(ss);
    valid = false;
  }

  QString outputPath = m_InputData.outputFilePath;

  QFileInfo dir(outputPath);
  QDir dPath = dir.path();
  if(dir.suffix().compare("") == 0)
  {
    outputPath.append(".h5");
  }
  if(!dPath.exists())
  {
    QString ss = QObject::tr("The directory path for the HDF5 file does not exist. DREAM.3D will attempt to create this path during execution of the filter");
    emit warningMessageGenerated(ss);
  }

  if(m_InputData.smallestDSpacing < 0)
  {
    QString ss = QObject::tr("dmin must be positive (see also help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  if(m_InputData.numOfMPPixels < 0)
  {
    QString ss = QObject::tr("Number of pixels must be positive");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  // test the Bethe Parameters (must be in increasing order)
  if((m_InputData.betheParametersX > m_InputData.betheParametersY) || (m_InputData.betheParametersY > m_InputData.betheParametersZ))
  {
    QString ss = QObject::tr("Bethe parameters must be listed from smallest to largest (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }
  if(m_InputData.betheParametersX < 0.0)
  {
    QString ss = QObject::tr("All Bethe parameters must be positive (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  return valid;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setUpdateProgress(int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops) const
{
  QString ss = QObject::tr("Master Pattern steps completed: %1 of %2; %3 of %4 energy bins").arg(loopCompleted).arg(totalLoops).arg(EloopCompleted).arg(totalEloops);
  emit stdOutputMessageGenerated(ss);
}
