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
#include <QtCore/QString>

class MasterPatternSimulationController : public QObject
{
  Q_OBJECT

public:
  MasterPatternSimulationController(QObject* parent = nullptr);
  ~MasterPatternSimulationController() override;

  const QString k_ExeName = QString("EMEBSDmaster");
  const QString k_NMLName = QString("EMEBSDmaster.nml");

  using EnumType = unsigned int;

  enum class MonteCarloMode : EnumType
  {
    EBSD,
    ECP
  };

  /**
   *
   */
  using InputDataType = struct
  {
    double smallestDSpacing;
    int numOfMPPixels;
    int betheParametersX;
    int betheParametersY;
    int betheParametersZ;
    int numOfOpenMPThreads;
    QString inputFilePath;
    QString outputFilePath;
  };

  /**
   * @brief setData
   * @param simData
   */
  void setData(const InputDataType& simData);

  /**
   * @brief validateMonteCarloValues
   * @param data
   * @return
   */
  bool validateInput() const;

  /**
   * @brief setUpdateProgress
   * @param loopCompleted
   * @param totalLoops
   * @param bseYield
   */
  void setUpdateProgress(int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops) const;

  /**
   * @brief getNumCPUCores
   * @return
   */
  int getNumCPUCores() const;

public slots:
  /**
   * @brief execute
   */
  void execute();

  /**
   * @brief cancelProcess
   */
  void cancelProcess();

protected slots:
  void processFinished(int exitCode, QProcess::ExitStatus exitStatus);

signals:
  void warningMessageGenerated(const QString& msg) const;
  void errorMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;

  void finished();

private:
  bool m_Cancel = false;
  size_t m_InstanceKey = 0;
  bool m_Executing = false;

  InputDataType m_InputData;
  QSharedPointer<QProcess> m_CurrentProcess;

  /**
   * @brief MonteCarloSimulationController::generateNMLFile
   * @param path
   */
  void generateNMLFile(const QString& path);

public:
  MasterPatternSimulationController(const MasterPatternSimulationController&) = delete;            // Copy Constructor Not Implemented
  MasterPatternSimulationController(MasterPatternSimulationController&&) = delete;                 // Move Constructor Not Implemented
  MasterPatternSimulationController& operator=(const MasterPatternSimulationController&) = delete; // Copy Assignment Not Implemented
  MasterPatternSimulationController& operator=(MasterPatternSimulationController&&) = delete;      // Move Assignment Not Implemented
};
