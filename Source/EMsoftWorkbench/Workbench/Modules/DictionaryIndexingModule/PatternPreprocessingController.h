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
#include <QtCore/QTemporaryDir>

#include "ADPMapController.h"

class PatternPreprocessingController : public QObject
{
  Q_OBJECT

public:
  const QString k_ExeName = QString("EMEBSDDIpreview");
  const QString k_NMLName = QString("EMEBSDDIpreview.nml");

  using InputType = EMsoftWorkbenchConstants::InputType;

  PatternPreprocessingController(QObject* parent = nullptr);
  ~PatternPreprocessingController() override;

  /**
    * @brief Getter property for Cancel
    * @return Value of Cancel
    */
  bool getCancel() const;

  /**
    * @brief Setter property for Cancel
    */
  void setCancel(const bool& value);

  using InputDataType = struct
  {
    int patternHeight;
    int patternWidth;
    int ipfHeight;
    int ipfWidth;
    float hipassValue;
    int hipassNumSteps;
    int minNumOfRegions;
    int maxNumOfRegions;
    int numOfRegionsStepSize;
    QString patternDataFile;
    InputType inputType;
    QStringList hdfStrings;
    int patternCoordinateX;
    int patternCoordinateY;

    // Used in wrapper routine, but not NML file...
    int numav;
    int numw;
    int numr;
    float hipassFilter;
  };

  /**
   * @brief setData
   * @param data
   */
  void setData(const InputDataType& data);

  /**
   * @brief setUpdateProgress
   * @param loopCompleted
   * @param totalLoops
   */
  void setUpdateProgress(int loopCompleted, int totalLoops);

  /**
     * @brief getNumCPUCores
     * @return
     */
  int getNumCPUCores();

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
  void preprocessedPatternsMatrixCreated(const QImage &adpMap) const;
  void warningMessageGenerated(const QString& msg) const;
  void errorMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;
  void finished();

private:
  QString m_StartTime = "";
  InputDataType m_InputData;
  QSharedPointer<QProcess> m_CurrentProcess;

  std::vector<float> m_OutputMaskVector;
  std::vector<float> m_OutputIQMapVector;
  std::vector<float> m_OutputADPMapVector;

  bool m_Cancel = false;
  size_t m_InstanceKey = 0;
  bool m_Executing = false;

  QTemporaryDir m_TempDir;

  /**
   * @brief initializeData
   */
  void initializeData();

  /**
   * @brief getPreprocessedPatternsMatrixExecutablePath
   * @return
   */
  QString getPreprocessedPatternsMatrixExecutablePath() const;

  /**
   * @brief generateNMLFile
   * @param filePath
   */
  void generateNMLFile(const QString& filePath) const;

  /**
   * @brief executeWrapper
   */
  void executeWrapper();

  //  std::vector<int32_t> getIParVector() const;

  //  std::vector<float> getFParVector() const;

  //  std::vector<char> getSParVector() const;

public:
  PatternPreprocessingController(const PatternPreprocessingController&) = delete; // Copy Constructor Not Implemented
  PatternPreprocessingController(PatternPreprocessingController&&) = delete;      // Move Constructor Not Implemented
  PatternPreprocessingController& operator=(const PatternPreprocessingController&) = delete; // Copy Assignment Not Implemented
  PatternPreprocessingController& operator=(PatternPreprocessingController&&) = delete;      // Move Assignment Not Implemented
};
