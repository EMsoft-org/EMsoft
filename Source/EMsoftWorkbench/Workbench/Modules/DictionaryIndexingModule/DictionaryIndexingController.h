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

#include "Common/Constants.h"

class DictionaryIndexingController : public QObject
{
  Q_OBJECT

public:
  const QString k_ExeName = QString("EMEBSDDI");
  const QString k_NMLName = QString("EMEBSDDI.nml");

  using InputType = EMsoftWorkbenchConstants::InputType;

  using EnumType = unsigned int;

  enum class IndexingMode : EnumType
  {
//    Static = 0,
    Dynamic = 0
  };

  enum class EnergyAveragingMethod : EnumType
  {
    Exact = 0,
    Approximate
  };

  enum class IntensityScalingMode : EnumType
  {
    NoScaling = 0,
    Linear,
    GammaCorrection
  };

  DictionaryIndexingController(QObject* parent = nullptr);
  ~DictionaryIndexingController() override;

  using InputDataType = struct
  {
    IndexingMode indexingMode;
    InputType inputType;
    QString patternDataFile;
    QStringList hdfStrings;
    QString eulerAngleFile;
    QString dictFile;
    QString masterFile;
    int ipfHeight;
    int ipfWidth;
    bool useROI;
    int roi_1;
    int roi_2;
    int roi_3;
    int roi_4;
    float samplingStepSizeX;
    float samplingStepSizeY;
    int nnk;
    int nnav;
    int nism;
    int nosm;
    float isangle;
    QString maskfile;
    bool useMask;
    int maskRadius;
    float hipassValue;
    int numOfRegions;
    int nCubochoric;
    float L;
    float thetac;
    float delta;
    int numsx;
    int numsy;
    float xpc;
    float ypc;
    float omega;
    float energymin;
    float energymax;
    EnergyAveragingMethod averagingMethod;
    bool useSpatialAveraging;
    float beamCurrent;
    float dwellTime;
    int binning;
    IntensityScalingMode scalingMode;
    float gammaCorrectionFactor;
    QString exptFile;
    QString outputDataFilePath;
    QString outputCtfFilePath;
    QString outputAngFilePath;
    QString outputAvgCtfFilePath;
    int numDictSingle;
    int numExptSingle;
    int numOfThreads;
    int platId;
    int devId;
  };

  /**
    * @brief Getter property for Cancel
    * @return Value of Cancel
    */
  bool getCancel() const;

  /**
    * @brief Setter property for Cancel
    */
  void setCancel(const bool& value);

  /**
   * @brief setData
   * @param simData
   */
  void setData(const InputDataType& simData);

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
  void diCreated(const QImage &adpMap) const;
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
   * @brief getDIExecutablePath
   * @return
   */
  QString getDIExecutablePath() const;

  /**
   * @brief DictionaryIndexingController::generateNMLFile
   * @param path
   */
  void generateNMLFile(const QString& path);

  /**
   * @brief executeWrapper
   */
  void executeWrapper();

public:
  DictionaryIndexingController(const DictionaryIndexingController&) = delete; // Copy Constructor Not Implemented
  DictionaryIndexingController(DictionaryIndexingController&&) = delete;      // Move Constructor Not Implemented
  DictionaryIndexingController& operator=(const DictionaryIndexingController&) = delete; // Copy Assignment Not Implemented
  DictionaryIndexingController& operator=(DictionaryIndexingController&&) = delete;      // Move Assignment Not Implemented
};
