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

#include <QtCore/QFutureWatcher>
#include <QtCore/QObject>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

class XtalFileReader;

class MonteCarloSimulationController : public QObject
{
  Q_OBJECT

public:
  MonteCarloSimulationController(QObject* parent = nullptr);
  ~MonteCarloSimulationController() override;

  using EnumType = unsigned int;

  enum class MonteCarloMode : EnumType
  {
    EBSD,
    ECP
  };

  SIMPL_INSTANCE_PROPERTY(bool, Cancel)

  struct MonteCarloSimulationData
  {
    double sampleTiltAngleSig;
    double sampleRotAngleOmega;
    double sampleStartTiltAngle;
    double sampleEndTiltAngle;
    double sampleTiltStepSize;
    double acceleratingVoltage;
    double minEnergyConsider;
    double energyBinSize;
    double maxDepthConsider;
    double depthStepSize;
    int mcMode;
    int numOfPixelsN;
    int numOfEPerWorkitem;
    int totalNumOfEConsidered;
    int multiplierForTotalNumOfE;
    int gpuPlatformID;
    int gpuDeviceID;
    int globalWorkGroupSize;
    QString inputFilePath;
    QString outputFilePath;
  };

  enum class StringType : EnumType
  {
    OpenCLFolder = 22,
    RandomSeedsFile = 25
  };

  /**
   * @brief createMonteCarlo
   * @param simData
   */
  void createMonteCarlo(MonteCarloSimulationController::MonteCarloSimulationData simData);

  /**
   * @brief validateMonteCarloValues
   * @param data
   * @return
   */
  bool validateMonteCarloValues(MonteCarloSimulationController::MonteCarloSimulationData data);

  /**
   * @brief setUpdateProgress
   * @param loopCompleted
   * @param totalLoops
   * @param bseYield
   */
  void setUpdateProgress(int loopCompleted, int totalLoops, float bseYield);

  /**
   * @brief getPlatformInfo
   * @return
   */
  QStringList getPlatformInfo();

  /**
   * @brief getDeviceInfo
   * @param platformID
   * @return
   */
  QStringList getDeviceInfo(int platformID);

signals:
  void warningMessageGenerated(const QString& msg);
  void errorMessageGenerated(const QString& msg);
  void stdOutputMessageGenerated(const QString& msg);
  void updateMCProgress(int loop, int totalLoops, float bseYield);

private:
  XtalFileReader* m_XtalReader = nullptr;
  QString m_StartTime = "";

  std::vector<int32_t> m_GenericAccumePtr;
  std::vector<int32_t> m_GenericAccumzPtr;
  std::vector<uint8_t> m_GenericXtalPtr;
  std::vector<uint8_t> m_GenericMCPtr;

  size_t m_InstanceKey;

  bool m_HasErrors = false;

  char* m_SPar;

  /**
   * @brief initializeData
   * @param data
   */
  void initializeData(MonteCarloSimulationData data);

  /**
   * @brief writeEMsoftHDFFile
   * @param simData
   * @return
   */
  bool writeEMsoftHDFFile(MonteCarloSimulationController::MonteCarloSimulationData simData);

  /**
   * @brief getnumCLPlatforms
   * @return
   */
  int getnumCLPlatforms();

  /**
   * @brief getPlatformInfo
   */
  void writePlatformInfo();

  /**
   * @brief getnumCLDevices
   * @param platformID
   * @return
   */
  int getnumCLDevices(int platformID);

  /**
   * @brief getDeviceInfo
   * @param platformID
   */
  void writeDeviceInfo(int platformID);

  /**
   * @brief getEMsoftUserName
   * @return
   */
  QString getEMsoftUserName();

  /**
   * @brief getEMsoftUserEmail
   * @return
   */
  QString getEMsoftUserEmail();

  /**
   * @brief getEMsoftUserLocation
   * @return
   */
  QString getEMsoftUserLocation();

  /**
   * @brief getIParPtr
   * @return
   */
  std::vector<int32_t> getIParPtr(MonteCarloSimulationController::MonteCarloSimulationData simData);

  /**
   * @brief getFParPtr
   * @return
   */
  std::vector<float> getFParPtr(MonteCarloSimulationController::MonteCarloSimulationData simData);

  /**
   * @brief setSParValue
   * @param type
   * @param value
   * @return
   */
  bool setSParValue(StringType type, const QString& value);

  /**
   * @brief convertToFortran
   * @param fstring
   * @param fstring_len
   * @param cstring
   */
  void convertToFortran(char* fstring, size_t fstring_len, const char* cstring);

public:
  MonteCarloSimulationController(const MonteCarloSimulationController&) = delete;            // Copy Constructor Not Implemented
  MonteCarloSimulationController(MonteCarloSimulationController&&) = delete;                 // Move Constructor Not Implemented
  MonteCarloSimulationController& operator=(const MonteCarloSimulationController&) = delete; // Copy Assignment Not Implemented
  MonteCarloSimulationController& operator=(MonteCarloSimulationController&&) = delete;      // Move Assignment Not Implemented
};
