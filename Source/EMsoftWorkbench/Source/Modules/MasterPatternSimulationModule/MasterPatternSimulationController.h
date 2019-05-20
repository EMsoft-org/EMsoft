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
#include "SIMPLib/DataArrays/DataArray.hpp"

class MonteCarloFileReader;
class EMsoftFileWriter;

class MasterPatternSimulationController : public QObject
{
  Q_OBJECT

public:
  MasterPatternSimulationController(QObject* parent = nullptr);
  ~MasterPatternSimulationController() override;

  using EnumType = unsigned int;

  enum class MonteCarloMode : EnumType
  {
    EBSD,
    ECP
  };

  SIMPL_INSTANCE_PROPERTY(bool, Cancel)

  struct MasterPatternSimulationData
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
   * @brief createMonteCarlo
   * @param data
   */
  void createMasterPattern(MasterPatternSimulationController::MasterPatternSimulationData data);

  /**
   * @brief validateMonteCarloValues
   * @param data
   * @return
   */
  bool validateMasterPatternValues(MasterPatternSimulationController::MasterPatternSimulationData data);

  /**
   * @brief setUpdateProgress
   * @param loopCompleted
   * @param totalLoops
   * @param bseYield
   */
  void setUpdateProgress(int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops);

  /**
   * @brief getNumCPUCores
   * @return
   */
  int getNumCPUCores();

#if 0
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

#endif

signals:
  void warningMessageGenerated(const QString& msg);
  void errorMessageGenerated(const QString& msg);
  void stdOutputMessageGenerated(const QString& msg);

private:
  MonteCarloFileReader* m_MonteCarloReader = nullptr;

  QString m_StartTime = "";

  Int32ArrayType::Pointer m_GenericAccumzPtr;
  FloatArrayType::Pointer m_GenericLPNHPtr;
  FloatArrayType::Pointer m_GenericLPSHPtr;

  std::vector<float> m_Atompos;
  std::vector<int32_t> m_Atomtypes;
  std::vector<float> m_Latparm;
  QString m_CreationDate = "";
  QString m_CreationTime = "";
  QString m_Creator = "";
  QString m_ProgramName = "";

  int m_CrystalSystem = -1;
  int m_Natomtypes = -1;
  int m_SpaceGroupNumber = -1;
  int m_SpaceGroupSetting = -1;

  size_t m_InstanceKey = 0;
  bool m_Executing = false;

  /**
   * @brief initializeData
   */
  void initializeData();

  /**
   * @brief writeEMsoftHDFFile
   * @param simData
   * @return
   */
  bool writeEMsoftHDFFile(MasterPatternSimulationController::MasterPatternSimulationData simData);

#if 0
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
#endif
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
  Int32ArrayType::Pointer getIParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData);

  /**
   * @brief getFParPtr
   * @return
   */
  FloatArrayType::Pointer getFParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData);

public:
  MasterPatternSimulationController(const MasterPatternSimulationController&) = delete;            // Copy Constructor Not Implemented
  MasterPatternSimulationController(MasterPatternSimulationController&&) = delete;                 // Move Constructor Not Implemented
  MasterPatternSimulationController& operator=(const MasterPatternSimulationController&) = delete; // Copy Assignment Not Implemented
  MasterPatternSimulationController& operator=(MasterPatternSimulationController&&) = delete;      // Move Assignment Not Implemented
};
