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

#include <array>

#include <QtCore/QTemporaryDir>

#include "Modules/IProcessController.h"

#include "Common/Constants.h"

class DictionaryIndexingController : public IProcessController
{
  Q_OBJECT

public:
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
    int roi_x;
    int roi_y;
    int roi_w;
    int roi_h;
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
   * @brief setData
   * @param simData
   */
  void setData(const InputDataType& simData);

  /**
   * @brief reportError
   * @param errorCode
   */
  void reportError(int errorCode);

  /**
   * @brief setUpdateProgress
   * @param loopCompleted
   * @param totalLoops
   * @param timeRemaining
   */
  void setUpdateProgress(int loopCompleted, int totalLoops, float timeRemaining);

  /**
   * @brief updateOutput
   * @param nDict
   * @param eulerArray
   * @param dpArray
   * @param indexArray
   */
  void updateOutput(int nDict, float* eulerArray, float* dpArray, int32_t* indexArray);

public slots:
  /**
   * @brief executeWrapper
   */
  void executeWrapper();

signals:
  void diCreated(const QImage& dIndex) const;

private:
  InputDataType m_InputData;
  size_t m_InstanceKey = 0;

  int m_SpaceGroupNumber = 0;

  QTemporaryDir m_TempDir;

  /**
   * @brief initializeData
   */
  void initializeData();

  /**
   * @brief DictionaryIndexingController::generateNMLFile
   * @param path
   */
  void generateNMLFile(const QString& path) override;

  /**
   * @brief readSpaceGroupNumber
   */
  int readSpaceGroupNumber(const QString& masterFile);

  /**
   * @brief processFinished
   */
  void processFinished() override;

  /**
   * @brief getRegionOfInterest
   * @return
   */
  QSize getRegionOfInterest(InputDataType inputData) const;

public:
  DictionaryIndexingController(const DictionaryIndexingController&) = delete; // Copy Constructor Not Implemented
  DictionaryIndexingController(DictionaryIndexingController&&) = delete;      // Move Constructor Not Implemented
  DictionaryIndexingController& operator=(const DictionaryIndexingController&) = delete; // Copy Assignment Not Implemented
  DictionaryIndexingController& operator=(DictionaryIndexingController&&) = delete;      // Move Assignment Not Implemented
};
