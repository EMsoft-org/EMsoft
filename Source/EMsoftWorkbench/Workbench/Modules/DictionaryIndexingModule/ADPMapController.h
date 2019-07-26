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
#include <QtCore/QTemporaryDir>

#include "Common/Constants.h"

class ADPMapController : public QObject
{
  Q_OBJECT

public:
  using InputType = EMsoftWorkbenchConstants::InputType;

  ADPMapController(QObject* parent = nullptr);
  ~ADPMapController() override;

  /**
    * @brief Getter property for Cancel
    * @return Value of Cancel
    */
  bool getCancel() const;

  /**
    * @brief Setter property for Cancel
    */
  void setCancel(const bool& value);

  struct ADPMapData
  {
    QString patternDataFile;
    InputType inputType;
    int patternHeight;
    int patternWidth;
    bool useROI;
    int roi_1;
    int roi_2;
    int roi_3;
    int roi_4;
//    int binningFactor;
//    int binningX;
//    int binningY;
    int ipfHeight;
    int ipfWidth;
//    int maskPattern;
    float maskRadius;
    float hipassFilter;
    int numOfRegions;
    int numOfThreads;
    QStringList hdfStrings;

    std::vector<int32_t> getIParVector() const;

    std::vector<float> getFParVector() const;

    std::vector<char> getSParVector() const;
  };

  /**
   * @brief createADPMap
   * @param data
   */
  void createADPMap(const ADPMapData &data);

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

protected slots:
  void listenADPMapFinished(int exitCode, QProcess::ExitStatus exitStatus);

signals:
  void adpMapCreated(const QImage &adpMap) const;
  void warningMessageGenerated(const QString& msg) const;
  void errorMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;

private:
  QString m_StartTime = "";

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
   * @brief getADPMapExecutablePath
   * @return
   */
  QString getADPMapExecutablePath() const;

  /**
   * @brief writeADPDataToFile
   * @param file
   * @param data
   */
  void writeADPDataToFile(const QString &filePath, const ADPMapController::ADPMapData &data) const;

public:
  ADPMapController(const ADPMapController&) = delete; // Copy Constructor Not Implemented
  ADPMapController(ADPMapController&&) = delete;      // Move Constructor Not Implemented
  ADPMapController& operator=(const ADPMapController&) = delete; // Copy Assignment Not Implemented
  ADPMapController& operator=(ADPMapController&&) = delete;      // Move Assignment Not Implemented
};
