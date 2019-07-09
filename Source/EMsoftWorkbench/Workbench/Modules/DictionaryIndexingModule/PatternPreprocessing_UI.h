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

#include "Modules/DictionaryIndexingModule/PatternPreprocessingController.h"

#include "ui_PatternPreprocessing_UI.h"

class ChoosePatternsDatasetDialog;

class PatternPreprocessing_UI : public QWidget
{
  Q_OBJECT

public:
  using InputType = EMsoftWorkbenchConstants::InputType;

  /**
   * @brief PatternPreprocessing_UI
   * @param parent
   */
  PatternPreprocessing_UI(QWidget *parent = nullptr);

  ~PatternPreprocessing_UI() override;

  /**
   * @brief validateData
   */
  bool validateData();

  /**
   * @brief readSession
   * @param obj
   */
  void readSession(const QJsonObject& obj);

  /**
   * @brief writeSession
   * @param obj
   */
  void writeSession(QJsonObject& obj) const;

public slots:
  /**
   * @brief setSelectedADPPatternPixel
   * @param pixel
   */
  void setSelectedADPPatternPixel(const QPoint &pixel);

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

protected slots:
  /**
   * @brief listenPatternPreprocessingStarted
   */
  void listenPatternPreprocessingStarted();

  /**
   * @brief listenPatternPreprocessingFinished
   */
  void listenPatternPreprocessingFinished();

  /**
   * @brief listenInputTypeChanged
   */
  void listenInputTypeChanged(int index);

  /**
   * @brief listenPatternDataFileChanged
   * @param filePath
   */
  void listenPatternDataFileChanged(const QString &filePath);

  /**
   * @brief listenSelectedPatternDatasetChanged
   * @param patternDSetPaths
   */
  void listenSelectedPatternDatasetChanged(QStringList patternDSetPaths);

  /**
   * @brief updateZoomFactor
   * @param zoomFactor
   */
  void updateZoomFactor(float zoomFactor);

signals:
  void errorMessageGenerated(const QString &msg);
  void warningMessageGenerated(const QString &msg);
  void stdOutputMessageGenerated(const QString &msg);

  void patternPreprocessingStarted();
  void patternPreprocessingFinished();

  void selectedHipassValueChanged(float value);
  void selectedHipassNumOfRegionsChanged(int value);

  void parametersChanged();

private:
  QSharedPointer<Ui::PatternPreprocessing_UI> m_Ui;

  PatternPreprocessingController* m_PPMatrixController = nullptr;

  QPoint m_SelectedADPPatternPixel = QPoint(-1, -1);

  InputType m_InputType = InputType::Binary;
  QString m_PatternDataFile;
  QStringList m_SelectedHDF5Path;

  ChoosePatternsDatasetDialog* m_ChoosePatternsDatasetDialog = nullptr;

  QString m_CurrentOpenFile;

  QString m_LastFilePath = "";
  QSharedPointer<QFutureWatcher<void>> m_PPMatrixWatcher;

  /**
   * @brief createValidators
   */
  void createValidators();

  /**
   * @brief createParametersChangedConnections
   */
  void createModificationConnections();

  /**
   * @brief initializeSpinBoxLimits
   */
  void initializeSpinBoxLimits();

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections();

  /**
   * @brief getPPMatrixData
   * @return
   */
  PatternPreprocessingController::PPMatrixData getPPMatrixData();

  /**
   * @brief setInputType
   * @param inputType
   */
  void setInputType(ADPMapController::InputType inputType);

  /**
   * @brief setPatternDataFile
   * @param filePath
   */
  void setPatternDataFile(const QString &filePath);

  /**
   * @brief setSelectedHDF5Path
   * @param path
   */
  void setSelectedHDF5Path(const QStringList &path);

public:
  PatternPreprocessing_UI(const PatternPreprocessing_UI&) = delete; // Copy Constructor Not Implemented
  PatternPreprocessing_UI(PatternPreprocessing_UI&&) = delete;      // Move Constructor Not Implemented
  PatternPreprocessing_UI& operator=(const PatternPreprocessing_UI&) = delete; // Copy Assignment Not Implemented
  PatternPreprocessing_UI& operator=(PatternPreprocessing_UI&&) = delete;      // Move Assignment Not Implemented
};
