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

#include "Common/Constants.h"

#include "Modules/DictionaryIndexingModule/DictionaryIndexingController.h"

#include "ui_DictionaryIndexing_UI.h"

class DictionaryIndexing_UI : public QWidget
{
  Q_OBJECT

public:
  using InputType = EMsoftWorkbenchConstants::InputType;

  /**
   * @brief DictionaryIndexing_UI
   * @param parent
   */
  DictionaryIndexing_UI(QWidget *parent = nullptr);

  ~DictionaryIndexing_UI() override;

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
   * @brief listenInputTypeChanged
   */
  void listenInputTypeChanged(EMsoftWorkbenchConstants::InputType inputType);

  /**
   * @brief listenPatternDataFileChanged
   * @param filePath
   */
  void listenPatternDataFileChanged(const QString& filePath);

  /**
   * @brief listenSelectedPatternDatasetChanged
   * @param patternDSetPaths
   */
  void listenSelectedPatternDatasetChanged(QStringList patternDSetPaths);

  /**
   * @brief setSelectedHipassValue
   * @param value
   */
  void setSelectedHipassValue(float value);

  /**
   * @brief setSelectedNumberOfRegions
   * @param value
   */
  void setSelectedNumberOfRegions(int value);

  /**
   * @brief setADPMap
   * @param adpMap
   */
  void setADPMap(const QImage& adpMap);

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

protected slots:
  /**
   * @brief listenDIGenerationStarted
   */
  void listenDIGenerationStarted();

  /**
   * @brief selectFilePath
   * @param caption
   * @param filter
   * @return
   */
  QString selectOpenFilePath(const QString &caption, const QString &filter);

  /**
   * @brief selectSaveFilePath
   * @param caption
   * @param filter
   * @return
   */
  QString selectSaveFilePath(const QString &caption, const QString &filter);

  /**
   * @brief listenADPGenerationFinished
   */
  void processFinished();

  /**
   * @brief listenROICheckboxStateChanged
   * @param state
   */
  void listenROICheckboxStateChanged(int state);

  /**
   * @brief listenIndexingModeChanged
   * @param index
   */
  void listenIndexingModeChanged(int index);

  /**
   * @brief updateZoomFactor
   * @param zoomFactor
   */
  void updateZoomFactor(float zoomFactor);

signals:
  void errorMessageGenerated(const QString &msg);
  void warningMessageGenerated(const QString &msg);
  void stdOutputMessageGenerated(const QString &msg);

  void selectedADPCoordinateChanged(const QPoint &patternPixel);

  void diGenerationStarted();
  void diGenerationFinished();

  void parametersChanged();

private:
  QSharedPointer<Ui::DictionaryIndexing_UI> m_Ui;

  DictionaryIndexingController* m_DIController = nullptr;
  QSharedPointer<QThread> m_WorkerThread;

  InputType m_InputType = InputType::Binary;
  QString m_PatternDataFile;
  QStringList m_SelectedHDF5Path;
  QImage m_ADPMap;

  float m_SelectedHipassValue = -1.0f;
  int m_SelectedNumOfRegions = -1;

  QString m_OpenDialogLastDirectory = "";

  /**
   * @brief createValidators
   */
  void createValidators();

  /**
   * @brief createParametersChangedConnections
   */
  void createModificationConnections();

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections();

  /**
   * @brief getData
   * @return
   */
  DictionaryIndexingController::InputDataType getDIData();

  /**
   * @brief setInputType
   * @param inputType
   */
  void setInputType(InputType inputType);

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

  /**
   * @brief setStaticIndexingMode
   */
  void setStaticIndexingMode();

  /**
   * @brief setDynamicIndexingMode
   */
  void setDynamicIndexingMode();

public:
  DictionaryIndexing_UI(const DictionaryIndexing_UI&) = delete; // Copy Constructor Not Implemented
  DictionaryIndexing_UI(DictionaryIndexing_UI&&) = delete;      // Move Constructor Not Implemented
  DictionaryIndexing_UI& operator=(const DictionaryIndexing_UI&) = delete; // Copy Assignment Not Implemented
  DictionaryIndexing_UI& operator=(DictionaryIndexing_UI&&) = delete;      // Move Assignment Not Implemented
};
