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

#include "Modules/IModuleUI.h"
#include "Modules/DictionaryIndexingModule/AverageDotProductMapController.h"
#include "Modules/DictionaryIndexingModule/PatternPreprocessingParametersController.h"

#include "ui_DictionaryIndexing_UI.h"

class ChoosePatternsDatasetDialog;

class DictionaryIndexing_UI : public IModuleUI
{
  Q_OBJECT

public:
  /**
   * @brief DictionaryIndexing_UI
   * @param parent
   */
  DictionaryIndexing_UI(QWidget *parent = nullptr);

  ~DictionaryIndexing_UI() override;

  /**
   * @brief readModuleSession
   * @param obj
   */
  void readModuleSession(QJsonObject& obj) override;

  /**
   * @brief writeModuleSession
   * @param obj
   */
  void writeModuleSession(QJsonObject& obj) const override;

  /**
   * @brief validateData
   */
  bool validateData() override;

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

  /**
   * @brief changeEvent
   * @param event
   */
  void changeEvent(QEvent* event) override;

protected slots:
  /**
   * @brief listenGenerateADPBtnPressed
   */
  void listenGenerateADPBtnPressed();

  /**
   * @brief listenGeneratePPPBtnPressed
   */
  void listenGeneratePPPBtnPressed();

  /**
   * @brief listenInputTypeChanged
   */
  void listenInputTypeChanged(int index);

  /**
   * @brief listenSelectedPatternDatasetChanged
   * @param patternDSetPaths
   */
  void listenSelectedPatternDatasetChanged(QStringList patternDSetPaths);

  /**
   * @brief listenROICheckboxStateChanged
   * @param state
   */
  void listenROICheckboxStateChanged(int state);

  /**
   * @brief updateZoomFactor
   * @param zoomFactor
   */
  void updateZoomFactor(float zoomFactor);

  /**
   * @brief parametersChanged
   */
  void parametersChanged();

private slots:
  /**
   * @brief generateADPThreadFinished
   */
  void generateADPThreadFinished();

  /**
   * @brief generatePPMatrixThreadFinished
   */
  void generatePPMatrixThreadFinished();

private:
  QSharedPointer<Ui::DictionaryIndexing_UI> m_Ui;

  AverageDotProductMapController* m_ADPController = nullptr;
  PatternPreprocessingParametersController* m_PPMatrixController = nullptr;

  ChoosePatternsDatasetDialog* m_ChoosePatternsDatasetDialog = nullptr;

  QPoint m_SelectedADPPatternCoords = QPoint(-1, -1);
  float m_SelectedHipassValue = -1.0f;
  int m_SelectedNumOfRegions = -1;

  QString m_CurrentOpenFile;

  QString m_LastFilePath = "";
  QSharedPointer<QFutureWatcher<void>> m_ADPWatcher;
  QSharedPointer<QFutureWatcher<void>> m_PPMatrixWatcher;

  /**
   * @brief readInputParameters
   * @param obj
   */
  void readInputParameters(QJsonObject& obj);

  /**
   * @brief readCrystalSystemParameters
   * @param obj
   */
  void readComputationalParameters(QJsonObject& obj);

  /**
   * @brief writeInputParameters
   * @param obj
   */
  void writeInputParameters(QJsonObject& obj) const;

  /**
   * @brief writeCrystalSystemParameters
   * @param obj
   */
  void writeComputationalParameters(QJsonObject& obj) const;

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
   * @brief getData
   * @return
   */
  AverageDotProductMapController::ADPMapData getADPMapData();

  /**
   * @brief getPPMatrixData
   * @return
   */
  PatternPreprocessingParametersController::PPMatrixData getPPMatrixData();

public:
  DictionaryIndexing_UI(const DictionaryIndexing_UI&) = delete; // Copy Constructor Not Implemented
  DictionaryIndexing_UI(DictionaryIndexing_UI&&) = delete;      // Move Constructor Not Implemented
  DictionaryIndexing_UI& operator=(const DictionaryIndexing_UI&) = delete; // Copy Assignment Not Implemented
  DictionaryIndexing_UI& operator=(DictionaryIndexing_UI&&) = delete;      // Move Assignment Not Implemented
};
