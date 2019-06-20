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

#include "Common/Constants.h"

#include "Modules/DictionaryIndexingModule/ADPMapController.h"

#include "ui_ADPMap_UI.h"

class ADPMap_UI : public QWidget
{
  Q_OBJECT

public:
  using InputType = EMsoftWorkbenchConstants::InputType;

  /**
   * @brief ADPMap_UI
   * @param parent
   */
  ADPMap_UI(QWidget *parent = nullptr);

  ~ADPMap_UI() override;

  /**
   * @brief validateData
   */
  bool validateData();

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

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

protected slots:
  /**
   * @brief listenADPGenerationStarted
   */
  void listenADPGenerationStarted();

  /**
   * @brief listenADPGenerationFinished
   */
  void listenADPGenerationFinished();

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

signals:
  void errorMessageGenerated(const QString &msg);
  void warningMessageGenerated(const QString &msg);
  void stdOutputMessageGenerated(const QString &msg);

  void selectedPatternPixelChanged(const QPoint &patternPixel);

  void adpMapGenerationStarted();
  void adpMapGenerationFinished();

  void parametersChanged();

private:
  QSharedPointer<Ui::ADPMap_UI> m_Ui;

  ADPMapController* m_ADPController = nullptr;

  InputType m_InputType = InputType::Binary;
  QString m_PatternDataFile;
  QStringList m_SelectedHDF5Path;

  QPoint m_SelectedADPPatternCoords = QPoint(-1, -1);

  QString m_CurrentOpenFile;

  QString m_LastFilePath = "";
  QSharedPointer<QFutureWatcher<void>> m_ADPWatcher;

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
  ADPMapController::ADPMapData getADPMapData();

public:
  ADPMap_UI(const ADPMap_UI&) = delete; // Copy Constructor Not Implemented
  ADPMap_UI(ADPMap_UI&&) = delete;      // Move Constructor Not Implemented
  ADPMap_UI& operator=(const ADPMap_UI&) = delete; // Copy Assignment Not Implemented
  ADPMap_UI& operator=(ADPMap_UI&&) = delete;      // Move Assignment Not Implemented
};
