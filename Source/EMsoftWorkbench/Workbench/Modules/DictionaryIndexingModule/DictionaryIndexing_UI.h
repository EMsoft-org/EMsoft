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
#include "Modules/DictionaryIndexingModule/ADPMapController.h"
#include "Modules/DictionaryIndexingModule/PatternPreprocessingController.h"

#include "ui_DictionaryIndexing_UI.h"

class DictionaryIndexing_UI : public QWidget
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
   * @brief validateData
   */
  void validateData();

  /**
   * @brief setHipassValue
   * @param hipassValue
   */
  void setHipassValue(float hipassValue);

  /**
   * @brief setHipassNumberOfRegions
   * @param numOfRegions
   */
  void setHipassNumberOfRegions(int numOfRegions);

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

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

protected slots:
  /**
   * @brief listenGenerateDIBtnPressed
   */
  void listenGenerateDIBtnPressed();

  /**
   * @brief parametersChanged
   */
  void parametersChanged();

private slots:
  /**
   * @brief generateDIThreadFinished
   */
  void generateDIThreadFinished();

private:
  QSharedPointer<Ui::DictionaryIndexing_UI> m_Ui;

  PatternPreprocessingController* m_DIController = nullptr;

  float m_HipassValue = -1.0f;
  int m_HipassNumOfRegions = -1;

  QString m_CurrentOpenFile;

  QString m_LastFilePath = "";
  QSharedPointer<QFutureWatcher<void>> m_DIWatcher;

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
  ADPMapController::ADPMapData getDIData();

public:
  DictionaryIndexing_UI(const DictionaryIndexing_UI&) = delete; // Copy Constructor Not Implemented
  DictionaryIndexing_UI(DictionaryIndexing_UI&&) = delete;      // Move Constructor Not Implemented
  DictionaryIndexing_UI& operator=(const DictionaryIndexing_UI&) = delete; // Copy Assignment Not Implemented
  DictionaryIndexing_UI& operator=(DictionaryIndexing_UI&&) = delete;      // Move Assignment Not Implemented
};
