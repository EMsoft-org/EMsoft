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

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include <QtCore/QFutureWatcher>

#include "Modules/IModuleUI.h"
#include "Modules/AverageDotProductMapModule/AverageDotProductMapController.h"

#include "ui_AverageDotProductMap_UI.h"

class AverageDotProductMap_UI : public IModuleUI
{
  Q_OBJECT

public:
  SIMPL_TYPE_MACRO(AverageDotProductMap_UI)

  /**
   * @brief AverageDotProductMap_UI
   * @param parent
   */
  AverageDotProductMap_UI(QWidget* parent = nullptr);

  ~AverageDotProductMap_UI() override;

  SIMPL_INSTANCE_PROPERTY(AverageDotProductMapController*, Controller)

  /**
   * @brief readModuleSession
   * @param obj
   */
  void readModuleSession(QJsonObject& obj) override;

  /**
   * @brief writeModuleSession
   * @param obj
   */
  void writeModuleSession(QJsonObject& obj) override;

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
  void listenGenerateBtnPressed();

  /**
   * @brief listenPatternSelectBtnClicked
   */
  void listenPatternSelectBtnClicked();

  /**
   * @brief listenOutputFileSelectBtnClicked
   */
  void listenOutputFileSelectBtnClicked();

  void parametersChanged();

private slots:
  void threadFinished();

private:
  QSharedPointer<Ui::AverageDotProductMap_UI> m_Ui;

  QString m_LastFilePath = "";
  QSharedPointer<QFutureWatcher<void>> m_Watcher;

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
   * @brief readOutputParameters
   * @param obj
   */
  void readOutputParameters(QJsonObject& obj);

  /**
   * @brief writeInputParameters
   * @param obj
   */
  void writeInputParameters(QJsonObject& obj);

  /**
   * @brief writeCrystalSystemParameters
   * @param obj
   */
  void writeComputationalParameters(QJsonObject& obj);

  /**
   * @brief writeOutputParameters
   * @param obj
   */
  void writeOutputParameters(QJsonObject& obj);

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
  AverageDotProductMapController::ADPMapData getData();

  AverageDotProductMap_UI(const AverageDotProductMap_UI&); // Copy Constructor Not Implemented
  void operator=(const AverageDotProductMap_UI&);          // Operator '=' Not Implemented
};
