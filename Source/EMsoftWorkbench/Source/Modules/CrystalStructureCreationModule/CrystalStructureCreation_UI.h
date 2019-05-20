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

#include "Modules/CrystalStructureCreationModule/CrystalStructureCreationController.h"
#include "Modules/IModuleUI.h"

#include "ui_CrystalStructureCreation_UI.h"

class QtSSettings;
class QSplashScreen;

class CrystalStructureCreation_UI : public IModuleUI, public Ui::CrystalStructureCreation_UI
{
  Q_OBJECT

public:
  SIMPL_TYPE_MACRO(CrystalStructureCreation_UI)

  /**
   * @brief CrystalStructureCreation_UI
   * @param parent
   */
  CrystalStructureCreation_UI(QWidget* parent = nullptr);

  ~CrystalStructureCreation_UI() override;

  SIMPL_INSTANCE_PROPERTY(CrystalStructureCreationController*, Controller)

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
  void on_csCB_currentIndexChanged(int index);

  void parametersChanged();

private:
  QString m_LastFilePath = "";

  /**
   * @brief readCrystalSystemParameters
   * @param obj
   */
  void readCrystalSystemParameters(QJsonObject& obj);

  /**
   * @brief readSpaceGroupParameters
   * @param obj
   */
  void readSpaceGroupParameters(QJsonObject& obj);

  /**
   * @brief writeCrystalSystemParameters
   * @param obj
   */
  void writeCrystalSystemParameters(QJsonObject& obj);

  /**
   * @brief writeSpaceGroupParameters
   * @param obj
   */
  void writeSpaceGroupParameters(QJsonObject& obj);

  /**
   * @brief createValidators
   */
  void createValidators();

  /**
   * @brief createModificationConnections
   */
  void createModificationConnections();

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections();

  /**
   * @brief getCreationData
   * @return
   */
  CrystalStructureCreationController::CrystalStructureCreationData getCreationData();

public:
  CrystalStructureCreation_UI(const CrystalStructureCreation_UI&) = delete;            // Copy Constructor Not Implemented
  CrystalStructureCreation_UI(CrystalStructureCreation_UI&&) = delete;                 // Move Constructor Not Implemented
  CrystalStructureCreation_UI& operator=(const CrystalStructureCreation_UI&) = delete; // Copy Assignment Not Implemented
  CrystalStructureCreation_UI& operator=(CrystalStructureCreation_UI&&) = delete;      // Move Assignment Not Implemented
};
