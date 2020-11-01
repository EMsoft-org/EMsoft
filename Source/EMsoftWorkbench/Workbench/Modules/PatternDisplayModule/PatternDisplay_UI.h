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

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QSplashScreen>


#include "Modules/IModuleUI.h"
#include "Modules/IWorkbenchModule.hpp"
#include "Modules/PatternDisplayModule/AngleWidgets/AngleReaderWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SampleCubochoricSpaceWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SamplingRateWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SingleAngleWidget.h"
#include "Modules/PatternDisplayModule/PatternDisplayController.h"
#include "Modules/PatternDisplayModule/SimulatedPatternDisplayWidget.h"

#include "ui_PatternDisplay_UI.h"

class QtSSettings;

class PatternDisplay_UI : public IModuleUI, public Ui::PatternDisplay_UI
{
  Q_OBJECT

public:
  /**
   * @brief PatternDisplay_UI
   * @param parent
   */
  PatternDisplay_UI(QWidget* parent = nullptr);

  ~PatternDisplay_UI() override = default;

  /**
   * @brief Getter property for Controller
   * @return Value of Controller
   */
  PatternDisplayController* getController() const;

  using EnumType = unsigned int;

  enum class AngleTypeMode : EnumType
  {
    SingleAngle = 0,
    ReadFile = 1,
    SampleCubochoricSpace = 2,
    SamplingRate = 3
  };

  /**
   * @brief readWindowSettings
   * @param prefs
   */
  void readWindowSettings(QtSSettings* prefs);

  /**
   * @brief writeWindowSettings
   * @param prefs
   */
  void writeWindowSettings(QtSSettings* prefs) const;

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
  void validateData() override;

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

  /**
   * @brief masterPathInitializationFinished
   */
  void masterPathInitializationFinished(const QJsonObject& obj);

protected slots:
  /**
   * @brief generateEBSDPatternImage
   * @param data
   */
  void generateEBSDPatternImage(SimulatedPatternDisplayWidget::PatternDisplayData data) const;

  /**
   * @brief on_generateBtn_clicked
   */
  void on_generateBtn_clicked();

  /**
   * @brief on_mpSelectBtn_clicked
   */
  void on_mpSelectBtn_clicked();

  /**
   * @brief on_angleTypeCB_currentIndexChanged
   * @param index
   */
  void on_angleTypeCB_currentIndexChanged(int index);

  /**
   * @brief setMinAndMaxEnergyLevelChoices
   * @param ekeVs
   */
  void setMinAndMaxEnergyLevelChoices(const std::vector<float>& ekeVs) const;

  /**
   * @brief setGenerateButtonAvailability
   * @param value
   */
  void setGenerateButtonAvailability(bool value) const;

  /**
   * @brief parametersChanged
   */
  void parametersChanged();

  /**
   * @brief resetDisplayWidgets
   */
  void resetDisplayWidgets() const;

signals:
  void patternNeedsGenerated(SimulatedPatternDisplayWidget::PatternDisplayData patternData, PatternDisplayController::DetectorData detectorData) const;

private:
  std::unique_ptr<PatternDisplayController> m_Controller = std::make_unique<PatternDisplayController>();
  std::unique_ptr<QThread> m_Thread = std::make_unique<QThread>();

  std::unique_ptr<SimulatedPatternDisplayWidget> m_PatternDisplayWidget;

  AbstractAngleWidget* m_CurrentAngleWidget = nullptr;
  std::unique_ptr<SingleAngleWidget> m_SingleAngleWidget = std::make_unique<SingleAngleWidget>();
  std::unique_ptr<AngleReaderWidget> m_AngleReaderWidget = std::make_unique<AngleReaderWidget>();
  std::unique_ptr<SamplingRateWidget> m_SamplingRateWidget = std::make_unique<SamplingRateWidget>();
  std::unique_ptr<SampleCubochoricSpaceWidget> m_SampleCubochoricSpaceWidget = std::make_unique<SampleCubochoricSpaceWidget>();

  /**
   * @brief createValidators
   */
  void createValidators() const;

  /**
   * @brief createPatternDisplayWidget
   * @return
   */
  std::unique_ptr<SimulatedPatternDisplayWidget> createPatternDisplayWidget();

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections() const;

  /**
   * @brief createModificationConnections
   */
  void createModificationConnections();

  /**
   * @brief readDetectorAndMicroscopeParameters
   * @param obj
   */
  void readDetectorAndMicroscopeParameters(QJsonObject& obj);

  /**
   * @brief writeDetectorAndMicroscopeParameters
   * @param obj
   */
  void writeDetectorAndMicroscopeParameters(QJsonObject& obj) const;

  /**
   * @brief getDetectorData
   * @return
   */
  PatternDisplayController::DetectorData getDetectorData() const;

public:
  PatternDisplay_UI(const PatternDisplay_UI&) = delete;            // Copy Constructor Not Implemented
  PatternDisplay_UI(PatternDisplay_UI&&) = delete;                 // Move Constructor Not Implemented
  PatternDisplay_UI& operator=(const PatternDisplay_UI&) = delete; // Copy Assignment Not Implemented
  PatternDisplay_UI& operator=(PatternDisplay_UI&&) = delete;      // Move Assignment Not Implemented
};
