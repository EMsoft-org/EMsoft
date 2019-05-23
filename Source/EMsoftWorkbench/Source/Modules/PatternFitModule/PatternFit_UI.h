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

#include "SIMPLib/Math/QuaternionMath.hpp"

#include "Common/IObserver.h"

#include "Modules/IModuleUI.h"
#include "Modules/PatternFitModule/PatternFitController.h"

#include "ui_PatternFit_UI.h"

class QtSSettings;
class QSplashScreen;

class PatternFit_UI : public IModuleUI, public Ui::PatternFit_UI, public IObserver
{
  Q_OBJECT

public:
  /**
   * @brief PatternFit_UI
   * @param parent
   */
  PatternFit_UI(QWidget* parent = nullptr);

  ~PatternFit_UI() override;

    /**
    * @brief Setter property for CurrentPatternChoice
    */
    void setCurrentPatternChoice(const PatternControlsWidget::PatternChoice& value); 

    /**
    * @brief Getter property for CurrentPatternChoice
    * @return Value of CurrentPatternChoice
    */
    PatternControlsWidget::PatternChoice getCurrentPatternChoice() const;

    /**
    * @brief Setter property for Controller
    */
    void setController(PatternFitController* value); 

    /**
    * @brief Getter property for Controller
    * @return Value of Controller
    */
    PatternFitController* getController() const;

  using EnumType = unsigned int;

  enum class FitMode : EnumType
  {
    Free,
    Detector,
    Orientation
  };

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

private slots:
  /**
   * @brief on_mpSelectBtn_clicked
   */
  void on_mpSelectBtn_clicked();

  /**
   * @brief on_expPatternSelectBtn_clicked
   */
  void on_expPatternSelectBtn_clicked();

  void on_detectorTiltAngle_valueChanged(double value);

  void on_circularMask_toggled(bool checked);
  void on_inverseGaussian_toggled(bool checked);
  void on_hipassFilter_toggled(bool checked);
  void on_linearRampSubtraction_toggled(bool checked);

  void on_fitModeCB_currentIndexChanged(int index) const;

  /**
   * @brief slot_patternChoiceChanged
   * @param index
   */
  void slot_patternChoiceChanged(int index);

  /**
   * @brief slot_controlsChoicePressed
   * @param choice
   */
  void slot_controlsChoicePressed(PatternControlsWidget::ControlsChoice choice);

  /**
   * @brief updateRotationQuaternions
   * @param rot
   * @param detValue
   */
  void updateRotationQuaternions(double rot, double detValue);

  /**
   * @brief parametersChanged
   */
  void parametersChanged();

private:
    PatternControlsWidget::PatternChoice m_CurrentPatternChoice;
    PatternFitController* m_Controller;

  GLImageViewer::GLImageData m_ReferencePattern;

  std::vector<float> m_SimulatedPatternData;
  std::vector<size_t> m_SimulatedPatternDims;

  QuaternionMath<double>::Quaternion m_NavigationQuatX;
  QuaternionMath<double>::Quaternion m_NavigationQuatY;
  QuaternionMath<double>::Quaternion m_NavigationQuatZ;

  bool m_ShowSplash = true;
  QSplashScreen* m_SplashScreen = nullptr;

  bool m_HiPassDataInitialized = false;
  bool m_HiPassValuesChanged = false;
  std::vector<double> m_HiPassData;

  double m_Opacity = 0.5;
  QTimer* m_FlickerTimer;
  bool m_FlickerIsChecked = false;
  PatternControlsWidget::PatternChoice m_BeforeFlickerChoice;

  /**
   * @brief setExperimentalPatternFilePath
   * @param filePath
   */
  void setExperimentalPatternFilePath(const QString& filePath);

  /**
   * @brief setMasterFilePath
   * @param filePath
   */
  void setMasterFilePath(const QString& filePath);

  /**
   * @brief readNonRefinableParameters
   * @param obj
   */
  void readNonRefinableParameters(QJsonObject& obj);

  /**
   * @brief readRefinableDetectorParameters
   * @param obj
   */
  void readRefinableDetectorParameters(QJsonObject& obj);

  /**
   * @brief readRefinableSampleParameters
   * @param obj
   */
  void readRefinableSampleParameters(QJsonObject& obj);

  /**
   * @brief readFitParameters
   * @param obj
   */
  void readFitParameters(QJsonObject& obj);

  /**
   * @brief writeNonRefinableParameters
   * @param obj
   */
  void writeNonRefinableParameters(QJsonObject& obj) const;

  /**
   * @brief writeRefinableDetectorParameters
   * @param obj
   */
  void writeRefinableDetectorParameters(QJsonObject& obj) const;

  /**
   * @brief writeRefinableSampleParameters
   * @param obj
   */
  void writeRefinableSampleParameters(QJsonObject& obj) const;

  /**
   * @brief writeFitParameters
   * @param obj
   */
  void writeFitParameters(QJsonObject& obj) const;

  /**
   * @brief createValidators
   */
  void createValidators() const;

  /**
   * @brief createParametersChangedConnections
   */
  void createParametersChangedConnections();

  /**
   * @brief createStepConnections
   */
  void createStepConnections() const;

  /**
   * @brief createWidgetConnections
   */
  void createWidgetConnections();

  /**
   * @brief generateNewImage
   */
  void displayImage();

  /**
   * @brief generateSimulatedPatternImage
   */
  void generateSimulatedPatternImage();

  /**
   * @brief getSimulationData
   * @return
   */
  PatternFitController::SimulationData getSimulationData() const;

  /**
   * @brief checkFitMode
   */
  void checkFitMode() const;

  /**
   * @brief initializeHipassFilterValues
   */
  void initializeHipassFilterValues();

  /**
   * @brief destroyHipassFilterValues
   */
  void destroyHipassFilterValues();

  /**
   * @brief validateData
   */
  bool validateData() override;

  PatternFit_UI(const PatternFit_UI&);  // Copy Constructor Not Implemented
  void operator=(const PatternFit_UI&); // Operator '=' Not Implemented
};
