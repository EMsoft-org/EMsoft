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

#ifndef _patterndisplay_ui_H_
#define _patterndisplay_ui_H_

#include <QtCore/QObject>

#include <QtWidgets/QSplashScreen>
#include <QtWidgets/QMainWindow>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include "Modules/IWorkbenchModule.h"
#include "Modules/IModuleUI.h"
#include "Modules/PatternDisplayModule/PatternDisplayController.h"
#include "Modules/PatternDisplayModule/SimulatedPatternDisplayWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SingleAngleWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/AngleReaderWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SamplingRateWidget.h"
#include "Modules/PatternDisplayModule/AngleWidgets/SampleCubochoricSpaceWidget.h"

#include "ui_PatternDisplay_UI.h"

class QtSSettings;

class PatternDisplay_UI : public IModuleUI, public Ui::PatternDisplay_UI
{
    Q_OBJECT

  public:
    SIMPL_TYPE_MACRO(PatternDisplay_UI)

    /**
     * @brief PatternDisplay_UI
     * @param parent
     */
    PatternDisplay_UI(QWidget* parent = 0);

    ~PatternDisplay_UI();

    SIMPL_INSTANCE_PROPERTY(PatternDisplayController*, Controller)

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
    void writeWindowSettings(QtSSettings* prefs);

    /**
     * @brief readModuleSession
     * @param obj
     */
    void readModuleSession(QJsonObject &obj);

    /**
     * @brief writeModuleSession
     * @param obj
     */
    void writeModuleSession(QJsonObject &obj);

  protected:
    /**
     * @brief setupGui
     */
    void setupGui();

    /**
     * @brief changeEvent
     * @param event
     */
    void changeEvent(QEvent* event);

  protected slots:

    /**
     * @brief generateEBSDPatternImage
     * @param data
     */
    void generateEBSDPatternImage(SimulatedPatternDisplayWidget::PatternDisplayData data);

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
    void setMinAndMaxEnergyLevelChoices(FloatArrayType::Pointer ekeVs);

    /**
     * @brief setGenerateButtonAvailability
     * @param value
     */
    void setGenerateButtonAvailability(bool value);

    /**
     * @brief parametersChanged
     */
    void parametersChanged();

    /**
     * @brief resetDisplayWidgets
     */
    void resetDisplayWidgets();

  signals:
    void patternNeedsGenerated(SimulatedPatternDisplayWidget::PatternDisplayData patternData, PatternDisplayController::DetectorData detectorData);

  private:
    SimulatedPatternDisplayWidget*                        m_PatternDisplayWidget = nullptr;

    AbstractAngleWidget*                                  m_CurrentAngleWidget = nullptr;
    SingleAngleWidget::Pointer                            m_SingleAngleWidget = SingleAngleWidget::NullPointer();
    AngleReaderWidget::Pointer                            m_AngleReaderWidget = AngleReaderWidget::NullPointer();
    SamplingRateWidget::Pointer                           m_SamplingRateWidget = SamplingRateWidget::NullPointer();
    SampleCubochoricSpaceWidget::Pointer                  m_SampleCubochoricSpaceWidget = SampleCubochoricSpaceWidget::NullPointer();

    /**
     * @brief createValidators
     */
    void createValidators();

    /**
     * @brief createWidgetConnections
     */
    void createWidgetConnections();

    /**
     * @brief createModificationConnections
     */
    void createModificationConnections();

    /**
     * @brief readDetectorAndMicroscopeParameters
     * @param obj
     */
    void readDetectorAndMicroscopeParameters(QJsonObject &obj);

    /**
     * @brief writeDetectorAndMicroscopeParameters
     * @param obj
     */
    void writeDetectorAndMicroscopeParameters(QJsonObject &obj);

    /**
     * @brief getDetectorData
     * @return
     */
    PatternDisplayController::DetectorData getDetectorData();

    /**
     * @brief validateData
     */
    bool validateData();

    PatternDisplay_UI(const PatternDisplay_UI&);    // Copy Constructor Not Implemented
    void operator=(const PatternDisplay_UI&);  // Operator '=' Not Implemented
};

#endif
