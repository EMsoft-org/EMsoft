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

#ifndef _EMsoftWorkbench_H_
#define _EMsoftWorkbench_H_

#include <QtCore/QObject>

#include <QtWidgets/QSplashScreen>
#include <QtWidgets/QMainWindow>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include "EMsoftWorkbench/EMsoftController.h"
#include "EMsoftWorkbench/PatternDisplayWidget.h"
#include "EMsoftWorkbench/AngleWidgets/SingleAngleWidget.h"
#include "EMsoftWorkbench/AngleWidgets/AngleReaderWidget.h"
#include "EMsoftWorkbench/AngleWidgets/SamplingRateWidget.h"
#include "EMsoftWorkbench/AngleWidgets/SampleCubochoricSpaceWidget.h"

#include "ui_EMsoftWorkbench.h"

class QtSSettings;

class EMsoftWorkbench : public QMainWindow, public Ui::EMsoftWorkbench
{
    Q_OBJECT

  public:
    EMsoftWorkbench(QString path, QWidget* parent = 0);
    ~EMsoftWorkbench();

    SIMPL_INSTANCE_PROPERTY(EMsoftController*, Controller)

    using EnumType = unsigned int;

    enum class AngleTypeMode : EnumType
    {
      SingleAngle = 0,
      ReadFile = 1,
      SampleCubochoricSpace = 2,
      SamplingRate = 3
    };

    /**
    * @brief Reads the preferences from the users pref file
    */
    void readSettings();

    /**
     * @brief readWindowSettings
     * @param prefs
     */
    void readWindowSettings(QtSSettings* prefs);

    /**
     * @brief Writes the preferences to the users pref file
     */
    void writeSettings();

    /**
     * @brief writeWindowSettings
     * @param prefs
     */
    void writeWindowSettings(QtSSettings* prefs);

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
     * @brief showStatusMessage
     * @param msg
     */
    void showStatusMessage(const QString &msg);

    /**
     * @brief on_saveLogBtn_pressed
     */
    void on_saveLogBtn_pressed();

    /**
     * @brief generateEBSDPatternImage
     * @param data
     */
    void generateEBSDPatternImage(PatternDisplayWidget::PatternDisplayData data);

    /**
     * @brief on_generateBtn_pressed
     */
    void on_generateBtn_pressed();

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

  signals:
    /**
     * @brief emSoftWindowChangedState
     * @param instance
     */
    void emSoftWindowChangedState(EMsoftWorkbench* instance);

    void patternNeedsGenerated(PatternDisplayWidget::PatternDisplayData patternData, EMsoftController::DetectorData detectorData);

  private slots:
    void beginSplashScreen();
    void showSplashScreenMessage(const QString &msg);
    void endSplashScreen();

  private:
    bool                                                  m_ShowSplash = true;
    QSplashScreen*                                        m_SplashScreen = nullptr;

    PatternDisplayWidget*                                 m_PatternDisplayWidget = nullptr;

    AbstractAngleWidget*                                  m_CurrentAngleWidget = nullptr;
    SingleAngleWidget::Pointer                            m_SingleAngleWidget = SingleAngleWidget::NullPointer();
    AngleReaderWidget::Pointer                            m_AngleReaderWidget = AngleReaderWidget::NullPointer();
    SamplingRateWidget::Pointer                           m_SamplingRateWidget = SamplingRateWidget::NullPointer();
    SampleCubochoricSpaceWidget::Pointer                  m_SampleCubochoricSpaceWidget = SampleCubochoricSpaceWidget::NullPointer();

    EMsoftWorkbench(const EMsoftWorkbench&);    // Copy Constructor Not Implemented
    void operator=(const EMsoftWorkbench&);  // Operator '=' Not Implemented
};

#endif
