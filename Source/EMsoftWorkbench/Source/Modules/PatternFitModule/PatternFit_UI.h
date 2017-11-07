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

#ifndef _PatternFit_UI_H_
#define _PatternFit_UI_H_

#include <QtCore/QObject>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"
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
    SIMPL_TYPE_MACRO(PatternFit_UI)

    /**
     * @brief PatternFit_UI
     * @param parent
     */
    PatternFit_UI(QWidget* parent = 0);

    ~PatternFit_UI();

    SIMPL_INSTANCE_PROPERTY(PatternControlsWidget::PatternChoice, CurrentPatternChoice)
    SIMPL_INSTANCE_PROPERTY(PatternFitController*, Controller)

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

    void on_fitModeCB_currentIndexChanged(int index);

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
    GLImageViewer::GLImageData     m_ReferencePattern;

    FloatArrayType::Pointer               m_SimulatedPatternData;
    QVector<size_t>                       m_SimulatedPatternDims;

    QuaternionMath<double>::Quaternion    m_NavigationQuatX;
    QuaternionMath<double>::Quaternion    m_NavigationQuatY;
    QuaternionMath<double>::Quaternion    m_NavigationQuatZ;

    bool                                  m_ShowSplash = true;
    QSplashScreen*                        m_SplashScreen = nullptr;

    bool                                  m_HiPassDataInitialized = false;
    bool                                  m_HiPassValuesChanged = false;
    DoubleArrayType::Pointer              m_HiPassData;

    double                                m_Opacity = 0.5;
    QTimer*                               m_FlickerTimer;
    bool                                  m_FlickerIsChecked = false;
    PatternControlsWidget::PatternChoice  m_BeforeFlickerChoice;

    /**
     * @brief setExperimentalPatternFilePath
     * @param filePath
     */
    void setExperimentalPatternFilePath(const QString &filePath);

    /**
     * @brief setMasterFilePath
     * @param filePath
     */
    void setMasterFilePath(const QString &filePath);

    /**
     * @brief readNonRefinableParameters
     * @param obj
     */
    void readNonRefinableParameters(QJsonObject &obj);

    /**
     * @brief readRefinableDetectorParameters
     * @param obj
     */
    void readRefinableDetectorParameters(QJsonObject &obj);

    /**
     * @brief readRefinableSampleParameters
     * @param obj
     */
    void readRefinableSampleParameters(QJsonObject &obj);

    /**
     * @brief readFitParameters
     * @param obj
     */
    void readFitParameters(QJsonObject &obj);

    /**
     * @brief writeNonRefinableParameters
     * @param obj
     */
    void writeNonRefinableParameters(QJsonObject &obj);

    /**
     * @brief writeRefinableDetectorParameters
     * @param obj
     */
    void writeRefinableDetectorParameters(QJsonObject &obj);

    /**
     * @brief writeRefinableSampleParameters
     * @param obj
     */
    void writeRefinableSampleParameters(QJsonObject &obj);

    /**
     * @brief writeFitParameters
     * @param obj
     */
    void writeFitParameters(QJsonObject &obj);

    /**
     * @brief createValidators
     */
    void createValidators();

    /**
     * @brief createParametersChangedConnections
     */
    void createParametersChangedConnections();

    /**
     * @brief createStepConnections
     */
    void createStepConnections();

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
    PatternFitController::SimulationData getSimulationData();

    /**
     * @brief checkFitMode
     */
    void checkFitMode();

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
    bool validateData();

    PatternFit_UI(const PatternFit_UI&);    // Copy Constructor Not Implemented
    void operator=(const PatternFit_UI&);  // Operator '=' Not Implemented
};

#endif
