/* ============================================================================
* Copyright (c) 2009-2016 BlueQuartz Software, LLC
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

#include "Modules/PatternDisplayModule/AngleWidgets/AbstractAngleWidget.h"

#include "OrientationLib/OrientationMath/OrientationArray.hpp"

#include "ui_SampleCubochoricSpaceWidget.h"

class SampleCubochoricSpaceWidget : public AbstractAngleWidget, public Ui::SampleCubochoricSpaceWidget
{
    Q_OBJECT

public:
    SIMPL_SHARED_POINTERS(SampleCubochoricSpaceWidget)
    SIMPL_STATIC_NEW_MACRO(SampleCubochoricSpaceWidget)

    SampleCubochoricSpaceWidget(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
    ~SampleCubochoricSpaceWidget() override;

    /**
     * @brief setupGui
     */
    void setupGui();

    /**
     * @brief getEulerAngles
     * @return
     */
    virtual FloatArrayType::Pointer getEulerAngles() override;

    /**
     * @brief hasValidAngles
     * @return
     */
    virtual bool hasValidAngles() override;

    /**
     * @brief readSession
     */
    virtual void readSession(QJsonObject &obj) override;

    /**
     * @brief writeSession
     */
    virtual void writeSession(QJsonObject &obj) override;

    /**
     * @brief createModificationConnections
     * @param ui
     */
    virtual void createModificationConnections(PatternDisplay_UI* ui) override;

  protected slots:
    void on_samplingModeCB_currentIndexChanged(int index);
    void void_on_offsetSamplingGridChkBox_stateChanged(int state);

    void lineEditChanged(const QString &text);

private:

    void valuesChanged();

    void RodriguesComposition(const DOrientArrayType &sigma, DOrientArrayType &rod);

    bool IsinsideFZ(double* rod, int FZtype, int FZorder);
    bool insideCyclicFZ(const double* rod, int order);
    bool insideDihedralFZ(const double* rod, int order);
    bool insideCubicFZ(const double* rod, int ot);

  public:
    SampleCubochoricSpaceWidget(const SampleCubochoricSpaceWidget&) = delete; // Copy Constructor Not Implemented
    SampleCubochoricSpaceWidget(SampleCubochoricSpaceWidget&&) = delete;      // Move Constructor Not Implemented
    SampleCubochoricSpaceWidget& operator=(const SampleCubochoricSpaceWidget&) = delete; // Copy Assignment Not Implemented
    SampleCubochoricSpaceWidget& operator=(SampleCubochoricSpaceWidget&&) = delete;      // Move Assignment Not Implemented
};
