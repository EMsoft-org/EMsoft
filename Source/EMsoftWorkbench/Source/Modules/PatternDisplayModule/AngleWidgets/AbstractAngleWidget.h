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

#include <QtWidgets/QWidget>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"
#include "SIMPLib/DataArrays/DataArray.hpp"
#include "SIMPLib/Math/SIMPLibMath.h"

class PatternDisplay_UI;

class AbstractAngleWidget : public QWidget
{
    Q_OBJECT

public:
    SIMPL_SHARED_POINTERS(AbstractAngleWidget)

    AbstractAngleWidget(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
    ~AbstractAngleWidget() override;

    static constexpr const float k_PiOver180 = static_cast<float>(M_PI / 180.0);
    static constexpr const float k_180OverPi = static_cast<float>(180.0f / M_PI);
    static const QString EulerStr;
    static const QString UnknownStr;
    static const QString EulerId;

    virtual FloatArrayType::Pointer getEulerAngles() = 0;

    static float ConvertToRadians(float value);
    static float ConvertToDegrees(float value);

    virtual bool hasValidAngles() = 0;

    /**
     * @brief readSession
     */
    virtual void readSession(QJsonObject &obj) = 0;

    /**
     * @brief writeSession
     */
    virtual void writeSession(QJsonObject &obj) = 0;

    /**
     * @brief createModificationConnections
     * @param ui
     */
    virtual void createModificationConnections(PatternDisplay_UI* ui) = 0;

  signals:
    void dataChanged(bool validAngles);

private:

public:
  AbstractAngleWidget(const AbstractAngleWidget&) = delete; // Copy Constructor Not Implemented
  AbstractAngleWidget(AbstractAngleWidget&&) = delete;      // Move Constructor Not Implemented
  AbstractAngleWidget& operator=(const AbstractAngleWidget&) = delete; // Copy Assignment Not Implemented
  AbstractAngleWidget& operator=(AbstractAngleWidget&&) = delete;      // Move Assignment Not Implemented
};
