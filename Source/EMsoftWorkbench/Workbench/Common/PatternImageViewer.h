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

#include "Common/GLImageViewer.h"

class IModuleUI;

class PatternImageViewer : public GLImageViewer
{
    Q_OBJECT
public:
    PatternImageViewer(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
    ~PatternImageViewer() override;

    struct ImageData
    {
        QImage image;
        float minValue = 0;
        float maxValue = 0;
        float keVValue = 0;
    };

    /**
    * @brief Setter property for HasKevValue
    */
    void setHasKevValue(bool value);

    /**
    * @brief Getter property for HasKevValue
    * @return Value of HasKevValue
    */
    bool getHasKevValue() const;
    /**
    * @brief Setter property for UseStatsOverlay
    */
    void setUseStatsOverlay(const bool& value); 

    /**
    * @brief Getter property for UseStatsOverlay
    * @return Value of UseStatsOverlay
    */
    bool getUseStatsOverlay() const;

    void loadImage(PatternImageViewer::ImageData data);

protected:
    void paintGL() Q_DECL_OVERRIDE;

private:
    bool m_HasKevValue;
    bool m_UseStatsOverlay;

    float         m_MinValue;
    float         m_MaxValue;
    float         m_keVValue;

  public:
    PatternImageViewer(const PatternImageViewer&) = delete; // Copy Constructor Not Implemented
    PatternImageViewer(PatternImageViewer&&) = delete;      // Move Constructor Not Implemented
    PatternImageViewer& operator=(const PatternImageViewer&) = delete; // Copy Assignment Not Implemented
    PatternImageViewer& operator=(PatternImageViewer&&) = delete;      // Move Assignment Not Implemented
};
