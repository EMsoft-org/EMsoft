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

#include "ui_AngleReaderWidget.h"

class AngleReaderWidget : public AbstractAngleWidget, public Ui::AngleReaderWidget
{
    Q_OBJECT

public:
    SIMPL_SHARED_POINTERS(AngleReaderWidget)
    SIMPL_STATIC_NEW_MACRO(AngleReaderWidget)

    AngleReaderWidget(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
    ~AngleReaderWidget() override;

    SIMPL_INSTANCE_PROPERTY(QString, OpenDialogLastDirectory)

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
    /**
     * @brief on_loadAngleFileBtn_clicked
     */
    void on_loadAngleFileBtn_clicked();

    /**
     * @brief on_partialFileCB_stateChanged
     * @param state
     */
    void on_partialFileCB_stateChanged(int state);

    /**
     * @brief on_minLineNum_valueChanged
     * @param value
     */
    void on_minLineNum_valueChanged(int value);

    /**
     * @brief on_maxLineNum_valueChanged
     * @param value
     */
    void on_maxLineNum_valueChanged(int value);

  protected:
    /**
     * @brief loadAngleFile
     * @param filePath
     */
    void loadAngleFile(const QString &filePath);

private:
    QString                                   m_LoadedFilePath = "";
    size_t                                    m_FileAngleCount = 0;

  public:
    AngleReaderWidget(const AngleReaderWidget&) = delete; // Copy Constructor Not Implemented
    AngleReaderWidget(AngleReaderWidget&&) = delete;      // Move Constructor Not Implemented
    AngleReaderWidget& operator=(const AngleReaderWidget&) = delete; // Copy Assignment Not Implemented
    AngleReaderWidget& operator=(AngleReaderWidget&&) = delete;      // Move Assignment Not Implemented
};
