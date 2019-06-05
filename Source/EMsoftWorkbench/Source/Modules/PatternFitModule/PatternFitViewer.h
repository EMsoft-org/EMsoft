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

#include "Common/GLImageViewer.h"


#include "Modules/PatternFitModule/PatternControlsWidget.h"

#include "ui_PatternFitViewer.h"

class PatternFitViewer : public QWidget, public Ui::PatternFitViewer
{
  Q_OBJECT

public:
  PatternFitViewer(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
  ~PatternFitViewer() override;

  void loadImage(GLImageViewer::GLImageData imageData);

  void clearImage();

  int getFlickerInterval() const;

  /**
   * @brief readSession
   * @param obj
   */
  void readSession(QJsonObject& obj);

  /**
   * @brief writeSession
   * @param obj
   */
  void writeSession(QJsonObject& obj) const;

protected slots:
  /**
   * @brief on_saveBtn_clicked
   */
  void on_saveBtn_clicked() const;

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

signals:
  void flickerBoxChecked(int state);

  void controlsChanged();

private:
  QImage m_CurrentImage;

public:
  PatternFitViewer(const PatternFitViewer&) = delete;            // Copy Constructor Not Implemented
  PatternFitViewer(PatternFitViewer&&) = delete;                 // Move Constructor Not Implemented
  PatternFitViewer& operator=(const PatternFitViewer&) = delete; // Copy Assignment Not Implemented
  PatternFitViewer& operator=(PatternFitViewer&&) = delete;      // Move Assignment Not Implemented
};

Q_DECLARE_METATYPE(PatternControlsWidget::PatternChoice)
