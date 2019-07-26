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

#include "ui_PatternControlsWidget.h"

class PatternControlsWidget : public QWidget, public Ui::PatternControlsWidget
{
  Q_OBJECT

public:
  PatternControlsWidget(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
  ~PatternControlsWidget() override;

  using EnumType = unsigned int;

  enum class PatternChoice : EnumType
  {
    Experimental,
    Simulated,
    Difference,
    Composite,
    Color
  };

  enum class ControlsChoice : EnumType
  {
    CW = 1,
    CCW = 2,
    UP = 3,
    DOWN = 4,
    LEFT = 5,
    RIGHT = 6,
    ZERO = 7
  };

  /**
   * @brief getRotationStepSize
   * @return
   */
  double getRotationStepSize() const;

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
   * @brief on_patternChooserCB_currentIndexChanged
   * @param index
   */
  void on_patternChooserCB_currentIndexChanged(int index) const;

  /**
   * @brief on_rotationStepSize_textChanged
   * @param text
   */
  void on_rotationStepSize_textChanged(const QString& text) const;

protected:
  /**
   * @brief setupGui
   */
  void setupGui();

signals:
  void patternChoiceChanged(int index) const;
  void controlsChoicePressed(PatternControlsWidget::ControlsChoice choice) const;
  void rotationStepSizeChanged(double rot) const;
  void opacityChanged(double value) const;

private:
  void createControlsConnections() const;

  void createValidators() const;

  void createWidgetConnections() const;

public:
  PatternControlsWidget(const PatternControlsWidget&) = delete;            // Copy Constructor Not Implemented
  PatternControlsWidget(PatternControlsWidget&&) = delete;                 // Move Constructor Not Implemented
  PatternControlsWidget& operator=(const PatternControlsWidget&) = delete; // Copy Assignment Not Implemented
  PatternControlsWidget& operator=(PatternControlsWidget&&) = delete;      // Move Assignment Not Implemented
};
