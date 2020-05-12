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

#include <QtCore/QMap>

#include "Common/GLImageViewer.h"

class PPMatrixImageViewer : public GLImageViewer
{
  Q_OBJECT

public:
  PPMatrixImageViewer(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
  ~PPMatrixImageViewer() override;

  /**
     * @brief loadImage
     * @param image
     */
  void loadImage(const QImage &image, float hipassValue, int hipassNumOfSteps);

  /**
   * @brief readSession
   * @param obj
   */
  void readSession(const QJsonObject& obj) override;

  /**
   * @brief writeSession
   * @param obj
   */
  void writeSession(QJsonObject& obj) const override;

protected:
  void paintGL() override;

  void mouseDoubleClickEvent(QMouseEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;
  void enterEvent(QEvent* event) override;
  void leaveEvent(QEvent* event) override;

signals:
  void selectedHipassValueChanged(float value);
  void selectedHipassNumOfRegionsChanged(int value);

private:
  QPoint m_MouseCoords = QPoint(-1, -1);
  QPoint m_SelectedImageCoords = QPoint(-1, -1);

  float m_HipassValue = 0.0f;
  int m_HipassNumOfSteps = 0;

  /**
   * @brief Returns whether or not there is a valid mouse coordinate
   * @return
   */
  bool isMouseCoordinateValid() const;

  /**
   * @brief Returns whether or not there is a pixel selected in the current image
   * @return
   */
  bool isPixelSelected() const;

  /**
   * @brief invalidateMouseCoordinate
   */
  void invalidateMouseCoordinate();

  /**
   * @brief clearSelectedPixel
   */
  void clearSelectedPixel();

  /**
   * @brief calculateHipassData
   * @return
   */
  std::pair<int, float> calculateHipassData();

public:
  PPMatrixImageViewer(const PPMatrixImageViewer&) = delete;            // Copy Constructor Not Implemented
  PPMatrixImageViewer(PPMatrixImageViewer&&) = delete;                 // Move Constructor Not Implemented
  PPMatrixImageViewer& operator=(const PPMatrixImageViewer&) = delete; // Copy Assignment Not Implemented
  PPMatrixImageViewer& operator=(PPMatrixImageViewer&&) = delete;      // Move Assignment Not Implemented
};
