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

class DIImageViewer : public GLImageViewer
{
  Q_OBJECT

public:
  DIImageViewer(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
  ~DIImageViewer() override;

  /**
   * @brief setROI
   * @param x
   * @param y
   * @param w
   * @param h
   */
  void setROI(int x, int y, int w, int h);

  /**
   * @brief clearROI
   */
  void clearROI();

protected:
  void paintGL() override;

  void mouseDoubleClickEvent(QMouseEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;

signals:
  void selectedADPCoordinateChanged(const QPoint &coord);

private:
  int m_RoiX = -1;
  int m_RoiY = -1;
  int m_RoiW = -1;
  int m_RoiH = -1;

  /**
   * @brief isMouseCoordinateValid
   * @param mouseCoord
   * @return
   */
  bool isMouseCoordinateValid(QPoint mouseCoord) const;

  /**
   * @brief Returns whether or not there is an ROI selected in the current image
   * @return
   */
  bool isROISelected() const;

public:
  DIImageViewer(const DIImageViewer&) = delete;            // Copy Constructor Not Implemented
  DIImageViewer(DIImageViewer&&) = delete;                 // Move Constructor Not Implemented
  DIImageViewer& operator=(const DIImageViewer&) = delete; // Copy Assignment Not Implemented
  DIImageViewer& operator=(DIImageViewer&&) = delete;      // Move Assignment Not Implemented
};
