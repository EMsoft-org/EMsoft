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

#include "DIImageViewer.h"

#include <QtCore/QDebug>
#include <QtCore/QJsonObject>

#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>

#include "Modules/DictionaryIndexingModule/Constants.h"

namespace ioConstants = DictionaryIndexingModuleConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DIImageViewer::DIImageViewer(QWidget* parent, Qt::WindowFlags windowFlags)
: GLImageViewer(parent, windowFlags)
{
  setMouseTracking(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DIImageViewer::~DIImageViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIImageViewer::paintGL()
{
  GLImageViewer::paintGL();

  if(getCurrentImage().isNull())
  {
    return;
  }

  QPainter painter;
  painter.begin(this);
  painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);

  if(isROISelected())
  {
    QPoint mouseCoords = {m_RoiX - 1, m_RoiY - 1};

    // We need to convert to image coordinates and then back to mouse coordinates
    // so that our mouse coordinates are within the image boundaries
    mouseCoords = mapFromImageCoordinates(mouseCoords);
    if(isMouseCoordinateValid(mouseCoords))
    {
      float zoomFactor = getZoomFactor();
      painter.setPen(QPen(QBrush(Qt::green, Qt::Dense4Pattern), 1));
      painter.drawRect(mouseCoords.x(), mouseCoords.y(), m_RoiW * zoomFactor, m_RoiH * zoomFactor);
    }
  }

  //  if(isMouseCoordinateValid())
  //  {
  //    QString selectedPixelStr = QObject::tr("Pixel: (%1, %2)").arg(QString::number(imageCoords.x()), QString::number(imageCoords.y()));

  //    painter.setPen(Qt::white);

  //    QFontMetrics fontMetrics(painter.fontMetrics());
  //    int maxStringWidth = fontMetrics.width(selectedPixelStr);
  //    int maxStringHeight = fontMetrics.height();

  //    int cursorXPadding = 10;
  //    int cursorYPadding = 10;
  //    int textXPadding = 5;
  //    int textYPadding = 5;

  //    QRect bgRect(m_MouseCoords.x() + cursorXPadding, m_MouseCoords.y() + cursorYPadding, maxStringWidth + textXPadding * 2, maxStringHeight + textYPadding * 2);

  //    // Keep the rect from going off the screen
  //    if(bgRect.x() + bgRect.width() > width())
  //    {
  //      int widthOverflow = bgRect.x() + bgRect.width() - width();
  //      bgRect.setX(bgRect.x() - widthOverflow);
  //    }
  //    if(bgRect.y() + bgRect.height() > height())
  //    {
  //      int heightOverflow = bgRect.y() + bgRect.height() - height();
  //      bgRect.setY(bgRect.y() - heightOverflow);
  //    }

  //    painter.fillRect(bgRect, QBrush(QColor(Qt::black)));
  //    painter.drawText(QPoint(bgRect.x() + textXPadding, bgRect.y() + maxStringHeight), selectedPixelStr);
  //  }

  painter.end();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIImageViewer::mouseDoubleClickEvent(QMouseEvent* event)
{
  GLImageViewer::mouseDoubleClickEvent(event);

  //  if (event->button() == Qt::LeftButton)
  //  {
  //    m_SelectedImageCoords = mapToImageCoordinates(m_MouseCoords);
  //    emit selectedADPCoordinateChanged(m_SelectedImageCoords);

  //    update();
  //  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIImageViewer::mouseMoveEvent(QMouseEvent* event)
{
  GLImageViewer::mouseMoveEvent(event);

  //  m_MouseCoords = event->pos();

  //  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool DIImageViewer::isMouseCoordinateValid(QPoint mouseCoord) const
{
  return (mouseCoord.x() >= 0 && mouseCoord.y() >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool DIImageViewer::isROISelected() const
{
  return (m_RoiX >= 0 && m_RoiY >= 0 && m_RoiW >= 0 && m_RoiH >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIImageViewer::clearROI()
{
  m_RoiX = -1;
  m_RoiY = -1;
  m_RoiW = -1;
  m_RoiH = -1;

  update();
}

// -----------------------------------------------------------------------------
void DIImageViewer::setROI(int x, int y, int w, int h)
{
  m_RoiX = x;
  m_RoiY = y;
  m_RoiW = w;
  m_RoiH = h;

  update();
}
