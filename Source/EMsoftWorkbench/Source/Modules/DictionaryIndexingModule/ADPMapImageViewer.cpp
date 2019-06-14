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

#include "ADPMapImageViewer.h"

#include <QtCore/QDebug>

#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapImageViewer::ADPMapImageViewer(QWidget *parent, Qt::WindowFlags windowFlags) :
  GLImageViewer(parent, windowFlags)
{
  setMouseTracking(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapImageViewer::~ADPMapImageViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::paintGL()
{
  GLImageViewer::paintGL();

  if (getCurrentImage().isNull())
  {
    return;
  }

  QPainter painter;
  painter.begin(this);
  painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);

  if (isPixelSelected())
  {
    QPoint selectedMouseCoords = mapFromImageCoordinates(m_SelectedImageCoords);

    painter.setPen(QPen(QBrush(Qt::green, Qt::SolidPattern), 1));
    painter.drawLine(0, selectedMouseCoords.y(), width(), selectedMouseCoords.y());
    painter.drawLine(selectedMouseCoords.x(), 0, selectedMouseCoords.x(), height());
  }

  if (m_MouseCoords.x() < 0 && m_MouseCoords.y() < 0)
  {
    return;
  }

  // We need to convert to image coordinates and then back to mouse coordinates
  // so that our mouse coordinates are within the image boundaries
  QPoint imageCoords = mapToImageCoordinates(m_MouseCoords);
  QPoint mouseCoords = mapFromImageCoordinates(imageCoords);
  painter.setPen(QPen(QBrush(Qt::darkGray, Qt::Dense2Pattern), 1));
  painter.drawLine(0, mouseCoords.y(), width(), mouseCoords.y());
  painter.drawLine(mouseCoords.x(), 0, mouseCoords.x(), height());

  // Get the current mouse coordinate and selected pixel coordinate relative to the image coordinate system
  int statsStartingHeightOffset = 40;
  int statsHeightSpacing = 20;
  QString mousePosStr = "Mouse: N/A";
  if (isMouseCoordinateValid())
  {
    mousePosStr = QObject::tr("Mouse: (%1, %2)").arg(QString::number(imageCoords.x()), QString::number(imageCoords.y()));
  }

  QString selectedPixelStr = "Selection: N/A";
  if (isPixelSelected())
  {
    selectedPixelStr = QObject::tr("Selection: (%1, %2)").arg(QString::number(m_SelectedImageCoords.x()), QString::number(m_SelectedImageCoords.y()));
  }

  // Figure out the length of the longest string
  int maxStrLen = mousePosStr.size();
  if (selectedPixelStr.size() > maxStrLen)
  {
    maxStrLen = selectedPixelStr.size();
  }

  int statsX = size().width() - (maxStrLen*8);

  painter.setPen(Qt::white);
  painter.fillRect(statsX - 10, size().height() - statsStartingHeightOffset - 20, maxStrLen*8 + 5, statsStartingHeightOffset + 10, QBrush(QColor(Qt::black)));
  painter.drawText(QPoint(statsX, size().height() - statsStartingHeightOffset), mousePosStr);
  painter.drawText(QPoint(statsX, size().height() - statsStartingHeightOffset + statsHeightSpacing), selectedPixelStr);

  painter.end();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::mouseDoubleClickEvent(QMouseEvent *event)
{
  if (event->button() == Qt::LeftButton)
  {
    m_SelectedImageCoords = mapToImageCoordinates(m_MouseCoords);
    emit selectedPatternCoordinateChanged(m_SelectedImageCoords);

    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::mouseMoveEvent(QMouseEvent *event)
{
  GLImageViewer::mouseMoveEvent(event);

  m_MouseCoords = event->pos();

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::leaveEvent(QEvent* event)
{
  GLImageViewer::leaveEvent(event);

  invalidateMouseCoordinate();

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ADPMapImageViewer::isMouseCoordinateValid() const
{
  return (m_MouseCoords.x() >= 0 && m_MouseCoords.y() >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ADPMapImageViewer::isPixelSelected() const
{
  return (m_SelectedImageCoords.x() >= 0 && m_SelectedImageCoords.y() >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::invalidateMouseCoordinate()
{
  m_MouseCoords.setX(-1);
  m_MouseCoords.setY(-1);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapImageViewer::clearSelectedPixel()
{
  m_SelectedImageCoords.setX(-1);
  m_SelectedImageCoords.setY(-1);
}

