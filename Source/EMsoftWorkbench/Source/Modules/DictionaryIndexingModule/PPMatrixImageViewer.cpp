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

#include "PPMatrixImageViewer.h"

#include <QtCore/QDebug>

#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PPMatrixImageViewer::PPMatrixImageViewer(QWidget *parent, Qt::WindowFlags windowFlags) :
  GLImageViewer(parent, windowFlags)
{
  setMouseTracking(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PPMatrixImageViewer::~PPMatrixImageViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::loadImage(const QImage &image, float hipassValue, int hipassNumOfSteps)
{
  GLImageViewer::loadImage(image);

  m_HipassValue = hipassValue;
  m_HipassNumOfSteps = hipassNumOfSteps;

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::paintGL()
{
  GLImageViewer::paintGL();

  if (getCurrentImage().isNull() || m_HipassNumOfSteps == 0 || m_HipassValue == 0.0f)
  {
    return;
  }

  QPainter painter;
  painter.begin(this);
  painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);

  // Draw a black grid that separates the patterns a bit more
  QImage image = getCurrentImage();
  int imageWidth = image.width();
  int imageHeight = image.height();
  int xStep = imageWidth / m_HipassNumOfSteps;
  int yStep = imageHeight / m_HipassNumOfSteps;
  painter.setPen(QPen(QBrush(Qt::black, Qt::SolidPattern), 2));

  // First, we need to draw the separation border around all the patterns
  for (int x = 0; x <= imageWidth; x += xStep)
  {
    for (int y = 0; y <= imageHeight; y += yStep)
    {
      // Draw the separation border around the pattern
      QRect rect(x, y, xStep, yStep);
      QRect widgetRect(mapFromImageCoordinates(rect.topLeft()), mapFromImageCoordinates(rect.bottomRight()));

      painter.setPen(QPen(QBrush(Qt::black, Qt::SolidPattern), 2));
      painter.drawRect(widgetRect);
    }
  }

  QPoint imageCoords = mapToImageCoordinates(m_MouseCoords);

  // Next, we need to draw the mouse hover and selection borders
  int xCounter = 1;
  int yCounter = m_HipassNumOfSteps;
  float selectedHipassValue = -1.0f;
  float hoveredHipassValue = -1.0f;
  int selectedNumOfRegions = -1;
  int hoveredNumOfRegions = -1;
  for (int x = 0; x <= imageWidth; x += xStep)
  {
    for (int y = 0; y <= imageHeight; y += yStep)
    {
      QRect rect(x, y, xStep, yStep);
      QRect widgetRect(mapFromImageCoordinates(rect.topLeft()), mapFromImageCoordinates(rect.bottomRight()));
      if (rect.contains(m_SelectedImageCoords))
      {
        // Draw the border around the currently selected pattern
        painter.setPen(QPen(QBrush(Qt::green, Qt::SolidPattern), 2));
        painter.drawRect(widgetRect);

        selectedNumOfRegions = yCounter;

        int divisor = (xCounter-1) * 2;
        if (divisor == 0)
        {
          selectedHipassValue = m_HipassValue;
        }
        else
        {
          selectedHipassValue = m_HipassValue / divisor;
        }
      }
      else if (isMouseCoordinateValid() && rect.contains(imageCoords))
      {
        // Draw the border around the currently hovered pattern
        painter.setPen(QPen(QBrush(Qt::cyan, Qt::SolidPattern), 2));
        painter.drawRect(widgetRect);

        hoveredNumOfRegions = yCounter;

        int divisor = (xCounter-1) * 2;
        if (divisor == 0)
        {
          hoveredHipassValue = m_HipassValue;
        }
        else
        {
          hoveredHipassValue = m_HipassValue / divisor;
        }
      }

      yCounter--;
    }

    xCounter++;
    yCounter = m_HipassNumOfSteps;
  }

  // Get the current mouse coordinate and selected pixel coordinate relative to the image coordinate system
  int statsStartingHeightOffset = 40;
  int statsHeightSpacing = 20;
  QString mousePosStr = "Selected Hipass: N/A";
  if (selectedHipassValue > 0)
  {
    mousePosStr = QObject::tr("Selected Hipass: %1").arg(QString::number(selectedHipassValue, 'g', 4));
  }

  QString selectedPixelStr = "Selected Num Of Regions: N/A";
  if (selectedNumOfRegions > 0)
  {
    selectedPixelStr = QObject::tr("Selected Num Of Regions: %1").arg(QString::number(selectedNumOfRegions));
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
void PPMatrixImageViewer::mouseDoubleClickEvent(QMouseEvent *event)
{
  if (event->button() == Qt::LeftButton)
  {
    m_SelectedImageCoords = mapToImageCoordinates(m_MouseCoords);

    QImage image = getCurrentImage();
    int imageWidth = image.width();
    int imageHeight = image.height();
    int xStep = imageWidth / m_HipassNumOfSteps;
    int yStep = imageHeight / m_HipassNumOfSteps;

    // We are counting down with the Y counter because Qt assumes that (0,0) is in the upper left, whereas
    // we want (0,0) to be located in the bottom left.
    int xCounter = 1;
    int yCounter = m_HipassNumOfSteps;
    for (int x = 0; x <= imageWidth; x += xStep)
    {
      for (int y = 0; y <= imageHeight; y += yStep)
      {
        QRect rect(x, y, xStep, yStep);
        if (rect.contains(m_SelectedImageCoords))
        {
          emit selectedHipassNumOfStepsChanged(yCounter);
          emit selectedHipassValueChanged(m_HipassValue / ((xCounter-1) * 2));
        }

        yCounter--;
      }

      xCounter++;
    }

    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::mouseMoveEvent(QMouseEvent *event)
{
  GLImageViewer::mouseMoveEvent(event);

  m_MouseCoords = event->pos();

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::leaveEvent(QEvent* event)
{
  GLImageViewer::leaveEvent(event);

  invalidateMouseCoordinate();

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PPMatrixImageViewer::isMouseCoordinateValid() const
{
  return (m_MouseCoords.x() >= 0 && m_MouseCoords.y() >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PPMatrixImageViewer::isPixelSelected() const
{
  return (m_SelectedImageCoords.x() >= 0 && m_SelectedImageCoords.y() >= 0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::invalidateMouseCoordinate()
{
  m_MouseCoords.setX(-1);
  m_MouseCoords.setY(-1);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PPMatrixImageViewer::clearSelectedPixel()
{
  m_SelectedImageCoords.setX(-1);
  m_SelectedImageCoords.setY(-1);
}

