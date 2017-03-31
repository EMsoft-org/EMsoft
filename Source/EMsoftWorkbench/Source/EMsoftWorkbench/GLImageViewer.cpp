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

#include "GLImageViewer.h"

#include <iostream>

#include <QtCore/QMimeData>

#include <QtGui/QDrag>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>

#include <QtWidgets/QApplication>

const float zoomOffset = 0.05f;
const float bounceBackSpeed = 3.0f;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
GLImageViewer::GLImageViewer(QWidget *parent, Qt::WindowFlags windowFlags) :
  QOpenGLWidget(parent, windowFlags)
{
  setAcceptDrops(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
GLImageViewer::~GLImageViewer()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::setZoomFactor(float zoomFactor)
{
  m_ZoomFactor = zoomFactor;
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomIn()
{
  m_ZoomFactor += zoomOffset;
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomOut()
{
  m_ZoomFactor -= zoomOffset;
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::resizeEvent(QResizeEvent* event)
{
  m_ViewportWidth = event->size().width();
  m_ViewportHeight = event->size().height();

  if (m_DefaultControls == true)
  {
    fitToScreen();
  }

  QOpenGLWidget::resizeEvent(event);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::fitToScreenBtn_pressed()
{
  m_DefaultControls = false;
  fitToScreen();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomInBtn_pressed()
{
  m_DefaultControls = false;
  zoomIn();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomOutBtn_pressed()
{
  m_DefaultControls = false;
  zoomOut();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::fitToScreen()
{
  int newWidth, newHeight;
  float percent;
  int sceneWidth = m_ViewportWidth;
  int sceneHeight = m_ViewportHeight;
  if (sceneWidth > sceneHeight)
  {
    newHeight = sceneHeight;
    percent = static_cast<float>(m_CurrentImage.height()) / newHeight;
    newWidth = m_CurrentImage.width() / percent;

    if (newWidth > sceneWidth)
    {
      newWidth = sceneWidth;
      percent = static_cast<float>(m_CurrentImage.width()) / newWidth;
      newHeight = m_CurrentImage.height() / percent;
    }
  }
  else
  {
    newWidth = sceneWidth;
    percent = static_cast<float>(m_CurrentImage.width()) / newWidth;
    newHeight = m_CurrentImage.height() / percent;

    if (newHeight > sceneHeight)
    {
      newHeight = sceneHeight;
      percent = static_cast<float>(m_CurrentImage.height()) / newHeight;
      newWidth = m_CurrentImage.width() / percent;
    }
  }

  if (percent != 0.0)
  {
    m_ZoomFactor = 1 / percent;
  }

  m_PanningOffset.setX(0);
  m_PanningOffset.setY(0);
  m_DefaultControls = true;
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::initializeGL()
{
  std::cout << "";
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::paintGL()
{
  if (m_CurrentImage.isNull()) { return; }

  QPainter painter;
  painter.begin(this);
  painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);
  bool needsRepaint = false;

//  QPoint mouseCoords = mapFromGlobal(QCursor::pos());
//  int xCoord = mouseCoords.x();
//  int yCoord = mouseCoords.y();
//  int xCoord = std::abs(std::abs(static_cast<int>(m_PrevDx)) - mouseCoords.x());
//  int yCoord = std::abs(std::abs(static_cast<int>(m_PrevDy)) - mouseCoords.y());

  if (m_DefaultControls == false)
  {
    if (m_ZoomFactor > 4)
    {
      // Limit the zoom factor to 4, so that we can't zoom in too far
      m_ZoomFactor = 4;
    }
    else if (m_ZoomFactor < 0.25)
    {
      // Limit the zoom factor to 0.25, so that we can't zoom out too far
      m_ZoomFactor = 0.25;
    }
  }

  float percent = 1 / m_ZoomFactor;
  int newWidth = m_CurrentImage.width() / percent;
  int newHeight = m_CurrentImage.height() / percent;

  int sceneWidth = m_ViewportWidth;
  int sceneHeight = m_ViewportHeight;

  QImage image = m_CurrentImage.scaled(newWidth, newHeight);

  int x, y, dx, dy, sx, sy;
  if (newWidth > sceneWidth && newHeight > sceneHeight)
  {
    // The image is larger than the viewport
    dx = (newWidth - sceneWidth) / 2;
    dy = (newHeight - sceneHeight) / 2;
    x = 0;
    y = 0;

    m_IsPannable = true;
    if (m_IsDragging == true)
    {
      setCursor(Qt::ClosedHandCursor);

      // Limit the x-coordinate panning to 30 pixels beyond the image border, when dragging
      if (m_PanningOffset.x() > dx + 30)
      {
        m_PanningOffset.setX(dx + 30);
        needsRepaint = true;
      }
      else if (m_PanningOffset.x() < -dx - 30)
      {
        m_PanningOffset.setX(-dx - 30);
        needsRepaint = true;
      }

      // Limit the y-coordinate panning to 30 pixels beyond the image border, when dragging
      if (m_PanningOffset.y() > dy + 30)
      {
        m_PanningOffset.setY(dy + 30);
        needsRepaint = true;
      }
      else if (m_PanningOffset.y() < -dy - 30)
      {
        m_PanningOffset.setY(-dy - 30);
        needsRepaint = true;
      }
    }
    else
    {
      setCursor(Qt::OpenHandCursor);

      // Rubber-banding animation for left side of image
      if (m_PanningOffset.x() > dx)
      {
        m_PanningOffset.setX(m_PanningOffset.x() - bounceBackSpeed);
        needsRepaint = true;
      }
      // Rubber-banding animation for right side of image
      else if (m_PanningOffset.x() < -dx)
      {
        m_PanningOffset.setX(m_PanningOffset.x() + bounceBackSpeed);
        needsRepaint = true;
      }

      // Rubber-banding animation for top side of image
      if (m_PanningOffset.y() > dy)
      {
        m_PanningOffset.setY(m_PanningOffset.y() - bounceBackSpeed);
        needsRepaint = true;
      }
      // Rubber-banding animation for bottom side of image
      else if (m_PanningOffset.y() < -dy)
      {
        m_PanningOffset.setY(m_PanningOffset.y() + bounceBackSpeed);
        needsRepaint = true;
      }
    }

    sx = dx - m_PanningOffset.x();
    sy = dy - m_PanningOffset.y();
  }
  else
  {
    // The image is smaller than the viewport
    m_PanningOffset.setX(0);
    m_PanningOffset.setY(0);

    dx = (sceneWidth - newWidth) / 2;
    dy = (sceneHeight - newHeight) / 2;
    x = dx;
    y = dy;
    sx = 0;
    sy = 0;

    m_IsPannable = false;
    setCursor(Qt::ArrowCursor);
  }

  painter.drawImage(x, y, image, sx, sy, newWidth, newHeight);

  painter.end();

  if (needsRepaint == true)
  {
    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::loadImage(const QImage &img)
{
  m_CurrentImage = img;
  m_DefaultControls = true;
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage GLImageViewer::getCurrentImage()
{
  return m_CurrentImage;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::enterEvent(QEvent* event)
{
  if (m_IsDragging == true)
  {
    setCursor(Qt::ClosedHandCursor);
  }
  else if (m_IsPannable == true)
  {
    setCursor(Qt::OpenHandCursor);
  }
  else
  {
    setCursor(Qt::ArrowCursor);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::leaveEvent(QEvent* event)
{
  m_IsDragging = false;
  setCursor(Qt::ArrowCursor);

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::mousePressEvent(QMouseEvent *event)
{
  if (event->button() == Qt::LeftButton)
  {
    if (m_IsPannable == true)
    {
      m_LastPos = event->pos();
      setCursor(Qt::ClosedHandCursor);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::mouseMoveEvent(QMouseEvent *event)
{
  if (!(event->buttons() & Qt::LeftButton) || m_IsPannable == false)
  {
    return;
  }
  if ((event->pos() - m_LastPos).manhattanLength() < QApplication::startDragDistance())
  {
    return;
  }

  m_IsDragging = true;

  QDrag *drag = new QDrag(this);
  QMimeData *mimeData = new QMimeData;
  drag->setMimeData(mimeData);
  QCursor dragCursor(Qt::ClosedHandCursor);
  drag->setDragCursor(dragCursor.pixmap(), Qt::MoveAction);
  drag->exec(Qt::CopyAction | Qt::MoveAction);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::dragEnterEvent(QDragEnterEvent *event)
{
  event->acceptProposedAction();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::dragMoveEvent(QDragMoveEvent *event)
{
  QPoint currentPos = event->pos();
  QPoint lastPos = m_LastPos;

  int xDiff = currentPos.x() - lastPos.x();
  int yDiff = currentPos.y() - lastPos.y();

  if (xDiff > 0)
  {
    m_PanningOffset.setX(m_PanningOffset.x() + std::abs(xDiff));
  }
  else
  {
    m_PanningOffset.setX(m_PanningOffset.x() - std::abs(xDiff));
  }

  if (yDiff > 0)
  {
    m_PanningOffset.setY(m_PanningOffset.y() + std::abs(yDiff));
  }
  else
  {
    m_PanningOffset.setY(m_PanningOffset.y() - std::abs(yDiff));
  }

  update();

  m_LastPos = currentPos;

  event->acceptProposedAction();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::dropEvent(QDropEvent *event)
{
  m_IsDragging = false;
  if (m_IsPannable == true)
  {
    setCursor(Qt::OpenHandCursor);
  }
  else
  {
    setCursor(Qt::ArrowCursor);
  }

  update();

  event->acceptProposedAction();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::wheelEvent(QWheelEvent* event)
{
  m_DefaultControls = false;
  if (event->pixelDelta().y() > 0)
  {
    zoomIn();
  }
  else
  {
    zoomOut();
  }
}



