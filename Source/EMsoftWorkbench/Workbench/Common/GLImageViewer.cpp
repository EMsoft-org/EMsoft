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
#include <QtCore/QJsonObject>

#include <QtGui/QDrag>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>

#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>

#include "Common/Constants.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Modules/IModuleUI.h"

const float zoomOffset = 0.25f;
const float bounceBackSpeed = 3.0f;
const float minZoomFactor = 0.25f;
const float maxZoomFactor = 4.0f;

namespace ivMod = EMsoftWorkbenchConstants::ImageViewerConstants;
namespace ioMod = EMsoftWorkbenchConstants::IOStrings;

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
GLImageViewer::~GLImageViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomIn()
{
  if (m_CurrentImage.isNull()) { return; }

  if (isZoomable())
  {
    m_ZoomFactor += zoomOffset;
    m_DefaultControls = false;
    emit viewerChanged();
    emit zoomFactorChanged(m_ZoomFactor);
    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::zoomOut()
{
  if (m_CurrentImage.isNull()) { return; }

  if (isZoomable())
  {
    m_ZoomFactor -= zoomOffset;
    m_DefaultControls = false;
    emit viewerChanged();
    emit zoomFactorChanged(m_ZoomFactor);
    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::saveImage()
{
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Image"), m_OpenDialogLastDirectory, tr("JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)"));
  m_OpenDialogLastDirectory = filePath;
  if(filePath.isEmpty())
  {
    return;
  }

  bool success = m_CurrentImage.save(filePath);
  if (!success)
  {
    emit errorMessageGenerated(tr("Error: Unable to save image to file path '%1'").arg(filePath));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
float GLImageViewer::getZoomFactor()
{
  return m_ZoomFactor;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::setZoomFactor(float val)
{
  m_ZoomFactor = val;
  emit zoomFactorChanged(m_ZoomFactor);
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::resizeEvent(QResizeEvent* event)
{
  m_ViewportWidth = event->size().width();
  m_ViewportHeight = event->size().height();

  if (m_DefaultControls)
  {
    fitToScreen();
  }

  QOpenGLWidget::resizeEvent(event);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::fitToScreen()
{
  if (m_CurrentImage.isNull()) { return; }

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
    emit zoomFactorChanged(m_ZoomFactor);
  }

  m_PanningOffset.setX(0);
  m_PanningOffset.setY(0);
  m_DefaultControls = true;
  emit viewerChanged();
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

//  if (!m_DefaultControls)
//  {
//    if (m_ZoomFactor > maxZoomFactor)
//    {
//      // Limit the zoom factor, so that we can't zoom in too far
//      m_ZoomFactor = maxZoomFactor;
//      emit zoomFactorChanged(m_ZoomFactor);
//    }
//    else if (m_ZoomFactor < minZoomFactor)
//    {
//      // Limit the zoom factor, so that we can't zoom out too far
//      m_ZoomFactor = minZoomFactor;
//      emit zoomFactorChanged(m_ZoomFactor);
//    }
//  }

//  emit zoomFactorChanged(m_ZoomFactor);

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
    if (m_IsDragging)
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

  m_TopLeftWidgetCoord.setX(x);
  m_TopLeftWidgetCoord.setY(y);
  m_TopLeftImageCoord.setX(sx);
  m_TopLeftImageCoord.setY(sy);
  m_ImageWidth = newWidth;
  m_ImageHeight = newHeight;

  painter.drawImage(x, y, image, sx, sy, newWidth, newHeight);

  painter.end();

  if (needsRepaint)
  {
    update();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QPoint GLImageViewer::mapToImageCoordinates(const QPoint &widgetCoords)
{
  QPoint imageCoords(-1, -1);
  if (m_TopLeftImageCoord.x() == 0 && m_TopLeftImageCoord.y() == 0 && (m_TopLeftWidgetCoord.x() > 0 || m_TopLeftWidgetCoord.y() > 0))
  {
    // The image is smaller than the viewport
    imageCoords.setX(widgetCoords.x() - m_TopLeftWidgetCoord.x());
    imageCoords.setY(widgetCoords.y() - m_TopLeftWidgetCoord.y());
    if (imageCoords.x() > m_ImageWidth)
    {
      imageCoords.setX(m_ImageWidth);
    }
    if (imageCoords.y() > m_ImageHeight)
    {
      imageCoords.setY(m_ImageHeight);
    }
    if (imageCoords.x() < 0)
    {
      imageCoords.setX(0);
    }
    if (imageCoords.y() < 0)
    {
      imageCoords.setY(0);
    }
  }
  else
  {
    // The image is larger than the viewport
    imageCoords.setX(widgetCoords.x() + m_TopLeftImageCoord.x());
    imageCoords.setY(widgetCoords.y() + m_TopLeftImageCoord.y());
  }

  // This needs to be done to scale the image coordinates to 100% image size
  imageCoords.setX(static_cast<int>((imageCoords.x() / static_cast<float>(m_ImageWidth)) * m_CurrentImage.width()));
  imageCoords.setY(static_cast<int>((imageCoords.y() / static_cast<float>(m_ImageHeight)) * m_CurrentImage.height()));

  return imageCoords;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QPoint GLImageViewer::mapFromImageCoordinates(const QPoint &imageCoords)
{
  QPoint widgetCoords = imageCoords;

  // This needs to be done to scale the image coordinates from 100% image size
  widgetCoords.setX(static_cast<int>((widgetCoords.x() / static_cast<float>(m_CurrentImage.width())) * m_ImageWidth));
  widgetCoords.setY(static_cast<int>((widgetCoords.y() / static_cast<float>(m_CurrentImage.height())) * m_ImageHeight));

  if (m_TopLeftImageCoord.x() == 0 && m_TopLeftImageCoord.y() == 0 && (m_TopLeftWidgetCoord.x() > 0 || m_TopLeftWidgetCoord.y() > 0))
  {
    // The image is smaller than the viewport
    widgetCoords.setX(widgetCoords.x() + m_TopLeftWidgetCoord.x());
    widgetCoords.setY(widgetCoords.y() + m_TopLeftWidgetCoord.y());
  }
  else
  {
    // The image is larger than the viewport
    widgetCoords.setX(widgetCoords.x() - m_TopLeftImageCoord.x());
    widgetCoords.setY(widgetCoords.y() - m_TopLeftImageCoord.y());
  }

  return widgetCoords;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::loadImage(QImage image)
{
  m_CurrentImage = std::move(image);

  fitToScreen();

  emit viewerChanged();
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage GLImageViewer::getCurrentImage() const
{
  return m_CurrentImage;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::enterEvent(QEvent* event)
{
  if (m_IsDragging)
  {
    setCursor(Qt::ClosedHandCursor);
  }
  else if (m_IsPannable)
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
    if (m_IsPannable)
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
  if (!(event->buttons() & Qt::LeftButton) || !m_IsPannable)
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

  emit viewerChanged();
  event->acceptProposedAction();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::dropEvent(QDropEvent *event)
{
  m_IsDragging = false;
  if (m_IsPannable)
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
  if (isZoomable())
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
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::readSession(const QJsonObject &obj)
{
  m_ZoomFactor = obj[ivMod::ZoomFactor].toDouble(m_ZoomFactor);
  emit zoomFactorChanged(m_ZoomFactor);

  QJsonObject panningOffsetObj = obj[ivMod::PanningOffset].toObject();
  m_PanningOffset.setX(panningOffsetObj[ioMod::X].toDouble(m_PanningOffset.x()));
  m_PanningOffset.setY(panningOffsetObj[ioMod::Y].toDouble(m_PanningOffset.y()));

  m_IsPannable = obj[ivMod::IsPannable].toBool(m_IsPannable);

  QJsonObject lastPositionObj = obj[ivMod::LastPos].toObject();
  m_LastPos.setX(lastPositionObj[ioMod::X].toDouble(m_LastPos.x()));
  m_LastPos.setY(lastPositionObj[ioMod::Y].toDouble(m_LastPos.y()));

  m_ViewportWidth = obj[ivMod::ViewportWidth].toInt(m_ViewportWidth);
  m_ViewportHeight = obj[ivMod::ViewportHeight].toInt(m_ViewportHeight);
  m_DefaultControls = obj[ivMod::DefaultControls].toBool(m_DefaultControls);

  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::writeSession(QJsonObject &obj) const
{
  obj[ivMod::ZoomFactor] = m_ZoomFactor;

  QJsonObject panningOffsetObj;
  panningOffsetObj[ioMod::X] = m_PanningOffset.x();
  panningOffsetObj[ioMod::Y] = m_PanningOffset.y();
  obj[ivMod::PanningOffset] = panningOffsetObj;

  obj[ivMod::IsPannable] = m_IsPannable;

  QJsonObject lastPositionObj;
  lastPositionObj[ioMod::X] = m_LastPos.x();
  lastPositionObj[ioMod::Y] = m_LastPos.y();
  obj[ivMod::LastPos] = lastPositionObj;

  obj[ivMod::ViewportWidth] = m_ViewportWidth;
  obj[ivMod::ViewportHeight] = m_ViewportHeight;
  obj[ivMod::DefaultControls] = m_DefaultControls;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool GLImageViewer::isZoomable() const
{
  return m_Zoomable;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void GLImageViewer::setZoomable(bool value)
{
  m_Zoomable = value;
}

