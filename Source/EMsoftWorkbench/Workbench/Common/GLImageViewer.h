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

#include <QtWidgets/QOpenGLWidget>

class IModuleUI;

class GLImageViewer : public QOpenGLWidget
{
    Q_OBJECT
public:
    GLImageViewer(QWidget* parent = nullptr, Qt::WindowFlags windowFlags = Qt::WindowFlags());
    ~GLImageViewer() override;

    /**
    * @brief Getter property for Zoomable
     * @return
     */
    bool isZoomable() const;

    /**
    * @brief Setter property for Zoomable
     * @param value
     */
    void setZoomable(bool value);

    /**
     * @brief loadImage
     * @param image
     */
    void loadImage(QImage image);

    /**
     * @brief zoomIn
     */
    void zoomIn();

    /**
     * @brief zoomOut
     */
    void zoomOut();

    /**
     * @brief fitToScreen
     */
    void fitToScreen();

    /**
     * @brief saveImage
     * @return
     */
    void saveImage();

    /**
     * @brief getCurrentImage
     * @return
     */
    QImage getCurrentImage() const;

    /**
     * @brief getZoomFactor
     * @return
     */
    float getZoomFactor();

    /**
     * @brief setZoomFactor
     * @param val
     * @return
     */
    void setZoomFactor(float val);

    virtual void readSession(const QJsonObject &obj);

    virtual void writeSession(QJsonObject &obj) const;

protected:
    void paintGL() Q_DECL_OVERRIDE;
    void initializeGL() Q_DECL_OVERRIDE;

    void enterEvent(QEvent* event) Q_DECL_OVERRIDE;
    void leaveEvent(QEvent* event) Q_DECL_OVERRIDE;
    void mousePressEvent(QMouseEvent* event) Q_DECL_OVERRIDE;
    void mouseMoveEvent(QMouseEvent *event) Q_DECL_OVERRIDE;
    void dragEnterEvent(QDragEnterEvent *event) Q_DECL_OVERRIDE;
    void dragMoveEvent(QDragMoveEvent *event) Q_DECL_OVERRIDE;
    void dropEvent(QDropEvent *event) Q_DECL_OVERRIDE;
    void wheelEvent(QWheelEvent* event) Q_DECL_OVERRIDE;
    void resizeEvent(QResizeEvent *event) Q_DECL_OVERRIDE;

    /**
     * @brief Maps image viewer widget coordinates relative to the currently loaded image.
     * Any widget coordinates that are outside the bounds of the image will be automatically
     * mapped to coordinates that are on the edge of the image.
     * @param widgetCoords The coordinates relative to GLImageViewer.
     * @return
     */
    QPoint mapToImageCoordinates(const QPoint &widgetCoords);

    /**
     * @brief Maps currently loaded image coordinates relative to the image viewer widget.
     * @param imageCoords The coordinates relative to currently loaded image.
     * @return
     */
    QPoint mapFromImageCoordinates(const QPoint &imageCoords);

  signals:
    void errorMessageGenerated(const QString &msg);
    void viewerChanged();
    void zoomFactorChanged(float zoomFactor);

private:
    QImage        m_CurrentImage;

    bool          m_Zoomable = true;
    float         m_ZoomFactor = 1.0f;
    QPointF       m_PanningOffset = QPointF(0.0f, 0.0f);
    bool          m_IsPannable = false;
    bool          m_IsDragging = false;
    QPoint        m_LastPos = QPoint(0.0f, 0.0f);
    int           m_ViewportWidth = 0;
    int           m_ViewportHeight = 0;
    bool          m_DefaultControls = true;

    QPoint m_TopLeftWidgetCoord = QPoint(0, 0);
    QPoint m_TopLeftImageCoord = QPoint(0, 0);
    int m_ImageWidth = 0;
    int m_ImageHeight = 0;

    QString m_OpenDialogLastDirectory;

  public:
    GLImageViewer(const GLImageViewer&) = delete; // Copy Constructor Not Implemented
    GLImageViewer(GLImageViewer&&) = delete;      // Move Constructor Not Implemented
    GLImageViewer& operator=(const GLImageViewer&) = delete; // Copy Assignment Not Implemented
    GLImageViewer& operator=(GLImageViewer&&) = delete;      // Move Assignment Not Implemented
};
