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

    struct GLImageData
    {
        QImage image;
        float minValue = 0;
        float maxValue = 0;
        float keVValue = 0;
    };

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
    * @brief Setter property for HasKevValue
    */
    void setHasKevValue(bool value);

    /**
    * @brief Getter property for HasKevValue
    * @return Value of HasKevValue
    */
    bool getHasKevValue() const;
    /**
    * @brief Setter property for UseStatsOverlay
    */
    void setUseStatsOverlay(const bool& value); 

    /**
    * @brief Getter property for UseStatsOverlay
    * @return Value of UseStatsOverlay
    */
    bool getUseStatsOverlay() const;

    void loadImage(GLImageViewer::GLImageData data);

    void zoomIn();
    void zoomOut();
    void fitToScreen();

    QImage getCurrentImage() const;

    void createModificationConnections(IModuleUI* ui) const;

    void readSession(QJsonObject &obj);

    void writeSession(QJsonObject &obj) const;

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

  signals:
    void viewerChanged();

private:
    bool m_HasKevValue = false;
    bool m_UseStatsOverlay = true;

    QImage        m_CurrentImage;
    float         m_MinValue = 0.0f;
    float         m_MaxValue = 0.0f;
    float         m_keVValue = 0.0f;

    bool          m_Zoomable = true;
    float         m_ZoomFactor = 1.0f;
    QPointF       m_PanningOffset = QPointF(0.0f, 0.0f);
    bool          m_IsPannable = false;
    bool          m_IsDragging = false;
    QPoint        m_LastPos = QPoint(0.0f, 0.0f);
    int           m_ViewportWidth = 0;
    int           m_ViewportHeight = 0;
    bool          m_DefaultControls = true;

  public:
    GLImageViewer(const GLImageViewer&) = delete; // Copy Constructor Not Implemented
    GLImageViewer(GLImageViewer&&) = delete;      // Move Constructor Not Implemented
    GLImageViewer& operator=(const GLImageViewer&) = delete; // Copy Assignment Not Implemented
    GLImageViewer& operator=(GLImageViewer&&) = delete;      // Move Assignment Not Implemented
};
