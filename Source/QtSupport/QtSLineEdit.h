/* ============================================================================
 * Copyright (c) 2017 BlueQuartz Software, LLC
 * All rights reserved.
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
 * Neither the names of any of the BlueQuartz Software contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
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
 *
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#pragma once

#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QLineEdit>

#include "EMsoftWorkbench/Source/Common/SVControlWidgets.h"

class SearchLineEditPrivate;

/**
 * @brief The IconButton class
 */
class IconButton : public QAbstractButton
{
    Q_OBJECT
    Q_PROPERTY(float iconOpacity READ iconOpacity WRITE setIconOpacity)
    Q_PROPERTY(bool autoHide READ hasAutoHide WRITE setAutoHide)
    Q_PROPERTY(QPixmap pixmap READ pixmap WRITE setPixmap)
  public:
    explicit IconButton(QWidget* parent = nullptr);
    void paintEvent(QPaintEvent* event) override;
    void setPixmap(const QPixmap& pixmap) { m_pixmap = pixmap; update(); }
    QPixmap pixmap() const { return m_pixmap; }
    float iconOpacity() { return m_iconOpacity; }
    void setIconOpacity(float value) { m_iconOpacity = value; update(); }
    void animateShow(bool visible);

    void setAutoHide(bool hide) { m_autoHide = hide; }
    bool hasAutoHide() const { return m_autoHide; }

  private:
    float         m_iconOpacity;
    bool          m_autoHide;
    QPixmap       m_pixmap;
};

/**
    @brief A line edit with an embedded pixmap on one side that is connected to
    a menu.

    Additionally, it can display a grayed hintText (like "Type Here to")
    when not focused and empty. When connecting to the changed signals and
    querying text, one has to be aware that the text is set to that hint
    text if isShowingHintText() returns true (that is, does not contain
    valid user input).
 */
class QtSLineEdit : public SVLineEdit
{
    Q_OBJECT
    Q_ENUMS(Side)

  public:
    enum Side {Left = 0, Right = 1};

    explicit QtSLineEdit(QWidget* parent = nullptr);
    ~QtSLineEdit() override;

    QPixmap buttonPixmap(Side side) const;
    void setButtonPixmap(Side side, const QPixmap& pixmap);

    QMenu* buttonMenu(Side side) const;
    void setButtonMenu(Side side, QMenu* menu);

    void setButtonVisible(Side side, bool visible);
    bool isButtonVisible(Side side) const;

    void setButtonDisabled(Side side, bool disabled);
    void setButtonEnabled(Side side, bool enabled);
    bool isButtonEnabled(Side side) const;

    void setButtonToolTip(Side side, const QString&);
    void setButtonFocusPolicy(Side side, Qt::FocusPolicy policy);

    // Set whether tabbing in will trigger the menu.
    void setMenuTabFocusTrigger(Side side, bool v);
    bool hasMenuTabFocusTrigger(Side side) const;

    // Set if icon should be hidden when text is empty
    void setAutoHideButton(Side side, bool h);
    bool hasAutoHideButton(Side side) const;

    void dragEnterEvent(QDragEnterEvent* event) override;
    void dropEvent(QDropEvent* event) override;

  signals:
    void buttonClicked(QtSLineEdit::Side side);
    void leftButtonClicked();
    void rightButtonClicked();
    void fileDropped(const QString& file);

    void keyPressed(QKeyEvent* event);

  private slots:
    void checkButtons(const QString&);
    void iconClicked();

  protected:
    void resizeEvent(QResizeEvent* e) override;

    void keyPressEvent(QKeyEvent* event) override;

  private:

    void updateMargins();
    void updateButtonPositions();
    friend class SearchLineEditPrivate;

    SearchLineEditPrivate* d;
    QString m_OldText;
};


