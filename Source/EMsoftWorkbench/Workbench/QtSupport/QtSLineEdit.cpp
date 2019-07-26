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
#include "QtSLineEdit.h"

#include <QtCore/QDebug>
#include <QtCore/QDir>
#include <QtCore/QEvent>
#include <QtCore/QFileInfo>
#include <QtCore/QMimeData>
#include <QtCore/QPropertyAnimation>
#include <QtCore/QString>
#include <QtGui/QMouseEvent>
#include <QtGui/QPaintEvent>
#include <QtGui/QPainter>
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDesktopWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMenu>
#include <QtWidgets/QStyle>

/**
 * @brief execMenuAtWidget
 * @param menu
 * @param widget
 */
static void execMenuAtWidget(QMenu* menu, QWidget* widget)
{
  QPoint p;
  QRect screen = QApplication::desktop()->availableGeometry(widget);
  QSize sh = menu->sizeHint();
  QRect rect = widget->rect();
  if(widget->isRightToLeft())
  {
    if(widget->mapToGlobal(QPoint(0, rect.bottom())).y() + sh.height() <= screen.height())
    {
      p = widget->mapToGlobal(rect.bottomRight());
    }
    else
    {
      p = widget->mapToGlobal(rect.topRight() - QPoint(0, sh.height()));
    }
    p.rx() -= sh.width();
  }
  else
  {
    if(widget->mapToGlobal(QPoint(0, rect.bottom())).y() + sh.height() <= screen.height())
    {
      p = widget->mapToGlobal(rect.bottomLeft());
    }
    else
    {
      p = widget->mapToGlobal(rect.topLeft() - QPoint(0, sh.height()));
    }
  }
  p.rx() = qMax(screen.left(), qMin(p.x(), screen.right() - sh.width()));
  p.ry() += 1;

  menu->exec(p);
}

enum
{
  margin = 6
};

#define ICONBUTTON_HEIGHT 18
#define FADE_TIME 160

/**
 * @brief The SearchLineEditPrivate class
 */
class SearchLineEditPrivate : public QObject
{
public:
  explicit SearchLineEditPrivate(QtSLineEdit* parent);

  bool eventFilter(QObject* obj, QEvent* event) override;

  QtSLineEdit* m_LineEdit;
  QPixmap m_PixMaps[2];
  QMenu* m_ButtonMenus[2];
  bool m_MenuTabFocusTriggers[2];
  IconButton* m_IconButtons[2];
  bool m_IconVisible[2];
  bool m_IconEnabled[2];
};

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SearchLineEditPrivate::SearchLineEditPrivate(QtSLineEdit* parent)
: QObject(parent)
, m_LineEdit(parent)
{
  for(int i = 0; i < 2; ++i)
  {
    m_ButtonMenus[i] = nullptr;
    m_MenuTabFocusTriggers[i] = false;
    m_IconButtons[i] = new IconButton(parent);
    m_IconButtons[i]->installEventFilter(this);
    m_IconButtons[i]->hide();
    m_IconButtons[i]->setAutoHide(false);
    m_IconEnabled[i] = false;
    m_IconVisible[i] = false;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SearchLineEditPrivate::eventFilter(QObject* obj, QEvent* event)
{
  int buttonIndex = -1;
  for(int i = 0; i < 2; ++i)
  {
    if(obj == m_IconButtons[i])
    {
      buttonIndex = i;
      break;
    }
  }
  if(buttonIndex == -1)
  {
    return QObject::eventFilter(obj, event);
  }
  switch(event->type())
  {
  case QEvent::FocusIn:
    if(m_MenuTabFocusTriggers[buttonIndex] && (m_ButtonMenus[buttonIndex] != nullptr))
    {
      m_LineEdit->setFocus();
      execMenuAtWidget(m_ButtonMenus[buttonIndex], m_IconButtons[buttonIndex]);
      return true;
    }
    break;
  default:
    break;
  }
  return QObject::eventFilter(obj, event);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSLineEdit::QtSLineEdit(QWidget* parent)
: SVLineEdit(parent)
, d(new SearchLineEditPrivate(this))
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
  ensurePolished();
  updateMargins();

  connect(this, SIGNAL(textChanged(QString)), this, SLOT(checkButtons(QString)));
  connect(d->m_IconButtons[Left], SIGNAL(clicked()), this, SLOT(iconClicked()));
  connect(d->m_IconButtons[Right], SIGNAL(clicked()), this, SLOT(iconClicked()));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::checkButtons(const QString& text)
{
  if(m_OldText.isEmpty() || text.isEmpty())
  {
    for(int i = 0; i < 2; ++i)
    {
      if(d->m_IconButtons[i]->hasAutoHide())
      {
        d->m_IconButtons[i]->animateShow(!text.isEmpty());
      }
    }
    m_OldText = text;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSLineEdit::~QtSLineEdit() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonVisible(Side side, bool visible)
{
  d->m_IconButtons[side]->setVisible(visible);
  d->m_IconVisible[side] = visible;
  updateMargins();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonDisabled(Side side, bool disabled)
{
  d->m_IconButtons[side]->setDisabled(disabled);
  d->m_IconEnabled[side] = !disabled;
  updateMargins();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonEnabled(Side side, bool enabled)
{
  d->m_IconButtons[side]->setEnabled(enabled);
  d->m_IconEnabled[side] = enabled;
  updateMargins();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool QtSLineEdit::isButtonVisible(Side side) const
{
  return d->m_IconVisible[side];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool QtSLineEdit::isButtonEnabled(Side side) const
{
  return d->m_IconEnabled[side];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::iconClicked()
{
  IconButton* button = qobject_cast<IconButton*>(sender());
  int index = -1;
  for(int i = 0; i < 2; ++i)
  {
    if(d->m_IconButtons[i] == button)
    {
      index = i;
    }
  }
  if(index == -1)
  {
    return;
  }
  if(d->m_ButtonMenus[index] != nullptr)
  {
    execMenuAtWidget(d->m_ButtonMenus[index], button);
  }
  else
  {
    emit buttonClicked((Side)index);
    if(index == Left)
    {
      emit leftButtonClicked();
    }
    else if(index == Right)
    {
      emit rightButtonClicked();
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::updateMargins()
{
  bool leftToRight = (layoutDirection() == Qt::LeftToRight);
  Side realLeft = (leftToRight ? Left : Right);
  Side realRight = (leftToRight ? Right : Left);

  int leftMargin = d->m_IconButtons[realLeft]->pixmap().width() + 8;
  int rightMargin = d->m_IconButtons[realRight]->pixmap().width() + 8;
  // Note KDE does not reserve space for the highlight color
  if(style()->inherits("OxygenStyle"))
  {
    leftMargin = qMax(24, leftMargin);
    rightMargin = qMax(24, rightMargin);
  }

  QMargins margins((d->m_IconVisible[realLeft] ? leftMargin : 0), 0, (d->m_IconVisible[realRight] ? rightMargin : 0), 0);

  setTextMargins(margins);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::updateButtonPositions()
{
  QRect contentRect = rect();
  for(int i = 0; i < 2; ++i)
  {
    Side iconpos = (Side)i;
    if(layoutDirection() == Qt::RightToLeft)
    {
      iconpos = (iconpos == Left ? Right : Left);
    }

    if(iconpos == QtSLineEdit::Right)
    {
      const int iconoffset = textMargins().right() + 4;
      d->m_IconButtons[i]->setGeometry(contentRect.adjusted(width() - iconoffset, 0, 0, 0));
    }
    else
    {
      const int iconoffset = textMargins().left() + 4;
      d->m_IconButtons[i]->setGeometry(contentRect.adjusted(0, 0, -width() + iconoffset, 0));
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::resizeEvent(QResizeEvent*)
{
  updateButtonPositions();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::dragEnterEvent(QDragEnterEvent* event)
{
  // accept just text/uri-list mime format
  if(event->mimeData()->hasFormat("text/uri-list"))
  {
    event->acceptProposedAction();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::dropEvent(QDropEvent* event)
{
  QList<QUrl> urlList;
  QString fName;
  QFileInfo info;

  if(event->mimeData()->hasUrls())
  {
    urlList = event->mimeData()->urls(); // returns list of QUrls
    // if just text was dropped, urlList is empty (size == 0)

    if(!urlList.empty()) // if at least one QUrl is present in list
    {
      fName = urlList[0].toLocalFile(); // convert first QUrl to local path
      fName = QDir::toNativeSeparators(fName);
      info.setFile(fName); // information about file
      setText(fName);      // if is file, setText
      emit fileDropped(fName);
    }
  }

  event->acceptProposedAction();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::keyPressEvent(QKeyEvent* event)
{
  emit keyPressed(event);
  QLineEdit::keyPressEvent(event);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonPixmap(Side side, const QPixmap& buttonPixmap)
{
  d->m_IconButtons[side]->setPixmap(buttonPixmap);
  updateMargins();
  updateButtonPositions();
  update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QPixmap QtSLineEdit::buttonPixmap(Side side) const
{
  return d->m_PixMaps[side];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonMenu(Side side, QMenu* buttonMenu)
{
  d->m_ButtonMenus[side] = buttonMenu;
  d->m_IconButtons[side]->setIconOpacity(1.0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QMenu* QtSLineEdit::buttonMenu(Side side) const
{
  return d->m_ButtonMenus[side];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool QtSLineEdit::hasMenuTabFocusTrigger(Side side) const
{
  return d->m_MenuTabFocusTriggers[side];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setMenuTabFocusTrigger(Side side, bool v)
{
  if(d->m_MenuTabFocusTriggers[side] == v)
  {
    return;
  }

  d->m_MenuTabFocusTriggers[side] = v;
  d->m_IconButtons[side]->setFocusPolicy(v ? Qt::TabFocus : Qt::NoFocus);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool QtSLineEdit::hasAutoHideButton(Side side) const
{
  return d->m_IconButtons[side]->hasAutoHide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setAutoHideButton(Side side, bool h)
{
  d->m_IconButtons[side]->setAutoHide(h);
  if(h)
  {
    d->m_IconButtons[side]->setIconOpacity(text().isEmpty() ? 0.0 : 1.0);
  }
  else
  {
    d->m_IconButtons[side]->setIconOpacity(1.0);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonToolTip(Side side, const QString& tip)
{
  d->m_IconButtons[side]->setToolTip(tip);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSLineEdit::setButtonFocusPolicy(Side side, Qt::FocusPolicy policy)
{
  d->m_IconButtons[side]->setFocusPolicy(policy);
}

// IconButton - helper class to represent a clickable icon
// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IconButton::IconButton(QWidget* parent)
: QAbstractButton(parent)
, m_autoHide(false)
{
  setCursor(Qt::ArrowCursor);
  setFocusPolicy(Qt::NoFocus);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void IconButton::paintEvent(QPaintEvent*)
{
  QPainter painter(this);
  QRect pixmapRect = QRect(0, 0, m_pixmap.width(), m_pixmap.height());
  pixmapRect.moveCenter(rect().center());

  if(m_autoHide)
  {
    painter.setOpacity(m_iconOpacity);
  }

  painter.drawPixmap(pixmapRect, m_pixmap);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void IconButton::animateShow(bool visible)
{
  if(visible)
  {
    QPropertyAnimation* animation = new QPropertyAnimation(this, "iconOpacity");
    animation->setDuration(FADE_TIME);
    animation->setEndValue(1.0);
    animation->start(QAbstractAnimation::DeleteWhenStopped);
  }
  else
  {
    QPropertyAnimation* animation = new QPropertyAnimation(this, "iconOpacity");
    animation->setDuration(FADE_TIME);
    animation->setEndValue(0.0);
    animation->start(QAbstractAnimation::DeleteWhenStopped);
  }
}
