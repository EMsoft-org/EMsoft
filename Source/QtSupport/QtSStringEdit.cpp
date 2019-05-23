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

#include "QtSStringEdit.h"

#include <QtWidgets/QShortcut>

#include "EMsoftWorkbench/Source/Common/SVStyle.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSStringEdit::QtSStringEdit(QWidget* parent) : QWidget(parent)
{
  setupUi(this);
  setupGui();
}

QtSStringEdit::~QtSStringEdit() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::setupGui()
{
  applyChangesBtn->setVisible(false);
  cancelChangesBtn->setVisible(false);
  value->setAttribute(Qt::WA_MacShowFocusRect, false);

  connect(value, SIGNAL(textChanged(const QString&)), this, SLOT(widgetChanged(const QString&)));

  QShortcut* applyShortcut = new QShortcut(QKeySequence(Qt::Key::Key_Return), this);
  QShortcut* cancelShortcut = new QShortcut(QKeySequence(Qt::Key::Key_Escape), this);
  applyShortcut->setContext(Qt::ShortcutContext::WidgetWithChildrenShortcut);
  cancelShortcut->setContext(Qt::ShortcutContext::WidgetWithChildrenShortcut);

  connect(applyShortcut, &QShortcut::activated, this, &QtSStringEdit::on_applyChangesBtn_clicked);
  connect(cancelShortcut, &QShortcut::activated, this, &QtSStringEdit::on_cancelChangesBtn_clicked);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSStringEdit::getStoredValue()
{
  return m_storedValue;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSStringEdit::getText()
{
  return m_storedValue;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::setText(QString newValue, bool signalsBlocked)
{
  value->blockSignals(signalsBlocked);

  m_storedValue = newValue;
  value->setText(newValue);

  hideButtons();

  value->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::on_applyChangesBtn_clicked()
{
  value->setStyleSheet(QString(""));
  m_storedValue = value->text();
  emit valueChanged(m_storedValue);

  hideButtons();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::on_cancelChangesBtn_clicked()
{
  value->setText(m_storedValue);
  value->setStyleSheet(QString(""));

  hideButtons();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::hideButtons()
{
  value->setToolTip("");
  applyChangesBtn->setVisible(false);
  cancelChangesBtn->setVisible(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::widgetChanged(const QString& text)
{
  if(text == getStoredValue())
  {
    value->setStyleSheet(QString(""));

    hideButtons();
  }
  else
  {
    SVStyle::Instance()->SetErrorColor("QLineEdit", value);
    value->setToolTip("Press the 'Return' key to apply your changes\nPress the 'Esc' key to cancel your changes");

    applyChangesBtn->setVisible(true);
    cancelChangesBtn->setVisible(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStringEdit::setValidator(const QValidator *v)
{
  value->setValidator(v);
}
