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

#include "StatusBarWidget.h"

#include <QtCore/QDebug>
#include <QtWidgets/QDockWidget>

#include "Common/QtSStyles.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StatusBarWidget::StatusBarWidget(QWidget* parent)
: QFrame(parent)
{
  this->setupUi(this);
  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
StatusBarWidget::~StatusBarWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::setupGui()
{
  QString style = generateStyleSheet(false);
  stdOutputBtn->setStyleSheet(style);
  issuesBtn->setStyleSheet(style);
  navigatorBtn->setStyleSheet(style);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::updateStyle()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString StatusBarWidget::generateStyleSheet(bool error)
{
  QFont font = QtSStyles::GetBrandingLabelFont();
  QString fontString;
  QTextStream fontStringStream(&fontString);

  fontStringStream << "font: " << font.weight() << " ";
#if defined(Q_OS_MAC)
  fontStringStream << font.pointSize();
#elif defined(Q_OS_WIN)
  fontStringStream << font.pointSize();
#else
  fontStringStream << font.pointSize();
#endif
  fontStringStream << "pt \"" << font.family() << "\";";

  QString style;
  QTextStream ss(&style);

  QColor offBgColor(200, 200, 200);
  QColor onBgColor(90, 90, 90);
  QColor offTextColor(50, 50, 50);
  QColor onTextColor(240, 240, 240);
  QColor offBorderColor(120, 120, 120);
  QColor onBorderColor(200, 200, 200);

  if(error)
  {
    offBgColor = QColor(255, 150, 150);
    onBgColor = QColor(220, 60, 60);
    offBorderColor = QColor(220, 0, 0);
    onBorderColor = QColor(200, 0, 0);
  }

  int borderRadius = 3;

  ss << "QPushButton  {";
  ss << fontString;
  ss << "background-color: " << offBgColor.name() << ";";
  ss << "color:" << offTextColor.name() << ";";
  ss << "border: 1px solid " << offBorderColor.name() << ";";
  ss << "border-radius: " << borderRadius << "px;";
  ss << "padding: 1 8 1 8px;";
  ss << "margin: 2 2 2 2px;";
  ss << "}";

  ss << "QPushButton:hover  {";
  ss << fontString;
  ss << "background-color: " << offBgColor.name() << ";";
  ss << "color:" << offTextColor.name() << ";";
  ss << "border: 2px solid " << offBorderColor.name() << ";";
  ss << "border-radius: " << borderRadius << "px;";
  ss << "padding: 1 8 1 8px;";
  ss << "margin: 1 1 1 1px;";
  ss << "}";

  ss << "QPushButton:checked {";
  ss << fontString;
  ss << "background-color: " << onBgColor.name() << ";";
  ss << "color:" << onTextColor.name() << ";";
  ss << "border: 1px solid " << onBorderColor.name() << ";";
  ss << "border-radius: " << borderRadius << "px;";
  ss << "padding: 1 8 1 8px;";
  ss << "margin: 2 2 2 2px;";
  ss << "}";

  ss << "QPushButton:checked:hover  {";
  ss << fontString;
  ss << "background-color: " << onBgColor.name() << ";";
  ss << "color:" << onTextColor.name() << ";";
  ss << "border: 1px solid " << onBorderColor.name() << ";";
  ss << "border-radius: " << borderRadius << "px;";
  ss << "padding: 1 8 1 8px;";
  ss << "margin: 1 1 1 1px;";
  ss << "}";

  return style;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::issuesVisibilityChanged(bool b)
{
  issuesBtn->blockSignals(true);
  issuesBtn->setChecked(b);
  issuesBtn->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::stdOutputVisibilityChanged(bool b)
{
  stdOutputBtn->blockSignals(true);
  stdOutputBtn->setChecked(b);
  stdOutputBtn->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::navigatorVisibilityChanged(bool b)
{
  navigatorBtn->blockSignals(true);
  navigatorBtn->setChecked(b);
  navigatorBtn->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::toolboxVisibilityChanged(bool b)
{
  //  toolboxBtn->blockSignals(true);
  //  toolboxBtn->setChecked(b);
  //  toolboxBtn->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::setButtonAction(QDockWidget* dock, Button btn)
{
  switch(btn)
  {
  case Button::Issues:
    connect(issuesBtn, SIGNAL(toggled(bool)), dock, SLOT(setVisible(bool)));
    connect(dock, SIGNAL(visibilityChanged(bool)), this, SLOT(issuesVisibilityChanged(bool)));
    break;
  case Button::StandardOutput:
    connect(stdOutputBtn, SIGNAL(toggled(bool)), dock, SLOT(setVisible(bool)));
    connect(dock, SIGNAL(visibilityChanged(bool)), this, SLOT(stdOutputVisibilityChanged(bool)));
    break;
  default:
    break;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::setButtonAction(QToolBar* toolBar, Button btn)
{
  switch(btn)
  {
  case Button::ModuleNavigator:
    connect(navigatorBtn, SIGNAL(toggled(bool)), toolBar, SLOT(setVisible(bool)));
    connect(toolBar, SIGNAL(visibilityChanged(bool)), this, SLOT(navigatorVisibilityChanged(bool)));
    break;
  default:
    break;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void StatusBarWidget::issuesTableHasErrors(bool b)
{
  QString style = generateStyleSheet(b);
  issuesBtn->setStyleSheet(style);
}
