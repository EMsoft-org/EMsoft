
#include "SVControlWidgets.h"


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVPushButton::SVPushButton(QWidget* parent) :
QPushButton(parent)
{}

SVPushButton::~SVPushButton() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVIconPushButton::SVIconPushButton(QWidget* parent) :
QPushButton(parent)
{}

SVIconPushButton::~SVIconPushButton() = default;



// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVToolButton::SVToolButton(QWidget* parent) :
QToolButton(parent)
{}

SVToolButton::~SVToolButton() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVSpinBox::SVSpinBox(QWidget* parent) :
QSpinBox(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVSpinBox::~SVSpinBox() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVDoubleSpinBox::SVDoubleSpinBox(QWidget* parent) :
QDoubleSpinBox(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVDoubleSpinBox::~SVDoubleSpinBox() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVLineEdit::SVLineEdit(QWidget* parent) :
QLineEdit(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVLineEdit::~SVLineEdit() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVSmallLabel::SVSmallLabel(QWidget* parent) :
QLabel(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVSmallLabel::~SVSmallLabel() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVTreeView::SVTreeView(QWidget* parent) :
QTreeView(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVTreeView::~SVTreeView() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVListView::SVListView(QWidget* parent) :
QListView(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVListView::~SVListView() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVListWidget::SVListWidget(QWidget* parent) :
QListWidget(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVListWidget::~SVListWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVTreeWidget::SVTreeWidget(QWidget* parent) :
QTreeWidget(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVTreeWidget::~SVTreeWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVTabWidget::SVTabWidget(QWidget* parent) :
  QTabWidget(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVTabWidget::~SVTabWidget() = default;


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVDialog::SVDialog(QWidget* parent) :
  QDialog(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVDialog::~SVDialog() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVFrame::SVFrame(QWidget* parent)
: QFrame(parent)
{
  setAttribute(Qt::WA_MacShowFocusRect, false);
}

SVFrame::~SVFrame() = default;
