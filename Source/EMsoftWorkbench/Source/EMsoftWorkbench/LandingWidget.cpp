/* ============================================================================
* Copyright (c) 2009-2017 BlueQuartz Software, LLC
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

#include "LandingWidget.h"

#include <QtWidgets/QFileDialog>

#include "EMsoftWorkbench/EMsoftWorkbench.h"
#include "EMsoftWorkbench/EMsoftApplication.h"
#include "EMsoftWorkbench/EMsoftMenuItems.h"
#include "EMsoftWorkbench/LandingWidgetListModel.h"
#include "EMsoftWorkbench/LandingWidgetListItemDelegate.h"
#include "EMsoftWorkbench/QtSRecentFileList.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidget::LandingWidget(QWidget* parent) :
  QWidget(parent),
  m_OpenDialogLastDirectory("")
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidget::~LandingWidget()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::setupGui()
{
  connect(this, SIGNAL(openBtnPressed(const QString &)), emSoftApp, SLOT(openMasterFile(const QString &)));

  m_ListModel = new LandingWidgetListModel(this);
  m_ListItemDelegate = new LandingWidgetListItemDelegate(this);

  recentsListView->setModel(m_ListModel);
  recentsListView->setItemDelegate(m_ListItemDelegate);
  connect(recentsListView->selectionModel(), SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)), this, SLOT(itemSelectionChanged(const QItemSelection &, const QItemSelection &)));

  QModelIndex openListItem = m_ListModel->index(0, 0);
  if (openListItem.isValid() == true)
  {
    recentsListView->selectionModel()->select(openListItem, QItemSelectionModel::Select);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::updateRecentFiles(const QString &filePath)
{
  Q_UNUSED(filePath)

  m_ListModel->removeAllRecents();

  // Get the list from the static object
  QStringList files = QtSRecentFileList::instance()->fileList();
  int index = 0;
  foreach(QString file, files)
  {
    m_ListModel->insertRecentItem(index, file);
    index++;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::changeEvent(QEvent* event)
{
  if (event->type() == QEvent::ActivationChange)
  {
    emit landingWidgetWindowChangedState();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::itemSelectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
  Q_UNUSED(deselected)

  QModelIndexList selectedIndexes = selected.indexes();
  if (selectedIndexes.size() <= 0)
  {
    openBtn->setDisabled(true);
  }
  else
  {
    openBtn->setEnabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::on_openBtn_pressed()
{
  QModelIndexList indexList = recentsListView->selectionModel()->selectedRows();

  for (int i = 0; i < indexList.size(); i++)
  {
    QString path = m_ListModel->itemPath(indexList[i]);

    if (path.isEmpty() == false)
    {
      emit openBtnPressed(path);
    }
    else
    {
      EMsoftMenuItems* menuItems = EMsoftMenuItems::Instance();
      menuItems->getActionOpen()->trigger();
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::on_recentsListView_doubleClicked(const QModelIndex &index)
{
  Q_UNUSED(index)

  on_openBtn_pressed();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidget::on_cancelBtn_pressed()
{

}



