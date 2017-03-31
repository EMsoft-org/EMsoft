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

#include "PatternListModel.h"

#include <QtWidgets>

PatternListModel* PatternListModel::m_Self = nullptr;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListModel::PatternListModel(QObject* parent)
: QAbstractListModel(parent)
{
  m_RootItem = new PatternListItem("");

  m_LoadingSpinner = new QMovie(":/loading.gif");
  m_LoadingSpinner->start();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListModel::~PatternListModel()
{
  delete m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListModel* PatternListModel::Instance()
{
  if (m_Self == nullptr)
  {
    m_Self = new PatternListModel();
  }

  return m_Self;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternListModel::clear()
{
  for (int i = rowCount() - 1; i >= 0; i--)
  {
    removeItem(i);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListItem* PatternListModel::insertItem(const int row, const QString &displayName)
{
  insertRow(row, QModelIndex());

  QModelIndex newNameIndex = index(row, PatternListItem::DefaultColumn, QModelIndex());
  PatternListItem* newItem = getItem(newNameIndex);
  newItem->setItemName(displayName);

  return newItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternListModel::removeItem(const int row)
{
  removeRow(row);
}

#include <iostream>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant PatternListModel::data(const QModelIndex& index, int role) const
{
  if(!index.isValid())
  {
    return QVariant();
  }

  PatternListItem* item = getItem(index);

  if(role == Qt::DisplayRole)
  {
    return item->getItemName();
  }
  else if(role == Qt::ToolTipRole)
  {
    return item->getItemName();
  }
  else if(role == Qt::DecorationRole)
  {
      PatternListItem* item = getItem(index);
      if (item->getPatternStatus() == PatternListItem::PatternStatus::Loading)
      {
        return m_LoadingSpinner->currentImage();
      }
      else if (item->getPatternStatus() == PatternListItem::PatternStatus::WaitingToLoad)
      {
        return QImage(":/bullet_ball_red.png");
      }
      else if (item->getPatternStatus() == PatternListItem::PatternStatus::Priority)
      {
        return QImage(":/bullet_ball_yellow.png");
      }
      else if (item->getPatternStatus() == PatternListItem::PatternStatus::Loaded)
      {
        return QImage(":/bullet_ball_green.png");
      }
      else
      {
        return QImage(":/delete.png");
      }
  }
  else
  {
    return QVariant();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::setPatternStatus(const int row, const PatternListItem::PatternStatus status)
{
  PatternListItem* item = getItem(index(row, PatternListItem::DefaultColumn));
  if (item == nullptr)
  {
    return false;
  }
  else
  {
    item->setPatternStatus(status);
    return true;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString PatternListModel::itemName(const QModelIndex &index)
{
  if (index.isValid() == true)
  {
    PatternListItem* item = getItem(index);
    return item->getItemName();
  }

  return QString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
Qt::ItemFlags PatternListModel::flags(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return 0;
  }

  return (Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemNeverHasChildren);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListItem* PatternListModel::getItem(const QModelIndex& index) const
{
  if(index.isValid())
  {
    PatternListItem* item = static_cast<PatternListItem*>(index.internalPointer());
    if(item)
    {
      return item;
    }
  }
  return m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex PatternListModel::getIndex(const PatternListItem* item) const
{
  int rowCount = this->rowCount(QModelIndex());
  for (int i = 0; i < rowCount; i++)
  {
    QModelIndex index = this->index(i, 0);
    PatternListItem* curItem = getItem(index);
    if (item == curItem)
    {
      return index;
    }
  }

  return QModelIndex();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant PatternListModel::headerData(int section, Qt::Orientation orientation, int role) const
{
  if(orientation == Qt::Horizontal && role == Qt::DisplayRole)
  {
    return m_RootItem->getItemName();
  }

  return QVariant();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex PatternListModel::index(int row, int column, const QModelIndex& parent) const
{
  if(parent.isValid() && parent.column() != PatternListItem::DefaultColumn)
  {
    return QModelIndex();
  }

  PatternListItem* childItem = m_RootItem->child(row);
  if(childItem)
  {
    return createIndex(row, column, childItem);
  }
  else
  {
    return QModelIndex();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::insertRows(int position, int rows, const QModelIndex& parent)
{
  PatternListItem* parentItem = getItem(parent);
  bool success;

  beginInsertRows(parent, position, position + rows - 1);
  success = parentItem->insertChildren(position, rows, PatternListItem::DefaultColumnCount);
  endInsertRows();

  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::removeRows(int position, int rows, const QModelIndex& parent)
{
  PatternListItem* parentItem = getItem(parent);
  bool success = true;

  beginRemoveRows(parent, position, position + rows - 1);
  success = parentItem->removeChildren(position, rows);
  endRemoveRows();

  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::moveRows(const QModelIndex& sourceParent, int sourceRow, int count, const QModelIndex& destinationParent, int destinationChild)
{
  beginMoveRows(sourceParent, sourceRow, sourceRow + count - 1, destinationParent, destinationChild);

  PatternListItem* srcParentItem = getItem(sourceParent);
  PatternListItem* destParentItem = getItem(destinationParent);

  for(int i = sourceRow; i < sourceRow + count; i++)
  {
    QModelIndex srcIndex = index(i, PatternListItem::DefaultColumn, sourceParent);
    PatternListItem* srcItem = getItem(srcIndex);

    destParentItem->insertChild(destinationChild, srcItem);
    srcItem->setParent(destParentItem);
    srcParentItem->removeChild(i);
  }

  endMoveRows();

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex PatternListModel::parent(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return QModelIndex();
  }

  PatternListItem* childItem = getItem(index);
  PatternListItem* parentItem = childItem->parent();

  if(parentItem == m_RootItem)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->childNumber(), 0, parentItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int PatternListModel::rowCount(const QModelIndex& parent) const
{
  PatternListItem* parentItem = getItem(parent);

  return parentItem->childCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
  PatternListItem* item = getItem(index);
  bool result = false;

  if(role == Qt::DecorationRole)
  {
    result = item->setIcon(value.value<QIcon>());
  }
  else if(role == Qt::ToolTipRole)
  {
    result = item->setItemTooltip(value.toString());
  }

  if(result)
  {
    emit dataChanged(index, index);
  }

  return result;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternListItem* PatternListModel::getRootItem()
{
  return m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternListModel::isEmpty()
{
  if(rowCount(QModelIndex()) <= 0)
  {
    return true;
  }
  return false;
}
