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

#include "LandingWidgetListModel.h"

#include <QtWidgets>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListModel::LandingWidgetListModel(QObject* parent)
: QAbstractListModel(parent)
{
  m_RootItem = new LandingWidgetListItem("", "");
  m_RootItem->setItemType(LandingWidgetListItem::ItemType::Root);

//  QVector<QVariant> recentsHeaderData;
//  recentsHeaderData.push_back(QVariant("Recents"));
//  m_RootItem->insertChild(0, new LandingWidgetListItem(recentsHeaderData, true, m_RootItem));

  insertItem(0, "Open Master File...", "", "", LandingWidgetListItem::ItemType::Open);

  m_RecentsItem = insertHeaderItem(1, "Recents");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListModel::~LandingWidgetListModel()
{
  delete m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListItem* LandingWidgetListModel::insertRecentItem(const int row, const QString &path)
{
  QFileInfo fi(path);
  QString displayName = fi.baseName();
  QString displayPath = fi.path();

  return insertItem(row, displayName, displayPath, path, LandingWidgetListItem::ItemType::Recent, m_RecentsItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidgetListModel::removeRecentItem(const int row)
{
  removeItem(row, m_RecentsItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidgetListModel::removeAllRecents()
{
  int start = getIndex(m_RecentsItem).row() + 1;
  int end = rowCount();
  for (int i = start; i < end; i++)
  {
    removeItem(i);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListItem* LandingWidgetListModel::insertHeaderItem(const int row, const QString &name)
{
  return insertItem(row, name, "", "", LandingWidgetListItem::ItemType::Header);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListItem* LandingWidgetListModel::insertItem(const int row, const QString &displayName, const QString &displayPath, const QString &fullPath, LandingWidgetListItem::ItemType itemType, LandingWidgetListItem *headerItem)
{
  int rowPos;
  if (headerItem)
  {
    rowPos = getIndex(headerItem).row() + row + 1;
  }
  else
  {
    rowPos = row;
  }

  insertRow(rowPos, QModelIndex());

  QModelIndex newNameIndex = index(rowPos, LandingWidgetListItem::DefaultColumn, QModelIndex());
  LandingWidgetListItem* newItem = getItem(newNameIndex);
  newItem->setItemType(itemType);
  newItem->setItemName(displayName);
  newItem->setItemParentPath(displayPath);
  newItem->setItemPath(fullPath);

  return newItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void LandingWidgetListModel::removeItem(const int row, LandingWidgetListItem* headerItem)
{
  int rowPos;
  if (headerItem)
  {
    rowPos = getIndex(headerItem).row() + row + 1;
  }
  else
  {
    rowPos = row;
  }

  removeRow(rowPos);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant LandingWidgetListModel::data(const QModelIndex& index, int role) const
{
  if(!index.isValid())
  {
    return QVariant();
  }

  LandingWidgetListItem* item = getItem(index);

  if(role == Qt::DisplayRole)
  {
    QString itemName = item->getItemName();
    QString displayStr = itemName;
    if (item->getItemType() == LandingWidgetListItem::ItemType::Recent)
    {
      QString pathName = item->getItemPath();
      displayStr.append("\n" + pathName);
    }

    return displayStr;
  }
  else if(role == Qt::BackgroundRole)
  {
    if (item->getItemType() == LandingWidgetListItem::ItemType::Header)
    {
      return QColor(245, 245, 245);
    }
    else
    {
      return QVariant();
    }
  }
  else if(role == Qt::ForegroundRole)
  {
    if (item->getItemType() == LandingWidgetListItem::ItemType::Header)
    {
      return QColor(120, 120, 120);
    }
    else
    {
      return QVariant();
    }
  }
  else if(role == Qt::FontRole)
  {
    if (item->getItemType() == LandingWidgetListItem::ItemType::Header)
    {
      QFont font;
      font.setPointSize(11);
      font.setBold(true);
      return font;
    }
    else
    {
      return QVariant();
    }
  }
  else if (role == Qt::SizeHintRole)
  {
    if (item->getItemType() == LandingWidgetListItem::ItemType::Header)
    {
      return QSize(10, 30);
    }
    else
    {
      return QSize(10, 50);
    }
  }
  else if(role == Qt::ForegroundRole)
  {
    return QColor(Qt::black);
  }
  else if(role == Qt::ToolTipRole)
  {
    return item->getItemTooltip();
  }
  else if(role == Qt::DecorationRole)
  {
    QModelIndex nameIndex = this->index(index.row(), LandingWidgetListItem::DefaultColumn, index.parent());
    if(nameIndex == index)
    {
      LandingWidgetListItem* item = getItem(index);
      return item->getIcon();
    }
    else
    {
      return QVariant();
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
QString LandingWidgetListModel::itemName(const QModelIndex &index)
{
  if (index.isValid() == true)
  {
    LandingWidgetListItem* item = getItem(index);
    return item->getItemName();
  }

  return QString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString LandingWidgetListModel::itemParentPath(const QModelIndex &index)
{
  if (index.isValid() == true)
  {
    LandingWidgetListItem* item = getItem(index);
    return item->getItemParentPath();
  }

  return QString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString LandingWidgetListModel::itemPath(const QModelIndex &index)
{
  if (index.isValid() == true)
  {
    LandingWidgetListItem* item = getItem(index);
    return item->getItemPath();
  }

  return QString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
Qt::ItemFlags LandingWidgetListModel::flags(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return 0;
  }

  LandingWidgetListItem* item = getItem(index);
  if(item->getItemType() == LandingWidgetListItem::ItemType::Header)
  {
    // This is a header
    return (Qt::ItemIsEnabled);
  }
  else
  {
    // This is an item
    return (Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemNeverHasChildren);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
LandingWidgetListItem* LandingWidgetListModel::getItem(const QModelIndex& index) const
{
  if(index.isValid())
  {
    LandingWidgetListItem* item = static_cast<LandingWidgetListItem*>(index.internalPointer());
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
QModelIndex LandingWidgetListModel::getIndex(const LandingWidgetListItem* item) const
{
  int rowCount = this->rowCount(QModelIndex());
  for (int i = 0; i < rowCount; i++)
  {
    QModelIndex index = this->index(i, 0);
    LandingWidgetListItem* curItem = getItem(index);
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
QVariant LandingWidgetListModel::headerData(int section, Qt::Orientation orientation, int role) const
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
QModelIndex LandingWidgetListModel::index(int row, int column, const QModelIndex& parent) const
{
  if(parent.isValid() && parent.column() != 0)
  {
    return QModelIndex();
  }

  LandingWidgetListItem* childItem = m_RootItem->child(row);
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
bool LandingWidgetListModel::insertRows(int position, int rows, const QModelIndex& parent)
{
  LandingWidgetListItem* parentItem = getItem(parent);
  bool success;

  beginInsertRows(parent, position, position + rows - 1);
  success = parentItem->insertChildren(position, rows, LandingWidgetListItem::DefaultColumnCount);
  endInsertRows();

  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool LandingWidgetListModel::removeRows(int position, int rows, const QModelIndex& parent)
{
  LandingWidgetListItem* parentItem = getItem(parent);
  bool success = true;

  beginRemoveRows(parent, position, position + rows - 1);
  success = parentItem->removeChildren(position, rows);
  endRemoveRows();

  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool LandingWidgetListModel::moveRows(const QModelIndex& sourceParent, int sourceRow, int count, const QModelIndex& destinationParent, int destinationChild)
{
  beginMoveRows(sourceParent, sourceRow, sourceRow + count - 1, destinationParent, destinationChild);

  LandingWidgetListItem* srcParentItem = getItem(sourceParent);
  LandingWidgetListItem* destParentItem = getItem(destinationParent);

  for(int i = sourceRow; i < sourceRow + count; i++)
  {
    QModelIndex srcIndex = index(i, LandingWidgetListItem::DefaultColumn, sourceParent);
    LandingWidgetListItem* srcItem = getItem(srcIndex);

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
QModelIndex LandingWidgetListModel::parent(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return QModelIndex();
  }

  LandingWidgetListItem* childItem = getItem(index);
  LandingWidgetListItem* parentItem = childItem->parent();

  if(parentItem == m_RootItem)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->childNumber(), 0, parentItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int LandingWidgetListModel::rowCount(const QModelIndex& parent) const
{
  LandingWidgetListItem* parentItem = getItem(parent);

  return parentItem->childCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool LandingWidgetListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
  LandingWidgetListItem* item = getItem(index);
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
LandingWidgetListItem* LandingWidgetListModel::getRootItem()
{
  return m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool LandingWidgetListModel::isEmpty()
{
  if(rowCount(QModelIndex()) <= 0)
  {
    return true;
  }
  return false;
}
