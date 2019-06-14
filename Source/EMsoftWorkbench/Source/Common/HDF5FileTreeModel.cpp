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

#include <QtGui/QIcon>
#include <QtGui/QPixmap>

#include "H5Support/H5Utilities.h"

#include "HDF5FileTreeModel.h"
#include "HDF5FileTreeModelItem.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModel::HDF5FileTreeModel(hid_t fileId, QObject* parent)
: QAbstractItemModel(parent)
, m_FileId(fileId)
{
  m_RootItem = new HDF5FileTreeModelItem(m_FileId, "HEADER");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModel::~HDF5FileTreeModel()
{
  delete m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModel::columnCount(const QModelIndex& parent) const
{
  if(parent.isValid())
  {
    return static_cast<HDF5FileTreeModelItem*>(parent.internalPointer())->columnCount();
  }
  {
    return m_RootItem->columnCount();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant HDF5FileTreeModel::data(const QModelIndex& index, int role) const
{
  if(!index.isValid())
  {
    return QVariant();
  }

  HDF5FileTreeModelItem* item = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());

  if(role == Qt::DecorationRole)
  {
    return item->icon();
  }
  if(role == Qt::DisplayRole)
  {
    return item->data(index.column());
  }
  if(role == Qt::CheckStateRole && !item->isGroup())
  {
    return item->getCheckState();
  }
  if(role == Qt::ForegroundRole)
  {
    bool itemHasErrors = item->getHasErrors();
    if(itemHasErrors)
    {
      return QColor(Qt::red);
    }

    return QVariant();
  }

  return QVariant();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool HDF5FileTreeModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
  if(index.column() != 0)
  {
    return false;
  }

  HDF5FileTreeModelItem* item = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());

  if(role == Qt::CheckStateRole && !item->isGroup())
  {
    Qt::CheckState checkState = static_cast<Qt::CheckState>(value.toInt());
    item->setCheckState(checkState);
    QString hdf5Path = item->generateHDFPath();
    if(checkState == Qt::Checked)
    {
      if (m_OneSelectionOnly && !m_SelectedHDF5Paths.isEmpty())
      {
        while (!m_SelectedHDF5Paths.isEmpty())
        {
          QString selectedPath = m_SelectedHDF5Paths[0];
          QModelIndex selectedIndex = hdf5PathToIndex(selectedPath);
          setData(selectedIndex, Qt::Unchecked, Qt::CheckStateRole);
        }
      }

      m_SelectedHDF5Paths.push_back(hdf5Path);
    }
    else if(checkState == Qt::Unchecked)
    {
      m_SelectedHDF5Paths.removeAll(hdf5Path);
    }

    emit selectedHDF5PathsChanged(m_SelectedHDF5Paths);
  }
  else if(role == Roles::HasErrorsRole)
  {
    item->setHasErrors(value.toBool());
  }

  emit dataChanged(index, index);

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
Qt::ItemFlags HDF5FileTreeModel::flags(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return nullptr;
  }

  HDF5FileTreeModelItem* item = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());

  Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

  if(!item->isGroup())
  {
    flags = flags | Qt::ItemIsUserCheckable;
  }

  return flags;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant HDF5FileTreeModel::headerData(int section, Qt::Orientation orientation, int role) const
{
  if(orientation == Qt::Horizontal && role == Qt::DisplayRole)
  {
    return QVariant("HEADER");
    // return rootItem->data(section);
  }
  return QVariant();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex HDF5FileTreeModel::index(int row, int column, const QModelIndex& parent) const
{
  if(!hasIndex(row, column, parent))
  {
    return {};
  }

  HDF5FileTreeModelItem* parentItem;

  if(!parent.isValid())
  {
    parentItem = m_RootItem;
  }
  else
  {
    parentItem = static_cast<HDF5FileTreeModelItem*>(parent.internalPointer());
  }

  HDF5FileTreeModelItem* childItem = parentItem->child(row);
  if(childItem != nullptr)
  {
    return createIndex(row, column, childItem);
  }
  {
    return {};
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex HDF5FileTreeModel::parent(const QModelIndex& index) const
{
  if(!index.isValid())
  {
    return {};
  }

  HDF5FileTreeModelItem* childItem = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());
  HDF5FileTreeModelItem* parentItem = childItem->parent();

  if(parentItem == m_RootItem)
  {
    return {};
  }

  return createIndex(parentItem->row(), 0, parentItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool HDF5FileTreeModel::hasChildren(const QModelIndex& parent) const
{
  if(parent.column() > 0)
  {
    return false;
  }

  HDF5FileTreeModelItem* parentItem;
  if(!parent.isValid())
  {
    parentItem = m_RootItem;
  }
  else
  {
    parentItem = static_cast<HDF5FileTreeModelItem*>(parent.internalPointer());
  }

  return parentItem->isGroup();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModel::rowCount(const QModelIndex& parent) const
{
  HDF5FileTreeModelItem* parentItem;
  if(parent.column() > 0)
  {
    return 0;
  }

  if(!parent.isValid())
  {
    parentItem = m_RootItem;
  }
  else
  {
    parentItem = static_cast<HDF5FileTreeModelItem*>(parent.internalPointer());
  }

  return parentItem->childCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModel::setupModelData()
{
  if(m_FileId < 0)
  {
    return;
  }

  m_RootItem->appendChild(new HDF5FileTreeModelItem(m_FileId, QString("/"), m_RootItem));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString HDF5FileTreeModel::indexToHDF5Path(const QModelIndex& index)
{
  HDF5FileTreeModelItem* item = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());
  return item->generateHDFPath();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QModelIndex HDF5FileTreeModel::hdf5PathToIndex(const QString& hdf5Path)
{
  QStringList hdf5PathTokens = hdf5Path.split("/", QString::SplitBehavior::SkipEmptyParts);
  if(!hdf5PathTokens.empty())
  {
    QModelIndex rootIndex = index(0, 0);
    QModelIndex startIndex = index(0, 0, rootIndex);
    QModelIndex foundIndex;
    for(int i = 0; i < hdf5PathTokens.size(); i++)
    {
      QString hdf5PathToken = hdf5PathTokens[i];
      QModelIndexList indexList = match(startIndex, Qt::DisplayRole, hdf5PathToken);
      if(indexList.size() == 1)
      {
        foundIndex = indexList[0];
        startIndex = index(0, 0, foundIndex);
      }
      else
      {
        return {};
      }
    }

    return foundIndex;
  }

  return {};
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModelItem* HDF5FileTreeModel::getItem(const QModelIndex& index) const
{
  if(index.isValid())
  {
    HDF5FileTreeModelItem* item = static_cast<HDF5FileTreeModelItem*>(index.internalPointer());
    if(item != nullptr)
    {
      return item;
    }
  }
  return m_RootItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList HDF5FileTreeModel::getSelectedHDF5Paths()
{
  return m_SelectedHDF5Paths;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModel::setOneSelectionOnly(bool value)
{
  m_OneSelectionOnly = value;
}
