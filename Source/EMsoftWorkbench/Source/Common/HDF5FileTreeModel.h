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

#include <QtCore/QAbstractItemModel>
#include <QtCore/QModelIndex>
#include <QtCore/QVariant>
#include <QtWidgets/QFileIconProvider>

#include <hdf5.h>

class HDF5FileTreeModelItem;

class HDF5FileTreeModel : public QAbstractItemModel
{
  Q_OBJECT

public:
  HDF5FileTreeModel(hid_t fileId, QObject* parent = 0);
  ~HDF5FileTreeModel() override;

  enum Roles
  {
    HasErrorsRole = Qt::UserRole + 1
  };

  /**
   * @brief data
   * @param index
   * @param role
   * @return
   */
  QVariant data(const QModelIndex& index, int role) const override;

  /**
   * @brief QAbstractItemModel::setData
   * @param index
   * @param value
   * @param role
   * @return
   */
  bool setData(const QModelIndex& index, const QVariant& value, int role) override;

  /**
   * @brief flags
   * @param index
   * @return
   */
  Qt::ItemFlags flags(const QModelIndex& index) const override;

  /**
   * @brief headerData
   * @param section
   * @param orientation
   * @param role
   * @return
   */
  QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

  /**
   * @brief index
   * @param row
   * @param column
   * @param parent
   * @return
   */
  QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const override;

  /**
   * @brief parent
   * @param index
   * @return
   */
  QModelIndex parent(const QModelIndex& index) const override;

  /**
   * @brief rowCount
   * @param parent
   * @return
   */
  int rowCount(const QModelIndex& parent = QModelIndex()) const override;

  /**
   * @brief columnCount
   * @param parent
   * @return
   */
  int columnCount(const QModelIndex& parent = QModelIndex()) const override;

  /**
   * @brief indexToHDF5Path
   * @param index
   * @return
   */
  QString indexToHDF5Path(const QModelIndex& index);

  /**
   * @brief hdf5PathToIndex
   * @param hdf5Path
   * @return
   */
  QModelIndex hdf5PathToIndex(const QString& hdf5Path);

  /**
   * @brief hasChildren
   * @param parent
   * @return
   */
  bool hasChildren(const QModelIndex& parent) const override;

  /**
   * @brief getSelectedHDF5Paths
   * @return
   */
  QStringList getSelectedHDF5Paths();

  /**
   * @brief setOneSelectionOnly
   * @param value
   */
  void setOneSelectionOnly(bool value);

signals:
  void selectedHDF5PathsChanged();

private:
  HDF5FileTreeModelItem* m_RootItem;
  hid_t m_FileId;
  QFileIconProvider m_IconProvider;
  QList<QString> m_SelectedHDF5Paths;
  bool m_OneSelectionOnly = false;

  /**
   * @brief getItem
   * @param index
   * @return
   */
  HDF5FileTreeModelItem* getItem(const QModelIndex& index) const;

  /**
   * @brief setupModelData
   */
  void setupModelData();
};
