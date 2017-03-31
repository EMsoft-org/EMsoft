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

#ifndef _landingwidgetlistmodel_h_
#define _landingwidgetlistmodel_h_

#include <QtCore/QAbstractListModel>
#include <QtCore/QModelIndex>
#include <QtCore/QVariant>
#include <QtCore/QItemSelection>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

#include "EMsoftWorkbench/LandingWidgetListItem.h"

class QtSSettings;
class LandingWidgetListItem;

class LandingWidgetListModel : public QAbstractListModel
{
    Q_OBJECT

  public:
    SIMPL_TYPE_MACRO(LandingWidgetListModel)

    LandingWidgetListModel(QObject* parent = 0);
    ~LandingWidgetListModel();

    QVariant data(const QModelIndex& index, int role) const Q_DECL_OVERRIDE;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const Q_DECL_OVERRIDE;

    QString itemName(const QModelIndex &index);
    QString itemParentPath(const QModelIndex &index);
    QString itemPath(const QModelIndex &index);

    bool isEmpty();

    QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const Q_DECL_OVERRIDE;
    QModelIndex parent(const QModelIndex& index) const Q_DECL_OVERRIDE;

    int rowCount(const QModelIndex& parent = QModelIndex()) const Q_DECL_OVERRIDE;

    bool insertRows(int position, int rows, const QModelIndex& parent = QModelIndex()) Q_DECL_OVERRIDE;
    bool removeRows(int position, int rows, const QModelIndex& parent = QModelIndex()) Q_DECL_OVERRIDE;

    bool moveRows(const QModelIndex & sourceParent, int sourceRow, int count, const QModelIndex & destinationParent, int destinationChild) Q_DECL_OVERRIDE;

    Qt::ItemFlags flags(const QModelIndex& index) const Q_DECL_OVERRIDE;

    bool setData(const QModelIndex& index, const QVariant& value, int role) Q_DECL_OVERRIDE;

    LandingWidgetListItem* getRootItem();

    LandingWidgetListItem* insertRecentItem(const int row, const QString &path);
    void removeRecentItem(const int row);
    void removeAllRecents();

    LandingWidgetListItem* insertHeaderItem(const int row, const QString &name);

  private:
    LandingWidgetListItem*            m_RootItem;
    LandingWidgetListItem*            m_RecentsItem;

    LandingWidgetListItem* getItem(const QModelIndex& index) const;

    QModelIndex getIndex(const LandingWidgetListItem* item) const;

    LandingWidgetListItem* insertItem(const int row, const QString &displayName, const QString &displayPath, const QString &fullPath, LandingWidgetListItem::ItemType itemType, LandingWidgetListItem* headerItem = nullptr);
    void removeItem(const int row, LandingWidgetListItem* headerItem = nullptr);

    LandingWidgetListModel(const LandingWidgetListModel&);    // Copy Constructor Not Implemented
    void operator=(const LandingWidgetListModel&);  // Operator '=' Not Implemented
};

#endif // _landingwidgetlistmodel_h_
