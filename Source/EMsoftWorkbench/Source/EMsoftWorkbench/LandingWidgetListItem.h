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

#ifndef _LandingWidgetListItem_h_
#define _LandingWidgetListItem_h_

#include <QtCore/QList>
#include <QtCore/QVariant>
#include <QtCore/QVector>

#include <QtGui/QIcon>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

class LandingWidgetListItem
{
  public:
    LandingWidgetListItem(const QString &name, const QString &path = QString(), LandingWidgetListItem* parent = 0);
    virtual ~LandingWidgetListItem();

    using EnumType = unsigned int;

    enum class ItemType : EnumType
    {
      Root,
      Header,
      Open,
      Recent
    };

    static const int DefaultColumn = 0;
    static const int DefaultColumnCount = 1;

    SIMPL_INSTANCE_PROPERTY(LandingWidgetListItem::ItemType, ItemType)
    SIMPL_INSTANCE_PROPERTY(QString, ItemName)
    SIMPL_INSTANCE_PROPERTY(QString, ItemParentPath)
    SIMPL_INSTANCE_PROPERTY(QString, ItemPath)

    LandingWidgetListItem* child(int number);
    LandingWidgetListItem* parent();

    int childCount() const;

    QString getItemTooltip();
    bool setItemTooltip(const QString& value);

    QIcon getIcon();
    bool setIcon(const QIcon& icon);

    bool insertChild(int position, LandingWidgetListItem* child);
    bool insertChildren(int position, int count, int columns);

    bool removeChild(int position);
    bool removeChildren(int position, int count);

    int childNumber() const;

    void setParent(LandingWidgetListItem* parent);

  private:
    QList<LandingWidgetListItem*>               m_ChildItems;
    LandingWidgetListItem*                      m_ParentItem;
    QString                                     m_ItemTooltip;
    QIcon                                       m_Icon;

    LandingWidgetListItem(const LandingWidgetListItem&);    // Copy Constructor Not Implemented
    void operator=(const LandingWidgetListItem&);  // Operator '=' Not Implemented
};

#endif // LandingWidgetListItem_H
