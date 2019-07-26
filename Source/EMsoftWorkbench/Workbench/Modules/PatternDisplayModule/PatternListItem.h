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

#pragma once

#include <QtCore/QList>
#include <QtCore/QVariant>
#include <QtCore/QVector>

#include <QtGui/QIcon>


class PatternListItem
{
public:
  PatternListItem(QString name, PatternListItem* parent = nullptr);
  virtual ~PatternListItem();

  using EnumType = unsigned int;
  enum class PatternStatus : EnumType
  {
    WaitingToLoad,
    Priority,
    Loading,
    Loaded,
    Error
  };

  static const int DefaultColumn = 0;
  static const int DefaultColumnCount = 1;

    /**
    * @brief Setter property for ItemName
    */
    void setItemName(const QString& value); 

    /**
    * @brief Getter property for ItemName
    * @return Value of ItemName
    */
    QString getItemName() const;
    /**
    * @brief Setter property for PatternStatus
    */
    void setPatternStatus(const PatternListItem::PatternStatus& value); 

    /**
    * @brief Getter property for PatternStatus
    * @return Value of PatternStatus
    */
    PatternListItem::PatternStatus getPatternStatus() const;

  PatternListItem* child(int number) const;
  PatternListItem* parent() const;

  int childCount() const;

  QString getItemTooltip() const;
  bool setItemTooltip(const QString& value);

  QIcon getIcon() const;
  bool setIcon(const QIcon& icon);

  bool insertChild(int position, PatternListItem* child);
  bool insertChildren(int position, int count, int columns);

  bool removeChild(int position);
  bool removeChildren(int position, int count);

  int childNumber() const;

  void setParent(PatternListItem* parent);

private:
    QString m_ItemName;
    PatternListItem::PatternStatus m_PatternStatus;

  QList<PatternListItem*> m_ChildItems;
  PatternListItem* m_ParentItem = nullptr;
  QString m_ItemTooltip = "";
  QIcon m_Icon;

public:
  PatternListItem(const PatternListItem&) = delete;            // Copy Constructor Not Implemented
  PatternListItem(PatternListItem&&) = delete;                 // Move Constructor Not Implemented
  PatternListItem& operator=(const PatternListItem&) = delete; // Copy Assignment Not Implemented
  PatternListItem& operator=(PatternListItem&&) = delete;      // Move Assignment Not Implemented
};
