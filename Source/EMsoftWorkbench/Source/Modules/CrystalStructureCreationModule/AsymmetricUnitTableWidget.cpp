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

#include "AsymmetricUnitTableWidget.h"

#include <QtCore/QMetaProperty>
#include <QtGui/QFontMetrics>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QScrollBar>

#include "Modules/CrystalStructureCreationModule/AsymmetricUnitTableData.h"
#include "Modules/CrystalStructureCreationModule/AsymmetricUnitTableItemDelegate.h"

const QString addRowTT = "Adds a row to the table.";
const QString deleteRowTT = "Removes the currently selected row from the table.";

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableWidget::AsymmetricUnitTableWidget(QWidget* parent)
: QFrame(parent)
{
  setupUi(this);
  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableWidget::~AsymmetricUnitTableWidget()
{
  delete m_ItemDelegate;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::setupGui()
{
  dynamicTable->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);

  // Set the item delegate so that we can only enter 'double' values into the table
  m_ItemDelegate = new AsymmetricUnitTableItemDelegate;
  dynamicTable->setItemDelegate(m_ItemDelegate);

  // Set button tooltips
  addRowBtn->setToolTip(addRowTT);
  deleteRowBtn->setToolTip(deleteRowTT);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::on_dynamicTable_cellChanged(int row, int col) const
{
  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<std::vector<double>> AsymmetricUnitTableWidget::getData() const
{
  int rCount = dynamicTable->rowCount(), cCount = dynamicTable->columnCount();
  std::vector<std::vector<double>> data(rCount, std::vector<double>(cCount));

  for(int row = 0; row < rCount; row++)
  {
    for(int col = 0; col < cCount; col++)
    {
      bool ok = false;
      QTableWidgetItem* item = dynamicTable->item(row, col);
      if(nullptr == item)
      {
        return std::vector<std::vector<double>>();
      }
      data[row][col] = item->data(Qt::DisplayRole).toDouble(&ok);

      if(!ok)
      {
        qDebug() << "Could not set the model data into the DynamicTableData object.";
        data.clear();
        return data;
      }
    }
  }

  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::on_addRowBtn_clicked() const
{
  int row = dynamicTable->rowCount();

  std::vector<double> columnData;
  int columnCount = dynamicTable->columnCount();
  columnData.reserve(columnCount);
  for(int i = 0; i < columnCount; i++)
  {
    columnData[i] = 0;
  }

  insertRow(row, columnData);

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::insertRow(int idx, std::vector<double> columnData) const
{
  if(columnData.size() != dynamicTable->columnCount())
  {
    return;
  }

  // If we are adding the first row, add the first column too.
  if(idx <= 0 && dynamicTable->columnCount() <= 0)
  {
    dynamicTable->insertColumn(0);
    dynamicTable->setHorizontalHeaderItem(0, new QTableWidgetItem("0"));
  }

  dynamicTable->insertRow(idx);
  dynamicTable->setVerticalHeaderItem(idx, new QTableWidgetItem(QString::number(dynamicTable->rowCount() - 1)));

  dynamicTable->blockSignals(true);
  for(int col = 0; col < dynamicTable->columnCount(); col++)
  {
    if(col + 1 == dynamicTable->columnCount())
    {
      // Only fire the signal after the last new item has been created
      dynamicTable->blockSignals(false);
    }

    QTableWidgetItem* item = new QTableWidgetItem(QString::number(columnData[col]));
    item->setTextAlignment(Qt::AlignCenter);
    dynamicTable->setItem(idx, col, item);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::on_deleteRowBtn_clicked() const
{
  dynamicTable->removeRow(dynamicTable->currentRow());

  emit parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::readSession(QJsonObject& obj) const
{
  while(dynamicTable->rowCount() > 0)
  {
    dynamicTable->removeRow(0);
  }

  AsymmetricUnitTableData tableData;
  tableData.readJson(obj);

  std::vector<std::vector<double>> data = tableData.getTableData();
  for(int i = 0; i < data.size(); i++)
  {
    std::vector<double> colData = data[i];
    insertRow(i, colData);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableWidget::writeSession(QJsonObject& obj) const
{
  AsymmetricUnitTableData tableData;
  tableData.setTableData(getData());
  tableData.writeJson(obj);
}
