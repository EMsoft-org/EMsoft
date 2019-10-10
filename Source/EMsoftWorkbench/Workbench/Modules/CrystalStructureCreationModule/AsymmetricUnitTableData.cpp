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

#include "AsymmetricUnitTableData.h"

#include <QtCore/QJsonArray>
#include <QtCore/QJsonObject>
#include <QtCore/QTextStream>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::AsymmetricUnitTableData()
: m_DynamicRows(false)
, m_DynamicCols(false)
, m_MinRows(0)
, m_MinCols(0)
, m_DefaultRowCount(0)
, m_DefaultColCount(0)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::AsymmetricUnitTableData(int nRows, int nCols)
: m_DynamicRows(false)
, m_DynamicCols(false)
, m_MinRows(0)
, m_MinCols(0)
, m_DefaultRowCount(0)
, m_DefaultColCount(0)
{
  std::vector<std::vector<double>> data(nRows, std::vector<double>(nCols, 0));
  m_TableData = data;

  QStringList rHeaders, cHeaders;

  for(int i = 0; i < nRows; i++)
  {
    rHeaders << QString::number(i);
  }

  for(int i = 0; i < nCols; i++)
  {
    cHeaders << QString::number(i);
  }

  m_RowHeaders = rHeaders;
  m_ColHeaders = cHeaders;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::AsymmetricUnitTableData(int nRows, int nCols, const QStringList& rHeaders, const QStringList& cHeaders)
: m_DynamicRows(false)
, m_DynamicCols(false)
, m_MinRows(0)
, m_MinCols(0)
, m_DefaultRowCount(0)
, m_DefaultColCount(0)
{
  std::vector<std::vector<double>> data(nRows, std::vector<double>(nCols, 0));
  m_TableData = data;

  m_RowHeaders = rHeaders;
  m_ColHeaders = cHeaders;

  // Adjust dimensions if they are not all the same
  checkAndAdjustDimensions();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::AsymmetricUnitTableData(const std::vector<std::vector<double>>& data, const QStringList& rHeaders, const QStringList& cHeaders)
: m_DynamicRows(false)
, m_DynamicCols(false)
, m_MinRows(0)
, m_MinCols(0)
, m_DefaultRowCount(0)
, m_DefaultColCount(0)
{
  m_TableData = data;
  m_RowHeaders = rHeaders;
  m_ColHeaders = cHeaders;

  // Adjust dimensions if they are not all the same
  checkAndAdjustDimensions();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::~AsymmetricUnitTableData() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::isEmpty() const
{
  return (!m_TableData.empty() || !m_RowHeaders.empty() || !m_ColHeaders.empty());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::checkAndAdjustDimensions()
{
  QSize dataSize(static_cast<int32_t>(m_TableData.size()), 0);
  QSize headerSize(m_RowHeaders.size(), m_ColHeaders.size());

  if(!m_TableData.empty())
  {
    dataSize.setHeight(static_cast<int32_t>(m_TableData[0].size()));
  }

  if(dataSize == headerSize)
  {
    return;
  }

  /* The header dimensions do not equal the data dimensions.
       The data dimensions will be used and will overwrite the current header dimensions.
       This may result in data loss.
    */
  int nRows = dataSize.width();
  int nCols = dataSize.height();

  // If row header dimension is greater than default row dimension, remove the extra headers
  if(m_RowHeaders.size() > nRows)
  {
    while(m_RowHeaders.size() > nRows)
    {
      m_RowHeaders.pop_back();
    }
  }
  // If column header dimension is greater than default column dimension, remove the extra headers
  if(m_ColHeaders.size() > nCols)
  {
    while(m_ColHeaders.size() > nCols)
    {
      m_ColHeaders.pop_back();
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString AsymmetricUnitTableData::serializeData(char delimiter) const
{
  QString str;
  QTextStream ss(&str);

  for(const auto& rowIter : m_TableData)
  {
    for(double value : rowIter)
    {
      ss << value << delimiter;
    }
  }
  str.chop(1); // Get rid of the last, unnecessary delimiter

  return str;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<std::vector<double>> AsymmetricUnitTableData::DeserializeData(const QString& dataStr, int nRows, int nCols, char delimiter)
{
  std::vector<std::vector<double>> data(nRows, std::vector<double>(nCols));
  int row = 0, col = 0;

  if(dataStr.isEmpty())
  {
    return data;
  }

  int start = 0;
  int tokenIndex = 0;

  while(tokenIndex >= 0)
  {
    tokenIndex = dataStr.indexOf(delimiter, start);
    QString valueStr = dataStr.mid(start, tokenIndex - start);
    double value = valueStr.toDouble();
    data[row][col] = value;
    start = tokenIndex + 1;

    col++;
    if(col == nCols)
    {
      row++;
      col = 0;
    }
  }

  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString AsymmetricUnitTableData::serializeRowHeaders(char delimiter) const
{
  QString str = "";
  QTextStream ss(&str);

  for(int i = 0; i < m_RowHeaders.size(); i++)
  {
    ss << m_RowHeaders[i];
    ss << delimiter;
  }
  str.chop(1); // Get rid of the last, unnecessary delimiter

  return str;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString AsymmetricUnitTableData::serializeColumnHeaders(char delimiter) const
{
  QString str = "";
  QTextStream ss(&str);

  for(int i = 0; i < m_ColHeaders.size(); i++)
  {
    ss << m_ColHeaders[i];
    ss << delimiter;
  }
  str.chop(1); // Get rid of the last, unnecessary delimiter

  return str;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList AsymmetricUnitTableData::DeserializeHeaders(const QString& headersStr, char delimiter)
{
  QStringList headers;

  if(headersStr.isEmpty())
  {
    return headers;
  }

  int start = 0;
  int tokenIndex = 0;

  while(tokenIndex >= 0)
  {
    tokenIndex = headersStr.indexOf(delimiter, start);
    QString header = headersStr.mid(start, tokenIndex);
    headers.push_back(header);
    start = tokenIndex + 1;
  }

  return headers;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVector<double> AsymmetricUnitTableData::flattenData() const
{
  int numRows = getNumRows();
  int numCols = getNumCols();

  QVector<double> flat(numRows * numCols);
  for(int row = 0; row < numRows; row++)
  {
    for(int col = 0; col < numCols; col++)
    {
      flat[row * numCols + col] = m_TableData[row][col];
    }
  }

  return flat;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<std::vector<double>> AsymmetricUnitTableData::ExpandData(std::vector<double> orig, int nRows, int nCols)
{
  std::vector<std::vector<double>> expand(nRows, std::vector<double>(nCols));

  if(orig.size() != nRows * nCols)
  {
    // Something went wrong
    return expand;
  }

  for(int row = 0; row < nRows; row++)
  {
    for(int col = 0; col < nCols; col++)
    {
      expand[row][col] = orig[row * nCols + col];
    }
  }

  return expand;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::writeJson(QJsonObject& json) const
{
  writeData(json);

  QJsonArray rHeaders;
  foreach(QString header, m_RowHeaders)
  {
    rHeaders.push_back(header);
  }
  json["Row Headers"] = rHeaders;

  QJsonArray cHeaders;
  foreach(QString header, m_ColHeaders)
  {
    cHeaders.push_back(header);
  }
  json["Column Headers"] = cHeaders;

  json["HasDynamicRows"] = m_DynamicRows;
  json["HasDynamicCols"] = m_DynamicCols;
  json["MinRowCount"] = m_MinRows;
  json["MinColCount"] = m_MinCols;
  json["DefaultRowCount"] = m_DefaultRowCount;
  json["DefaultColCount"] = m_DefaultColCount;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::readJson(QJsonObject& json)
{
  if(json.contains("Dynamic Table Data"))
  {
    QJsonObject obj = json["Dynamic Table Data"].toObject();
    m_TableData = readData(obj);
  }
  else
  {
    m_TableData = readData(json);
  }

  QJsonArray rHeaders = json["Row Headers"].toArray();
  foreach(QJsonValue val, rHeaders)
  {
    if(val.isString())
    {
      m_RowHeaders.push_back(val.toString());
    }
  }

  QJsonArray cHeaders = json["Column Headers"].toArray();
  foreach(QJsonValue val, cHeaders)
  {
    if(val.isString())
    {
      m_ColHeaders.push_back(val.toString());
    }
  }

  m_DynamicRows = json["HasDynamicRows"].toBool();
  m_DynamicCols = json["HasDynamicCols"].toBool();
  m_MinRows = json["MinRowCount"].toInt();
  m_MinCols = json["MinColCount"].toInt();
  m_DefaultRowCount = json["DefaultRowCount"].toInt();
  m_DefaultColCount = json["DefaultColCount"].toInt();

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::writeData(QJsonObject& object) const
{
  QJsonArray rows;
  foreach(std::vector<double> vector, m_TableData)
  {
    QJsonArray cols;
    foreach(double val, vector)
    {
      cols.push_back(val);
    }
    rows.push_back(cols);
  }

  object["Table Data"] = rows;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<std::vector<double>> AsymmetricUnitTableData::readData(QJsonObject object) const
{
  std::vector<std::vector<double>> data;
  if(object["Table Data"].isArray())
  {
    QJsonArray rowArray = object["Table Data"].toArray();
    data.resize(rowArray.size());

    for(int row = 0; row < rowArray.size(); row++)
    {
      QJsonValue rowObj = rowArray.at(row);
      if(rowObj.isArray())
      {
        QJsonArray colArray = rowObj.toArray();
        data[row].resize(colArray.size());

        for(int col = 0; col < colArray.size(); col++)
        {
          QJsonValue colObj = colArray.at(col);

          if(colObj.isDouble())
          {
            data[row][col] = colObj.toDouble();
          }
        }
      }
    }
    return data;
  }
  return std::vector<std::vector<double>>();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<std::vector<double>> AsymmetricUnitTableData::getTableData() const
{
  return m_TableData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setTableData(const std::vector<std::vector<double>>& data)
{
  m_TableData = data;

  // Adjust dimensions
  checkAndAdjustDimensions();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getNumRows() const
{
  return static_cast<int32_t>(m_TableData.size());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getNumCols() const
{
  if(!m_TableData.empty())
  {
    return static_cast<int32_t>(m_TableData[0].size());
  }

  return 0;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AsymmetricUnitTableData::AsymmetricUnitTableData(const AsymmetricUnitTableData& rhs)
{
  m_TableData = rhs.m_TableData;
  m_RowHeaders = rhs.m_RowHeaders;
  m_ColHeaders = rhs.m_ColHeaders;
  m_DynamicRows = rhs.m_DynamicRows;
  m_DynamicCols = rhs.m_DynamicCols;
  m_MinRows = rhs.m_MinRows;
  m_MinCols = rhs.m_MinCols;
  m_DefaultRowCount = rhs.m_DefaultRowCount;
  m_DefaultColCount = rhs.m_DefaultColCount;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::operator=(const AsymmetricUnitTableData& rhs)
{
  m_TableData = rhs.m_TableData;
  m_RowHeaders = rhs.m_RowHeaders;
  m_ColHeaders = rhs.m_ColHeaders;
  m_DynamicRows = rhs.m_DynamicRows;
  m_DynamicCols = rhs.m_DynamicCols;
  m_MinRows = rhs.m_MinRows;
  m_MinCols = rhs.m_MinCols;
  m_DefaultRowCount = rhs.m_DefaultRowCount;
  m_DefaultColCount = rhs.m_DefaultColCount;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::operator==(const AsymmetricUnitTableData& rhs) const
{
  if(m_RowHeaders == rhs.m_RowHeaders && m_ColHeaders == rhs.m_ColHeaders && m_DynamicRows == rhs.m_DynamicRows && m_DynamicCols == rhs.m_DynamicCols && m_MinRows == rhs.m_MinRows &&
     m_MinCols == rhs.m_MinCols && m_DefaultRowCount == rhs.m_DefaultRowCount && m_DefaultColCount == rhs.m_DefaultColCount)
  {
    for(size_t i = 0; i < m_TableData.size(); i++)
    {
      for(size_t j = 0; j < m_TableData[i].size(); j++)
      {
        if(m_TableData[i][j] != rhs.m_TableData[i][j])
        {
          return false;
        }
      }
    }
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::operator!=(const AsymmetricUnitTableData& rhs) const
{
  if(m_RowHeaders == rhs.m_RowHeaders && m_ColHeaders == rhs.m_ColHeaders && m_DynamicRows == rhs.m_DynamicRows && m_DynamicCols == rhs.m_DynamicCols && m_MinRows == rhs.m_MinRows &&
     m_MinCols == rhs.m_MinCols && m_DefaultRowCount == rhs.m_DefaultRowCount && m_DefaultColCount == rhs.m_DefaultColCount)
  {
    for(size_t i = 0; i < m_TableData.size(); i++)
    {
      for(size_t j = 0; j < m_TableData[i].size(); j++)
      {
        if(m_TableData[i][j] != rhs.m_TableData[i][j])
        {
          return true;
        }
      }
    }
  }
  else
  {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setColHeaders(const QStringList& value)
{
  m_ColHeaders = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList AsymmetricUnitTableData::getColHeaders() const
{
  return m_ColHeaders;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setRowHeaders(const QStringList& value)
{
  m_RowHeaders = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList AsymmetricUnitTableData::getRowHeaders() const
{
  return m_RowHeaders;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setDynamicRows(const bool& value)
{
  m_DynamicRows = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::getDynamicRows() const
{
  return m_DynamicRows;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setDynamicCols(const bool& value)
{
  m_DynamicCols = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AsymmetricUnitTableData::getDynamicCols() const
{
  return m_DynamicCols;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setMinRows(const int& value)
{
  m_MinRows = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getMinRows() const
{
  return m_MinRows;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setMinCols(const int& value)
{
  m_MinCols = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getMinCols() const
{
  return m_MinCols;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setDefaultRowCount(const int& value)
{
  m_DefaultRowCount = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getDefaultRowCount() const
{
  return m_DefaultRowCount;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AsymmetricUnitTableData::setDefaultColCount(const int& value)
{
  m_DefaultColCount = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AsymmetricUnitTableData::getDefaultColCount() const
{
  return m_DefaultColCount;
}

