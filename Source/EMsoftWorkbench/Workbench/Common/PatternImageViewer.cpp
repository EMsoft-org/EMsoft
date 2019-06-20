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

#include "PatternImageViewer.h"

#include <QtGui/QPainter>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternImageViewer::PatternImageViewer(QWidget *parent, Qt::WindowFlags windowFlags) :
  GLImageViewer(parent, windowFlags),
  m_HasKevValue(false),
  m_UseStatsOverlay(true)
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternImageViewer::~PatternImageViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternImageViewer::paintGL()
{
  GLImageViewer::paintGL();

  QPainter painter;
  painter.begin(this);
  painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);

  if (m_UseStatsOverlay)
  {
    // Get the minimum, maximum, and keV values as strings
    int statsStartingHeightOffset = 40;
    int statsHeightSpacing = 20;
    QString minStr = QObject::tr("Min: %1").arg(QString::number(m_MinValue, 'g', 4));
    QString maxStr = QObject::tr("Max: %1").arg(QString::number(m_MaxValue, 'g', 4));
    QString kevStr = "";
    if (m_HasKevValue)
    {
      kevStr = QObject::tr("keV: %1").arg(QString::number(m_keVValue));
      statsStartingHeightOffset = statsStartingHeightOffset + statsHeightSpacing;
    }

    // Figure out the length of the longest string
    int maxStrLen = minStr.size();
    if (maxStr.size() > maxStrLen)
    {
      maxStrLen = maxStr.size();
    }
    if (kevStr.size() > maxStrLen)
    {
      maxStrLen = kevStr.size();
    }

    int statsX = size().width() - (maxStrLen*8);

    painter.setPen(Qt::white);
    painter.fillRect(statsX - 10, size().height() - statsStartingHeightOffset - 20, maxStrLen*8 + 5, statsStartingHeightOffset + 10, QBrush(QColor(Qt::black)));
    painter.drawText(QPoint(statsX, size().height() - statsStartingHeightOffset), minStr);
    painter.drawText(QPoint(statsX, size().height() - statsStartingHeightOffset + statsHeightSpacing), maxStr);
    if (m_HasKevValue)
    {
      painter.drawText(QPoint(statsX, size().height() - statsStartingHeightOffset + (statsHeightSpacing*2)), kevStr);
    }
  }

  painter.end();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternImageViewer::loadImage(ImageData data)
{
  m_MinValue = data.minValue;
  m_MaxValue = data.maxValue;

  if (m_HasKevValue)
  {
    m_keVValue = data.keVValue;
  }

  GLImageViewer::loadImage(data.image);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternImageViewer::setHasKevValue(bool value)
{
  m_HasKevValue = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternImageViewer::getHasKevValue() const
{
  return m_HasKevValue;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternImageViewer::setUseStatsOverlay(const bool& value)
{
  m_UseStatsOverlay = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternImageViewer::getUseStatsOverlay() const
{
  return m_UseStatsOverlay;
}

