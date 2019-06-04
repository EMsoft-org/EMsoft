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

#include "QtSStyles.h"

#include <QtCore/QDebug>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QFile>
#include <QtCore/QTextStream>
#include <QtCore/QJsonValue>
#include <QtCore/QJsonArray>

#include <QtWidgets/QApplication>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>

//#include "SIMPLib/Common/Constants.h"

namespace SIMPL
{
namespace FilterGroups
{
  const QString CoreFilters("Core");
  const QString Generic("Generic");
  const QString IOFilters("IO");
  const QString ImageProcessing("Image Processing");
  const QString ProcessingFilters("Processing");
  const QString ReconstructionFilters("Reconstruction");
  const QString SamplingFilters("Sampling");
  const QString StatisticsFilters("Statistics");
  const QString SyntheticBuildingFilters("Synthetic Building");
  const QString SurfaceMeshingFilters("Surface Meshing");
  const QString Utilities("Utilities");
  const QString CustomFilters("Custom");
  const QString Unsupported("Unsupported");
}
}


namespace  {
const QString kNormalColor("#8f8f91");
const QString kErrorColor("#BC0000");
}

QString QtSStyles::currentStyleSheetName = "light";
QMap<QString, QColor>  QtSStyles::filterColorMap = QMap<QString, QColor>();
QMap<QString, QIcon>  QtSStyles::filterIconMap  = QMap<QString, QIcon>();

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSStyles::QtSStyles() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSStyles::~QtSStyles() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QFont QtSStyles::GetBrandingLabelFont()
{
  QFont brandingFont(GetUIFont());
  brandingFont.setBold(true);
  brandingFont.setItalic(true);
  brandingFont.setWeight(QFont::Bold);
  brandingFont.setStyleStrategy(QFont::PreferAntialias);
  brandingFont.setFamily(GetUIFont());

#if defined(Q_OS_MAC)
  brandingFont.setPointSize(11);
#elif defined(Q_OS_WIN)
  brandingFont.setPointSize(10);
#else
  brandingFont.setPointSize(10);
#endif
  return brandingFont;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSStyles::GetUIFont()
{
#if defined(Q_OS_MAC)
  QString fontString("FiraSans");
#elif defined(Q_OS_WIN)
  QString fontString("FiraSans");
#else
  QString fontString("Arial");
#endif

  QFont font(fontString);
  if(font.fromString(fontString))
  {
    return font.toString();
  }

  QFont blankFont;
  return blankFont.toString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor QtSStyles::ColorForFilterGroup(const QString &grpName)
{
  if (filterColorMap.count() == 0)
  {
    parseJson();
  }

  return filterColorMap.value(grpName);
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QIcon QtSStyles::IconForGroup(const QString &grpName)
{
  if (filterIconMap.count() == 0)
  {
    parseJson();
  }

  return filterIconMap.value(grpName);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStyles::setStyleSheetName(const QString &name)
{
    QtSStyles::currentStyleSheetName = name;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSStyles::styleSheetName()
{
    return currentStyleSheetName;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor QtSStyles::getPipelineFontColor()
{
  QColor color;
  if ( QtSStyles::currentStyleSheetName == "light")
  {
    color.setNamedColor("white");
  }
  else if ( QtSStyles::currentStyleSheetName == "dark")
  {
    color.setNamedColor("white");
  }
  else
  {
    color.setNamedColor("black");
  }

  return color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor QtSStyles::ColorForFilterState(QtSStyles::FilterState state)
{
  QColor color;
  if (state == QtSStyles::FilterComplete)
  {
    color.setNamedColor("green");
  }
  else if (state == QtSStyles::FilterError)
  {
    color.setNamedColor("red");
  }
  else if (state == QtSStyles::FilterProcessing)
  {
    color = QColor(0, 0xc0, 0xff);
  }
  else
  {
    color = QColor(0x6d, 0x7f, 0x89);
  }

  return color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSStyles::parseJson()
{
    QString jsonString;
    QFile jsonFile;
    jsonFile.setFileName(":Resources/FilterStyle/SVFilterColors.json");

    if(jsonFile.exists())
    {
        jsonFile.open(QIODevice::ReadOnly | QIODevice::Text);
        jsonString = jsonFile.readAll();
        jsonFile.close();

        QJsonDocument doc = QJsonDocument::fromJson(jsonString.toUtf8());
        QJsonObject jsonObj = doc.object();
        QJsonArray filterColorArray = jsonObj.value("Filter Group Properties").toArray();

        for(const QJsonValueRef &jsonValue : filterColorArray)
        {
            QJsonObject jsonObject  = jsonValue.toObject();
            QString filterKey = jsonObject.value("group").toString();
            QString filterColorString = jsonObject.value("color").toString();
            QColor filterColor;
            QIcon filterIcon(jsonObject.value("icon").toString());
            if(filterColorString != QString())
            {
                filterColor.setNamedColor(filterColorString);
            }
            else
            {
                filterColor.setNamedColor("black");
            }

            filterColorMap.insert(filterKey, filterColor);
            filterIconMap.insert(filterKey, filterIcon);
        }
    }

}
