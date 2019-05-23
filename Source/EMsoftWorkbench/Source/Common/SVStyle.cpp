/* ============================================================================
* Copyright (c) 2018 BlueQuartz Software, LLC
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

#include "SVStyle.h"

#include <QtCore/QDebug>
#include <QtCore/QMetaProperty>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>
#include <QtCore/QJsonValue>

#include <QtWidgets/QApplication>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>

#include "SIMPLib/Common/Constants.h"

static QMap<QString, QImage> s_NameToImage;

SVStyle* SVStyle::self = nullptr;

namespace  {
const QString kNormalColor("#8f8f91");
const QString kErrorColor("#BC0000");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVStyle::SVStyle()
{
  Q_ASSERT_X(!self, "SVStyle", "There should be only one SVStyle object");
  SVStyle::self = this;

  const QMetaObject* metaObj = metaObject();
  for (int i = metaObj->propertyOffset(); i < metaObj->propertyCount(); ++i)
  {
    QMetaProperty property = metaObj->property(i);
    if (property.type() == QVariant::Color)
    {
      m_ColorProperties.push_back(property.name());
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVStyle::~SVStyle() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SVStyle* SVStyle::Instance()
{
  if(self == nullptr)
  {
    self = new SVStyle();
  }
  return self;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::invalidateColorProperties()
{
  for (int i = 0; i < m_ColorProperties.size(); i++)
  {
    QString colorPropertyName = m_ColorProperties[i];
    setProperty(colorPropertyName.toStdString().c_str(), QColor());
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SVStyle::loadStyleSheet(const QString &jsonFilePath)
{
//  qDebug() << "SVStyle::loadStyleSheet() " << jsonFilePath;
  invalidateColorProperties();

  bool success = true;
  
  QFileInfo jsonFileInfo(jsonFilePath);
  
  QFile jsonFile(jsonFilePath);
  if(!jsonFile.open(QFile::ReadOnly))
  {
    qDebug() << "Could not open JSON File " << jsonFilePath;
    return false; 
  }
  
  QByteArray jsonContent = jsonFile.readAll();
  QJsonParseError parseError;
  QJsonDocument jsonDoc = QJsonDocument::fromJson(jsonContent, &parseError);
  if(parseError.error != QJsonParseError::NoError)
  {
    qDebug() << "JSON Parsing Error:";
    qDebug() << parseError.errorString();
    return false;
  }
  QJsonObject rootObj = jsonDoc.object();
  
  // Create the CSS File Path and try to read the CSS template file
  QString cssFileName = rootObj["CSS_File_Name"].toString();
  cssFileName = QString("%1/%2").arg(jsonFileInfo.absolutePath(), 1).arg(cssFileName, 2);
  
  QFile cssFile(cssFileName);
  if(!cssFile.open(QFile::ReadOnly))
  {
    qDebug() << "Could not open CSS File " << cssFileName;
    return false; 
  }
  QString cssContent = QString::fromLatin1(cssFile.readAll());
  
  // Read the variable mapping from the JSON file
  QJsonObject varMapping = rootObj["Named_Variables"].toObject();
  
  // Get the CSS Replacements that need to be made
  QJsonObject cssRepl = rootObj["CSS_Replacements"].toObject();
  QStringList keys = cssRepl.keys();
  QStringList::const_iterator constIterator;
  for (constIterator = keys.constBegin(); constIterator != keys.constEnd(); ++constIterator)
  {
    const QString key = *constIterator;

    QString value = loadStringProperty(key, cssRepl, varMapping);
    if (!value.isNull())
    {
      // Do the replacement
      cssContent = cssContent.replace(key, value);

      if(value.startsWith("rgb"))
      {
        bool ok = false;
        QString tokenString = value;
        tokenString = tokenString.replace("rgb(", "");
        tokenString = tokenString.replace(")", "");
        tokenString = tokenString.replace(" ", "");
        QStringList tokens = tokenString.split(",");
        if(tokens.size() == 3)
        {
          int r = tokens[0].toInt(&ok);
          int g = tokens[1].toInt(&ok);
          int b = tokens[2].toInt(&ok);
          bool didSet = this->setProperty(key.toLocal8Bit().constData(), QColor(r, g, b));
          if(!didSet)
          {
            qDebug() << "Property: " << key << " was not set correctly";
          }
        }
      }
      else
      {
        this->setProperty( key.toLocal8Bit().constData(), QColor(value));
      }
    }
  }
  
  //
  //
  keys.clear();
  keys << "FilterBackgroundColor"  << "FilterSelectionColor" << "FilterFontColor";
  for (constIterator = keys.constBegin(); constIterator != keys.constEnd(); ++constIterator)
  {
    const QString key = *constIterator;
    QString value = rootObj[key].toString();
    // First see if it is a varible and if it is, then get the real value from
    // the Named_Variables section
    if(varMapping.contains(value))
    {
      value = varMapping[value].toString();
    }
    
    if(value.startsWith("#"))
    {
      this->setProperty( key.toLocal8Bit().constData(), QColor(value));
    }
    else
    {
      bool ok = false;
      value = value.replace("rgb(", "");
      value = value.replace(")", "");
      value = value.replace(" ", "");
      QStringList tokens = value.split(",");
      if(tokens.size() == 3)
      {
        int r = tokens[0].toInt(&ok);
        int g = tokens[1].toInt(&ok);
        int b = tokens[2].toInt(&ok);
        bool didSet = this->setProperty( key.toLocal8Bit().constData(), QColor(r, g, b));
        if(!didSet)
        {
          qDebug() << "Property: " << key << " was not set correctly";
        }
      }
    }
  }
  
  keys.clear();

  // Get the CSS Font replacements that need to be made
  QJsonObject fontRepl = rootObj["Font_Replacements"].toObject();
  keys = fontRepl.keys();
  #if defined(Q_OS_MAC)
  QString os("macOS");
#elif defined(Q_OS_WIN)
  QString os("Windows");
#else
  QString os("Linux");
#endif
  
  QJsonObject osFontRepl = fontRepl[os].toObject();
  keys = osFontRepl.keys();
  for (constIterator = keys.constBegin(); constIterator != keys.constEnd(); ++constIterator)
  {
    const QString key = *constIterator;
    QString value = osFontRepl[key].toString();
    cssContent = cssContent.replace(key, value);
  }
  
  m_CurrentThemeFilePath = jsonFilePath;
  
  // FINALLY, Set the style sheet into the app object
  qApp->setStyleSheet(cssContent);
  emit styleSheetLoaded(jsonFilePath);
  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SVStyle::loadStringProperty(const QString &key, QJsonObject cssRepl, QJsonObject varMapping)
{
  QString value = cssRepl[key].toString();
  if (!value.isNull())
  {
    // See if it is a variable and if it is, then get the real value from
    // the Named_Variables section
    for (QJsonObject::iterator iter = varMapping.begin(); iter != varMapping.end(); iter++)
    {
      QString mapKey = iter.key();
      QString mapValue = iter.value().toString();
      value.replace(mapKey, mapValue);
    }

    return value;
  }

  return QString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int SVStyle::loadIntegerProperty(const QString &key, QJsonObject cssRepl, QJsonObject varMapping)
{
  QString value = cssRepl[key].toString();
  if (!value.isNull())
  {
    // See if it is a variable and if it is, then get the real value from
    // the Named_Variables section
    if (varMapping.contains(value))
    {
      int intValue = varMapping[value].toInt();
      return intValue;
    }
  }

  int intValue = cssRepl[key].toInt();
  return intValue;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SVStyle::GetUIFont()
{
#if defined(Q_OS_MAC)
  QString fontString("Lato");
#elif defined(Q_OS_WIN)
  QString fontString("Lato");
#else
  QString fontString("Lato");
#endif

  QFont font(fontString);
  if(font.fromString(fontString))
  {
    return font.toString();
  }

  font = QFont();
  return font.toString();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QFont SVStyle::GetHumanLabelFont()
{
  QFont humanLabelFont(GetUIFont());
  humanLabelFont.setBold(true);
  humanLabelFont.setItalic(false);
  humanLabelFont.setWeight(QFont::Bold);
  humanLabelFont.setStyleStrategy(QFont::PreferAntialias);
  humanLabelFont.setFamily(GetUIFont());

#if defined(Q_OS_MAC)
  humanLabelFont.setPointSize(16);
#elif defined(Q_OS_WIN)
  humanLabelFont.setPointSize(13);
#else
  humanLabelFont.setPointSize(12);
#endif
  return humanLabelFont;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QFont SVStyle::GetBrandingLabelFont()
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
QFont SVStyle::GetCategoryFont()
{
  QFont categoryFont(GetUIFont());
  categoryFont.setBold(true);
  categoryFont.setWeight(QFont::Bold);
  categoryFont.setStyleStrategy(QFont::PreferAntialias);
  categoryFont.setFamily(GetUIFont());

#if defined(Q_OS_MAC)
  categoryFont.setPointSize(11);
#elif defined(Q_OS_WIN)
  categoryFont.setPointSize(10);
#else
  categoryFont.setPointSize(10);
#endif

  return categoryFont;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QFont SVStyle::GetTitleFont()
{
  QFont categoryFont(GetUIFont());
  categoryFont.setBold(true);
  categoryFont.setWeight(QFont::Bold);
  categoryFont.setStyleStrategy(QFont::PreferAntialias);
  categoryFont.setFamily(GetUIFont());

#if defined(Q_OS_MAC)
  categoryFont.setPointSize(16);
#elif defined(Q_OS_WIN)
  categoryFont.setPointSize(12);
#else
  categoryFont.setPointSize(12);
#endif

  return categoryFont;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::LineEditErrorStyle(QLineEdit* lineEdit)
{
  QString str;
  QTextStream ss(&str);
  ss << "QLineEdit#" << lineEdit->objectName() << "{";
  ss << "border: 1px solid rgb("
  << m_Widget_Error_color.red() << ", "
  << m_Widget_Error_color.green() << ", "
  << m_Widget_Error_color.blue() << ");"
  << "}";
  lineEdit->setStyleSheet(str);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::LineEditClearStyle(QLineEdit* lineEdit)
{
  lineEdit->setStyleSheet("");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::LineEditBackgroundErrorStyle(QLineEdit* lineEdit)
{
  QString str;
  QTextStream ss(&str);
  ss << "QLineEdit#" << lineEdit->objectName() << "{";
  //  ss << "border: 1px solid rgb(180, 0, 0);";
  ss << "background-color: rgb("
  << m_QLineEditError_background_color.red() << ", "
  << m_QLineEditError_background_color.green() << ", "
  << m_QLineEditError_background_color.blue() << ");"
  << "}";
  
  lineEdit->setStyleSheet(str);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::SetErrorColor(const QString &widgetType, QWidget* widget)
{
  QString str;
  QTextStream ss(&str);
  ss << widgetType << "#" << widget->objectName() << "{";
  //  ss << "border: 1px solid rgb(180, 0, 0);";
  ss << "color: rgb("
  << m_Text_Error_color.red() << ", "
  << m_Text_Error_color.green() << ", "
  << m_Text_Error_color.blue() << ");"
  << "}";
  
  widget->setStyleSheet(str);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SVStyle::QToolSelectionButtonStyle(bool exists)
{
  QString str;
  #if 0
  QTextStream ss(&str);

  QFont font;
  font.setBold(true);
  font.setItalic(true);
  font.setWeight(QFont::Bold);
  font.setStyleStrategy(QFont::PreferAntialias);
  font.setFamily(GetUIFont());

  QString fontString;
  QTextStream in(&fontString);

#if defined(Q_OS_MAC)
  font.setPointSize(12);
#elif defined(Q_OS_WIN)
  font.setPointSize(10);
#else
  font.setPointSize(11);
  in << "color; #000000;\n";
  in << "font-weight: Medium;";
#endif

  in << "font: " << font.weight() << " " << font.pointSize() << "pt \"" << font.family()  << "\";";

  ss << "QToolButton {\n";
  if(exists)
  {
    ss << " border: 1px solid " << ::kNormalColor << ";\n";
  }
  else
  {
    ss << " border: 1px solid " << ::kErrorColor << ";\n";
  }
  ss << " border-radius: 4px;\n";
  ss << " background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #DDDDDD, stop: 1 #FFFFFF);\n";
  ss << fontString << "\n";
  ss << " padding-left: 16px;\n";
  ss << " padding-right: 12px;\n";
  ss << " padding-top: 2px;\n";
  ss << " padding-bottom: 2px;\n";
  ss << "}\n";

  ss << "QToolButton::menu-indicator {\n";
  ss << " subcontrol-origin: content;\n";
  ss << " subcontrol-position:  right; /* */\n";
  ss << "}\n";

  ss << "QToolButton::menu-indicator:pressed, QToolButton::menu-indicator:open {\n";
  ss << " position: relative;\n";
  ss << "}\n";

  ss << "QToolButton:pressed {\n";
  ss << " background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,\nstop: 0 " << QApplication::palette().highlight().color().name() << ", stop: 1 #FFFFFF);\n";
  ss << "}\n";

  ss << "QToolButton:flat {\n";
  ss << " border: none;\n";
  ss << "}\n";

  ss << " QToolTip {\
              border: 2px solid #434343;\
              padding: 2px;\
              border-radius: 3px;\
              opacity: 255;\
              background-color: #FFFCEA;\
              color: #000000;\
              }";
#endif
  return str;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::ColorForFilterGroup(const QString &grpName)
{
  QColor color(102, 96, 255);

  QString jsonString;
  QFile jsonFile;
  jsonFile.setFileName(":/QtSupportResources/FilterStyle/SVFilterColors.json");

  if(jsonFile.exists())
  {
    jsonFile.open(QIODevice::ReadOnly | QIODevice::Text);
    jsonString = jsonFile.readAll();
    jsonFile.close();

    QJsonDocument doc = QJsonDocument::fromJson(jsonString.toUtf8());
    QJsonObject jsonObj = doc.object();
    QJsonValue jsonValue = jsonObj.value("Filter Group Colors");

    if(jsonValue.isObject())
    {
      QJsonValue jsonColor = jsonValue.toObject().value(grpName);
      if(jsonColor.isString())
      {
        color.setNamedColor(jsonColor.toString());
      }
      else
      {
        jsonColor = jsonValue.toObject().value("Filter Group Not Found");
        if(jsonColor.isString())
        {
          color.setNamedColor(jsonColor.toString());
        }
      }
    }
  }
  else
  {
    int saturation = 110;
    int brightness = 190;
    if(grpName.compare(SIMPL::FilterGroups::Unsupported) == 0)
    {
      color = QColor::fromHsv(0, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::Generic) == 0)
    {
      color = QColor::fromHsv(30, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::ReconstructionFilters) == 0)
    {
      color = QColor::fromHsv(54, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::SamplingFilters) == 0)
    {
      color = QColor::fromHsv(84, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::StatisticsFilters) == 0)
    {
      color = QColor::fromHsv(120, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::SyntheticBuildingFilters) == 0)
    {
      color = QColor::fromHsv(150, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::SurfaceMeshingFilters) == 0)
    {
      color = QColor::fromHsv(180, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::ProcessingFilters) == 0)
    {
      color = QColor::fromHsv(210, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::CoreFilters) == 0)
    {
      color = QColor::fromHsv(240, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::IOFilters) == 0)
    {
      color = QColor::fromHsv(270, saturation, brightness);
    }
    else if(grpName.compare(SIMPL::FilterGroups::Utilities) == 0)
    {
      color = QColor::fromHsv(300, saturation, brightness);
    }
    else
    {
      color = QColor::fromHsv(330, saturation, brightness);
    }
  }


  return color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::GetFilterBackgroundColor()
{
  return self->getFilterBackgroundColor();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::GetFilterSelectionColor()
{
  return self->getFilterSelectionColor();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::GetFilterFontColor()
{
  return self->getFilterFontColor();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QIcon SVStyle::IconForGroup(const QString &grpName)
{
  
  QColor color = ColorForFilterGroup(grpName);
  QImage grpImage;
  
  if(s_NameToImage.contains(grpName))
  {
    return QIcon(QPixmap::fromImage(s_NameToImage[grpName]));
  }

  QIcon grpIcon(":/SIMPL/icons/images/BlankGroup_Icon.png");
  if(!grpIcon.isNull())
  {
    grpImage = grpIcon.pixmap(QSize(48, 48)).toImage();

    QSize imageSize = grpImage.size();
    for(int h = 0; h < imageSize.height(); h++)
    {
      for(int w = 0; w < imageSize.width(); w++)
      {
        QColor pixel = grpImage.pixelColor(w, h);
        if(pixel.red() == 228 && pixel.green() == 228 && pixel.blue() == 228 && pixel.alpha() != 0)
        {
          pixel = color;
          pixel.setRedF((pixel.redF() * 1.50 > 1.0) ? 1.0 : pixel.redF() * 1.50);
          pixel.setGreenF((pixel.greenF() * 1.50 > 1.0) ? 1.0 : pixel.greenF() * 1.50);
          pixel.setBlueF((pixel.blueF() * 1.50 > 1.0) ? 1.0 : pixel.blueF() * 1.50);

          if (pixel.isValid())
          {
            grpImage.setPixelColor(w, h, pixel);
          }
        }

        if(pixel.red() == 150 && pixel.green() == 150 && pixel.blue() == 150 && pixel.alpha() != 0)
        {
          pixel = color;
          //          pixel.setRedF(pixel.redF() * 1.50);
          //          pixel.setGreenF(pixel.greenF() * 1.50);
          //          pixel.setBlueF(pixel.blueF() * 1.50);
          if (pixel.isValid())
          {
            grpImage.setPixelColor(w, h, pixel);
          }
        }

        if(pixel.red() == 53 && pixel.green() == 53 && pixel.blue() == 53 && pixel.alpha() != 0)
        {
          pixel = color;
          pixel.setRedF(pixel.redF() * .50);
          pixel.setGreenF(pixel.greenF() * .50);
          pixel.setBlueF(pixel.blueF() * .50);

          if (pixel.isValid())
          {
            grpImage.setPixelColor(w, h, pixel);
          }
        }
      }
    }
  }
  s_NameToImage[grpName] = grpImage;

  return QIcon(QPixmap::fromImage(grpImage));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SVStyle::WrapTextWithHtmlStyle(const QString& msg, bool bold) const
{
  QString formattedMessage;
  QTextStream out(&formattedMessage);
  QString msg2 = msg;
  msg2 = msg2.replace("\n", "<br>");
  if(bold)
  {
    out << "<b ";
  }
  else {
    out << "<span ";
  }
  out << "style=\"color: " << getQLabel_color().name(QColor::HexRgb) << ";\">";
  out << msg2;
  if(bold)
  {
    out << " </b>";
  }
  else {
    out << "</span>";
  }
  return formattedMessage;
}
