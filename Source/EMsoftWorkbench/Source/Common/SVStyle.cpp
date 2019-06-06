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

#include <iostream>

#include <QtCore/QDebug>
#include <QtCore/QMetaProperty>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>
#include <QtCore/QJsonValue>
#include <QtCore/QMimeDatabase>

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
bool SVStyle::loadStyleSheet(const QString &filePath)
{
//  qDebug() << "SVStyle::loadStyleSheet() " << jsonFilePath;
  invalidateColorProperties();

  bool success;

  QFileInfo fi(filePath);
  if (fi.completeSuffix() == "qss")
  {
    QFile file(filePath);
    file.open(QFile::ReadOnly);
    QString styleSheet = QString::fromLatin1(file.readAll());

    // FINALLY, Set the style sheet into the app object
    m_CurrentThemeFilePath = filePath;
    qApp->setStyleSheet(styleSheet);
    emit styleSheetLoaded(filePath);
    success = true;
  }
  else if (fi.completeSuffix() == "json")
  {
    success = loadStyleSheetFromJson(filePath);
  }
  else
  {
    std::cout << tr("Style sheet with path '%1' could not be loaded because the file extension is not .qss or .json.").arg(filePath).toStdString();
    success = false;
  }
  
  return success;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool SVStyle::loadStyleSheetFromJson(const QString &filePath)
{
  QFileInfo fi(filePath);
  QFile jsonFile(filePath);
  if(!jsonFile.open(QFile::ReadOnly))
  {
    qDebug() << "Could not open JSON File " << filePath;
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
  cssFileName = QString("%1/%2").arg(fi.absolutePath(), 1).arg(cssFileName, 2);

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

  m_CurrentThemeFilePath = filePath;

  // FINALLY, Set the style sheet into the app object
  qApp->setStyleSheet(cssContent);
  emit styleSheetLoaded(filePath);

  return true;
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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFader_color(const QColor& value)
{
  m_Fader_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFader_color() const
{
  return m_Fader_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setWidget_Error_color(const QColor& value)
{
  m_Widget_Error_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getWidget_Error_color() const
{
  return m_Widget_Error_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setText_Error_color(const QColor& value)
{
  m_Text_Error_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getText_Error_color() const
{
  return m_Text_Error_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setCentralWidget_background_color(const QColor& value)
{
  m_CentralWidget_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getCentralWidget_background_color() const
{
  return m_CentralWidget_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLabel_color(const QColor& value)
{
  m_QLabel_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLabel_color() const
{
  return m_QLabel_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterInputWidget_background_color(const QColor& value)
{
  m_FilterInputWidget_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterInputWidget_background_color() const
{
  return m_FilterInputWidget_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQScrollArea_background_color(const QColor& value)
{
  m_QScrollArea_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQScrollArea_background_color() const
{
  return m_QScrollArea_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setVariablesTabContents_background_color(const QColor& value)
{
  m_VariablesTabContents_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getVariablesTabContents_background_color() const
{
  return m_VariablesTabContents_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterParameterWidget_background_color(const QColor& value)
{
  m_FilterParameterWidget_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterParameterWidget_background_color() const
{
  return m_FilterParameterWidget_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterParameterWidget_border_color(const QColor& value)
{
  m_FilterParameterWidget_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterParameterWidget_border_color() const
{
  return m_FilterParameterWidget_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQGroupBoxTitle_background_color(const QColor& value)
{
  m_QGroupBoxTitle_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQGroupBoxTitle_background_color() const
{
  return m_QGroupBoxTitle_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQGroupBox_background_color(const QColor& value)
{
  m_QGroupBox_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQGroupBox_background_color() const
{
  return m_QGroupBox_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQDockWidget_border_color(const QColor& value)
{
  m_QDockWidget_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQDockWidget_border_color() const
{
  return m_QDockWidget_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQDockWidget_color(const QColor& value)
{
  m_QDockWidget_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQDockWidget_color() const
{
  return m_QDockWidget_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQDockWidgetTitle_background_color(const QColor& value)
{
  m_QDockWidgetTitle_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQDockWidgetTitle_background_color() const
{
  return m_QDockWidgetTitle_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQDockWidgetTitle_color(const QColor& value)
{
  m_QDockWidgetTitle_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQDockWidgetTitle_color() const
{
  return m_QDockWidgetTitle_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListView_background_color(const QColor& value)
{
  m_QListView_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListView_background_color() const
{
  return m_QListView_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListView_color(const QColor& value)
{
  m_QListView_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListView_color() const
{
  return m_QListView_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListViewItemHover_background_color(const QColor& value)
{
  m_QListViewItemHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListViewItemHover_background_color() const
{
  return m_QListViewItemHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListViewItemHover_color(const QColor& value)
{
  m_QListViewItemHover_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListViewItemHover_color() const
{
  return m_QListViewItemHover_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListViewItemSelected_background_color(const QColor& value)
{
  m_QListViewItemSelected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListViewItemSelected_background_color() const
{
  return m_QListViewItemSelected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQListViewItemSelected_color(const QColor& value)
{
  m_QListViewItemSelected_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQListViewItemSelected_color() const
{
  return m_QListViewItemSelected_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMainWindowSeparator_background_color(const QColor& value)
{
  m_QMainWindowSeparator_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMainWindowSeparator_background_color() const
{
  return m_QMainWindowSeparator_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuBar_background_color(const QColor& value)
{
  m_QMenuBar_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuBar_background_color() const
{
  return m_QMenuBar_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuBarItem_color(const QColor& value)
{
  m_QMenuBarItem_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuBarItem_color() const
{
  return m_QMenuBarItem_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuBarItemPressed_color(const QColor& value)
{
  m_QMenuBarItemPressed_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuBarItemPressed_color() const
{
  return m_QMenuBarItemPressed_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuBarItemSelected_background_color(const QColor& value)
{
  m_QMenuBarItemSelected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuBarItemSelected_background_color() const
{
  return m_QMenuBarItemSelected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuBarItemSelected_color(const QColor& value)
{
  m_QMenuBarItemSelected_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuBarItemSelected_color() const
{
  return m_QMenuBarItemSelected_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenu_background_color(const QColor& value)
{
  m_QMenu_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenu_background_color() const
{
  return m_QMenu_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenu_color(const QColor& value)
{
  m_QMenu_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenu_color() const
{
  return m_QMenu_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuItemSelected_background_color(const QColor& value)
{
  m_QMenuItemSelected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuItemSelected_background_color() const
{
  return m_QMenuItemSelected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuItemSelected_color(const QColor& value)
{
  m_QMenuItemSelected_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuItemSelected_color() const
{
  return m_QMenuItemSelected_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQMenuItemDisabled_background_color(const QColor& value)
{
  m_QMenuItemDisabled_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQMenuItemDisabled_background_color() const
{
  return m_QMenuItemDisabled_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButton_background_color(const QColor& value)
{
  m_QPushButton_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButton_background_color() const
{
  return m_QPushButton_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButton_border_color(const QColor& value)
{
  m_QPushButton_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButton_border_color() const
{
  return m_QPushButton_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonHover_background_color(const QColor& value)
{
  m_QPushButtonHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonHover_background_color() const
{
  return m_QPushButtonHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonPressed_background_color(const QColor& value)
{
  m_QPushButtonPressed_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonPressed_background_color() const
{
  return m_QPushButtonPressed_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonDefault_background_color(const QColor& value)
{
  m_QPushButtonDefault_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonDefault_background_color() const
{
  return m_QPushButtonDefault_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonDefaultPressed_background_color(const QColor& value)
{
  m_QPushButtonDefaultPressed_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonDefaultPressed_background_color() const
{
  return m_QPushButtonDefaultPressed_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonDefaultHover_background_color(const QColor& value)
{
  m_QPushButtonDefaultHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonDefaultHover_background_color() const
{
  return m_QPushButtonDefaultHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonDefault_border_color(const QColor& value)
{
  m_QPushButtonDefault_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonDefault_border_color() const
{
  return m_QPushButtonDefault_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQPushButtonDefault_text_color(const QColor& value)
{
  m_QPushButtonDefault_text_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQPushButtonDefault_text_color() const
{
  return m_QPushButtonDefault_text_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQSplitter_handle_start_color(const QColor& value)
{
  m_QSplitter_handle_start_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQSplitter_handle_start_color() const
{
  return m_QSplitter_handle_start_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQSplitter_handle_end_color(const QColor& value)
{
  m_QSplitter_handle_end_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQSplitter_handle_end_color() const
{
  return m_QSplitter_handle_end_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButton_background_color(const QColor& value)
{
  m_QToolButton_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButton_background_color() const
{
  return m_QToolButton_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButton_color(const QColor& value)
{
  m_QToolButton_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButton_color() const
{
  return m_QToolButton_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButton_border_color(const QColor& value)
{
  m_QToolButton_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButton_border_color() const
{
  return m_QToolButton_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButtonChecked_background_color(const QColor& value)
{
  m_QToolButtonChecked_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButtonChecked_background_color() const
{
  return m_QToolButtonChecked_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButtonDisabled_background_color(const QColor& value)
{
  m_QToolButtonDisabled_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButtonDisabled_background_color() const
{
  return m_QToolButtonDisabled_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolButtonDisabled_color(const QColor& value)
{
  m_QToolButtonDisabled_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolButtonDisabled_color() const
{
  return m_QToolButtonDisabled_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQStatusBar_border_color(const QColor& value)
{
  m_QStatusBar_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQStatusBar_border_color() const
{
  return m_QStatusBar_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTableWidget_color(const QColor& value)
{
  m_QTableWidget_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTableWidget_color() const
{
  return m_QTableWidget_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTableWidget_selected_background_color(const QColor& value)
{
  m_QTableWidget_selected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTableWidget_selected_background_color() const
{
  return m_QTableWidget_selected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderView_background_color(const QColor& value)
{
  m_QHeaderView_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderView_background_color() const
{
  return m_QHeaderView_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderView_border_color(const QColor& value)
{
  m_QHeaderView_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderView_border_color() const
{
  return m_QHeaderView_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderView_color(const QColor& value)
{
  m_QHeaderView_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderView_color() const
{
  return m_QHeaderView_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderViewDisabled_background_color(const QColor& value)
{
  m_QHeaderViewDisabled_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderViewDisabled_background_color() const
{
  return m_QHeaderViewDisabled_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderViewDisabled_border_color(const QColor& value)
{
  m_QHeaderViewDisabled_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderViewDisabled_border_color() const
{
  return m_QHeaderViewDisabled_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQHeaderViewDisabled_color(const QColor& value)
{
  m_QHeaderViewDisabled_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQHeaderViewDisabled_color() const
{
  return m_QHeaderViewDisabled_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabWidgetPane_border_color(const QColor& value)
{
  m_QTabWidgetPane_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabWidgetPane_border_color() const
{
  return m_QTabWidgetPane_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabWidgetPane_background_color(const QColor& value)
{
  m_QTabWidgetPane_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabWidgetPane_background_color() const
{
  return m_QTabWidgetPane_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabBarTab_border_color(const QColor& value)
{
  m_QTabBarTab_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabBarTab_border_color() const
{
  return m_QTabBarTab_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabBarTabSelected_background_color(const QColor& value)
{
  m_QTabBarTabSelected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabBarTabSelected_background_color() const
{
  return m_QTabBarTabSelected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabBarTabSelected_color(const QColor& value)
{
  m_QTabBarTabSelected_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabBarTabSelected_color() const
{
  return m_QTabBarTabSelected_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabBarTabNotSelected_background_color(const QColor& value)
{
  m_QTabBarTabNotSelected_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabBarTabNotSelected_background_color() const
{
  return m_QTabBarTabNotSelected_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTabBarTabNotSelectedHover_background_color(const QColor& value)
{
  m_QTabBarTabNotSelectedHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTabBarTabNotSelectedHover_background_color() const
{
  return m_QTabBarTabNotSelectedHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTextEdit_background_color(const QColor& value)
{
  m_QTextEdit_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTextEdit_background_color() const
{
  return m_QTextEdit_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTextEdit_color(const QColor& value)
{
  m_QTextEdit_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTextEdit_color() const
{
  return m_QTextEdit_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLineEdit_color(const QColor& value)
{
  m_QLineEdit_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLineEdit_color() const
{
  return m_QLineEdit_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLineEdit_background_color(const QColor& value)
{
  m_QLineEdit_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLineEdit_background_color() const
{
  return m_QLineEdit_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLineEdit_border_color(const QColor& value)
{
  m_QLineEdit_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLineEdit_border_color() const
{
  return m_QLineEdit_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLineEditDisabled_background_color(const QColor& value)
{
  m_QLineEditDisabled_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLineEditDisabled_background_color() const
{
  return m_QLineEditDisabled_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQLineEditError_background_color(const QColor& value)
{
  m_QLineEditError_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQLineEditError_background_color() const
{
  return m_QLineEditError_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQSpinBoxArrow_background_color(const QColor& value)
{
  m_QSpinBoxArrow_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQSpinBoxArrow_background_color() const
{
  return m_QSpinBoxArrow_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQSpinBoxArrow_hover_background_color(const QColor& value)
{
  m_QSpinBoxArrow_hover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQSpinBoxArrow_hover_background_color() const
{
  return m_QSpinBoxArrow_hover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBox_border_color(const QColor& value)
{
  m_QComboBox_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBox_border_color() const
{
  return m_QComboBox_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBox_background_color(const QColor& value)
{
  m_QComboBox_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBox_background_color() const
{
  return m_QComboBox_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxArrow_background_color(const QColor& value)
{
  m_QComboBoxArrow_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxArrow_background_color() const
{
  return m_QComboBoxArrow_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxArrow_hover_background_color(const QColor& value)
{
  m_QComboBoxArrow_hover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxArrow_hover_background_color() const
{
  return m_QComboBoxArrow_hover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxItem_border_color(const QColor& value)
{
  m_QComboBoxItem_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxItem_border_color() const
{
  return m_QComboBoxItem_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxItem_selection_color(const QColor& value)
{
  m_QComboBoxItem_selection_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxItem_selection_color() const
{
  return m_QComboBoxItem_selection_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxItem_background_color(const QColor& value)
{
  m_QComboBoxItem_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxItem_background_color() const
{
  return m_QComboBoxItem_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxDisabled_background_color(const QColor& value)
{
  m_QComboBoxDisabled_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxDisabled_background_color() const
{
  return m_QComboBoxDisabled_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQComboBoxDisabled_color(const QColor& value)
{
  m_QComboBoxDisabled_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQComboBoxDisabled_color() const
{
  return m_QComboBoxDisabled_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolBar_background_color(const QColor& value)
{
  m_QToolBar_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolBar_background_color() const
{
  return m_QToolBar_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQToolBar_border_color(const QColor& value)
{
  m_QToolBar_border_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQToolBar_border_color() const
{
  return m_QToolBar_border_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeView_background_color(const QColor& value)
{
  m_QTreeView_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeView_background_color() const
{
  return m_QTreeView_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItem_background_color(const QColor& value)
{
  m_QTreeViewItem_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItem_background_color() const
{
  return m_QTreeViewItem_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItem_error_background_color(const QColor& value)
{
  m_QTreeViewItem_error_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItem_error_background_color() const
{
  return m_QTreeViewItem_error_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItem_color(const QColor& value)
{
  m_QTreeViewItem_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItem_color() const
{
  return m_QTreeViewItem_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItem_error_color(const QColor& value)
{
  m_QTreeViewItem_error_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItem_error_color() const
{
  return m_QTreeViewItem_error_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItem_font_size(const int& value)
{
  m_QTreeViewItem_font_size = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int SVStyle::getQTreeViewItem_font_size() const
{
  return m_QTreeViewItem_font_size;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemHover_background_color(const QColor& value)
{
  m_QTreeViewItemHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemHover_background_color() const
{
  return m_QTreeViewItemHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemHover_color(const QColor& value)
{
  m_QTreeViewItemHover_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemHover_color() const
{
  return m_QTreeViewItemHover_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemSelectedActive_background_color(const QColor& value)
{
  m_QTreeViewItemSelectedActive_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemSelectedActive_background_color() const
{
  return m_QTreeViewItemSelectedActive_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemSelectedActive_color(const QColor& value)
{
  m_QTreeViewItemSelectedActive_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemSelectedActive_color() const
{
  return m_QTreeViewItemSelectedActive_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemSelectedNotActive_background_color(const QColor& value)
{
  m_QTreeViewItemSelectedNotActive_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemSelectedNotActive_background_color() const
{
  return m_QTreeViewItemSelectedNotActive_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewItemSelectedNotActive_color(const QColor& value)
{
  m_QTreeViewItemSelectedNotActive_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewItemSelectedNotActive_color() const
{
  return m_QTreeViewItemSelectedNotActive_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewBranch_background_color(const QColor& value)
{
  m_QTreeViewBranch_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewBranch_background_color() const
{
  return m_QTreeViewBranch_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewBranchHover_background_color(const QColor& value)
{
  m_QTreeViewBranchHover_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewBranchHover_background_color() const
{
  return m_QTreeViewBranchHover_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewBranchHover_color(const QColor& value)
{
  m_QTreeViewBranchHover_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewBranchHover_color() const
{
  return m_QTreeViewBranchHover_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewBranchSelectedActive_background_color(const QColor& value)
{
  m_QTreeViewBranchSelectedActive_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewBranchSelectedActive_background_color() const
{
  return m_QTreeViewBranchSelectedActive_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setQTreeViewBranchSelectedNotActive_background_color(const QColor& value)
{
  m_QTreeViewBranchSelectedNotActive_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getQTreeViewBranchSelectedNotActive_background_color() const
{
  return m_QTreeViewBranchSelectedNotActive_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterBackgroundColor(const QColor& value)
{
  m_FilterBackgroundColor = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterBackgroundColor() const
{
  return m_FilterBackgroundColor;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterSelectionColor(const QColor& value)
{
  m_FilterSelectionColor = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterSelectionColor() const
{
  return m_FilterSelectionColor;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setFilterFontColor(const QColor& value)
{
  m_FilterFontColor = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getFilterFontColor() const
{
  return m_FilterFontColor;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_DataContainer_color(const QColor& value)
{
  m_DataArrayPath_DataContainer_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_DataContainer_color() const
{
  return m_DataArrayPath_DataContainer_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_AttributeMatrix_color(const QColor& value)
{
  m_DataArrayPath_AttributeMatrix_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_AttributeMatrix_color() const
{
  return m_DataArrayPath_AttributeMatrix_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_DataArray_color(const QColor& value)
{
  m_DataArrayPath_DataArray_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_DataArray_color() const
{
  return m_DataArrayPath_DataArray_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_Invalid_color(const QColor& value)
{
  m_DataArrayPath_Invalid_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_Invalid_color() const
{
  return m_DataArrayPath_Invalid_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_DataContainer_background_color(const QColor& value)
{
  m_DataArrayPath_DataContainer_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_DataContainer_background_color() const
{
  return m_DataArrayPath_DataContainer_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_AttributeMatrix_background_color(const QColor& value)
{
  m_DataArrayPath_AttributeMatrix_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_AttributeMatrix_background_color() const
{
  return m_DataArrayPath_AttributeMatrix_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_DataArray_background_color(const QColor& value)
{
  m_DataArrayPath_DataArray_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_DataArray_background_color() const
{
  return m_DataArrayPath_DataArray_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_Invalid_background_color(const QColor& value)
{
  m_DataArrayPath_Invalid_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_Invalid_background_color() const
{
  return m_DataArrayPath_Invalid_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_border_normal(const QColor& value)
{
  m_DataArrayPath_border_normal = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_border_normal() const
{
  return m_DataArrayPath_border_normal;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_border_not_found(const QColor& value)
{
  m_DataArrayPath_border_not_found = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_border_not_found() const
{
  return m_DataArrayPath_border_not_found;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_border_drag_enabled(const QColor& value)
{
  m_DataArrayPath_border_drag_enabled = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_border_drag_enabled() const
{
  return m_DataArrayPath_border_drag_enabled;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataArrayPath_border_drag_disabled(const QColor& value)
{
  m_DataArrayPath_border_drag_disabled = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataArrayPath_border_drag_disabled() const
{
  return m_DataArrayPath_border_drag_disabled;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataPathLabel_color(const QColor& value)
{
  m_DataPathLabel_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataPathLabel_color() const
{
  return m_DataPathLabel_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setDataPathLabel_Disabled_color(const QColor& value)
{
  m_DataPathLabel_Disabled_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getDataPathLabel_Disabled_color() const
{
  return m_DataPathLabel_Disabled_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setSIMPLViewPipelineDockWidgetTitle_inactive_background_color(const QColor& value)
{
  m_SIMPLViewPipelineDockWidgetTitle_inactive_background_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getSIMPLViewPipelineDockWidgetTitle_inactive_background_color() const
{
  return m_SIMPLViewPipelineDockWidgetTitle_inactive_background_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SVStyle::setSIMPLViewPipelineDockWidgetTitle_inactive_text_color(const QColor& value)
{
  m_SIMPLViewPipelineDockWidgetTitle_inactive_text_color = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QColor SVStyle::getSIMPLViewPipelineDockWidgetTitle_inactive_text_color() const
{
  return m_SIMPLViewPipelineDockWidgetTitle_inactive_text_color;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SVStyle::getCurrentThemeFilePath() const
{
  return m_CurrentThemeFilePath;
}

