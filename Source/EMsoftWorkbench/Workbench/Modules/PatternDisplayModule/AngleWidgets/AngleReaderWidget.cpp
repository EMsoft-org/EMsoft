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

#include "AngleReaderWidget.h"

#include <QtCore/QJsonObject>
#include <QtCore/QString>

#include <QtWidgets/QFileDialog>

#include "Modules/PatternDisplayModule/PatternDisplay_UI.h"

#include "Common/Constants.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AngleReaderWidget::AngleReaderWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: AbstractAngleWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AngleReaderWidget::~AngleReaderWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::setupGui()
{
  numOfAngles->hide();
  angleType->hide();
  numOfAnglesLabel->hide();
  angleTypeLabel->hide();

  minLineNum->hide();
  maxLineNum->hide();
  lineNumDash->hide();
  lineNumbersLabel->hide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::on_loadAngleFileBtn_clicked()
{
  QString proposedDir = m_OpenDialogLastDirectory;
  QString filePath = QFileDialog::getOpenFileName(nullptr, tr("Load Angle File"), proposedDir, tr("Angle Files (*.txt);;All Files (*.*)"));
  if(filePath.isEmpty())
  {
    return;
  }

  // Cache the last directory on old instance
  m_OpenDialogLastDirectory = filePath;

  loadAngleFile(filePath);

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AngleReaderWidget::hasValidAngles() const
{
  QFile file(m_LoadedFilePath);
  bool fileExists = file.exists();

  return (fileExists && numOfAngles->text().toInt() > 0 && angleType->text() != UnknownStr);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::readSession(QJsonObject& obj)
{
  angleFileLineEdit->setText(obj[EMsoftWorkbenchConstants::IOStrings::AngleFilePath].toString());
  partialFileCB->setChecked(obj[EMsoftWorkbenchConstants::IOStrings::PartialFile].toBool());
  minLineNum->setValue(obj[EMsoftWorkbenchConstants::IOStrings::MinLineNum].toInt());
  maxLineNum->setValue(obj[EMsoftWorkbenchConstants::IOStrings::MaxLineNum].toInt());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::writeSession(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::AngleFilePath] = angleFileLineEdit->text();
  obj[EMsoftWorkbenchConstants::IOStrings::PartialFile] = partialFileCB->isChecked();
  obj[EMsoftWorkbenchConstants::IOStrings::MinLineNum] = minLineNum->value();
  obj[EMsoftWorkbenchConstants::IOStrings::MaxLineNum] = maxLineNum->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::createModificationConnections(PatternDisplay_UI* ui) const
{
  // Line Edits
  connect(angleFileLineEdit, &QLineEdit::textChanged, [=] { emit ui->moduleParametersChanged(); });

  // Checkboxes
  connect(partialFileCB, &QCheckBox::stateChanged, [=] { emit ui->moduleParametersChanged(); });

  // Spin Boxes
  connect(minLineNum, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { emit ui->moduleParametersChanged(); });
  connect(maxLineNum, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { emit ui->moduleParametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::on_partialFileCB_stateChanged(int state) const
{
  if(state == 0)
  {
    minLineNum->hide();
    maxLineNum->hide();
    lineNumDash->hide();
    lineNumbersLabel->hide();

    numOfAngles->setText(QString::number(m_FileAngleCount));
  }
  else
  {
    minLineNum->show();
    maxLineNum->show();
    lineNumDash->show();
    lineNumbersLabel->show();

    numOfAngles->setText(QString::number(maxLineNum->text().toInt() - minLineNum->text().toInt() + 1));
  }

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::loadAngleFile(const QString& filePath)
{
  numOfAngles->setText("0");

  QFile file(filePath);
  if(file.open(QIODevice::ReadOnly))
  {
    angleFileLineEdit->setText(filePath);
    m_LoadedFilePath = filePath;

    QTextStream in(&file);
    QString angleTypeStr = in.readLine();
    QStringList parts = angleTypeStr.split(QRegularExpression("[ \t]+"), QString::SkipEmptyParts);
    if(parts.size() == 1 && parts[0] == EulerId)
    {
      angleType->setText(EulerStr);
    }
    else
    {
      angleType->setText(UnknownStr);
    }

    m_FileAngleCount = in.readLine().toInt();
    numOfAngles->setText(QString::number(m_FileAngleCount));

    minLineNum->setMaximum(m_FileAngleCount + 2);
    minLineNum->setValue(3);
    maxLineNum->setMaximum(m_FileAngleCount + 2);
    maxLineNum->setValue(m_FileAngleCount + 2);

    angleTypeLabel->show();
    numOfAnglesLabel->show();
    angleType->show();
    numOfAngles->show();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::on_minLineNum_valueChanged(int value) const
{
  if(value > maxLineNum->value())
  {
    maxLineNum->setValue(value);
  }

  numOfAngles->setText(QString::number(maxLineNum->text().toInt() - minLineNum->text().toInt() + 1));

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::on_maxLineNum_valueChanged(int value) const
{
  if(value < minLineNum->value())
  {
    minLineNum->setValue(value);
  }

  numOfAngles->setText(QString::number(maxLineNum->text().toInt() - minLineNum->text().toInt() + 1));

  emit dataChanged(hasValidAngles());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> AngleReaderWidget::getEulerAngles() const
{
  QFile file(m_LoadedFilePath);
  if(file.open(QIODevice::ReadOnly))
  {
    QTextStream in(&file);

    size_t numberOfAngles;
    if(partialFileCB->isChecked())
    {
      numberOfAngles = maxLineNum->value() - minLineNum->value() + 1;

      // Skip to the part of the file that we want to read
      for(int i = 0; i < minLineNum->value() - 1; i++)
      {
        in.readLine();
      }
    }
    else
    {
      in.readLine(); // Angle Type
      numberOfAngles = in.readLine().toInt();
    }

    std::vector<float> angleArray(numberOfAngles * 3);

    for(int i = 0; i < angleArray.size(); i += 3)
    {
      QString line = in.readLine();
      QStringList parts = line.split(QRegularExpression("[ \t]+"), QString::SkipEmptyParts);
      for(int j = 0; j < parts.size(); j++)
      {
        QString part = parts[j];
        float value = part.toFloat();
        value = AbstractAngleWidget::ConvertToRadians(value);
        angleArray.at(i + j) = value;
      }
    }

    return angleArray;
  }

  return std::vector<float>();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AngleReaderWidget::setOpenDialogLastDirectory(const QString& value)
{
  m_OpenDialogLastDirectory = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString AngleReaderWidget::getOpenDialogLastDirectory() const
{
  return m_OpenDialogLastDirectory;
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AngleReaderWidget::Pointer AngleReaderWidget::NullPointer()
{
  return Pointer(static_cast<Self*>(nullptr));
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AngleReaderWidget::Pointer AngleReaderWidget::New()
{
  Pointer sharedPtr (new Self);
  return sharedPtr;
}

