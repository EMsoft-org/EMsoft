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

#include "InitDialog.h"

#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>

#include "EMsoftLib/EMsoftStringConstants.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
InitDialog::InitDialog(QWidget *parent) :
  QDialog(parent)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void InitDialog::setupGui()
{
  QFileInfo fi(QString("%1/.config/EMsoft/EMsoftConfig.json").arg(QDir::homePath()));
  if(!fi.exists())
  {
    QMessageBox::critical(this, "Config Error", "EMsoft config file does not exist.", QMessageBox::Ok, QMessageBox::Ok);
    reject();
  }

  QFile file(fi.filePath());
  if (!file.open(QFile::ReadOnly))
  {
    QMessageBox::critical(this, "Config Error", "Unable to open EMsoft config file.", QMessageBox::Ok, QMessageBox::Ok);
    reject();
  }

  QByteArray data = file.readAll();
  QJsonParseError error;
  QJsonDocument doc = QJsonDocument::fromJson(data, &error);
  if (error.error != QJsonParseError::NoError)
  {
    QMessageBox::critical(this, "Config Error", "Unable to read json data from EMsoft config file.", QMessageBox::Ok, QMessageBox::Ok);
    reject();
  }

  QJsonObject root = doc.object();
  for (QJsonObject::iterator iter = root.begin(); iter != root.end(); iter++)
  {
    QString propName = iter.key();
    QString propPath = iter.value().toString();

    initDialogLayout->addWidget(new QLabel(propName), m_NumOfPairs, 0);
    initDialogLayout->addWidget(new QLineEdit(propPath), m_NumOfPairs, 1);
    m_NumOfPairs++;
  }

  initDialogLayout->addWidget(buttonBox, m_NumOfPairs, 0, 1, 2);

  connect (this, &InitDialog::finished, [=] (int result) {
    if (result == QDialog::Accepted)
    {
      QJsonObject root;
      QString propName = "";
      QString propPath = "";
      QString xtalFolderPathName = "";
      for (int i = 0; i < m_NumOfPairs; i++)
      {
        QLayoutItem* lItem = initDialogLayout->itemAtPosition(i, 0);
        if (lItem != nullptr)
        {
          QLabel* label = dynamic_cast<QLabel*>(lItem->widget());
          if (label != nullptr)
          {
            propName = label->text();
          }
        }

        QLayoutItem* leItem = initDialogLayout->itemAtPosition(i, 1);
        if (leItem != nullptr)
        {
          QLineEdit* lineEdit = dynamic_cast<QLineEdit*>(leItem->widget());
          if (lineEdit != nullptr)
          {
            propPath = lineEdit->text();
          }
        }

        if (propName == EMsoft::Constants::EMXtalFolderpathname)
        {
          xtalFolderPathName = propPath;
        }

        root[propName] = propPath;
      }

      QJsonDocument doc(root);
      QByteArray jsonArray = doc.toJson();

      QFile file(fi.filePath());
      if (file.open(QFile::WriteOnly))
      {
        file.write(jsonArray);
        file.close();
        emit emSoftConfigurationChanged();
      }
    }

    deleteLater();
  });
}



