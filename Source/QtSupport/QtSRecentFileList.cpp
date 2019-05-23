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

#include "QtSRecentFileList.h"

#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QSettings>
#include <QtWidgets/QMenu>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSRecentFileList::QtSRecentFileList(QObject* parent)
: QObject(parent)
, m_Watcher(new QFileSystemWatcher(this))
{
  connect(m_Watcher, SIGNAL(fileChanged(const QString&)), this, SLOT(removeFile(const QString&)));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSRecentFileList::~QtSRecentFileList()
{
  delete m_Watcher;
  m_Watcher = nullptr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QtSRecentFileList* QtSRecentFileList::instance()
{
  // qDebug() << "QtSRecentFileList::instance()" << "\n";
  static QtSRecentFileList* singleton;

  if(singleton == nullptr)
  {
    singleton = new QtSRecentFileList();
  }
  return singleton;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool QtSRecentFileList::contains(const QString& file)
{
  return this->recentFiles.contains(file);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSRecentFileList::addFile(const QString& file, AddType type)
{
  if(QFile::exists(file) == true)
  {
    // Remove the file from wherever it is in the list
    removeFile(file);

    if(recentFiles.size() == 7)
    {
      recentFiles.pop_back();
    }

    if(type == APPEND)
    {
      this->recentFiles.append(file);
    }
    else
    {
      this->recentFiles.prepend(file);
    }

    // Add the path to the watcher
    m_Watcher->addPath(file);

    emit fileListChanged(file); // Emit the signal so all the menus can update their contents
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList QtSRecentFileList::fileList()
{
  return this->recentFiles;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSRecentFileList::removeFile(const QString& file)
{
  this->recentFiles.removeAll(file);

  // Remove the path from the watcher
  m_Watcher->removePath(file);

  emit fileListChanged(file); // Emit the signal so all the menus can update their contents
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSRecentFileList::clear()
{
  this->recentFiles.clear();

  emit fileListChanged(""); // Emit the signal so all the menus can update their contents
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSRecentFileList::writeList(QtSSettings* prefs)
{
  prefs->setValue("Recent Files", this->fileList());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void QtSRecentFileList::readList(QtSSettings* prefs)
{
  this->clear();

  QStringList list = prefs->value("Recent Files", QStringList());

  for(int i = 0; i < list.size(); i++)
  {
    QString filePath = list[i];
    QFile file(filePath);
    if(file.exists())
    {
      this->addFile(filePath, APPEND);
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSRecentFileList::strippedName(const QString& fullFileName)
{
  return QFileInfo(fullFileName).fileName();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString QtSRecentFileList::parentAndFileName(const QString& file)
{
  QFileInfo fileinfo(file);

  QDir parent = fileinfo.dir();
  return parent.dirName() + QDir::separator() + fileinfo.fileName();
}
