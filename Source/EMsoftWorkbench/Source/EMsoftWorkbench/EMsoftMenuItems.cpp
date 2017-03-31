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

#include "EMsoftMenuItems.h"

#include <QtCore/QJsonDocument>
#include <QtCore/QJsonParseError>

#include <QtWidgets/QApplication>

EMsoftMenuItems* EMsoftMenuItems::self = nullptr;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftMenuItems::EMsoftMenuItems(QObject* parent)
: QObject(parent)
{
  createMenus();
  createActions();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftMenuItems::~EMsoftMenuItems()
{
  delete m_MenuRecentFiles;
  delete m_ActionOpen;
  delete m_ActionSave;
  delete m_ActionSaveAs;
  delete m_ActionClearRecentFiles;
  delete m_ActionExit;

  // View Menu
//  delete m_ActionShowFilterLibrary;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EMsoftMenuItems* EMsoftMenuItems::Instance()
{
  if(nullptr == self)
  {
    self = new EMsoftMenuItems();
  }

  return self;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftMenuItems::createMenus()
{
  m_MenuRecentFiles = new QMenu("Recent Files");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void EMsoftMenuItems::createActions()
{
  m_ActionExit = new QAction("Exit " + QApplication::applicationName(), this);
  m_ActionOpen = new QAction("Open Master File...", this);
  m_ActionClearRecentFiles = new QAction("Clear Recent Files", this);
  m_ActionAboutEMsoftWorkbench = new QAction("About " + QApplication::applicationName(), this);
  m_ActionSave = new QAction("Save", this);
  m_ActionSaveAs = new QAction("Save As...", this);

  m_ActionOpen->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_O));
#if defined(Q_OS_WIN)
  m_ActionExit->setShortcut(QKeySequence(Qt::ALT + Qt::Key_F4));
#else
  m_ActionExit->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_Q));
#endif
  m_ActionSave->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_S));
  m_ActionSaveAs->setShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_S));
}
