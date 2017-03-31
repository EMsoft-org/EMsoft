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

#ifndef _EMsoftMenuItems_h_
#define _EMsoftMenuItems_h_

#include <QtCore/QObject>

#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

class EMsoftMenuItems : public QObject
{
    Q_OBJECT

  public:
    virtual ~EMsoftMenuItems();

    static EMsoftMenuItems* Instance();

  protected:
    EMsoftMenuItems(QObject* parent = 0);

  signals:
    void clipboardHasChanged(bool canPaste);

  private:
    static EMsoftMenuItems*          self;

    SIMPL_INSTANCE_PROPERTY(bool, CanPaste)

    // File Menu
    SIMPL_INSTANCE_PROPERTY(QMenu*, MenuRecentFiles)
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionOpen)
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionSave)
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionSaveAs)
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionClearRecentFiles)
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionExit)

    // View Menu
//    SIMPL_INSTANCE_PROPERTY(QAction*, ActionShowFilterLibrary)

    // Help Menu
    SIMPL_INSTANCE_PROPERTY(QAction*, ActionAboutEMsoftWorkbench)

    void createMenus();
    void createActions();

    EMsoftMenuItems(const EMsoftMenuItems&); // Copy Constructor Not Implemented
    void operator=(const EMsoftMenuItems&); // Operator '=' Not Implemented
};

#endif /* EMsoftMenuItems_H_ */

