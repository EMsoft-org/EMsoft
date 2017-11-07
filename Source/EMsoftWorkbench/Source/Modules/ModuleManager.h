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

#ifndef _ModuleManager_h_
#define _ModuleManager_h_

#include <QtCore/QStringList>

#include "Modules/IModuleFactory.hpp"

class IWorkbenchModule;

/**
 * @brief The ModuleManager class manages instances of modules and is mainly used to keep
 * track of all module instances. This class uses the Singleton design pattern.
 */
class ModuleManager
{
  public:
    virtual ~ModuleManager();

    typedef QMap<QString, IModuleFactory::Pointer> Collection;
    typedef QMapIterator<QString, IModuleFactory::Pointer> CollectionIterator;

    /**
     * @brief Static instance to retrieve the global instance of this class
     * @return
     */
    static ModuleManager* Instance();

    /**
     * @brief addModuleFactory
     * @param name
     * @param module
     */
    void addModuleFactory(const QString &moduleName, IModuleFactory::Pointer moduleFactory);

    /**
     * @brief ModuleManager::printModuleNames
     */
    void printModuleNames();

    /**
     * @brief getFactoryForModule
     * @param moduleName
     * @return
     */
    IModuleFactory::Pointer getFactoryForModule(const QString& moduleName);

    /**
     * @brief getModuleNames Returns all module names as a QStringList
     * @return
     */
    QStringList getModuleNames();

    /**
     * @brief getModuleFromName
     * @param moduleName
     * @return
     */
    IModuleUI* getModuleFromName(const QString &moduleName, QJsonObject initObj = QJsonObject(), QWidget *parent = nullptr);

  protected:
    ModuleManager();

  private:
    static ModuleManager* self;

    Collection m_Factories;

    ModuleManager(const ModuleManager&); // Copy Constructor Not Implemented
    void operator=(const ModuleManager&); // Operator '=' Not Implemented
};

#endif /* _ModuleManager_H_ */

