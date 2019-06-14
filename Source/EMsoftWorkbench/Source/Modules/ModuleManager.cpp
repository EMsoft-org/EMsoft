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

#include "ModuleManager.h"

#include <QtCore/QDebug>

#include "Common/Constants.h"

#include "Modules/AverageDotProductMapModule/AverageDotProductMapModule.h"
#include "Modules/AverageDotProductMapModule/Constants.h"
#include "Modules/CrystalStructureCreationModule/CrystalStructureCreationModule.h"
#include "Modules/MasterPatternSimulationModule/MasterPatternSimulationModule.h"
#include "Modules/ModuleFactory.hpp"
#include "Modules/MonteCarloSimulationModule/MonteCarloSimulationModule.h"
#include "Modules/PatternDisplayModule/PatternDisplayModule.h"
#include "Modules/PatternFitModule/PatternFitModule.h"

ModuleManager* ModuleManager::self = nullptr;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModuleManager::ModuleManager()
{
  //  qDebug() << "ModuleManager()" << this;
  Q_ASSERT_X(!self, "ModuleManager", "There should be only one ModuleManager object");
  ModuleManager::self = this;

  // These modules are hard-coded, but can eventually be registered using CMake in the future.
  ModuleFactory<PatternDisplayModule>::Pointer patternDisplayModuleFactory = ModuleFactory<PatternDisplayModule>::New();
  addModuleFactory(EMsoftWorkbenchConstants::ModuleNames::PatternDisplay, patternDisplayModuleFactory);

  ModuleFactory<PatternFitModule>::Pointer patternFitModuleFactory = ModuleFactory<PatternFitModule>::New();
  addModuleFactory(EMsoftWorkbenchConstants::ModuleNames::PatternFit, patternFitModuleFactory);

  ModuleFactory<CrystalStructureCreationModule>::Pointer crystalStructCreationModuleFactory = ModuleFactory<CrystalStructureCreationModule>::New();
  addModuleFactory(EMsoftWorkbenchConstants::ModuleNames::CrystalStructureCreation, crystalStructCreationModuleFactory);

  ModuleFactory<MonteCarloSimulationModule>::Pointer mcSimulationModuleFactory = ModuleFactory<MonteCarloSimulationModule>::New();
  addModuleFactory(EMsoftWorkbenchConstants::ModuleNames::MonteCarloSimulation, mcSimulationModuleFactory);

  ModuleFactory<MasterPatternSimulationModule>::Pointer mpSimulationModuleFactory = ModuleFactory<MasterPatternSimulationModule>::New();
  addModuleFactory(EMsoftWorkbenchConstants::ModuleNames::MasterPatternSimulation, mpSimulationModuleFactory);

  ModuleFactory<AverageDotProductMapModule>::Pointer avgDotProductMapModuleFactory = ModuleFactory<AverageDotProductMapModule>::New();
  addModuleFactory(AverageDotProductMapModuleConstants::ModuleName, avgDotProductMapModuleFactory);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModuleManager::~ModuleManager() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModuleManager* ModuleManager::Instance()
{
  if(self == nullptr)
  {
    //  qDebug() << "ModuleManager::Instance self was nullptr" << "\n";
    self = new ModuleManager();
  }
  return self;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModuleManager::addModuleFactory(const QString& moduleName, const IModuleFactory::Pointer& moduleFactory)
{
  m_Factories.insert(moduleName, moduleFactory);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModuleManager::printModuleNames() const
{
  QStringList names = getModuleNames();
  for(int i = 0; i < names.size(); i++)
  {
    qDebug() << "Name: " << names[i] << "\n";
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList ModuleManager::getModuleNames() const
{
  QStringList names;
  for(Collection::const_iterator iter = m_Factories.begin(); iter != m_Factories.end(); ++iter)
  {
    names.push_back(iter.key());
  }

  return names;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IModuleFactory::Pointer ModuleManager::getFactoryForModule(const QString& moduleName) const
{
  return m_Factories[moduleName];
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
IModuleUI* ModuleManager::getModuleFromName(const QString& moduleName, const QJsonObject& initObj, QWidget* parent) const
{
  IModuleFactory::Pointer moduleFactory = getFactoryForModule(moduleName);
  if(moduleFactory != IModuleFactory::NullPointer())
  {
    IWorkbenchModule::Pointer module = moduleFactory->createModule();
    if(module != IWorkbenchModule::NullPointer())
    {
      IModuleUI* module_ui = module->createModuleUI(initObj, parent);
      return module_ui;
    }
  }

  return nullptr;
}
