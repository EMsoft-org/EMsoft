/* ============================================================================
 * Copyright (c) 2009-2017 BlueQuartz Software, LLC
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

#include "MasterPatternSimulationController.h"

#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Workbench/Common/FileIOTools.h"

const QString k_ExeName = QString("EMEBSDmaster");
const QString k_NMLName = QString("EMEBSDmaster.nml");

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationController(QObject* parent)
: IProcessController(k_ExeName, k_NMLName, parent)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::~MasterPatternSimulationController()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setData(const InputDataType& data)
{
  m_InputData = data;
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::generateNMLFile(const QString& path)
{
  QFileInfo fi(path);
  QString betheParametersFilePath = fi.path() + QDir::separator() + "BetheParameters.nml";
  generateBetheParametersFile(betheParametersFilePath);

  std::vector<std::string> nml;

  m_InputData.energyFilePath = FileIOTools::GetAbsolutePath(m_InputData.energyFilePath);

  nml.emplace_back(std::string(" &EBSDmastervars"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dmin, static_cast<float>(m_InputData.smallestDSpacing)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::npx, m_InputData.numOfMPPixels));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nthreads, m_InputData.numOfOpenMPThreads));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energyfile, m_InputData.energyFilePath));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::BetheParametersFile, betheParametersFilePath));
  nml.emplace_back(FileIOTools::CreateNMLEntry(QString("Notify"), QString("Off")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::copyfromenergyfile, QString("undefined")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::h5copypath, QString("undefined")));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::restart, false));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::uniform, false));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::useEnergyWeighting, false));

  nml.emplace_back(std::string(" /"));

  QFile outputFile(path);
  if(outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    for(const auto& entry : nml)
    {
      out << QString::fromStdString(entry) << "\n";
    }
    outputFile.close();

    outputFile.copy("/tmp/EMEBSDmaster.nml");
  }
  else
  {
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(path));
  }
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::generateBetheParametersFile(const QString& path)
{
  std::vector<std::string> nml;

  m_InputData.energyFilePath = FileIOTools::GetAbsolutePath(m_InputData.energyFilePath);

  nml.emplace_back(std::string(" &Bethelist"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c1, static_cast<float>(m_InputData.betheParametersX)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c2, m_InputData.betheParametersY));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::c3, m_InputData.betheParametersZ));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sgdbdiff, 1.0));

  nml.emplace_back(std::string(" /"));

  QFile outputFile(path);
  if(outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    for(const auto& entry : nml)
    {
      out << QString::fromStdString(entry) << "\n";
    }
    outputFile.close();

    outputFile.copy("/tmp/BetheParameters.nml");
  }
  else
  {
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(path));
  }
}

// -----------------------------------------------------------------------------
void MasterPatternSimulationController::processFinished()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MasterPatternSimulationController::validateInput() const
{
  bool valid = true;

  QString energyFilePath = m_InputData.energyFilePath;
  QFileInfo inFi(energyFilePath);
  if(inFi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The energy file at path '%1' needs a '.h5' suffix.").arg(energyFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if(!inFi.exists())
  {
    QString ss = QObject::tr("The energy file with path '%1' does not exist.").arg(energyFilePath);
    emit errorMessageGenerated(ss);
    valid = false;
  }

  if(m_InputData.smallestDSpacing < 0)
  {
    QString ss = QObject::tr("dmin must be positive (see also help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  if(m_InputData.numOfMPPixels < 0)
  {
    QString ss = QObject::tr("Number of pixels must be positive");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  // test the Bethe Parameters (must be in increasing order)
  if((m_InputData.betheParametersX > m_InputData.betheParametersY) || (m_InputData.betheParametersY > m_InputData.betheParametersZ))
  {
    QString ss = QObject::tr("Bethe parameters must be listed from smallest to largest (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }
  if(m_InputData.betheParametersX < 0.0)
  {
    QString ss = QObject::tr("All Bethe parameters must be positive (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  return valid;
}
