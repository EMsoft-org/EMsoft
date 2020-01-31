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

#include "MonteCarloSimulationController.h"

#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Workbench/Common/FileIOTools.h"

const QString k_ExeName = QString("EMMCOpenCL");
const QString k_NMLName = QString("EMMCOpenCL.nml");

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulationController::MonteCarloSimulationController(QObject* parent)
: IProcessController(k_ExeName, k_NMLName, parent)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulationController::~MonteCarloSimulationController()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::setData(const InputDataType& data)
{
  m_InputData = data;
}

// -----------------------------------------------------------------------------
void MonteCarloSimulationController::generateNMLFile(const QString& path)
{
  std::vector<std::string> nml;

  m_InputData.inputFilePath = FileIOTools::GetAbsolutePath(m_InputData.inputFilePath);
  m_InputData.outputFilePath = FileIOTools::GetAbsolutePath(m_InputData.outputFilePath);

  nml.emplace_back(std::string(" &MCCLdata"));
  nml.emplace_back(std::string("! only bse1, full or Ivol simulation"));
  if(m_InputData.mcMode == 1)
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry("mode", EMsoft::Constants::full, false));
  }

  nml.emplace_back(std::string("! name of the crystal structure file"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::xtalname, m_InputData.inputFilePath));
  nml.emplace_back(std::string("! number of pixels along x-direction of square projection [odd number!]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numsx, m_InputData.numOfPixelsN));
  nml.emplace_back(std::string("! for full mode: sample tilt angle from horizontal [degrees]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sig, static_cast<float>(m_InputData.sampleTiltAngleSig)));
  nml.emplace_back(std::string("! sample tilt angle around RD axis [degrees]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::omega, static_cast<float>(m_InputData.sampleRotAngleOmega)));

  nml.emplace_back(std::string("! for bse1 mode: start angle"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sigstart, static_cast<float>(m_InputData.sampleStartTiltAngle)));
  nml.emplace_back(std::string("! for bse1 mode: end angle"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sigend, static_cast<float>(m_InputData.sampleEndTiltAngle)));

  nml.emplace_back(std::string("! for bse1 mode: sig step size"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::sigstep, static_cast<float>(m_InputData.sampleTiltStepSize)));

  nml.emplace_back(std::string("! x, y, z number of voxels [odd numbers!]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivolx, m_InputData.ivolx));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivoly, m_InputData.ivoly));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivolz, m_InputData.ivolz));
  nml.emplace_back(std::string("! x, y, z voxel step sizes [nm]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivolstepx, static_cast<float>(m_InputData.ivolstepx)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivolstepy, static_cast<float>(m_InputData.ivolstepy)));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ivolstepz, static_cast<float>(m_InputData.ivolstepz)));

  nml.emplace_back(std::string("! number of incident electrons per thread"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::num_el, m_InputData.numOfEPerWorkitem));
  nml.emplace_back(std::string("! GPU platform ID selector"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::platid, m_InputData.gpuPlatformID + 1));
  nml.emplace_back(std::string("! GPU device ID selector"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::devid, m_InputData.gpuDeviceID + 1));
  nml.emplace_back(std::string("! number of work items (depends on GPU card; leave unchanged)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::globalworkgrpsz, m_InputData.globalWorkGroupSize));
  nml.emplace_back(std::string("! total number of incident electrons and multiplier (to get more than 2^(31)-1 electrons)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::totnumel, m_InputData.totalNumOfEConsidered));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::multiplier, m_InputData.multiplierForTotalNumOfE));
  nml.emplace_back(std::string("! incident beam energy [keV]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::EkeV, m_InputData.acceleratingVoltage));
  nml.emplace_back(std::string("! minimum energy to consider [keV]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::Ehistmin, m_InputData.minEnergyConsider));
  nml.emplace_back(std::string("! energy binsize [keV]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::Ebinsize, m_InputData.energyBinSize));
  nml.emplace_back(std::string("! maximum depth to consider for exit depth statistics [nm]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::depthmax, m_InputData.maxDepthConsider));
  nml.emplace_back(std::string("! depth step size [nm]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::depthstep, m_InputData.depthStepSize));
  nml.emplace_back(std::string("! should the user be notified by email or Slack that the program has completed its run?"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(QString("Notify"), QString("Off")));
  nml.emplace_back(std::string("! output data file name; pathname is relative to the EMdatapathname path !!!"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dataname, m_InputData.outputFilePath));

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
    // outputFile.copy("/tmp/EMMCOpenCL.nml");
  }
  else
  {
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(path));
  }
}

// -----------------------------------------------------------------------------
void MonteCarloSimulationController::processFinished()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloSimulationController::validateInput() const
{
  if(m_InputData.inputFilePath.isEmpty())
  {
    QString ss = QObject::tr("The crystal structure input file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }

  {
    QString inputFilePath = m_InputData.inputFilePath;

    QFileInfo fi(inputFilePath);
    if(fi.completeSuffix() != "xtal")
    {
      QString ss = QObject::tr("The crystal structure input file at path '%1' needs a '.xtal' suffix.").arg(inputFilePath);
      emit errorMessageGenerated(ss);
      return false;
    }
    if(!fi.exists())
    {
      QString ss = QObject::tr("The crystal structure input file at path '%1' does not exist.").arg(inputFilePath);
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(m_InputData.outputFilePath.isEmpty())
  {
    QString ss = QObject::tr("The monte carlo output file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }

  QString outputFilePath = m_InputData.outputFilePath;
  QFileInfo fi(outputFilePath);
  if(fi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The monte carlo output file at path '%1' needs a '.h5' suffix.").arg(outputFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  if(m_InputData.totalNumOfEConsidered < 0)
  {
    QString ss = QObject::tr("The total number of electrons must be > 0. Value = %1").arg(m_InputData.totalNumOfEConsidered);
    emit errorMessageGenerated(ss);
    return false;
  }

  // test sample tilt angles for EBSD
  if((m_InputData.sampleTiltAngleSig < 0) || (m_InputData.sampleTiltAngleSig >= 90))
  {
    QString ss = QObject::tr("Sample TD tilt angle must be in interval [0,90[");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.sampleRotAngleOmega < -45) || (m_InputData.sampleRotAngleOmega > 45))
  {
    QString ss = QObject::tr("Sample RD tilt angle must be in interval [-45,45]");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test sample tilt angles for ECP
  if((m_InputData.sampleStartTiltAngle < 0) || (m_InputData.sampleStartTiltAngle > m_InputData.sampleEndTiltAngle) || (m_InputData.sampleStartTiltAngle > 90))
  {
    QString ss = QObject::tr("Sample start tilt angle must be in interval [0,endangle]");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.sampleEndTiltAngle < m_InputData.sampleStartTiltAngle) || (m_InputData.sampleEndTiltAngle > 90))
  {
    QString ss = QObject::tr("Sample end tilt angle must be in interval [startangle,90]");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.sampleTiltStepSize < 0) || (m_InputData.sampleTiltStepSize > m_InputData.sampleEndTiltAngle - m_InputData.sampleStartTiltAngle))
  {
    QString ss = QObject::tr("Sample tilt step size must be positive, with at least one step in the [start,end] interval.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test voltage and step size
  if(m_InputData.acceleratingVoltage < 0)
  {
    QString ss = QObject::tr("Microscope accelerating voltage must be positive");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.minEnergyConsider < 0) || m_InputData.minEnergyConsider > m_InputData.acceleratingVoltage)
  {
    QString ss = QObject::tr("Voltage must be positive and less than the accelerating voltage");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.energyBinSize < 0) || m_InputData.energyBinSize > m_InputData.acceleratingVoltage - m_InputData.minEnergyConsider)
  {
    QString ss = QObject::tr("Voltage step size must be positive, with at least one bin.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test depth parameters
  if(m_InputData.maxDepthConsider <= 0)
  {
    QString ss = QObject::tr("Maximum depth must be strictly positive");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((m_InputData.depthStepSize <= 0) || (m_InputData.depthStepSize > m_InputData.maxDepthConsider))
  {
    QString ss = QObject::tr("Depth step size must be strictly positive, with at least one bin.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // numsx must be an odd number ...
  if((m_InputData.numOfPixelsN % 2) == 0)
  {
    QString ss = QObject::tr("Number of points must be odd.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // make sure the multiplier is strictly positive
  if(m_InputData.multiplierForTotalNumOfE < 1)
  {
    QString ss = QObject::tr("Multiplier must be at least 1");
    emit errorMessageGenerated(ss);
    return false;
  }

  return true;
}
