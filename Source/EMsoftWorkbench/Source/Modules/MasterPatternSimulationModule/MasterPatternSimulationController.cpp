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

#include <cmath>
#include <functional>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include <QtCore/QDateTime>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QThread>

#include <QtNetwork/QHostInfo>

#include "Common/Constants.h"
#include "Common/EMsoftFileWriter.h"
#include "Common/MonteCarloFileReader.h"
#include "Common/ProjectionConversions.hpp"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"
#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "Modules/MonteCarloSimulationModule/cl.hpp"

#define CL_VECTOR std::vector

static size_t k_InstanceKey = 0;
static QMap<size_t, MasterPatternSimulationController*> instances;

/**
 * @brief MasterPatternSimulationControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void MasterPatternSimulationControllerProgress(size_t instance, int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops)
{
  MasterPatternSimulationController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops, EloopCompleted, totalEloops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationController(QObject* parent)
: QObject(parent)
, m_Cancel(false)
{
  m_InstanceKey = ++k_InstanceKey;

  m_MonteCarloReader = new MonteCarloFileReader();
  connect(m_MonteCarloReader, &MonteCarloFileReader::errorMessageGenerated, [=](const QString& msg) { emit errorMessageGenerated(msg); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::~MasterPatternSimulationController()
{
  k_InstanceKey--;
  delete m_MonteCarloReader;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::createMasterPattern(MasterPatternSimulationController::MasterPatternSimulationData simData)
{
  {
    QFileInfo fi(simData.inputFilePath);
    if(fi.suffix().compare("") == 0)
    {
      simData.inputFilePath.append(".h5");
    }
  }
  {
    QFileInfo fi(simData.outputFilePath);
    if(fi.suffix().compare("") == 0)
    {
      simData.outputFilePath.append(".h5");
    }
  }

  initializeData();

  //  std::cout << "Xtal name = " << XtalName << std::endl;

  //  /**
  //   * @brief save the entries in a json file for EMsoftEBSDmaster reading.
  //   * @return
  //   */
  //  if (getwriteJSON())
  //  {
  //      EMsoftToolboxPlugin p;
  //      QString EMDataPathName = p.getEMdatapathname();
  //      QString EMJsonFilePath = EMDataPathName.append("/").append(getJsonFile());

  //      QFileInfo fi(EMJsonFilePath);
  //      QString parentPath = fi.path();
  //      QDir dir;
  //      if(!dir.mkpath(parentPath))
  //      {
  //        setErrorCondition(-720059);
  //        QString ss = QObject::tr("Error creating parent path '%1'").arg(parentPath);
  //        notifyErrorMessage(getHumanLabel(), ss, getErrorCondition());
  //        return;
  //      }

  //      QFile saveFile(EMJsonFilePath);
  //      saveFile.open(QIODevice::WriteOnly);
  //      QJsonObject topObject;
  //      QJsonObject rootObject;
  //      rootObject.insert("npx",getNpx());
  //      rootObject.insert("nthreads",getNthreads());
  //      rootObject.insert("dmin",getDmin());
  //      rootObject.insert("outname",getDataName());
  //      topObject.insert("EBSDmastervars",rootObject);
  //      QJsonDocument saveDoc(topObject);
  //      saveFile.write(saveDoc.toJson());
  //  }

  QString inputFilePath = simData.inputFilePath;

  // If we couldn't open the Monte Carlo file, then bail
  if(!m_MonteCarloReader->openFile(inputFilePath))
  {
    return;
  }

  // If we couldn't get these three variables, then bail
  if(!m_MonteCarloReader->getAtomPos(m_Atompos) || !m_MonteCarloReader->getAtomTypes(m_Atomtypes) || !m_MonteCarloReader->getLatticeParameters(m_Latparm))
  {
    return;
  }

  std::vector<int32_t> iParPtr = getIParPtr(simData);
  if(iParPtr.empty())
  {
    return;
  }

  std::vector<float> fParPtr = getFParPtr(simData);
  if(fParPtr.empty())
  {
    return;
  }

  // adjust the size of the mLPNH and mLPSH arrays to the correct one, since we did not have access
  // to the sizes in the datacheck() routine
  std::vector<size_t> dims(4, 0);
  dims[0] = 2 * iParPtr[16] + 1;
  dims[1] = dims[0];
  dims[2] = iParPtr[11];
  //   cDims[3] = genericIPar[8];
  dims[3] = 1;

  size_t dimsSize = std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<size_t>());

  std::vector<int32_t> accumzPtr = m_MonteCarloReader->getAccumzPtr();
  if(accumzPtr.empty())
  {
    return;
  }

  // Create a new mLPNH Array
  m_GenericLPNHPtr.resize(dimsSize);
  std::fill(m_GenericLPNHPtr.begin(), m_GenericLPNHPtr.end(), 0.0f);

  // Create a new mLPSH Array
  m_GenericLPSHPtr.resize(dimsSize);
  std::fill(m_GenericLPSHPtr.begin(), m_GenericLPSHPtr.end(), 0.0f);

  // Set the start time for this run (m_StartTime)
  m_StartTime = QDateTime::currentDateTime().time().toString();

  // the EMsoft call will return two arrays: mLPNH and mLPSH
  // call the EMsoft EMsoftCgetEBSDmaster routine to compute the patterns;
  // m_Executing enables the Cancel button to properly work by passing
  // on a m_Cancel flag to the EMsoft routine; the m_InstanceKey provides
  // a unique label to this particular instantiation of this filter, so that
  // multiple simultaneous instantiations of this filter become possible without
  // incorrect interactions between the callback routines.
  m_Executing = true;
  instances[m_InstanceKey] = this;
  EMsoftCgetEBSDmaster(iParPtr.data(), fParPtr.data(), m_Atompos.data(), m_Atomtypes.data(), m_Latparm.data(), accumzPtr.data(), m_GenericLPNHPtr.data(), m_GenericLPSHPtr.data(), &MasterPatternSimulationControllerProgress, m_InstanceKey,
                       &m_Cancel);
  m_Executing = false;
  instances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(!m_Cancel)
  {
    bool success = writeEMsoftHDFFile(simData);
    if(!success)
    {
      emit stdOutputMessageGenerated("Master Pattern File Generation Failed");
      return;
    }

    emit stdOutputMessageGenerated("Master Pattern File Generation Complete");
  }
  else
  {
    m_MonteCarloReader->closeFile();
    emit stdOutputMessageGenerated("Master Pattern File Generation was successfully canceled");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::initializeData()
{
  m_Atompos.clear();
  m_Atomtypes.clear();
  m_Latparm.clear();
  m_GenericAccumzPtr.clear();
  m_StartTime = "";
  m_CrystalSystem = 0;
  m_Natomtypes = 0;
  m_SpaceGroupNumber = 0;
  m_SpaceGroupSetting = 0;
  m_Executing = false;
  m_Cancel = false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MasterPatternSimulationController::validateMasterPatternValues(MasterPatternSimulationController::MasterPatternSimulationData data) const
{
  bool valid = true;

  QString inputPath = data.inputFilePath;
  QFileInfo inFi(inputPath);
  if(inFi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The input Monte Carlo file at path '%1' needs a '.h5' suffix.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if(!inFi.exists())
  {
    QString ss = QObject::tr("The input Monte Carlo file with path '%1' does not exist.").arg(inputPath);
    emit errorMessageGenerated(ss);
    valid = false;
  }

  QString outputPath = data.outputFilePath;

  QFileInfo dir(outputPath);
  QDir dPath = dir.path();
  if(dir.suffix().compare("") == 0)
  {
    outputPath.append(".h5");
  }
  if(!dPath.exists())
  {
    QString ss = QObject::tr("The directory path for the HDF5 file does not exist. DREAM.3D will attempt to create this path during execution of the filter");
    emit warningMessageGenerated(ss);
  }

  if(data.smallestDSpacing < 0)
  {
    QString ss = QObject::tr("dmin must be positive (see also help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  if(data.numOfMPPixels < 0)
  {
    QString ss = QObject::tr("Number of pixels must be positive");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  // test the Bethe Parameters (must be in increasing order)
  if((data.betheParametersX > data.betheParametersY) || (data.betheParametersY > data.betheParametersZ))
  {
    QString ss = QObject::tr("Bethe parameters must be listed from smallest to largest (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }
  if(data.betheParametersX < 0.0)
  {
    QString ss = QObject::tr("All Bethe parameters must be positive (see help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  return valid;
}

// -----------------------------------------------------------------------------
//  This routine creates an EMsoft h5 file with the master pattern data in it, so
//  that EMEBSD and similar programs can read it... This is not precisely the same
//  file as would be written by the EMEBSDmaster program, since the JSONfiles group
//  actually contains a complete (serialized) json file instead of a namelist file; this is not
//  a problem, since all the entries in the json/nml file are explicitly parsed out
//  in the NMLparameters group anyway...
// -----------------------------------------------------------------------------
bool MasterPatternSimulationController::writeEMsoftHDFFile(MasterPatternSimulationController::MasterPatternSimulationData simData) const
{
  QString inputFilePath = simData.inputFilePath;
  QString outputFilePath = simData.outputFilePath;
  QString tmpOutputFilePath = outputFilePath + ".tmp";
  QFileInfo tmpFi(tmpOutputFilePath);

  if(tmpFi.exists())
  {
    if(!QFile::remove(tmpOutputFilePath))
    {
      QString ss = QObject::tr("Error creating temporary output file '%1'").arg(tmpFi.fileName());
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(!QFile::copy(inputFilePath, tmpOutputFilePath))
  {
    QString ss = QObject::tr("Error creating temporary output file '%1'").arg(tmpFi.fileName());
    emit errorMessageGenerated(ss);
    return false;
  }

  QSharedPointer<EMsoftFileWriter> writer = QSharedPointer<EMsoftFileWriter>(new EMsoftFileWriter());
  connect(writer.data(), &EMsoftFileWriter::errorMessageGenerated, [=](const QString& msg) { emit errorMessageGenerated(msg); });

  // Open the HDF5 file
  if(!writer->openFile(tmpOutputFilePath))
  {
    return false;
  }

  // Get the iPar and fPar arrays
  std::vector<int32_t> genericIParPtr = getIParPtr(simData);
  std::vector<float> genericFParPtr = getFParPtr(simData);
  if(genericIParPtr.empty() || genericFParPtr.empty())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // Create the EMData/EBSDmaster group
  if(!writer->openGroup(EMsoft::Constants::EMData))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(genericIParPtr[13] == 1)
  {
    if(!writer->openGroup(EMsoft::Constants::EBSDmaster))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    if(!writer->openGroup(EMsoft::Constants::ECPmaster))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // we use the standard names "mLPNH" and "mLPSH", regardless of what the user entered for this array name;
  // the user-defined name is only relevant within DREAM.3D
  {
    QVector<hsize_t> dims(4);
    dims[0] = genericIParPtr[8];          // number of atom types
    dims[1] = genericIParPtr[11];         // number of energy bins
    dims[2] = 2 * genericIParPtr[16] + 1; // number of x pixels
    dims[3] = dims[2];                 // number of y pixels

    if(!writer->writePointerDataset(EMsoft::Constants::mLPNH, m_GenericLPNHPtr.data(), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    // Create the stereographic northern hemisphere master pattern
    dims.pop_front();
    size_t zDim = dims[0];
    std::vector<float> genericSPNHPtr;
    size_t offset = 0;
    for(int z = 0; z < zDim; z++)
    {
      ProjectionConversions projConversion(this);
      std::vector<float> conversion =
          projConversion.convertLambertSquareData<float>(m_GenericLPNHPtr, dims[2], ModifiedLambertProjection::ProjectionType::Stereographic, z, ModifiedLambertProjection::Square::NorthSquare);

      genericSPNHPtr.resize(genericSPNHPtr.size() + conversion.size());
      for(const float &value : conversion)
      {
        genericSPNHPtr.at(offset) = value;
        offset++;
      }
    }

    // Write the stereographic northern hemisphere master pattern to the file
    if(!writer->writePointerDataset(EMsoft::Constants::masterSPNH, genericSPNHPtr.data(), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  {
    QVector<hsize_t> dims(4);
    dims[0] = genericIParPtr[8];          // number of atom types
    dims[1] = genericIParPtr[11];         // number of energy bins
    dims[2] = 2 * genericIParPtr[16] + 1; // number of x pixels
    dims[3] = dims[2];                 // number of y pixels

    if(!writer->writePointerDataset(EMsoft::Constants::mLPSH, m_GenericLPSHPtr.data(), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    // Create the stereographic southern hemisphere master pattern
    dims.pop_front();
    size_t zDim = dims[0];
    std::vector<float> genericSPSHPtr(0);
    size_t offset = 0;
    for(int z = 0; z < zDim; z++)
    {
      ProjectionConversions projConversion(this);
      std::vector<float> conversion =
          projConversion.convertLambertSquareData<float>(m_GenericLPSHPtr, dims[2], ModifiedLambertProjection::ProjectionType::Stereographic, z, ModifiedLambertProjection::Square::NorthSquare);

      genericSPSHPtr.resize(genericSPSHPtr.size() + conversion.size());
      for(const float &value : conversion)
      {
        genericSPSHPtr.at(offset) = value;
        offset++;
      }
    }

    // Write the stereographic southern hemisphere master pattern to a file
    if(!writer->writePointerDataset(EMsoft::Constants::masterSPSH, genericSPSHPtr.data(), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // and a few constants
  std::string dname;
  if(genericIParPtr[13] == 1)
  {
    if(!writer->writeScalarDataset(EMsoft::Constants::numEbins, genericIParPtr[11]))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    int i = 1;
    if(!writer->writeScalarDataset(EMsoft::Constants::lastEnergy, i))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    QVector<hsize_t> cDims(1, 1);
    cDims[0] = 4;
    QVector<float> BP(cDims[0]);
    BP[0] = simData.betheParametersX;
    BP[1] = simData.betheParametersY;
    BP[2] = simData.betheParametersZ;
    BP[3] = 1.0;

    if(!writer->writeVectorDataset(EMsoft::Constants::BetheParameters, BP, cDims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    cDims[0] = genericIParPtr[11];
    QVector<float> EkeV(cDims[0]);
    for(int i = 0; i < genericIParPtr[11]; i++)
    {
      EkeV[i] = genericFParPtr[3] + (float)i * genericFParPtr[4];
    }

    if(!writer->writeVectorDataset(EMsoft::Constants::EkeVs, EkeV, cDims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    double ekev;
    if(!m_MonteCarloReader->getAcceleratingVoltage(ekev))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
    if(!writer->writeScalarDataset(EMsoft::Constants::EkeV, ekev))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::numset, genericIParPtr[8]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  QString xtalFileName = "";
  if(!m_MonteCarloReader->getXtalFileName(xtalFileName))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  dname = EMsoft::Constants::xtalname.toStdString();
  if(!writer->writeStringDataset(EMsoft::Constants::xtalname, xtalFileName))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if(!writer->closeGroup())
  {
    return false;
  }
  if(!writer->closeGroup())
  {
    return false;
  }

  //--------------------------------
  // Create the EMheader group; this is common to all EMsoft output files, so in the future
  // we will move this to the EMsoftToolboxPlugin.cpp file
  if(!writer->openGroup(EMsoft::Constants::EMheader))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->openGroup(EMsoft::Constants::EBSDmaster))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Date  (use QDateTime)
  QString date = QDateTime::currentDateTime().date().toString();
  if(!writer->writeStringDataset(EMsoft::Constants::Date, date))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // StartTime, already defined in Execute()
  if(!writer->writeStringDataset(EMsoft::Constants::StartTime, m_StartTime))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // StopTime
  QString time = QDateTime::currentDateTime().time().toString();
  if(!writer->writeStringDataset(EMsoft::Constants::StopTime, time))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Hostname
  QString localHost = QHostInfo::localHostName();
  if(!writer->writeStringDataset(EMsoft::Constants::HostName, localHost))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // ProgramName
  QString programName = "EMsoftWorkbench Master Pattern Simulation Module";
  if(!writer->writeStringDataset(EMsoft::Constants::ProgramName, programName))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserName
  if(!writer->writeStringDataset(EMsoft::Constants::UserName, getEMsoftUserName()))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserEmail
  if(!writer->writeStringDataset(EMsoft::Constants::UserEmail, getEMsoftUserEmail()))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserLocation
  if(!writer->writeStringDataset(EMsoft::Constants::UserLocation, getEMsoftUserLocation()))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Version
  QString version = "EMsoft 3.1.0";
  if(!writer->writeStringDataset(EMsoft::Constants::Version, version))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // add the FixedLength identifier to this header
  // FixedLength
  int i = 1;
  if(!writer->writeScalarDataset(EMsoft::Constants::FixedLength, i))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if(!writer->closeGroup())
  {
    return false;
  }
  if(!writer->closeGroup())
  {
    return false;
  }

  //--------------------------------
  // here we create a JSONfiles group that contains the jsonobject in its entirety
  if(!writer->openGroup(EMsoft::Constants::JSONfiles))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  QJsonObject topObject;
  QJsonObject rootObject;
  rootObject.insert(EMsoft::Constants::npx, simData.numOfMPPixels);
  rootObject.insert(EMsoft::Constants::nthreads, simData.numOfOpenMPThreads);
  rootObject.insert(EMsoft::Constants::dmin, simData.smallestDSpacing);
  rootObject.insert(EMsoft::Constants::outname, simData.outputFilePath);
  topObject.insert(EMsoft::Constants::EBSDmastervars, rootObject);
  QJsonDocument doc(topObject);
  QString strJson(doc.toJson(QJsonDocument::Compact));
  if(!writer->writeStringDataset(EMsoft::Constants::EBSDmasterJSON, strJson))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the group
  if(!writer->closeGroup())
  {
    return false;
  }

  //--------------------------------
  // Create the NMLparameters group
  if(!writer->openGroup(EMsoft::Constants::NMLparameters))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->openGroup(EMsoft::Constants::BetheList))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::c1, simData.betheParametersX))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::c2, simData.betheParametersY))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::c3, simData.betheParametersZ))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  float val = 1.0f;
  if(!writer->writeScalarDataset(EMsoft::Constants::sgdbdiff, val))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the group
  if(!writer->closeGroup())
  {
    return false;
  }

  //--------------------------------
  // Create the EBSDMasterNameList group
  if(!writer->openGroup(EMsoft::Constants::EBSDMasterNameList))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::dmin, simData.smallestDSpacing))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::npx, simData.numOfMPPixels))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::nthreads, simData.numOfOpenMPThreads))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = 0;
  if(!writer->writeScalarDataset(EMsoft::Constants::restart, i))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = -1;
  if(!writer->writeScalarDataset(EMsoft::Constants::Esel, i))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = 6;
  if(!writer->writeScalarDataset(EMsoft::Constants::Stdout, i))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeStringDataset(EMsoft::Constants::outname, simData.outputFilePath))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  int uniform = 0;
  if(!writer->writeScalarDataset(EMsoft::Constants::uniform, uniform))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // and finally the energy (Monte Carlo) file name
  if(!writer->writeStringDataset(EMsoft::Constants::energyfile, simData.inputFilePath))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if(!writer->closeGroup())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->closeGroup())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the file
  if(!writer->closeFile())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  QFileInfo outFi(outputFilePath);
  if(outFi.exists())
  {
    if(!QFile::remove(outputFilePath))
    {
      QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
      emit errorMessageGenerated(ss);
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if(!QFile::rename(tmpOutputFilePath, outputFilePath))
  {
    QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
    emit errorMessageGenerated(ss);
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  return true;
}

int MasterPatternSimulationController::getNumCPUCores() const
{
  return QThread::idealThreadCount();
}
#if 0

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int MasterPatternSimulationController::getnumCLPlatforms()
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);

  return int (platforms.size());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList MasterPatternSimulationController::getPlatformInfo()
{
  QStringList platformInfos;
  CL_VECTOR<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  QString str;
  QTextStream out(&str);
  for(size_t i = 0; i < platforms.size(); i++)
  {
    cl::Platform curPlat = platforms[i];
    QString ss = QObject::tr("Platform ");
    ss.append(QString::number(i + 1));
    ss.append(": ");
    QString tt = QString::fromStdString(curPlat.getInfo<CL_PLATFORM_NAME>());
    ss.append(tt);
    platformInfos.push_back(ss);
  }
  return platformInfos;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int MasterPatternSimulationController::getNumCPUCores(int platformID)
{
  CL_VECTOR<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  if (platformID > platforms.size()) {
    return -1020;
  }
  cl::Platform selectedPlatform = platforms[platformID-1];

  CL_VECTOR<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_CPU, &devices);
  int numCores = 0;
  if (devices.size() > 0)
  {
	  cl::Device curDev = devices[0];
	  numCores = curDev.getInfo<CL_DEVICE_MAX_COMPUTE_UNITS>();
  }
  return numCores;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::writePlatformInfo()
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);

  for(size_t i=0; i<platforms.size(); i++){
    cl::Platform curPlat = platforms[i];
    QString ss = QObject::tr("Platform %1 info: ").arg(QString::number(i+1));
    QString tt = QString::fromStdString(curPlat.getInfo<CL_PLATFORM_NAME>());
    ss.append(tt);
    std::cout << ss.toStdString();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int MasterPatternSimulationController::getnumCLDevices(int platformID)
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  if (platformID > platforms.size()) {
    return -1000;
  }
  cl::Platform selectedPlatform = platforms[platformID - 1];

  std::vector<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  return int (devices.size());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::writeDeviceInfo(int platformID)
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  cl::Platform selectedPlatform = platforms[platformID-1];

  std::vector<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  for(int i=0; i<devices.size(); i++){
    cl::Device curDev = devices[i];
    QString ss = QObject::tr("Device %1 info: ").arg(QString::number(i+1));
    QString tt = QString::fromStdString(curDev.getInfo<CL_DEVICE_NAME>());
    ss.append(tt);
    std::cout << ss.toStdString();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList MasterPatternSimulationController::getDeviceInfo(int platformID)
{
  QStringList deviceInfos;

  CL_VECTOR<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  cl::Platform selectedPlatform = platforms[platformID - 1];

  CL_VECTOR<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  for(int i = 0; i < devices.size(); i++)
  {
    cl::Device curDev = devices[i];
    QString ss = QObject::tr("GPU Device ");
    ss.append(QString::number(i + 1));
    ss.append(": ");
    QString tt = QString::fromStdString(curDev.getInfo<CL_DEVICE_NAME>());
    ss.append(tt);
    deviceInfos.push_back(ss);
  }
  return deviceInfos;
}
#endif

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MasterPatternSimulationController::getEMsoftUserName() const
{
  // get the UserName
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder + "/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserName"));
  return QString(EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MasterPatternSimulationController::getEMsoftUserEmail() const
{
  // get the UserEmail
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder + "/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserEmail"));
  return QString(EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MasterPatternSimulationController::getEMsoftUserLocation() const
{
  // get the UserLocation
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder + "/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserLocation"));
  return QString(EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> MasterPatternSimulationController::getIParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData) const
{
  std::vector<int32_t> iParPtr = m_MonteCarloReader->getIParPtr();
  if(iParPtr.empty())
  {
    return std::vector<int32_t>();
  }

  iParPtr[16] = static_cast<size_t>(simData.numOfMPPixels);      // number of pixels in master pattern
  iParPtr[17] = static_cast<size_t>(simData.numOfOpenMPThreads); // number of OpenMP threads to be used

  return iParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> MasterPatternSimulationController::getFParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData) const
{
  std::vector<float> fParPtr = m_MonteCarloReader->getFParPtr();
  if(fParPtr.empty())
  {
    return std::vector<float>();
  }

  fParPtr[10] = simData.smallestDSpacing;
  fParPtr[11] = simData.betheParametersX;
  fParPtr[12] = simData.betheParametersY;
  fParPtr[13] = simData.betheParametersZ;

  return fParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setUpdateProgress(int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops) const
{
  QString ss = QObject::tr("Master Pattern steps completed: %1 of %2; %3 of %4 energy bins").arg(loopCompleted).arg(totalLoops).arg(EloopCompleted).arg(totalEloops);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setCancel(const bool& value)
{
  m_Cancel = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MasterPatternSimulationController::getCancel() const
{
  return m_Cancel;
}

