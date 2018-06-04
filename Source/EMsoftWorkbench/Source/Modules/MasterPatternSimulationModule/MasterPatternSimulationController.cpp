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

#include <math.h>
#include <functional>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QDateTime>
#include <QtCore/QThread>

#include <QtNetwork/QHostInfo>

#include "Common/Constants.h"
#include "Common/ProjectionConversions.hpp"
#include "Common/MonteCarloFileReader.h"
#include "Common/EMsoftFileWriter.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/QH5Utilities.h"
#include "H5Support/HDF5ScopedFileSentinel.h"
#include "H5Support/QH5Lite.h"

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
  if(nullptr != obj) {
    obj->setUpdateProgress(loopCompleted,totalLoops, EloopCompleted, totalEloops);
  }

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MasterPatternSimulationController::MasterPatternSimulationController(QObject* parent) :
  QObject(parent),
  m_Cancel(false)
{
  m_InstanceKey = ++k_InstanceKey;

  m_MonteCarloReader = new MonteCarloFileReader();
  connect(m_MonteCarloReader, &MonteCarloFileReader::errorMessageGenerated, [=] (const QString &msg) { emit errorMessageGenerated(msg); });
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
    if (fi.suffix().compare("") == 0)
    {
      simData.inputFilePath.append(".h5");
    }
  }
  {
    QFileInfo fi(simData.outputFilePath);
    if (fi.suffix().compare("") == 0)
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
  //  if (getwriteJSON() == true)
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
  if (!m_MonteCarloReader->openFile(inputFilePath)) {
    return;
  }

  // If we couldn't get these three variables, then bail
  if (!m_MonteCarloReader->getAtomPos(m_Atompos) ||
      !m_MonteCarloReader->getAtomTypes(m_Atomtypes) ||
      !m_MonteCarloReader->getLatticeParameters(m_Latparm)) {
    return;
  }

  Int32ArrayType::Pointer iParPtr = getIParPtr(simData);
  if (iParPtr == Int32ArrayType::NullPointer()) {
    return;
  }

  FloatArrayType::Pointer fParPtr = getFParPtr(simData);
  if (fParPtr == FloatArrayType::NullPointer()) {
    return;
  }

  int32_t* iPar = iParPtr->getPointer(0);
  float* fPar = fParPtr->getPointer(0);

  // adjust the size of the mLPNH and mLPSH arrays to the correct one, since we did not have access
  // to the sizes in the datacheck() routine
  QVector<size_t> numTuples(1,1);
  QVector<size_t> cDims(4, 0);
  cDims[0] = 2*iPar[16]+1;
  cDims[1] = cDims[0];
  cDims[2] = iPar[11];
  //   cDims[3] = genericIPar[8];
  cDims[3] = 1;

  Int32ArrayType::Pointer accumzPtr = m_MonteCarloReader->getAccumzPtr();
  if (accumzPtr == Int32ArrayType::NullPointer()) {
    return;
  }

  int32_t *genericAccumz = accumzPtr->getPointer(0);

  // Create a new mLPNH Array
  m_GenericLPNHPtr = FloatArrayType::CreateArray(numTuples, cDims, "mLPNH", true);

  // Create a new mLPSH Array
  m_GenericLPSHPtr = FloatArrayType::CreateArray(numTuples, cDims, "mLPSH", true);

  float *genericLPNH = m_GenericLPNHPtr->getPointer(0);
  float *genericLPSH = m_GenericLPSHPtr->getPointer(0);

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
  EMsoftCgetEBSDmaster(iPar, fPar, m_Atompos.data(), m_Atomtypes.data(), m_Latparm.data(),
                       genericAccumz, genericLPNH, genericLPSH, &MasterPatternSimulationControllerProgress, m_InstanceKey, &m_Cancel);
  m_Executing = false;
  instances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if (m_Cancel == false)
  {
    bool success = writeEMsoftHDFFile(simData);
    if (success == false)
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
  m_GenericAccumzPtr.reset();
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
bool MasterPatternSimulationController::validateMasterPatternValues(MasterPatternSimulationController::MasterPatternSimulationData data)
{
  bool valid = true;

  QString inputPath = data.inputFilePath;
  QFileInfo inFi(inputPath);
  if (inFi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The input Monte Carlo file at path '%1' needs a '.h5' suffix.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }
  if (inFi.exists() == false) {
    QString ss =
        QObject::tr("The input Monte Carlo file with path '%1' does not exist.")
        .arg(inputPath);
    emit errorMessageGenerated(ss);
    valid = false;
  }

  QString outputPath = data.outputFilePath;

  QFileInfo dir(outputPath);
  QDir dPath = dir.path();
  if (dir.suffix().compare("") == 0)
  {
    outputPath.append(".h5");
  }
  if (dPath.exists() == false)
  {
    QString ss = QObject::tr("The directory path for the HDF5 file does not exist. DREAM.3D will attempt to create this path during execution of the filter");
    emit warningMessageGenerated(ss);
  }

  if(data.smallestDSpacing < 0) {
    QString ss = QObject::tr("dmin must be positive (see also help page)");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  if(data.numOfMPPixels < 0) {
    QString ss = QObject::tr("Number of pixels must be positive");
    emit errorMessageGenerated(ss);
    valid = false;
  }

  // test the Bethe Parameters (must be in increasing order)
  if( (data.betheParametersX > data.betheParametersY) || (data.betheParametersY > data.betheParametersZ) )
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
bool MasterPatternSimulationController::writeEMsoftHDFFile(MasterPatternSimulationController::MasterPatternSimulationData simData)
{
  QString inputFilePath = simData.inputFilePath;
  QString outputFilePath = simData.outputFilePath;
  QString tmpOutputFilePath = outputFilePath + ".tmp";
  QFileInfo tmpFi(tmpOutputFilePath);

  if (tmpFi.exists() == true)
  {
    if (!QFile::remove(tmpOutputFilePath))
    {
      QString ss = QObject::tr("Error creating temporary output file '%1'").arg(tmpFi.fileName());
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if (!QFile::copy(inputFilePath, tmpOutputFilePath))
  {
    QString ss = QObject::tr("Error creating temporary output file '%1'").arg(tmpFi.fileName());
    emit errorMessageGenerated(ss);
    return false;
  }

  QSharedPointer<EMsoftFileWriter> writer = QSharedPointer<EMsoftFileWriter>(new EMsoftFileWriter());
  connect(writer.data(), &EMsoftFileWriter::errorMessageGenerated, [=] (const QString &msg) { emit errorMessageGenerated(msg); });

  // Open the HDF5 file
  if (writer->openFile(tmpOutputFilePath) == false)
  {
    return false;
  }

  // Get the iPar and fPar arrays
  Int32ArrayType::Pointer genericIParPtr = m_MonteCarloReader->getIParPtr();
  FloatArrayType::Pointer genericFParPtr = m_MonteCarloReader->getFParPtr();
  if (genericIParPtr == Int32ArrayType::NullPointer() ||
      genericFParPtr == FloatArrayType::NullPointer())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // Create the EMData/EBSDmaster group
  if (writer->openGroup(EMsoft::Constants::EMData) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  int32_t *genericIPar = genericIParPtr->getPointer(0);
  float *genericFPar = genericFParPtr->getPointer(0);

  if (genericIPar[13] == 1)
  {
    if (writer->openGroup(EMsoft::Constants::EBSDmaster) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    if (writer->openGroup(EMsoft::Constants::ECPmaster) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // We need to store these variables first, because we are going to close the file in the file reader
  float* genericLPNH = m_GenericLPNHPtr->getPointer(0);
  float* genericLPSH = m_GenericLPSHPtr->getPointer(0);

  // we use the standard names "mLPNH" and "mLPSH", regardless of what the user entered for this array name;
  // the user-defined name is only relevant within DREAM.3D
  {
    QVector<hsize_t> dims(4);
    dims[0] = genericIPar[8];         // number of atom types
    dims[1] = genericIPar[11];        // number of energy bins
    dims[2] = 2*genericIPar[16]+1;    // number of x pixels
    dims[3] = dims[2];                  // number of y pixels

    if (writer->writePointerDataset(EMsoft::Constants::mLPNH, genericLPNH, dims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    // Create the stereographic northern hemisphere master pattern
    dims.pop_front();
    size_t zDim = dims[0];
    FloatArrayType::Pointer genericSPNHPtr = FloatArrayType::CreateArray(0, "masterSPNH");
    size_t offset = 0;
    for (int z = 0; z < zDim; z++)
    {
      ProjectionConversions projConversion(this);
      FloatArrayType::Pointer conversion = projConversion.convertLambertSquareData<float>(m_GenericLPNHPtr, dims[2], ModifiedLambertProjection::ProjectionType::Stereographic, z, ModifiedLambertProjection::Square::NorthSquare);

      genericSPNHPtr->resize(genericSPNHPtr->getNumberOfTuples() + conversion->getNumberOfTuples());
      for (int i = 0; i < conversion->getNumberOfTuples(); i++)
      {
        genericSPNHPtr->setValue(offset, conversion->getValue(i));
        offset++;
      }
    }

    float* genericSPNH = genericSPNHPtr->getPointer(0);

    // Write the stereographic northern hemisphere master pattern to the file
    if (writer->writePointerDataset(EMsoft::Constants::masterSPNH, genericSPNH, dims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  {
    QVector<hsize_t> dims(4);
    dims[0] = genericIPar[8];         // number of atom types
    dims[1] = genericIPar[11];        // number of energy bins
    dims[2] = 2*genericIPar[16]+1;    // number of x pixels
    dims[3] = dims[2];                  // number of y pixels

    if (writer->writePointerDataset(EMsoft::Constants::mLPSH, genericLPSH, dims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    // Create the stereographic southern hemisphere master pattern
    dims.pop_front();
    size_t zDim = dims[0];
    FloatArrayType::Pointer genericSPSHPtr = FloatArrayType::CreateArray(0, EMsoft::Constants::masterSPSH);
    size_t offset = 0;
    for (int z = 0; z < zDim; z++)
    {
      ProjectionConversions projConversion(this);
      FloatArrayType::Pointer conversion = projConversion.convertLambertSquareData<float>(m_GenericLPSHPtr, dims[2], ModifiedLambertProjection::ProjectionType::Stereographic, z, ModifiedLambertProjection::Square::NorthSquare);

      genericSPSHPtr->resize(genericSPSHPtr->getNumberOfTuples() + conversion->getNumberOfTuples());
      for (int i = 0; i < conversion->getNumberOfTuples(); i++)
      {
        genericSPSHPtr->setValue(offset, conversion->getValue(i));
        offset++;
      }
    }

    float* genericSPSH = genericSPSHPtr->getPointer(0);

    // Write the stereographic southern hemisphere master pattern to a file
    if (writer->writePointerDataset(EMsoft::Constants::masterSPSH, genericSPSH, dims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // and a few constants
  std::string dname;
  if (genericIPar[13] == 1)
  {
    if (writer->writeScalarDataset(EMsoft::Constants::numEbins, genericIPar[11]) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    int i = 1;
    if (writer->writeScalarDataset(EMsoft::Constants::lastEnergy, i) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }


    QVector<hsize_t> cDims(1,1);
    cDims[0] = 4;
    QVector<float> BP(cDims[0]);
    BP[0] = simData.betheParametersX;
    BP[1] = simData.betheParametersY;
    BP[2] = simData.betheParametersZ;
    BP[3] = 1.0;

    if (writer->writeVectorDataset(EMsoft::Constants::BetheParameters, BP, cDims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }

    cDims[0] = genericIPar[11];
    QVector<float> EkeV(cDims[0]);
    for(int i=0; i<genericIPar[11]; i++)
    {
      EkeV[i] = genericFPar[3]+ (float)i * genericFPar[4];
    }

    if (writer->writeVectorDataset(EMsoft::Constants::EkeVs, EkeV, cDims) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    double ekev;
    if (!m_MonteCarloReader->getAcceleratingVoltage(ekev))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
    if (writer->writeScalarDataset(EMsoft::Constants::EkeV, ekev) == false)
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if (writer->writeScalarDataset(EMsoft::Constants::numset, genericIPar[8]) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  QString xtalFileName = "";
  if (!m_MonteCarloReader->getXtalFileName(xtalFileName))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  dname = EMsoft::Constants::xtalname.toStdString();
  if (writer->writeStringDataset(EMsoft::Constants::xtalname, xtalFileName) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if (writer->closeGroup() == false)
  {
    return false;
  }
  if (writer->closeGroup() == false)
  {
    return false;
  }

  //--------------------------------
  // Create the EMheader group; this is common to all EMsoft output files, so in the future
  // we will move this to the EMsoftToolboxPlugin.cpp file
  if (writer->openGroup(EMsoft::Constants::EMheader) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->openGroup(EMsoft::Constants::EBSDmaster) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Date  (use QDateTime)
  QString date = QDateTime::currentDateTime().date().toString();
  if (writer->writeStringDataset(EMsoft::Constants::Date, date) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // StartTime, already defined in Execute()
  if (writer->writeStringDataset(EMsoft::Constants::StartTime, m_StartTime) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // StopTime
  QString time = QDateTime::currentDateTime().time().toString();
  if (writer->writeStringDataset(EMsoft::Constants::StopTime, time) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Hostname
  QString localHost = QHostInfo::localHostName();
  if (writer->writeStringDataset(EMsoft::Constants::HostName, localHost) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // ProgramName
  QString programName = "EMsoftWorkbench Master Pattern Simulation Module";
  if (writer->writeStringDataset(EMsoft::Constants::ProgramName, programName) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserName
  if (writer->writeStringDataset(EMsoft::Constants::UserName, getEMsoftUserName()) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserEmail
  if (writer->writeStringDataset(EMsoft::Constants::UserEmail, getEMsoftUserEmail()) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // UserLocation
  if (writer->writeStringDataset(EMsoft::Constants::UserLocation, getEMsoftUserLocation()) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Version
  QString version = "EMsoft 3.1.0";
  if (writer->writeStringDataset(EMsoft::Constants::Version, version) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // add the FixedLength identifier to this header
  // FixedLength
  int i = 1;
  if (writer->writeScalarDataset(EMsoft::Constants::FixedLength, i) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if (writer->closeGroup() == false)
  {
    return false;
  }
  if (writer->closeGroup() == false)
  {
    return false;
  }

  //--------------------------------
  // here we create a JSONfiles group that contains the jsonobject in its entirety
  if (writer->openGroup(EMsoft::Constants::JSONfiles) == false)
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
  if (writer->writeStringDataset(EMsoft::Constants::EBSDmasterJSON, strJson) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the group
  if (writer->closeGroup() == false)
  {
    return false;
  }

  //--------------------------------
  // Create the NMLparameters group
  if (writer->openGroup(EMsoft::Constants::NMLparameters) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->openGroup(EMsoft::Constants::BetheList) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if (writer->writeScalarDataset(EMsoft::Constants::c1, simData.betheParametersX) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->writeScalarDataset(EMsoft::Constants::c2, simData.betheParametersY) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->writeScalarDataset(EMsoft::Constants::c3, simData.betheParametersZ) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  float val = 1.0f;
  if (writer->writeScalarDataset(EMsoft::Constants::sgdbdiff, val) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the group
  if (writer->closeGroup() == false)
  {
    return false;
  }

  //--------------------------------
  // Create the EBSDMasterNameList group
  if (writer->openGroup(EMsoft::Constants::EBSDMasterNameList) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if (writer->writeScalarDataset(EMsoft::Constants::dmin, simData.smallestDSpacing) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->writeScalarDataset(EMsoft::Constants::npx, simData.numOfMPPixels) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->writeScalarDataset(EMsoft::Constants::nthreads, simData.numOfOpenMPThreads) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = 0;
  if (writer->writeScalarDataset(EMsoft::Constants::restart, i) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = -1;
  if (writer->writeScalarDataset(EMsoft::Constants::Esel, i) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  i = 6;
  if (writer->writeScalarDataset(EMsoft::Constants::Stdout, i) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->writeStringDataset(EMsoft::Constants::outname, simData.outputFilePath) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  int uniform = 0;
  if (writer->writeScalarDataset(EMsoft::Constants::uniform, uniform) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // and finally the energy (Monte Carlo) file name
  if (writer->writeStringDataset(EMsoft::Constants::energyfile, simData.inputFilePath) == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the groups
  if (writer->closeGroup() == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if (writer->closeGroup() == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Close the file
  if (writer->closeFile() == false)
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  QFileInfo outFi(outputFilePath);
  if (outFi.exists() == true)
  {
    if (!QFile::remove(outputFilePath))
    {
      QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
      emit errorMessageGenerated(ss);
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if (!QFile::rename(tmpOutputFilePath, outputFilePath))
  {
    QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
    emit errorMessageGenerated(ss);
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  return true;
}


int MasterPatternSimulationController::getNumCPUCores()
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
QString MasterPatternSimulationController::getEMsoftUserName()
{
  // get the UserName
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder+"/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserName"));
  return QString (EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MasterPatternSimulationController::getEMsoftUserEmail()
{
  // get the UserEmail
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder+"/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserEmail"));
  return QString (EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MasterPatternSimulationController::getEMsoftUserLocation()
{
  // get the UserLocation
  std::string homeFolder = QDir::homePath().toStdString();
  std::string configFile = homeFolder+"/.config/EMsoft/EMsoftConfig.json";
  QString val;
  QFile envFile(QString::fromStdString(configFile));
  envFile.open(QIODevice::ReadOnly);
  val = envFile.readAll();
  envFile.close();

  QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
  QJsonObject s = d.object();
  QJsonValue EMdatapathname = s.value(QString("UserLocation"));
  return QString (EMdatapathname.toString());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
Int32ArrayType::Pointer MasterPatternSimulationController::getIParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData)
{
  Int32ArrayType::Pointer iParPtr = m_MonteCarloReader->getIParPtr();
  if (iParPtr == Int32ArrayType::NullPointer()) {
    return Int32ArrayType::NullPointer();
  }

  int32_t* iPar = iParPtr->getPointer(0);

  iPar[16] = static_cast<size_t>(simData.numOfMPPixels);         // number of pixels in master pattern
  iPar[17] = static_cast<size_t>(simData.numOfOpenMPThreads);    // number of OpenMP threads to be used

  return iParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer MasterPatternSimulationController::getFParPtr(MasterPatternSimulationController::MasterPatternSimulationData simData)
{
  FloatArrayType::Pointer fParPtr = m_MonteCarloReader->getFParPtr();
  if (fParPtr == FloatArrayType::NullPointer()) {
    return FloatArrayType::NullPointer();
  }

  float* fPar = fParPtr->getPointer(0);

  fPar[10] = simData.smallestDSpacing;
  fPar[11] = simData.betheParametersX;
  fPar[12] = simData.betheParametersY;
  fPar[13] = simData.betheParametersZ;

  return fParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MasterPatternSimulationController::setUpdateProgress(int loopCompleted, int totalLoops, int EloopCompleted, int totalEloops)
{
  QString ss = QObject::tr("Master Pattern steps completed: %1 of %2; %3 of %4 energy bins").arg(loopCompleted).arg(totalLoops).arg(EloopCompleted).arg(totalEloops);
  emit stdOutputMessageGenerated(ss);
}
