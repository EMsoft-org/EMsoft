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

#include <cmath>

#include <functional>
#include <iostream>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include <QtCore/QCoreApplication>
#include <QtCore/QDateTime>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

#include <QtNetwork/QHostInfo>

#include "Common/Constants.h"
#include "Common/EMsoftFileWriter.h"
#include "Common/XtalFileReader.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#include "H5Support/HDF5ScopedFileSentinel.h"
#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "Modules/MonteCarloSimulationModule/cl.hpp"

#define CL_VECTOR std::vector

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

static size_t s_InstanceKey = 0;
static QMap<size_t, MonteCarloSimulationController*> s_ControllerInstances;

/**
 * @brief EMsoftMCOpenCLProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void MonteCarloSimulationControllerProgress(size_t instance, int loopCompleted, int totalLoops, float bseYield)
{
  MonteCarloSimulationController* obj = s_ControllerInstances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops, bseYield);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulationController::MonteCarloSimulationController(QObject* parent)
: QObject(parent)
, m_Cancel(false)
{
  m_XtalReader = new XtalFileReader();
  connect(m_XtalReader, &XtalFileReader::errorMessageGenerated, [=](const QString& msg) { emit errorMessageGenerated(msg); });

  int numberOfStrings = EMsoftWorkbenchConstants::Constants::SParSize;
  int stringSize = EMsoftWorkbenchConstants::Constants::SParStringSize;
  m_SPar = new char[numberOfStrings * stringSize];
  std::memset(m_SPar, '\0', numberOfStrings * stringSize);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MonteCarloSimulationController::~MonteCarloSimulationController()
{
  s_InstanceKey--;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::createMonteCarlo(MonteCarloSimulationController::MonteCarloSimulationData simData)
{
  initializeData(simData);

  //    /**
  //     * @brief save the entries in a json file for EMMCOpenCL reading.
  //     * @return
  //     */
  //    if (getwriteJSON())
  //    {
  //        EMsoftToolboxPlugin p;
  //        QString EMDataPathName = p.getEMdatapathname();
  //        QString EMJsonFilePath = EMDataPathName.append("/").append(getJsonFile());

  //        QFile saveFile(EMJsonFilePath);
  //        saveFile.open(QIODevice::WriteOnly);
  //        QJsonObject topObject;
  //        QJsonObject rootObject;
  //        if (data.mcMode == 0) {
  //            rootObject.insert("mode","full");
  //            rootObject.insert("sig",data.sampleTiltAngleSig);
  //            rootObject.insert("sig",data.sampleRotAngleOmega);
  //        } else {
  //            rootObject.insert("mode","bse1");
  //            rootObject.insert("sigstart",data.sampleStartTiltAngle);
  //            rootObject.insert("sigend",data.sampleEndTiltAngle);
  //            rootObject.insert("sigstep",data.sampleTiltAngleSig);
  //        }
  //        rootObject.insert("xtalname", data.inputCrystalFileName);
  //        rootObject.insert("numsx",data.numOfPixelsN);
  //        rootObject.insert("num_el",data.numOfEPerWorkitem);
  //        rootObject.insert("platid",data.gpuPlatformID);
  //        rootObject.insert("devid",data.gpuDeviceID);
  //        rootObject.insert("globalworkgrpsz",data.globalWorkGroupSize);
  //        rootObject.insert("totnum_el",data.totalNumOfEConsidered);
  //        rootObject.insert("multiplier",data.multiplierForTotalNumOfE);
  //        rootObject.insert("EkeV",data.acceleratingVoltage);
  //        rootObject.insert("Ehistmin",data.minEnergyConsider);
  //        rootObject.insert("Ebinsize",data.energyBinSize);
  //        rootObject.insert("depthmax",data.maxDepthConsider);
  //        rootObject.insert("depthstep",data.depthStepSize);
  //        rootObject.insert("dataname", data.outputFileName);
  //        topObject.insert("MCCLdata",rootObject);
  //        QJsonDocument saveDoc(topObject);
  //        saveFile.write(saveDoc.toJson());
  //    }

  QString inputFilePath = simData.inputFilePath;

  QFileInfo fi(inputFilePath);
  if(fi.suffix().compare("") == 0)
  {
    inputFilePath.append(".xtal");
  }

  // If we couldn't read the crystal structure file, then bail
  if(!m_XtalReader->openFile(inputFilePath))
  {
    return;
  }

  Int32ArrayType::Pointer iParPtr = getIParPtr(simData);
  if(iParPtr == Int32ArrayType::NullPointer())
  {
    return;
  }

  FloatArrayType::Pointer fParPtr = getFParPtr(simData);
  if(fParPtr == FloatArrayType::NullPointer())
  {
    return;
  }

  // Set the start time for this run (m_StartTime)
  m_StartTime = QDateTime::currentDateTime().time().toString();

  // the EMsoft call will return two arrays: accum_e and accum_z
  // call the EMsoft EMsoftCgetMCOpenCL routine to compute the patterns;
  // m_Executing enables the Cancel button to properly work by passing
  // on a m_Cancel flag to the EMsoft routine; the m_InstanceKey provides
  // a unique label to this particular instantiation of this filter, so that
  // multiple simultaneous instantiations of this filter become possible without
  // incorrect interactions between the callback routines.
  //  EMsoftCgetMCOpenCL(m_GenericIParPtr->getPointer(0), m_GenericFParPtr->getPointer(0), m_Atompos.data(), m_Atomtypes.data(),
  //                     m_Latparm.data(), m_GenericAccumePtr->getPointer(0), m_GenericAccumzPtr->getPointer(0),
  //                     &EMsoftMCOpenCLProgress, 0, &m_Cancel);
  emit stdOutputMessageGenerated(tr("Creating Monte Carlo Simulation..."));
  m_InstanceKey = ++s_InstanceKey;
  s_ControllerInstances.insert(m_InstanceKey, this);

  std::vector<float> latParm;
  std::vector<float> atomPos;
  std::vector<int32_t> atomTypes;

  // If we couldn't get these three variables, then bail
  if(!m_XtalReader->getAtomPos(atomPos) || !m_XtalReader->getAtomTypes(atomTypes) || !m_XtalReader->getLatticeParameters(latParm))
  {
    return;
  }

  QString appDirPath = QCoreApplication::applicationDirPath();
  QDir dir = QDir(appDirPath);

  emit stdOutputMessageGenerated(tr("dir: %1").arg(dir.absolutePath()));
  std::cout << "dir: " << dir.absolutePath().toStdString() << std::endl;

  QString openCLPath = dir.absolutePath(); // Initialize to SOMETHING other than empty.

  // Look to see if we are inside an .app package or inside the 'tools' directory
#if defined(Q_OS_MAC)
  if(dir.dirName() == "MacOS")
  {
    dir.cdUp();
    if(dir.cd("bin"))
    {
      openCLPath = dir.absolutePath();
    }
    else
    {
      dir.cdUp();
      dir.cdUp();
    }
  }
#endif
  // We use Bin for development builds and bin for deployments
  if(dir.dirName() == "Bin" || dir.dirName() == "bin")
  {
    openCLPath = dir.absolutePath();
  }

  if(!dir.cd("opencl"))
  {
    std::cout << "Unable to find opencl folder at path" << std::endl;
    // We are not able to find the opencl folder, so throw an error and bail
    errorMessageGenerated(tr("Unable to find opencl folder at path '%1'").arg(openCLPath));
    return;
  }

  emit stdOutputMessageGenerated(tr("Found opencl folder path: %1").arg(dir.absolutePath()));
  dir.cdUp();

  QString randomSeedsPath = openCLPath;

  openCLPath.append(QDir::separator());
  openCLPath.append("opencl");

  if(!dir.cd("resources"))
  {
    // We are not able to find the resources folder, so throw an error and bail
    errorMessageGenerated(tr("Unable to find resources folder at path '%1'").arg(randomSeedsPath));
    return;
  }

  emit stdOutputMessageGenerated(tr("Found resources folder path: %1").arg(dir.absolutePath()));
  dir.cdUp();

  randomSeedsPath.append(QDir::separator());
  randomSeedsPath.append("resources");
  randomSeedsPath.append(QDir::separator());
  randomSeedsPath.append("RandomSeeds.data");

  //  static const size_t k_BufferSize = 255;
  //  char string[k_BufferSize];

  //  convertToFortran(string, k_BufferSize, thePath.toLatin1().data());

  if(!setSParValue(StringType::OpenCLFolder, openCLPath))
  {
    return;
  }

  if(!setSParValue(StringType::RandomSeedsFile, randomSeedsPath))
  {
    return;
  }

  //  for (int i = 0; i < EMsoftWorkbenchConstants::Constants::SParSize; i++)
  //  {
  //    for (int j = 0; j < EMsoftWorkbenchConstants::Constants::SParStringSize; j++)
  //    {
  //      printf("%x ", m_SPar[i*EMsoftWorkbenchConstants::Constants::SParStringSize + j]);
  //    }
  //    printf("\n");
  //  }

  EMsoftCgetMCOpenCL(iParPtr->getPointer(0), fParPtr->getPointer(0), m_SPar, atomPos.data(), atomTypes.data(), latParm.data(), m_GenericAccumePtr->getPointer(0), m_GenericAccumzPtr->getPointer(0),
                     &MonteCarloSimulationControllerProgress, m_InstanceKey, &m_Cancel);

  s_ControllerInstances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(!m_Cancel)
  {
    bool success = writeEMsoftHDFFile(simData);
    if(!success)
    {
      emit stdOutputMessageGenerated("Monte Carlo File Generation Failed");
      return;
    }

    emit stdOutputMessageGenerated("Monte Carlo File Generation Complete");
  }
  else
  {
    m_XtalReader->closeFile();
    emit stdOutputMessageGenerated("Monte Carlo File Generation was successfully canceled");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloSimulationController::setSParValue(StringType type, const QString& value)
{
  int stringSize = EMsoftWorkbenchConstants::Constants::SParStringSize;
  if(value.size() > stringSize)
  {
    errorMessageGenerated(tr("The string '%1' is longer than %2 characters").arg(value).arg(EMsoftWorkbenchConstants::Constants::SParStringSize));
    return false;
  }

  int index = static_cast<int>(type);

  char* valueArray = value.toLatin1().data();
  std::memcpy(m_SPar + (index * stringSize), valueArray, value.size());
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::convertToFortran(char* fstring, size_t fstring_len, const char* cstring)
{
  std::size_t inlen = std::strlen(cstring);
  std::size_t cpylen = std::min(inlen, fstring_len);

  if(inlen > fstring_len)
  {
    // TODO: truncation error or warning
  }

  std::copy(cstring, cstring + cpylen, fstring);
  std::fill(fstring + cpylen, fstring + fstring_len, ' ');
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::initializeData(MonteCarloSimulationController::MonteCarloSimulationData data)
{
  // allocate space for the required DataArrays
  {
    // allocate space for the Accume and Accumz arrays, which will subsequently be filled by the EMsoftCgetMCOpenCL code.
    QVector<size_t> cDims(3);
    cDims[0] = static_cast<int>((data.acceleratingVoltage - data.minEnergyConsider) / data.energyBinSize + 1);
    cDims[1] = data.numOfPixelsN;
    cDims[2] = data.numOfPixelsN;

    m_GenericAccumePtr = Int32ArrayType::CreateArray(1, cDims, "genericAccume");
    m_GenericAccumePtr->initializeWithZeros();
    m_GenericAccumePtr->setInitValue(0);

    cDims.resize(4);
    if(data.mcMode == 1)
    {
      cDims[0] = static_cast<int>((data.acceleratingVoltage - data.minEnergyConsider) / data.energyBinSize + 1);
    }
    else
    {
      cDims[0] = static_cast<size_t>((data.sampleEndTiltAngle - data.sampleStartTiltAngle) / data.sampleTiltStepSize + 1);
    }
    cDims[1] = static_cast<int>(data.maxDepthConsider / data.depthStepSize + 1);
    cDims[2] = (data.numOfPixelsN - 1) / 10 + 1;
    cDims[3] = (data.numOfPixelsN - 1) / 10 + 1;

    m_GenericAccumzPtr = Int32ArrayType::CreateArray(1, cDims, "genericAccumz");
    m_GenericAccumzPtr->initializeWithZeros();
    m_GenericAccumzPtr->setInitValue(0);

    // allocate space for the IPar and FPar arrays, which will be used to communicate parameters with the EMsoftMCOpenCL routine.
    cDims.resize(1);

    cDims[0] = data.inputFilePath.length();
    m_GenericXtalPtr = UInt8ArrayType::CreateArray(1, cDims, "genericXtal");
    m_GenericXtalPtr->setInitValue(0);

    cDims[0] = data.outputFilePath.length();
    m_GenericMCPtr = UInt8ArrayType::CreateArray(1, cDims, "genericMC");
    m_GenericMCPtr->setInitValue(0);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool MonteCarloSimulationController::validateMonteCarloValues(MonteCarloSimulationController::MonteCarloSimulationData data)
{
  if(data.inputFilePath.isEmpty())
  {
    QString ss = QObject::tr("The crystal structure input file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }

  {
    QString inputFilePath = data.inputFilePath;

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

  if(data.outputFilePath.isEmpty())
  {
    QString ss = QObject::tr("The monte carlo output file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }

  QString outputFilePath = data.outputFilePath;
  QFileInfo fi(outputFilePath);
  if(fi.completeSuffix() != "h5")
  {
    QString ss = QObject::tr("The monte carlo output file at path '%1' needs a '.h5' suffix.").arg(outputFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  if((data.totalNumOfEConsidered > INT_MAX) || (data.totalNumOfEConsidered < 0))
  {
    data.totalNumOfEConsidered = 2000000000;
  }

  // test sample tilt angles for EBSD
  if((data.sampleTiltAngleSig < 0) || (data.sampleTiltAngleSig >= 90))
  {
    QString ss = QObject::tr("Sample TD tilt angle must be in interval [0,90[");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.sampleRotAngleOmega < -45) || (data.sampleRotAngleOmega > 45))
  {
    QString ss = QObject::tr("Sample RD tilt angle must be in interval [-45,45]");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test sample tilt angles for ECP
  if((data.sampleStartTiltAngle < 0) || (data.sampleStartTiltAngle > data.sampleEndTiltAngle) || (data.sampleStartTiltAngle > 90))
  {
    QString ss = QObject::tr("Sample start tilt angle must be in interval [0,endangle]");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.sampleEndTiltAngle < data.sampleStartTiltAngle) || (data.sampleEndTiltAngle > 90))
  {
    QString ss = QObject::tr("Sample end tilt angle must be in interval [startangle,90]");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.sampleTiltStepSize < 0) || (data.sampleTiltStepSize > data.sampleEndTiltAngle - data.sampleStartTiltAngle))
  {
    QString ss = QObject::tr("Sample tilt step size must be positive, with at least one step in the [start,end] interval.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test voltage and step size
  if(data.acceleratingVoltage < 0)
  {
    QString ss = QObject::tr("Microscope accelerating voltage must be positive");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.minEnergyConsider < 0) || data.minEnergyConsider > data.acceleratingVoltage)
  {
    QString ss = QObject::tr("Voltage must be positive and less than the accelerating voltage");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.energyBinSize < 0) || data.energyBinSize > data.acceleratingVoltage - data.minEnergyConsider)
  {
    QString ss = QObject::tr("Voltage step size must be positive, with at least one bin.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // test depth parameters
  if(data.maxDepthConsider <= 0)
  {
    QString ss = QObject::tr("Maximum depth must be strictly positive");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.depthStepSize <= 0) || (data.depthStepSize > data.maxDepthConsider))
  {
    QString ss = QObject::tr("Depth step size must be strictly positive, with at least one bin.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // numsx must be an odd number ...
  if((data.numOfPixelsN % 2) == 0)
  {
    QString ss = QObject::tr("Number of points must be odd.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // display a warning when the workitem or work group size parameters are changed from their defaults
  /*
          if((data.numOfEPerWorkitem != 10)||(data.globalWorkGroupSize != 150)) {
    setWarningCondition(-10031);
    QString ss = QObject::tr("There is no guarantee that GPU parameters different from the default values will actually work.");
    notifyWarningMessage(getHumanLabel(), ss, getWarningCondition());
  }
*/
  // make sure the multiplier is strictly positive
  if(data.multiplierForTotalNumOfE < 1)
  {
    QString ss = QObject::tr("Multiplier must be at least 1");
    emit errorMessageGenerated(ss);
    return false;
  }

  //  // platid and devid must be strictly positive integers; we also check on the upper bounds
  //  {
  //    int pID = getnumCLPlatforms();
  //    int platformID = data.gpuPlatformID;
  //    int dID = getnumCLDevices(platformID);

  //    if(platformID < 1) {
  //      QString ss = QObject::tr("Platform ID must be at least 1");
  //      emit errorMessageGenerated(ss);
  //      writePlatformInfo();
  //      return false;
  //    }

  //    if(platformID > pID) {
  //      QString ss = QObject::tr("Platform ID exceeds number of platforms (%1) available on this system.").arg(QString::number(pID));
  //      emit errorMessageGenerated(ss);
  //      writePlatformInfo();
  //      return false;
  //    }

  //    if (data.gpuDeviceID < 1) {
  //      QString ss = QObject::tr("Device ID must be at least 1");
  //      emit errorMessageGenerated(ss);
  //      writeDeviceInfo(platformID);
  //      return false;
  //    }

  //    if (dID != -1000) {
  //      if (data.gpuDeviceID > dID) {
  //        QString ss = QObject::tr("Device ID exceeds number of devices (%1) available on this platform on this system.").arg(QString::number(dID));
  //        emit errorMessageGenerated(ss);
  //        writeDeviceInfo(platformID);
  //        return false;
  //      }
  //    }
  //  }

  return true;
}

// -----------------------------------------------------------------------------
//  This routine creates an EMsoft h5 file with the Monte Carlo data in it, so
//  that EMEBSDmaster and similar programs can read it... This is not precisely the same
//  file as would be written by the EMMCOpenCL program, since the JSONfiles group
//  actually contains a complete (serialized) json file instead of a namelist file; this is not
//  a problem, since all the entries in the json/nml file are explicitly parsed out
//  in the NMLparameters group anyway...
//
//  Also, DREAM.3D tends to produce fixed length strings whereas EMsoft expects variable
//  length strings... we are adding a DREAM3D integer data set, set to 1, to identify this
//  file as being generated by DREAM.3D;  EMsoft will parse this data set and if it is
//  present, it knows to expect fixed length strings...  The identifier is added to the EMheader group.
// -----------------------------------------------------------------------------
bool MonteCarloSimulationController::writeEMsoftHDFFile(MonteCarloSimulationController::MonteCarloSimulationData simData)
{
  {
    QFileInfo inputFi(simData.inputFilePath);
    if(inputFi.suffix().compare("") == 0)
    {
      simData.inputFilePath.append(".xtal");
    }

    QFileInfo outputFi(simData.outputFilePath);
    if(outputFi.suffix().compare("") == 0)
    {
      simData.outputFilePath.append(".h5");
    }
  }

  QString creationDate;
  QString creationTime;
  QString creator;
  QString programName;
  std::vector<float> atomPos;
  std::vector<int32_t> atomTypes;
  int crystalSystem;
  std::vector<float> latParam;
  int natomTypes;
  int sgNumber;
  int sgSetting;

  if(!m_XtalReader->getCreationDate(creationDate) || !m_XtalReader->getCreationTime(creationTime) || !m_XtalReader->getCreator(creator) || !m_XtalReader->getProgramName(programName) ||
     !m_XtalReader->getAtomPos(atomPos) || !m_XtalReader->getAtomTypes(atomTypes) || !m_XtalReader->getCrystalSystem(crystalSystem) || !m_XtalReader->getLatticeParameters(latParam) ||
     !m_XtalReader->getNatomTypes(natomTypes) || !m_XtalReader->getSpaceGroupNumber(sgNumber) || !m_XtalReader->getSpaceGroupSetting(sgSetting))
  {
    return false;
  }

  Int32ArrayType::Pointer iParPtr = m_XtalReader->getIParPtr();
  FloatArrayType::Pointer fParPtr = m_XtalReader->getFParPtr();
  if(iParPtr == Int32ArrayType::NullPointer() || fParPtr == FloatArrayType::NullPointer())
  {
    return false;
  }

  // open the output HDF5 file
  QString inputFilePath = simData.inputFilePath;
  QString outputFilePath = simData.outputFilePath;

  QString tmpOutputFilePath = outputFilePath + ".tmp";
  inputFilePath = QDir::toNativeSeparators(inputFilePath);
  tmpOutputFilePath = QDir::toNativeSeparators(tmpOutputFilePath);
  tmpOutputFilePath = QDir::toNativeSeparators(tmpOutputFilePath);
  QFileInfo outputFi(outputFilePath);
  QFileInfo tmpFi(tmpOutputFilePath);

  if(tmpFi.exists())
  {
    if(!QFile::remove(tmpOutputFilePath))
    {
      QString ss = QObject::tr("Error creating temporary output file at path '%1'").arg(tmpOutputFilePath);
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(!QFile::copy(inputFilePath, tmpOutputFilePath))
  {
    QString ss = QObject::tr("Error creating temporary output file at path '%1'").arg(tmpOutputFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  emit stdOutputMessageGenerated(tr("Writing Monte Carlo File '%1'...").arg(outputFi.fileName()));

  QSharedPointer<EMsoftFileWriter> writer = QSharedPointer<EMsoftFileWriter>(new EMsoftFileWriter());
  connect(writer.data(), &EMsoftFileWriter::errorMessageGenerated, [=](const QString& msg) { emit errorMessageGenerated(msg); });

  // Open the HDF5 file
  if(!writer->openFile(tmpOutputFilePath))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // Create the EMData group
  if(!writer->openGroup(EMsoft::Constants::EMData))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Create the MCOpenCL group
  if(!writer->openGroup(EMsoft::Constants::MCOpenCL))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // write the accum_e array
  {
    QVector<hsize_t> dims(3);
    if(simData.mcMode == 1)
    {
      dims[2] = static_cast<int>((simData.acceleratingVoltage - simData.minEnergyConsider) / simData.energyBinSize + 1);
    }
    else
    {
      dims[2] = static_cast<size_t>((simData.sampleEndTiltAngle - simData.sampleStartTiltAngle) / simData.sampleTiltAngleSig + 1);
    }
    dims[1] = simData.numOfPixelsN;
    dims[0] = simData.numOfPixelsN;

    if(!writer->writePointerDataset(EMsoft::Constants::accume, m_GenericAccumePtr->getPointer(0), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // write the accum_z array
  {
    QVector<hsize_t> dims(4);
    if(simData.mcMode == 1)
    {
      dims[3] = static_cast<int>((simData.acceleratingVoltage - simData.minEnergyConsider) / simData.energyBinSize + 1);
    }
    else
    {
      dims[3] = static_cast<size_t>((simData.sampleEndTiltAngle - simData.sampleStartTiltAngle) / simData.sampleTiltAngleSig + 1);
    }
    dims[2] = static_cast<int>(simData.maxDepthConsider / simData.depthStepSize + 1);
    dims[1] = (simData.numOfPixelsN - 1) / 10 + 1;
    dims[0] = (simData.numOfPixelsN - 1) / 10 + 1;

    if(!writer->writePointerDataset(EMsoft::Constants::accumz, m_GenericAccumzPtr->getPointer(0), dims))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  int32_t* genericIPar = iParPtr->getPointer(0);

  // and a few constants
  if(simData.mcMode == 1)
  {
    if(!writer->writeScalarDataset(EMsoft::Constants::numEbins, genericIPar[11]))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    if(!writer->writeScalarDataset(EMsoft::Constants::numangle, genericIPar[11]))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::numzbins, genericIPar[12]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::totnumel, genericIPar[3]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::multiplier, genericIPar[4]))
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

  //--------------------------------
  // Create the EMheader group; this is common to all EMsoft output files, so in the future
  // we will move this to the EMsoftToolboxPlugin.cpp file
  if(!writer->openGroup(EMsoft::Constants::EMheader))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  // Create the MCOpenCL group
  if(!writer->openGroup(EMsoft::Constants::MCOpenCL))
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

  // programName
  programName = "EMsoftWorkbench Monte Carlo Simulation Module";
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
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(!writer->closeGroup())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // here we create a JSONfiles group that contains the jsonobject in its entirety
  if(!writer->openGroup(EMsoft::Constants::JSONfiles))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  {
    QJsonObject topObject;
    QJsonObject rootObject;
    if(simData.mcMode == 1)
    {
      rootObject.insert(EMsoft::Constants::mode, EMsoft::Constants::full);
      rootObject.insert(EMsoft::Constants::sig, simData.sampleTiltAngleSig);
      rootObject.insert(EMsoft::Constants::sig, simData.sampleRotAngleOmega);
    }
    else
    {
      rootObject.insert(EMsoft::Constants::mode, EMsoft::Constants::bse1);
      rootObject.insert(EMsoft::Constants::sigstart, simData.sampleStartTiltAngle);
      rootObject.insert(EMsoft::Constants::sigend, simData.sampleEndTiltAngle);
      rootObject.insert(EMsoft::Constants::sigstep, simData.sampleTiltAngleSig);
    }

    QFileInfo simDataInputFileInfo(simData.inputFilePath);
    rootObject.insert(EMsoft::Constants::xtalname, simDataInputFileInfo.fileName());
    rootObject.insert(EMsoft::Constants::numsx, simData.numOfPixelsN);
    rootObject.insert(EMsoft::Constants::num_el, simData.numOfEPerWorkitem);
    rootObject.insert(EMsoft::Constants::platid, simData.gpuPlatformID);
    rootObject.insert(EMsoft::Constants::devid, simData.gpuDeviceID);
    rootObject.insert(EMsoft::Constants::globalworkgrpsz, simData.globalWorkGroupSize);
    rootObject.insert(EMsoft::Constants::totnumel, simData.totalNumOfEConsidered);
    rootObject.insert(EMsoft::Constants::multiplier, simData.multiplierForTotalNumOfE);
    rootObject.insert(EMsoft::Constants::EkeV, simData.acceleratingVoltage);
    rootObject.insert(EMsoft::Constants::Ehistmin, simData.minEnergyConsider);
    rootObject.insert(EMsoft::Constants::Ebinsize, simData.energyBinSize);
    rootObject.insert(EMsoft::Constants::depthmax, simData.maxDepthConsider);
    rootObject.insert(EMsoft::Constants::depthstep, simData.depthStepSize);
    rootObject.insert(EMsoft::Constants::dataname, simData.outputFilePath);
    topObject.insert(EMsoft::Constants::MCCLdata, rootObject);
    QJsonDocument doc(topObject);
    QString strJson(doc.toJson(QJsonDocument::Compact));

    if(!writer->writeStringDataset(EMsoft::Constants::MCOpenCLJSON, strJson))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  // Close the group
  if(!writer->closeGroup())
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // Create the NMLparameters group
  if(!writer->openGroup(EMsoft::Constants::NMLparameters))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  //--------------------------------
  // Create the MCCLNameList group
  if(!writer->openGroup(EMsoft::Constants::MCCLNameList))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  float* genericFPar = fParPtr->getPointer(0);

  if(!writer->writeScalarDataset(EMsoft::Constants::Ebinsize, static_cast<double>(genericFPar[4])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::Ehistmin, static_cast<double>(genericFPar[3])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::EkeV, static_cast<double>(genericFPar[2])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeStringDataset(EMsoft::Constants::MCmode, "CSDA"))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeStringDataset(EMsoft::Constants::dataname, simData.outputFilePath))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::depthmax, static_cast<double>(genericFPar[5])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::depthstep, static_cast<double>(genericFPar[6])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::devid, genericIPar[5]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::globalworkgrpsz, genericIPar[1]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(genericIPar[13] == 1)
  {
    if(!writer->writeStringDataset(EMsoft::Constants::mode, EMsoft::Constants::full))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    if(!writer->writeStringDataset(EMsoft::Constants::mode, EMsoft::Constants::bse1))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::multiplier, genericIPar[4]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::num_el, genericIPar[2]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  int iv = 2 * genericIPar[0] + 1;
  if(!writer->writeScalarDataset(EMsoft::Constants::numsx, iv))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::omega, static_cast<double>(genericFPar[1])))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::platid, genericIPar[6]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }

  if(simData.mcMode == 1)
  {
    if(!writer->writeScalarDataset(EMsoft::Constants::sig, static_cast<double>(genericFPar[0])))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }
  else
  {
    if(!writer->writeScalarDataset(EMsoft::Constants::sigstart, simData.sampleStartTiltAngle))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
    if(!writer->writeScalarDataset(EMsoft::Constants::sigend, simData.sampleEndTiltAngle))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
    if(!writer->writeScalarDataset(EMsoft::Constants::sigstep, simData.sampleTiltStepSize))
    {
      QFile::remove(tmpOutputFilePath);
      return false;
    }
  }

  if(!writer->writeScalarDataset(EMsoft::Constants::Stdout, m_InstanceKey))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::totnumel, genericIPar[3]))
  {
    QFile::remove(tmpOutputFilePath);
    return false;
  }
  QFileInfo simDataInputFileInfo(simData.inputFilePath);
  if(!writer->writeStringDataset(EMsoft::Constants::xtalname, simDataInputFileInfo.fileName()))
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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int MonteCarloSimulationController::getnumCLPlatforms()
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);

  return int(platforms.size());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList MonteCarloSimulationController::getPlatformInfo()
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
void MonteCarloSimulationController::writePlatformInfo()
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);

  for(size_t i = 0; i < platforms.size(); i++)
  {
    cl::Platform curPlat = platforms[i];
    QString ss = QObject::tr("Platform %1 info: ").arg(QString::number(i + 1));
    QString tt = QString::fromStdString(curPlat.getInfo<CL_PLATFORM_NAME>());
    ss.append(tt);
    // std::cout << ss.toStdString();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int MonteCarloSimulationController::getnumCLDevices(int platformID)
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  if(platformID > platforms.size())
  {
    return -1000;
  }
  cl::Platform selectedPlatform = platforms[platformID - 1];

  std::vector<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  return int(devices.size());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::writeDeviceInfo(int platformID)
{
  std::vector<cl::Platform> platforms;
  cl::Platform::get(&platforms);
  cl::Platform selectedPlatform = platforms[platformID - 1];

  std::vector<cl::Device> devices;
  selectedPlatform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  for(int i = 0; i < devices.size(); i++)
  {
    cl::Device curDev = devices[i];
    QString ss = QObject::tr("Device %1 info: ").arg(QString::number(i + 1));
    QString tt = QString::fromStdString(curDev.getInfo<CL_DEVICE_NAME>());
    ss.append(tt);
    // std::cout << ss.toStdString();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList MonteCarloSimulationController::getDeviceInfo(int platformID)
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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString MonteCarloSimulationController::getEMsoftUserName()
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
QString MonteCarloSimulationController::getEMsoftUserEmail()
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
QString MonteCarloSimulationController::getEMsoftUserLocation()
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
Int32ArrayType::Pointer MonteCarloSimulationController::getIParPtr(MonteCarloSimulationController::MonteCarloSimulationData simData)
{
  Int32ArrayType::Pointer iParPtr = m_XtalReader->getIParPtr();
  int32_t* iPar = iParPtr->getPointer(0);

  iPar[0] = static_cast<int>((simData.numOfPixelsN - 1) / 2);                        // number of pixels along x
  iPar[1] = static_cast<int>(simData.globalWorkGroupSize);                           // global work group size
  iPar[2] = static_cast<int>(simData.numOfEPerWorkitem);                             // number of electrons in work group
  iPar[3] = static_cast<int>(simData.totalNumOfEConsidered);                         // total number of electrons in single MCstep
  iPar[4] = static_cast<int>(simData.multiplierForTotalNumOfE);                      // multiplier for # of electrons
  iPar[5] = static_cast<int>(simData.gpuDeviceID + 1);                               // OpenCL device ID
  iPar[6] = static_cast<int>(simData.gpuPlatformID + 1);                             // OpenCL platform ID
  iPar[12] = static_cast<int>(simData.maxDepthConsider / simData.depthStepSize + 1); // num z bins
  iPar[13] = simData.mcMode;                                                         // simulation mode (1=full, 2=bse1)

  // this next pair of values is a bit tricky since we use the accum_e and accum_z arrays for
  // two different cases, 'full' and 'bse1'
  if(iPar[13] == 1)
  {
    iPar[11] = static_cast<int>((simData.acceleratingVoltage - simData.minEnergyConsider) / simData.energyBinSize + 1); // num E bins
    iPar[14] = 1;                                                                                                       // only one major loop to be executed
  }
  else
  {
    iPar[11] = static_cast<int>((simData.sampleEndTiltAngle - simData.sampleStartTiltAngle) / simData.sampleTiltStepSize + 1); // number of bse1 angles
    iPar[14] = iPar[11];
  }
  iPar[15] = static_cast<int>((simData.numOfPixelsN - 1) / 20); // number of depth bins along x

  return iParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer MonteCarloSimulationController::getFParPtr(MonteCarloSimulationController::MonteCarloSimulationData simData)
{
  FloatArrayType::Pointer fParPtr = m_XtalReader->getFParPtr();
  float* fPar = fParPtr->getPointer(0);

  fPar[0] = simData.sampleTiltAngleSig;   // sample tilt angle
  fPar[1] = simData.sampleRotAngleOmega;  // omega sample tilt angle
  fPar[2] = simData.acceleratingVoltage;  // accelerating voltage
  fPar[3] = simData.minEnergyConsider;    // Energy minimum in histogram
  fPar[4] = simData.energyBinSize;        // Energy histogram bin size
  fPar[5] = simData.maxDepthConsider;     // maximum depth to store
  fPar[6] = simData.depthStepSize;        // depth step size
  fPar[7] = simData.sampleStartTiltAngle; // get starting angle
  fPar[8] = simData.sampleEndTiltAngle;   // end angle
  fPar[9] = simData.sampleTiltStepSize;   // angle step size

  return fParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MonteCarloSimulationController::setUpdateProgress(int loopCompleted, int totalLoops, float bseYield)
{
  QString ss = QObject::tr("MonteCarlo steps completed: %1/%2; BSE Yield %3%").arg(loopCompleted).arg(totalLoops).arg(bseYield);
  emit stdOutputMessageGenerated(ss);
  emit updateMCProgress(loopCompleted, totalLoops, bseYield);
}
