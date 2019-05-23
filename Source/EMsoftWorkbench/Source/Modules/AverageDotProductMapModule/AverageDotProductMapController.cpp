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

#include "AverageDotProductMapController.h"

#include "EMsoftWrapperLib/DictionaryIndexing/EMsoftDIwrappers.h"

#include <QtCore/QMimeDatabase>
#include <QtCore/QDateTime>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QThread>

#include "Common/Constants.h"

#include "Constants.h"

#include "EMsoftLib/EMsoftStringConstants.h"

#define CL_VECTOR std::vector

static size_t k_InstanceKey = 0;
static QMap<size_t, AverageDotProductMapController*> instances;

namespace SizeConstants = AverageDotProductMapModuleConstants::ArraySizes;

/**
 * @brief AverageDotProductMapControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void AverageDotProductMapControllerProgress(size_t instance, int loopCompleted, int totalLoops)
{
  AverageDotProductMapController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AverageDotProductMapController::AverageDotProductMapController(QObject* parent)
: QObject(parent)
, m_Cancel(false)
{
  m_InstanceKey = ++k_InstanceKey;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
AverageDotProductMapController::~AverageDotProductMapController()
{
  k_InstanceKey--;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMapController::createADPMap(const ADPMapData &data)
{
  initializeData();

  SizeTArrayType::Pointer iParPtr = data.getIParPtr();
  FloatArrayType::Pointer fParPtr = data.getFParPtr();
  std::vector<char> sParVector = data.getSParVector();

  size_t* iPar = iParPtr->getPointer(0);
  float* fPar = fParPtr->getPointer(0);
  char* sPar = sParVector.data();

  QVector<size_t> numTuples(1, 1);
  QVector<size_t> cDims(1, data.patternHeight * data.patternWidth);

  // Create a new Mask Array
  m_OutputMaskPtr = FloatArrayType::CreateArray(numTuples, cDims, "Mask", true);

  // Create a new IQ Map Array
  m_OutputIQMapPtr = FloatArrayType::CreateArray(numTuples, cDims, "IQ Map", true);

  // Create a new ADP Map Array
  m_OutputADPMapPtr = FloatArrayType::CreateArray(numTuples, cDims, "ADP Map", true);

  float* outputMask = m_OutputMaskPtr->getPointer(0);
  float* outputIQMap = m_OutputIQMapPtr->getPointer(0);
  float* outputADPMap = m_OutputADPMapPtr->getPointer(0);

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
  EMsoftCpreprocessEBSDPatterns(iPar, fPar, sPar, outputMask, outputIQMap, outputADPMap, &AverageDotProductMapControllerProgress, m_InstanceKey, &m_Cancel);
  m_Executing = false;
  instances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(!m_Cancel)
  {
    emit stdOutputMessageGenerated("Average Dot Product Map Generation Complete");
  }
  else
  {
    emit stdOutputMessageGenerated("Average Dot Product Map Generation was successfully canceled");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMapController::initializeData()
{
  m_OutputMaskPtr.reset();
  m_OutputIQMapPtr.reset();
  m_OutputADPMapPtr.reset();
  m_StartTime = "";
  m_Executing = false;
  m_Cancel = false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AverageDotProductMapController::validateADPMapValues(ADPMapData data)
{
  QString inputPath = data.patternDataFile;
  if (inputPath.isEmpty())
  {
    QString ss = QObject::tr("The input pattern data file path is empty.");
    emit errorMessageGenerated(ss);
    return false;
  }

  QFileInfo inFi(inputPath);
  if(!inFi.exists())
  {
    QString ss = QObject::tr("The input pattern data file with path '%1' does not exist.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }

  QMimeDatabase db;
  QMimeType mime = db.mimeTypeForFile(inputPath);
  if (!mime.inherits("application/x-hdf5") && !mime.inherits("application/x-hdf"))
  {
    QString ss = QObject::tr("The input pattern data file at path '%1' is not an HDF5 file.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }

  QString outputPath = data.outputFilePath;
  if (outputPath.isEmpty())
  {
    QString ss = QObject::tr("The output image file at path '%1' is empty.").arg(inputPath);
    emit errorMessageGenerated(ss);
    return false;
  }

  QFileInfo dir(outputPath);
  QDir dPath = dir.path();
  if(dir.suffix().isEmpty())
  {
    outputPath.append(".tif");
  }

  mime = db.mimeTypeForFile(outputPath);
  if (!mime.inherits("image/tiff"))
  {
    QString ss = QObject::tr("The output image file at path '%1' is not a tiff file.").arg(data.outputFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  if(!dPath.exists())
  {
    QString ss = QObject::tr("The directory path for the output image file does not exist. DREAM.3D will attempt to create this path during execution of the filter");
    emit warningMessageGenerated(ss);
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SizeTArrayType::Pointer AverageDotProductMapController::ADPMapData::getIParPtr() const
{
  // allocate space for the iPar array, which will be used to communicate parameters with the EMsoftCpreprocessEBSDPatterns routine.
  size_t iParSize = static_cast<size_t>(SizeConstants::IParSize);
  QVector<size_t> cDims = { iParSize };

  SizeTArrayType::Pointer iParPtr = SizeTArrayType::CreateArray(QVector<size_t>(1, 1), cDims, "iPar", true);
  iParPtr->initializeWithZeros();

  size_t* iPar = iParPtr->getPointer(0);

  iPar[18] = numOfThreads;
  iPar[19] = patternWidth;
  iPar[20] = patternHeight;
  iPar[22] = binningFactor;
  iPar[23] = binningX;
  iPar[24] = binningY;
  iPar[26] = ipfWidth;
  iPar[27] = ipfHeight;
  iPar[28] = numOfRegions;
  iPar[29] = maskPattern;

  if (useROI)
  {
    iPar[30] = 1;
  }
  else
  {
    iPar[30] = 0;
  }

  iPar[31] = roi_1;
  iPar[32] = roi_2;
  iPar[33] = roi_3;
  iPar[34] = roi_4;
  iPar[35] = static_cast<int>(inputType);

  return iParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer AverageDotProductMapController::ADPMapData::getFParPtr() const
{
  QVector<size_t> cDims = { SizeConstants::FParSize };

  // allocate space for the fPar array, which will be used to communicate parameters with the EMsoftCpreprocessEBSDPatterns routine.
  FloatArrayType::Pointer fParPtr = FloatArrayType::CreateArray(QVector<size_t>(1, 1), cDims, "fPar", true);
  fParPtr->initializeWithZeros();

  float* fPar = fParPtr->getPointer(0);

  fPar[23] = maskRadius;
  fPar[24] = hipassFilter;

  return fParPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<char> AverageDotProductMapController::ADPMapData::getSParVector() const
{
  // Move each string from the string array into the char array.  Each string has SParStringSize as its max size.
  std::vector<char> sParVector(SizeConstants::SParSize * SizeConstants::SParStringSize, 0);
  char* sPar = sParVector.data();

  // Move Output File Path into the vector
  const char* charArray = outputFilePath.toStdString().c_str();
  std::memcpy(sPar + (31 * SizeConstants::SParStringSize), charArray, outputFilePath.size());

  // Move Pattern Data File into the vector
  charArray = patternDataFile.toStdString().c_str();
  std::memcpy(sPar + (32 * SizeConstants::SParStringSize), charArray, patternDataFile.size());

  // Move HDF Strings into the vector
  int count = 41;
  for (const QString &hdfString : hdfStrings)
  {
    charArray = hdfString.toStdString().c_str();
    std::memcpy(sPar + (count * SizeConstants::SParStringSize), charArray, hdfString.size());
    count++;
  }

  return sParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMapController::setUpdateProgress(int loopCompleted, int totalLoops)
{
  QString ss = QObject::tr("Average Dot Product: %1 of %2").arg(loopCompleted, totalLoops);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int AverageDotProductMapController::getNumCPUCores()
{
  return QThread::idealThreadCount();
}
