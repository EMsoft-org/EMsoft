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
#include <QtCore/QMap>

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

  std::vector<int32_t> iParVector = data.getIParVector();
  std::vector<float> fParVector = data.getFParVector();
  std::vector<char> sParVector = data.getSParVector();

  // Create a new Mask Array
  m_OutputMaskVector.resize(data.patternHeight * data.patternWidth);
  std::fill(m_OutputMaskVector.begin(), m_OutputMaskVector.end(), 0.0f);

  // Create a new IQ Map Array
  m_OutputIQMapVector.resize(data.patternHeight * data.patternWidth);
  std::fill(m_OutputIQMapVector.begin(), m_OutputIQMapVector.end(), 0.0f);

  // Create a new ADP Map Array
  m_OutputADPMapVector.resize(data.patternHeight * data.patternWidth);
  std::fill(m_OutputADPMapVector.begin(), m_OutputADPMapVector.end(), 0.0f);

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
  EMsoftCpreprocessEBSDPatterns(iParVector.data(), fParVector.data(), sParVector.data(), m_OutputMaskVector.data(), m_OutputIQMapVector.data(), m_OutputADPMapVector.data(), &AverageDotProductMapControllerProgress, m_InstanceKey, &m_Cancel);
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
  m_OutputMaskVector.clear();
  m_OutputIQMapVector.clear();
  m_OutputADPMapVector.clear();
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
std::vector<int32_t> AverageDotProductMapController::ADPMapData::getIParVector() const
{
  std::vector<int32_t> iParVector(SizeConstants::IParSize, 0);

  iParVector[17] = numOfThreads;
  iParVector[18] = patternWidth;
  iParVector[19] = patternHeight;
  iParVector[21] = binningFactor;
  iParVector[22] = patternWidth;
  iParVector[23] = patternHeight;
  iParVector[25] = ipfWidth;
  iParVector[26] = ipfHeight;
  iParVector[27] = numOfRegions;
  iParVector[28] = maskPattern;

  if (useROI)
  {
    iParVector[29] = 1;
  }
  else
  {
    iParVector[29] = 0;
  }

  iParVector[30] = roi_1;
  iParVector[31] = roi_2;
  iParVector[32] = roi_3;
  iParVector[33] = roi_4;
  iParVector[34] = static_cast<int>(inputType);

  return iParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> AverageDotProductMapController::ADPMapData::getFParVector() const
{
  std::vector<float> fParVector(SizeConstants::FParSize, 0.0f);

  fParVector[22] = maskRadius;
  fParVector[23] = hipassFilter;

  return fParVector;
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
  std::memcpy(sPar + (30 * SizeConstants::SParStringSize), charArray, outputFilePath.size());

  // Move Pattern Data File into the vector
  charArray = patternDataFile.toStdString().c_str();
  std::memcpy(sPar + (31 * SizeConstants::SParStringSize), charArray, patternDataFile.size());

  // Move HDF Strings into the vector
  int count = 40;
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

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool AverageDotProductMapController::getCancel() const
{
  return m_Cancel;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void AverageDotProductMapController::setCancel(const bool& value)
{
  m_Cancel = value;
}
