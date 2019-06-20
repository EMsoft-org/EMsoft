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

#include "PatternPreprocessingController.h"

//#include "EMsoftWrapperLib/DictionaryIndexing/EMsoftDIwrappers.h"

#include <QtCore/QMimeDatabase>
#include <QtCore/QDateTime>
#include <QtCore/QThread>
#include <QtCore/QMap>
#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QSharedPointer>

#include <QtGui/QImage>

#include "Constants.h"

static size_t k_InstanceKey = 0;
static QMap<size_t, PatternPreprocessingController*> instances;

namespace SizeConstants = DictionaryIndexingModuleConstants::ArraySizes;

/**
 * @brief PatternPreprocessingControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void PatternPreprocessingControllerProgress(size_t instance, int loopCompleted, int totalLoops)
{
  PatternPreprocessingController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternPreprocessingController::PatternPreprocessingController(QObject* parent)
: QObject(parent)
{
  m_InstanceKey = ++k_InstanceKey;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternPreprocessingController::~PatternPreprocessingController()
{
  k_InstanceKey--;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessingController::createPreprocessedPatternsMatrix(const PPMatrixData &data)
{
  // WIP: This code currently calls out to a QProcess, but eventually it will use a C++ callback method instead

//  initializeData();

//  std::vector<int32_t> iParVector = data.getIParVector();
//  std::vector<float> fParVector = data.getFParVector();
//  std::vector<char> sParVector = data.getSParVector();

//  // Create a new Mask Array
//  m_OutputMaskVector.resize(data.patternHeight * data.patternWidth);
//  std::fill(m_OutputMaskVector.begin(), m_OutputMaskVector.end(), 0.0f);

//  // Create a new IQ Map Array
//  m_OutputIQMapVector.resize(data.patternHeight * data.patternWidth);
//  std::fill(m_OutputIQMapVector.begin(), m_OutputIQMapVector.end(), 0.0f);

//  // Create a new ADP Map Array
//  m_OutputADPMapVector.resize(data.patternHeight * data.patternWidth);
//  std::fill(m_OutputADPMapVector.begin(), m_OutputADPMapVector.end(), 0.0f);

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
//  EMsoftCpreprocessEBSDPatterns(iParVector.data(), fParVector.data(), sParVector.data(), m_OutputMaskVector.data(), m_OutputIQMapVector.data(), m_OutputADPMapVector.data(), &PatternPreprocessingControllerProgress, m_InstanceKey, &m_Cancel);

  QSharedPointer<QProcess> ppMatrixProcess = QSharedPointer<QProcess>(new QProcess());
  connect(ppMatrixProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit stdOutputMessageGenerated(QString::fromStdString(ppMatrixProcess->readAllStandardOutput().toStdString())); });
  connect(ppMatrixProcess.data(), &QProcess::readyReadStandardError, [=] { emit stdOutputMessageGenerated(QString::fromStdString(ppMatrixProcess->readAllStandardOutput().toStdString())); });
  connect(ppMatrixProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { listenPreprocessedPatternsMatrixFinished(exitCode, exitStatus); });
  QString ppMatrixExecutablePath = getPreprocessedPatternsMatrixExecutablePath();
  if (!ppMatrixExecutablePath.isEmpty())
  {
    QString nmlFilePath = m_TempDir.path() + QDir::separator() + "EMEBSDDIpreview.nml";
    writePreprocessedPatternsMatrixToFile(nmlFilePath, data);
    QStringList parameters = {nmlFilePath};
    ppMatrixProcess->start(ppMatrixExecutablePath, parameters);

    // Wait until the QProcess is finished to exit this thread.
    // PatternPreprocessingController::createADPMap is currently on a separate thread, so the GUI will continue to operate normally
    ppMatrixProcess->waitForFinished(-1);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessingController::writePreprocessedPatternsMatrixToFile(const QString &filePath, const PatternPreprocessingController::PPMatrixData &data) const
{
  QFile outputFile(filePath);
  if (outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    out << "&EBSDDIpreviewdata\n";
    out << "! The line above must not be changed\n";
    out << "!\n";
    out << "! The values below are the default values for this program\n";
    out << "!\n";
    out << "! number of pattern pixels along x and y\n";
    out << tr("numsx = %1,\n").arg(data.patternWidth);
    out << tr("numsy = %1,\n").arg(data.patternHeight);
    out << "! number of patterns along x and y\n";
    out << tr("ipf_wd = %1,\n").arg(data.ipfWidth);
    out << tr("ipf_ht = %1,\n").arg(data.ipfHeight);
    out << "! hipass w parameter range and number of steps (starts near zero)\n";
    out << tr("hipasswmax = %1,\n").arg(data.hipassValue);
    out << tr("hipasswnsteps = %1,\n").arg(data.hipassNumSteps);
    out << "! number of regions for adaptive histogram equalization\n";
    out << tr("nregionsmin = %1,\n").arg(data.minNumOfRegions);
    out << tr("nregionsmax = %1,\n").arg(data.maxNumOfRegions);
    out << tr("nregionsstepsize = %1,\n").arg(data.numOfRegionsStepSize);
    out << "! name of tiff output file; will contain an array of pre-processed patterns\n";
    out << tr("tifffile = '%1/MatrixResult.tiff',\n").arg(m_TempDir.path());
    out << "! name of pattern output file; will contain raw experimental pattern (tiff, pgm, bmp)\n";
    out << tr("patternfile = '%1/ReferenceResult.tiff',\n").arg(m_TempDir.path());
    out << "! name of datafile where the patterns are stored; path relative to EMdatapathname\n";
    out << tr("exptfile = '%1',\n").arg(data.patternDataFile);
    out << "! input file type parameter: Binary, EMEBSD, TSLHDF, TSLup2, OxfordHDF, OxfordBinary, BrukerHDF\n";

    switch(data.inputType)
    {
    case InputType::Binary:
      out << "inputtype = 'Binary',\n";
      break;
    case InputType::TSLup1:
      out << "inputtype = 'TSLup1',\n";
      break;
    case InputType::TSLup2:
      out << "inputtype = 'TSLup2',\n";
      break;
    case InputType::OxfordBinary:
      out << "inputtype = 'OxfordBinary',\n";
      break;
    case InputType::EMEBSD:
      out << "inputtype = 'EMEBSD',\n";
      break;
    case InputType::TSLHDF:
      out << "inputtype = 'TSLHDF',\n";
      break;
    case InputType::OxfordHDF:
      out << "inputtype = 'OxfordHDF',\n";
      break;
    case InputType::BrukerHDF:
      out << "inputtype = 'BrukerHDF',\n";
      break;
    }

    out << "! here we enter the HDF group names and data set names as individual strings (up to 10)\n";
    out << "! enter the full path of a data set in individual strings for each group, in the correct order,\n";
    out << "! and with the data set name as the last name; leave the remaining strings empty (they should all\n";
    out << "! be empty for the Binary and TSLup2 formats)\n";

    QString hdfStringsStr = " HDFstrings = @@HDF_STRINGS@@,\n";
    QString hdfStringsJoined = data.hdfStrings.join("' '");
    hdfStringsJoined.prepend("'");
    hdfStringsJoined.append("'");
    hdfStringsStr.replace("@@HDF_STRINGS@@", hdfStringsJoined);

    out << hdfStringsStr;
    out << "! pattern coordinates to be used for the preview\n";
    out << tr("patx = %1,\n").arg(data.patternCoordinateX);
    out << tr("paty = %1,\n").arg(data.patternCoordinateY);
    out << "/\n";

    outputFile.close();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessingController::listenPreprocessedPatternsMatrixFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  // EMgetADP program automatically adds "_ADP" onto the end of the output image name
  QString imagePath = tr("%1/MatrixResult.tiff").arg(m_TempDir.path());
  QImage imageResult(imagePath);
  if (!imageResult.isNull())
  {
    emit preprocessedPatternsMatrixCreated(imageResult);
  }

  m_Executing = false;
  instances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(!m_Cancel)
  {
    emit stdOutputMessageGenerated("Preprocessed Patterns Matrix Generation Complete");
  }
  else
  {
    emit stdOutputMessageGenerated("Preprocessed Patterns Matrix Generation was successfully canceled");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString PatternPreprocessingController::getPreprocessedPatternsMatrixExecutablePath() const
{
  QString executableName = "EMEBSDDIpreview";
  QString executablePath;

  QDir workingDirectory = QDir(QCoreApplication::applicationDirPath());

#if defined(Q_OS_WIN)
  if (workingDirectory.exists(tr("%1.exe").arg(executableName)))
  {
    executablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), tr("%1.exe").arg(executableName));
    return executablePath;
  }
#elif defined(Q_OS_MAC)
  // Look to see if we are inside an .app package or inside the 'tools' directory
  if(workingDirectory.dirName() == "MacOS")
  {
    workingDirectory.cdUp();
    if (workingDirectory.cd("bin"))
    {
      if (workingDirectory.exists(executableName))
      {
        executablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), executableName);
        return executablePath;
      }
      workingDirectory.cdUp();
    }

    workingDirectory.cdUp();
    workingDirectory.cdUp();

    if(workingDirectory.dirName() == "Bin" && workingDirectory.exists(executableName))
    {
      executablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), executableName);
      return executablePath;
    }
  }
#else
  // We are on Linux - I think
  // Try the current location of where the application was launched from which is
  // typically the case when debugging from a build tree
  QDir workingDirectory = workbenchDir;
  if(workingDirectory.cd("bin"))
  {
    if (workingDirectory.exists(executableName))
    {
      executablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), executableName);
      return executablePath;
    }

    workingDirectory.cdUp();
  }

  // Now try moving up a directory which is what should happen when running from a
  // proper distribution of SIMPLView
  workingDirectory = workbenchDir;
  workingDirectory.cdUp();
  if(workingDirectory.cd("bin"))
  {
    if (workingDirectory.exists(executableName))
    {
      executablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), executableName);
      return executablePath;
    }

    workingDirectory.cdUp();
  }
#endif

  if (executablePath.isEmpty())
  {
    QString errMsg = tr("Could not find '%1' executable!").arg(executableName);
    emit errorMessageGenerated(errMsg);
  }

  return executablePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessingController::initializeData()
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
bool PatternPreprocessingController::validatePPValues(const PPMatrixData &data)
{
  if (data.patternCoordinateX < 0 || data.patternCoordinateY < 0)
  {
    QString errMsg = "The 'Chosen ADP Coordinate' field is invalid.  Please double-click inside "
                     "the average dot product map generated in the 'Average Dot Product Map' tab to "
                     "choose a coordinate.";
    emit errorMessageGenerated(errMsg);
    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<int32_t> PatternPreprocessingController::PPMatrixData::getIParVector() const
{
  std::vector<int32_t> iParVector(SizeConstants::IParSize, 0);

  iParVector[18] = patternWidth;
  iParVector[19] = patternHeight;
  iParVector[25] = ipfWidth;
  iParVector[26] = ipfHeight;
  iParVector[34] = static_cast<int>(inputType);
  iParVector[44] = minNumOfRegions;
  iParVector[45] = numOfRegionsStepSize;
  iParVector[47] = patternCoordinateX;
  iParVector[48] = patternCoordinateY;

  // Used in wrapper routine, but not NML file...
  iParVector[46] = numav;
  iParVector[49] = numw;
  iParVector[50] = numr;

  return iParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternPreprocessingController::PPMatrixData::getFParVector() const
{
  std::vector<float> fParVector(SizeConstants::FParSize, 0.0f);

  fParVector[23] = hipassFilter;
  fParVector[25] = hipassValue;

  return fParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<char> PatternPreprocessingController::PPMatrixData::getSParVector() const
{
  // Move each string from the string array into the char array.  Each string has SParStringSize as its max size.
  std::vector<char> sParVector(SizeConstants::SParSize * SizeConstants::SParStringSize, 0);
  char* sPar = sParVector.data();

//  // Move Output File Path into the vector
//  const char* charArray = outputFilePath.toStdString().c_str();
//  std::memcpy(sPar + (30 * SizeConstants::SParStringSize), charArray, outputFilePath.size());

  // Move Pattern Data File into the vector
  const char* charArray = patternDataFile.toStdString().c_str();
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
void PatternPreprocessingController::setUpdateProgress(int loopCompleted, int totalLoops)
{
  QString ss = QObject::tr("Average Dot Product: %1 of %2").arg(loopCompleted, totalLoops);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int PatternPreprocessingController::getNumCPUCores()
{
  return QThread::idealThreadCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool PatternPreprocessingController::getCancel() const
{
  return m_Cancel;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternPreprocessingController::setCancel(const bool& value)
{
  m_Cancel = value;
}
