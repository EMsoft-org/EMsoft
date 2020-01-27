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

#include "ADPMapController.h"

#include <cstring>

#include <QtCore/QCoreApplication>
#include <QtCore/QDateTime>
#include <QtCore/QDebug>
#include <QtCore/QMap>
#include <QtCore/QMimeDatabase>
#include <QtCore/QSharedPointer>
#include <QtCore/QTextStream>
#include <QtCore/QThread>

#include <QtGui/QImage>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Workbench/Common/FileIOTools.h"

#include "Constants.h"

static size_t k_InstanceKey = 0;
static QMap<size_t, ADPMapController*> s_ControllerInstances;

using InputType = EMsoftWorkbenchConstants::InputType;

namespace SizeConstants = DictionaryIndexingModuleConstants::ArraySizes;

/**
 * @brief ADPMapControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void ADPMapControllerProgress(size_t instance, int loopCompleted, int totalLoops)
{
  ADPMapController* obj = s_ControllerInstances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapController::ADPMapController(QObject* parent)
: QObject(parent)
{
  m_InstanceKey = ++k_InstanceKey;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ADPMapController::~ADPMapController()
{
  k_InstanceKey--;
}

// -----------------------------------------------------------------------------
void ADPMapController::setData(const InputDataType& data)
{
  m_InputData = data;
}

// -----------------------------------------------------------------------------
void ADPMapController::cancelProcess()
{
  if(m_CurrentProcess != nullptr)
  {
    m_CurrentProcess->kill();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::executeWrapper()
{
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

  //  // Set the start time for this run (m_StartTime)
  //  m_StartTime = QDateTime::currentDateTime().time().toString();

  //  // the EMsoft call will return two arrays: mLPNH and mLPSH
  //  // call the EMsoft EMsoftCgetEBSDmaster routine to compute the patterns;
  //  // m_Executing enables the Cancel button to properly work by passing
  //  // on a m_Cancel flag to the EMsoft routine; the m_InstanceKey provides
  //  // a unique label to this particular instantiation of this filter, so that
  //  // multiple simultaneous instantiations of this filter become possible without
  //  // incorrect interactions between the callback routines.
  //  m_Executing = true;
  //  s_ControllerInstances[m_InstanceKey] = this;
  //  //  EMsoftCpreprocessEBSDPatterns(iParVector.data(), fParVector.data(), sParVector.data(), m_OutputMaskVector.data(), m_OutputIQMapVector.data(), m_OutputADPMapVector.data(),
  //  //  &ADPMapControllerProgress, m_InstanceKey, &m_Cancel);

  //  QSharedPointer<QProcess> avgDotProductMapProcess = QSharedPointer<QProcess>(new QProcess());
  //  connect(avgDotProductMapProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit
  //  stdOutputMessageGenerated(QString::fromStdString(avgDotProductMapProcess->readAllStandardOutput().toStdString())); }); connect(avgDotProductMapProcess.data(), &QProcess::readyReadStandardError,
  //          [=] { emit stdOutputMessageGenerated(QString::fromStdString(avgDotProductMapProcess->readAllStandardError().toStdString())); });
  //  connect(avgDotProductMapProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { listenADPMapFinished(exitCode,
  //  exitStatus); }); QString adpExecutablePath = getADPMapExecutablePath(); if (!adpExecutablePath.isEmpty())
  //  {
  //    QString nmlFilePath = m_TempDir.path() + QDir::separator() + "EMgetADP.nml";
  //    generateNMLFile(nmlFilePath, data);
  //    QStringList parameters = {nmlFilePath};
  //    avgDotProductMapProcess->start(adpExecutablePath, parameters);

  //    // Wait until the QProcess is finished to exit this thread.
  //    // ADPMapController::createADPMap is currently on a separate thread, so the GUI will continue to operate normally
  //    avgDotProductMapProcess->waitForFinished(-1);
  //  }

  //  emit finished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::execute()
{
  QString dtFormat("yyyy:MM:dd hh:mm:ss.zzz");

  QTemporaryDir tempDir;
  // Set the start time for this run (m_StartTime)
  QString str;
  QTextStream out(&str);

  m_CurrentProcess = QSharedPointer<QProcess>(new QProcess());
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit stdOutputMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardOutput().toStdString())); });
  connect(m_CurrentProcess.data(), &QProcess::readyReadStandardError, [=] { emit errorMessageGenerated(QString::fromStdString(m_CurrentProcess->readAllStandardError().toStdString())); });
  connect(m_CurrentProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { processFinished(exitCode, exitStatus); });
  std::pair<QString, QString> result = FileIOTools::GetExecutablePath(k_ExeName);
  if(!result.first.isEmpty())
  {
    QProcessEnvironment env = m_CurrentProcess->processEnvironment();
    env.insert("EMSOFTPATHNAME", QString::fromStdString(FileIOTools::GetEMsoftPathName()));
    m_CurrentProcess->setProcessEnvironment(env);
    out << k_ExeName << ": Executable Path:" << result.first << "\n";
    out << k_ExeName << ": Start Time: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
    out << k_ExeName << ": Insert EMSOFTPATHNAME=" << QString::fromStdString(FileIOTools::GetEMsoftPathName()) << "\n";
    out << k_ExeName << ": Output from " << k_ExeName << " follows next...."
        << "\n";
    out << "===========================================================\n";

    emit stdOutputMessageGenerated(str);

    QString nmlFilePath = tempDir.path() + QDir::separator() + k_NMLName;
    generateNMLFile(nmlFilePath);
    QStringList parameters = {nmlFilePath};
    m_CurrentProcess->start(result.first, parameters);

    // Wait until the QProcess is finished to exit this thread.
    m_CurrentProcess->waitForFinished(-1);
  }
  else
  {
    emit errorMessageGenerated(result.second);
  }

  str = "";
  out << "===========================================================\n";
  out << k_ExeName << ": Finished: " << QDateTime::currentDateTime().toString(dtFormat) << "\n";
  //  out << k_ExeName << ": Output File Location: " << m_InputData.inputFilePath << "\n";
  emit stdOutputMessageGenerated(str);

  emit finished();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::generateNMLFile(const QString& filePath) const
{
  std::vector<std::string> nml;

  nml.emplace_back(std::string(" &getADP"));
  nml.emplace_back(std::string("! The line above must not be changed"));
  nml.emplace_back(std::string("! The values below are the default values for this program"));
  nml.emplace_back(std::string("! height of inverse pole figure in pixels"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ipfht, m_InputData.ipfHeight));
  nml.emplace_back(std::string("! width of inverse pole figure in pixels"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ipfwd, m_InputData.ipfWidth));
  nml.emplace_back(std::string("! define the region of interest;  leave all at 0 for full field of view"));
  if(m_InputData.useROI)
  {
    QString roi = QString("%1 %2 %3 %4").arg(QString::number(m_InputData.roi_1), QString::number(m_InputData.roi_2), QString::number(m_InputData.roi_3), (QString::number(m_InputData.roi_4)));
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ROI, roi, false, false));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ROI, QString("0 0 0 0"), false, false));
  }
  nml.emplace_back(std::string("! to use a custom mask, enter the mask filename here; leave undefined for standard mask option"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskfile, QString("undefined")));
  nml.emplace_back(std::string("! filter patterns or not"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::filterpattern, QString("y")));
  nml.emplace_back(std::string("! mask patterns or not"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskpattern, QString("n")));
  nml.emplace_back(std::string("! mask radius in pixels"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskradius, m_InputData.maskRadius));
  nml.emplace_back(std::string("! hi pass filter w parameter; 0.05 is a reasonable value"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::hipassw, m_InputData.hipassFilter));
  nml.emplace_back(std::string("! number of regions for adaptive histogram equalization"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nregions, m_InputData.numOfRegions));
  nml.emplace_back(std::string("! number of pattern pixels along x and y"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numsx, m_InputData.patternWidth));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numsy, m_InputData.patternHeight));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! INPUT FILE PARAMETERS"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! name of datafile where the patterns are stored; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::exptfile, m_InputData.patternDataFile));
  nml.emplace_back(std::string("! input file type parameter: Binary, EMEBSD, TSLHDF, TSLup2, OxfordHDF, OxfordBinary, BrukerHDF"));

  switch(m_InputData.inputType)
  {
  case InputType::Binary:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("Binary")));
    break;
  case InputType::TSLup1:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("TSLup1")));
    break;
  case InputType::TSLup2:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("TSLup2")));
    break;
  case InputType::OxfordBinary:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("OxfordBinary")));
    break;
  case InputType::EMEBSD:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("EMEBSD")));
    break;
  case InputType::TSLHDF:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("TSLHDF")));
    break;
  case InputType::OxfordHDF:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("OxfordHDF")));
    break;
  case InputType::BrukerHDF:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::inputtype, QString("BrukerHDF")));
    break;
  }

  nml.emplace_back(std::string("! here we enter the HDF group names and data set names as individual strings (up to 10)"));
  nml.emplace_back(std::string("! enter the full path of a data set in individual strings for each group, in the correct order,"));
  nml.emplace_back(std::string("! and with the data set name as the last name; leave the remaining strings empty (they should all"));
  nml.emplace_back(std::string("! be empty for the Binary and TSLup2 formats)"));

  QString hdfStrings;
  for(int k = 0; k < 10; k++)
  {
    if(k < m_InputData.hdfStrings.size())
    {
      hdfStrings.append("'" + m_InputData.hdfStrings.at(k) + "'");
    }
    else
    {
      hdfStrings.append("''");
    }
    if(k < 9)
    {
      hdfStrings.append(" ");
    }
  }
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::HDFstrings, hdfStrings, false, false));

  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! OTHER FILE PARAMETERS"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! temporary data storage file name ; will be stored in $HOME/.config/EMsoft/tmp"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::tmpfile, tr("%1/EMEBSDDict_tmp.data").arg(m_TempDir.path())));
  nml.emplace_back(std::string("! if set to 'y', then use the existing file by the above name (if it exists), and keep the file"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::usetmpfile, QString("n")));
  nml.emplace_back(std::string("! keep or delete the tmp file at the end of the run"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::keeptmpfile, QString("n")));
  nml.emplace_back(std::string("! output tifffile: only the name part WITHOUT EXTENSION ; path relative to EMdatapathname"));

  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::tiffname, QString("%1/Result").arg(m_TempDir.path())));
  nml.emplace_back(std::string("! number of threads for parallel execution"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nthreads, m_InputData.numOfThreads));
  nml.emplace_back(std::string(" /"));

  QFile outputFile(filePath);
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
    emit errorMessageGenerated(QString("Could not create temp NML file at path %1").arg(filePath));
  }
}

// -----------------------------------------------------------------------------
void ADPMapController::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  // EMgetADP program automatically adds "_ADP" onto the end of the output image name
  QString imagePath = tr("%1/Result_ADP.tiff").arg(m_TempDir.path());
  QImage imageResult(imagePath);
  if(!imageResult.isNull())
  {
    emit adpMapCreated(imageResult);
  }

  m_Executing = false;
  s_ControllerInstances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(m_Cancel)
  {
    emit stdOutputMessageGenerated(QString("%1 was canceled.").arg(k_ExeName));
  }

  if(exitStatus == QProcess::CrashExit)
  {
    emit stdOutputMessageGenerated(QString("%1n process crashed with exit code %2").arg(k_ExeName).arg(exitCode));
  }

  if(exitStatus == QProcess::NormalExit)
  {
    emit stdOutputMessageGenerated(QString("%1 Completed").arg(k_ExeName));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString ADPMapController::getADPMapExecutablePath() const
{
  QString adpExecutablePath;

  QDir workingDirectory = QDir(QCoreApplication::applicationDirPath());

#if defined(Q_OS_WIN)
  if (workingDirectory.exists("EMgetADP.exe"))
  {
    adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), "EMgetADP.exe");
    return adpExecutablePath;
  }
#elif defined(Q_OS_MAC)
  // Look to see if we are inside an .app package or inside the 'tools' directory
  if(workingDirectory.dirName() == "MacOS")
  {
    workingDirectory.cdUp();
    if (workingDirectory.cd("bin"))
    {
      if (workingDirectory.exists("EMgetADP"))
      {
        adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), "EMgetADP");
        return adpExecutablePath;
      }
      workingDirectory.cdUp();
    }

    workingDirectory.cdUp();
    workingDirectory.cdUp();

    if(workingDirectory.dirName() == "Bin" && workingDirectory.exists("EMgetADP"))
    {
      adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), "EMgetADP");
      return adpExecutablePath;
    }
  }
#else
  // We are on Linux - I think
  // Try the current location of where the application was launched from which is
  // typically the case when debugging from a build tree
  if (workingDirectory.exists("EMgetADP.exe"))
  {
    adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), "EMgetADP.exe");
    return adpExecutablePath;
  }
#endif

  if (adpExecutablePath.isEmpty())
  {
    QString errMsg = "Could not find Average Dot Product Map executable!";
    emit errorMessageGenerated(errMsg);
  }

  return adpExecutablePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::initializeData()
{
  m_OutputMaskVector.clear();
  m_OutputIQMapVector.clear();
  m_OutputADPMapVector.clear();
  m_StartTime = "";
  m_Executing = false;
  m_Cancel = false;
}

//// -----------------------------------------------------------------------------
////
//// -----------------------------------------------------------------------------
// std::vector<int32_t> ADPMapController::ADPMapData::getIParVector() const
//{
//  std::vector<int32_t> iParVector(SizeConstants::IParSize, 0);

//  iParVector[17] = numOfThreads;
//  iParVector[18] = patternWidth;
//  iParVector[19] = patternHeight;
////  iParVector[21] = binningFactor;
//  iParVector[22] = patternWidth;
//  iParVector[23] = patternHeight;
//  iParVector[25] = ipfWidth;
//  iParVector[26] = ipfHeight;
//  iParVector[27] = numOfRegions;
////  iParVector[28] = maskPattern;

//  if (useROI)
//  {
//    iParVector[29] = 1;
//  }
//  else
//  {
//    iParVector[29] = 0;
//  }

//  iParVector[30] = roi_1;
//  iParVector[31] = roi_2;
//  iParVector[32] = roi_3;
//  iParVector[33] = roi_4;
//  iParVector[34] = static_cast<int>(inputType);

//  return iParVector;
//}

//// -----------------------------------------------------------------------------
////
//// -----------------------------------------------------------------------------
// std::vector<float> ADPMapController::ADPMapData::getFParVector() const
//{
//  std::vector<float> fParVector(SizeConstants::FParSize, 0.0f);

//  fParVector[22] = maskRadius;
//  fParVector[23] = hipassFilter;

//  return fParVector;
//}

//// -----------------------------------------------------------------------------
////
//// -----------------------------------------------------------------------------
// std::vector<char> ADPMapController::ADPMapData::getSParVector() const
//{
//  // Move each string from the string array into the char array.  Each string has SParStringSize as its max size.
//  std::vector<char> sParVector(SizeConstants::SParSize * SizeConstants::SParStringSize, 0);
//  char* sPar = sParVector.data();

////  // Move Output File Path into the vector
////  const char* charArray = outputFilePath.toStdString().c_str();
////  std::memcpy(sPar + (30 * SizeConstants::SParStringSize), charArray, outputFilePath.size());

//  // Move Pattern Data File into the vector
//  const char* charArray = patternDataFile.toStdString().c_str();
//  std::memcpy(sPar + (31 * SizeConstants::SParStringSize), charArray, patternDataFile.size());

//  // Move HDF Strings into the vector
//  int count = 40;
//  for (const QString &hdfString : hdfStrings)
//  {
//    charArray = hdfString.toStdString().c_str();
//    std::memcpy(sPar + (count * SizeConstants::SParStringSize), charArray, hdfString.size());
//    count++;
//  }

//  return sParVector;
//}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::setUpdateProgress(int loopCompleted, int totalLoops)
{
  QString ss = QObject::tr("Average Dot Product: %1 of %2").arg(loopCompleted, totalLoops);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int ADPMapController::getNumCPUCores()
{
  return QThread::idealThreadCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ADPMapController::getCancel() const
{
  return m_Cancel;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ADPMapController::setCancel(const bool& value)
{
  m_Cancel = value;
}
