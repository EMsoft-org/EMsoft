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

#include "DictionaryIndexingController.h"

//#include "EMsoftWrapperLib/DictionaryIndexing/EMsoftDIwrappers.h"

#include <QtCore/QMimeDatabase>
#include <QtCore/QDateTime>
#include <QtCore/QThread>
#include <QtCore/QMap>
#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QSharedPointer>

#include <QtGui/QImage>

#include "Common/EbsdLoader.h"

#include "Constants.h"

static size_t k_InstanceKey = 0;
static QMap<size_t, DictionaryIndexingController*> instances;

namespace SizeConstants = DictionaryIndexingModuleConstants::ArraySizes;

/**
 * @brief DIControllerProgress
 * @param instance
 * @param loopCompleted
 * @param totalLoops
 * @param bseYield
 */
void DIControllerProgress(size_t instance, int loopCompleted, int totalLoops)
{
  DictionaryIndexingController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingController::DictionaryIndexingController(QObject* parent)
: QObject(parent)
{
  m_InstanceKey = ++k_InstanceKey;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingController::~DictionaryIndexingController()
{
  k_InstanceKey--;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::createDI(const DIData &data)
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
//  EMsoftCpreprocessEBSDPatterns(iParVector.data(), fParVector.data(), sParVector.data(), m_OutputMaskVector.data(), m_OutputIQMapVector.data(), m_OutputADPMapVector.data(), &ADPMapControllerProgress, m_InstanceKey, &m_Cancel);

  QString diExecutablePath = getDIExecutablePath();

  QSharedPointer<QProcess> diProcess = QSharedPointer<QProcess>(new QProcess());
  connect(diProcess.data(), &QProcess::readyReadStandardOutput, [=] { emit stdOutputMessageGenerated(QString::fromStdString(diProcess->readAllStandardOutput().toStdString())); });
  connect(diProcess.data(), &QProcess::readyReadStandardError, [=] { emit stdOutputMessageGenerated(QString::fromStdString(diProcess->readAllStandardOutput().toStdString())); });
  connect(diProcess.data(), &QProcess::errorOccurred, [=] (QProcess::ProcessError error) { emit stdOutputMessageGenerated(tr("Process Error: %1").arg(QString::number(error))); });
  connect(diProcess.data(), QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [=](int exitCode, QProcess::ExitStatus exitStatus) { listenDIFinished(exitCode, exitStatus, data); });

  if (!diExecutablePath.isEmpty())
  {
    QFileInfo fi(diExecutablePath);
    QString emsoftPathName = fi.path();

    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("EMSOFTPATHNAME", emsoftPathName); // Add an environment variable
    diProcess->setProcessEnvironment(env);

    QString nmlFilePath = m_TempDir.path() + QDir::separator() + "EMEBSDDI.nml";
    writeDIDataToFile(nmlFilePath, data);
    QStringList parameters = {nmlFilePath};
    diProcess->start(diExecutablePath, parameters);

    // Wait until the QProcess is finished to exit this thread.
    // DictionaryIndexingController::createADPMap is currently on a separate thread, so the GUI will continue to operate normally
    diProcess->waitForFinished(-1);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::writeDIDataToFile(const QString &filePath, const DictionaryIndexingController::DIData &data) const
{
  QFile outputFile(filePath);
  if (outputFile.open(QFile::WriteOnly))
  {
    QTextStream out(&outputFile);

    out << " &EBSDIndexingdata\n";
    out << "! The line above must not be changed\n";
    out << "!\n";
    out << "! The values below are the default values for this program\n";
    out << "!\n";
    out << "!###################################################################\n";
    out << "! INDEXING MODE\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! 'dynamic' for on the fly indexing or 'static' for pre calculated dictionary\n";
    switch(data.indexingMode)
    {
//    case IndexingMode::Static:
//      out << " indexingmode = 'static',\n";
//      break;
    case IndexingMode::Dynamic:
      out << " indexingmode = 'dynamic',\n";
      break;
    }
    out << "!\n";
    out << "!###################################################################\n";
    out << "! DICTIONARY PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! do you want Email or Slack notification when the run has completed?\n";
    out << " Notify = 'Off',\n";
    out << "! width of data set in pattern input file\n";
    out << tr(" ipf_wd = %1,\n").arg(data.ipfWidth);
    out << "! height of data set in pattern input file\n";
    out << tr(" ipf_ht = %1,\n").arg(data.ipfHeight);
    out << "! define the region of interest as x0 y0 w h;  leave all at 0 for full field of view\n";
    out << "! region of interest has the point (x0,y0) as its lower left corner and is w x h patterns\n";
    if (data.useROI)
    {
      out << tr(" ROI = %1 %2 %3 %4,\n").arg(QString::number(data.roi_1), QString::number(data.roi_2), QString::number(data.roi_3), QString::number(data.roi_4));
    }
    else
    {
      out << " ROI = 0 0 0 0,\n";
    }
    out << "! X and Y sampling step sizes\n";
    out << tr(" stepX = %1,\n").arg(data.samplingStepSizeX);
    out << tr(" stepY = %1,\n").arg(data.samplingStepSizeY);
    out << "! number of top matches to keep from the dot product results\n";
    out << tr(" nnk = %1,\n").arg(data.nnk);
    out << "! number of top matches to use for orientation averaging (<nnk)\n";
    out << tr(" nnav = %1,\n").arg(data.nnav);
    out << "! number of top matches to use for Orientation Similarity Map computation (<nnk)\n";
    out << tr(" nosm = %1,\n").arg(data.nosm);
    out << "! number of top matches to use for Indexing Success Map computation (<nnk)\n";
    out << tr(" nism = %1,\n").arg(data.nism);
    out << "! Indexing Success threshold angle (degrees)\n";
    out << tr(" isangle = %1,\n").arg(data.isangle);
    out << "! to use a custom mask, enter the mask filename here; leave undefined for standard mask option\n";
    if (data.maskfile.isEmpty())
    {
      out << " maskfile = 'undefined',\n";
    }
    else
    {
      out << tr(" maskfile = '%1',\n").arg(data.maskfile);
    }
    out << "! mask or not\n";
    if (data.useMask)
    {
      out << " maskpattern = 'y',\n";
    }
    else
    {
      out << " maskpattern = 'n',\n";
    }
    out << "! mask radius (in pixels, AFTER application of the binning operation)\n";
    out << tr(" maskradius = %1,\n").arg(data.maskRadius);
    out << "! hi pass filter w parameter; 0.05 is a reasonable value\n";
    out << tr(" hipassw = %1,\n").arg(data.hipassValue);
    out << "! number of regions for adaptive histogram equalization\n";
    out << tr(" nregions = %1,\n").arg(data.numOfRegions);
    out << "\n";
    out << "!###################################################################\n";
    out << "! ONLY SPECIFY WHEN INDEXINGMODE IS 'DYNAMIC'\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! number of cubochoric points to generate list of orientations\n";
    out << tr(" ncubochoric = %1,\n").arg(data.nCubochoric);
    out << "! distance between scintillator and illumination point [microns]\n";
    out << tr(" L = %1,\n").arg(data.L);
    out << "! tilt angle of the camera (positive below horizontal, [degrees])\n";
    out << tr(" thetac = %1,\n").arg(data.thetac);
    out << "! CCD pixel size on the scintillator surface [microns]\n";
    out << tr(" delta = %1,\n").arg(data.delta);
    out << "! number of CCD pixels along x and y\n";
    out << tr(" numsx = %1,\n").arg(data.numsx);
    out << tr(" numsy = %1,\n").arg(data.numsy);
    out << "! pattern center coordinates in units of pixels\n";
    out << tr(" xpc = %1,\n").arg(data.xpc);
    out << tr(" ypc = %1,\n").arg(data.ypc);
    out << "! angle between normal of sample and detector\n";
    out << tr(" omega = %1,\n").arg(data.omega);
    out << "! minimum and maximum energy to use for interpolation [keV]\n";
    out << tr(" energymin = %1,\n").arg(data.energymin);
    out << tr(" energymax = %1,\n").arg(data.energymax);
    out << "! energy averaging method (0 for exact, 1 for approximate)\n";
    switch(data.averagingMethod)
    {
    case EnergyAveragingMethod::Exact:
      out << " energyaverage = 0,\n";
      break;
    case EnergyAveragingMethod::Approximate:
      out << " energyaverage = 1,\n";
      break;
    }
    out << "! spatial averaging method ('y' or 'n' ;can't be used with approximate energy average)\n";
    if (data.useSpatialAveraging)
    {
      out << " spatialaverage = 'y',\n";
    }
    else
    {
      out << " spatialaverage = 'n',\n";
    }
    out << "! incident beam current [nA]\n";
    out << tr(" beamcurrent = %1,\n").arg(data.beamCurrent);
    out << "! beam dwell time [micro s]\n";
    out << tr(" dwelltime = %1,\n").arg(data.dwellTime);
    out << "! binning mode (1, 2, 4, or 8)\n";
    out << tr(" binning = %1,\n").arg(data.binning);
    out << "! intensity scaling mode 'not' = no scaling, 'lin' = linear, 'gam' = gamma correction\n";
    switch(data.scalingMode)
    {
    case IntensityScalingMode::NoScaling:
      out << " scalingmode = 'not',\n";
      break;
    case IntensityScalingMode::Linear:
      out << " scalingmode = 'lin',\n";
      break;
    case IntensityScalingMode::GammaCorrection:
      out << " scalingmode = 'gam',\n";
      break;
    }
    out << "! gamma correction factor\n";
    out << tr(" gammavalue = %1,\n").arg(data.gammaCorrectionFactor);
    out << "!\n";
    out << "!###################################################################\n";
    out << "! INPUT FILE PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! name of datafile where the patterns are stored; path relative to EMdatapathname\n";
    out << tr(" exptfile = '%1',\n").arg(data.exptFile);
    out << "! input file type parameter: Binary, EMEBSD, TSLHDF, TSLup2, OxfordHDF, OxfordBinary, BrukerHDF\n";
    switch(data.inputType)
    {
    case InputType::Binary:
      out << " inputtype = 'Binary',\n";
      break;
    case InputType::TSLup1:
      out << " inputtype = 'TSLup1',\n";
      break;
    case InputType::TSLup2:
      out << " inputtype = 'TSLup2',\n";
      break;
    case InputType::OxfordBinary:
      out << " inputtype = 'OxfordBinary',\n";
      break;
    case InputType::EMEBSD:
      out << " inputtype = 'EMEBSD',\n";
      break;
    case InputType::TSLHDF:
      out << " inputtype = 'TSLHDF',\n";
      break;
    case InputType::OxfordHDF:
      out << " inputtype = 'OxfordHDF',\n";
      break;
    case InputType::BrukerHDF:
      out << " inputtype = 'BrukerHDF',\n";
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
    out << "!\n";
    out << "!###################################################################\n";
    out << "! OTHER FILE PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! temporary data storage file name ; will be stored in $HOME/.config/EMsoft/tmp\n";
    out << tr(" tmpfile = '%1/EMEBSDDict_tmp.data',\n").arg(m_TempDir.path());
    out << " keeptmpfile = 'n',\n";
    out << "! output file ; path relative to EMdatapathname\n";
    out << tr(" datafile = '%1',\n").arg(data.outputDataFilePath);
    out << "! ctf output file ; path relative to EMdatapathname\n";
    out << tr(" ctffile = '%1',\n").arg(data.outputCtfFilePath);
    out << "! average ctf output file ; path relative to EMdatapathname\n";
    if (data.outputAvgCtfFilePath.isEmpty())
    {
      out << " avctffile = 'undefined',\n";
    }
    else
    {
      out << tr(" avctffile = '%1',\n").arg(data.outputAvgCtfFilePath);
    }
    out << "! ang output file ; path relative to EMdatapathname\n";
    out << tr(" angfile = '%1',\n").arg(data.outputAngFilePath);
    out << "! euler angle input file\n";
    if (data.eulerAngleFile.isEmpty())
    {
      out << " eulerfile = 'undefined'\n";
    }
    else
    {
      out << tr(" eulerfile = '%1'\n").arg(data.eulerAngleFile);
    }
    out << "\n";
    out << "!###################################################################\n";
    out << "! ONLY IF INDEXINGMODE IS STATIC\n";
    out << "!###################################################################\n";
    out << "!\n";
    if (data.eulerAngleFile.isEmpty())
    {
      out << " dictfile = 'undefined'\n";
    }
    else
    {
      out << tr(" dictfile = '%1'\n").arg(data.dictFile);
    }
    out << "!\n";
    out << "!###################################################################\n";
    out << "! ONLY IF INDEXINGMODE IS DYNAMIC\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! master pattern input file; path relative to EMdatapathname\n";
    out << tr(" masterfile = '%1',\n").arg(data.masterFile);
    out << "!\n";
    out << "!###################################################################\n";
    out << "! SYSTEM PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'\n";
    out << "!###################################################################\n";
    out << "!\n";
    out << "! number of dictionary files arranged in column for dot product on GPU (multiples of 16 perform better)\n";
    out << tr(" numdictsingle = %1,\n").arg(data.numDictSingle);
    out << "! number of experimental files arranged in column for dot product on GPU (multiples of 16 perform better)\n";
    out << tr(" numexptsingle = %1,\n").arg(data.numExptSingle);
    out << "! number of threads for parallel execution\n";
    out << tr(" nthreads = %1,\n").arg(data.numOfThreads);
    out << "! GPU platform ID selector\n";
    out << tr(" platid = %1,\n").arg(data.platId);
    out << "! GPU device ID selector\n";
    out << tr(" devid = %1,\n").arg(data.devId);
    out << "! if you are running EMEBSDDImem on multiple GPUs, enter their device ids (up to eight) here; leave others at zero\n";
    out << tr(" multidevid = %1 0 0 0 0 0 0 0,\n").arg(data.devId);
    out << "! how many GPU devices do you want to use?\n";
    out << " usenumd = 1,\n";
    out << " /\n";

    outputFile.close();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::listenDIFinished(int exitCode, QProcess::ExitStatus exitStatus, const DIData &data)
{
  std::array<float,3> refDirection = { 0.0f, 0.0f, 1.0f };
  std::tuple<QImage, int32_t> ipfColorMapResults = EbsdLoader::CreateIPFColorMap(data.outputAngFilePath, refDirection);
  QImage imageResult = std::get<0>(ipfColorMapResults);
  int32_t err = std::get<1>(ipfColorMapResults);
  if (!imageResult.isNull() && err == 0)
  {
    emit diCreated(imageResult);
  }

  m_Executing = false;
  instances.remove(m_InstanceKey);

  // do we need to write this accumulator data into an EMsoft .h5 file?
  // This is so that the results can be read by other EMsoft programs outside of DREAM.3D...
  if(!m_Cancel)
  {
    emit stdOutputMessageGenerated("Dictionary Indexing Generation Complete");
  }
  else
  {
    emit stdOutputMessageGenerated("Dictionary Indexing Generation was successfully canceled");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString DictionaryIndexingController::getDIExecutablePath() const
{
  QString adpExecutablePath;
  QString programName = "EMEBSDDI";

  QDir workingDirectory = QDir(QCoreApplication::applicationDirPath());

#if defined(Q_OS_WIN)
  if (workingDirectory.exists(tr("%1.exe").arg(programName)))
  {
    adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), tr("%1.exe").arg(programName));
    return adpExecutablePath;
  }
#elif defined(Q_OS_MAC)
  // Look to see if we are inside an .app package or inside the 'tools' directory
  if(workingDirectory.dirName() == "MacOS")
  {
    workingDirectory.cdUp();
    if (workingDirectory.cd("bin"))
    {
      if (workingDirectory.exists(programName))
      {
        adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), programName);
        return adpExecutablePath;
      }
      workingDirectory.cdUp();
    }

    workingDirectory.cdUp();
    workingDirectory.cdUp();

    if(workingDirectory.dirName() == "Bin" && workingDirectory.exists(programName))
    {
      adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), programName);
      return adpExecutablePath;
    }
  }
#else
  // We are on Linux - I think
  // Try the current location of where the application was launched from which is
  // typically the case when debugging from a build tree
  QDir workingDirectory = workbenchDir;
  if(workingDirectory.cd("bin"))
  {
    if (workingDirectory.exists(programName))
    {
      adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), programName);
      return adpExecutablePath;
    }

    workingDirectory.cdUp();
  }

  // Now try moving up a directory which is what should happen when running from a
  // proper distribution of SIMPLView
  workingDirectory = workbenchDir;
  workingDirectory.cdUp();
  if(workingDirectory.cd("bin"))
  {
    if (workingDirectory.exists(programName))
    {
      adpExecutablePath = tr("%1%2%3").arg(workingDirectory.absolutePath(), QDir::separator(), programName);
      return adpExecutablePath;
    }

    workingDirectory.cdUp();
  }
#endif

  if (adpExecutablePath.isEmpty())
  {
    QString errMsg = "Could not find EMEBSDDI executable!";
    emit errorMessageGenerated(errMsg);
  }

  return adpExecutablePath;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::initializeData()
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
std::vector<int32_t> DictionaryIndexingController::DIData::getIParVector() const
{
  std::vector<int32_t> iParVector(SizeConstants::IParSize, 0);

  iParVector[17] = numOfThreads;
//  iParVector[18] = patternWidth;
//  iParVector[19] = patternHeight;
//  iParVector[21] = binningFactor;
//  iParVector[22] = patternWidth;
//  iParVector[23] = patternHeight;
  iParVector[25] = ipfWidth;
  iParVector[26] = ipfHeight;
  iParVector[27] = numOfRegions;
//  iParVector[28] = maskPattern;

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
std::vector<float> DictionaryIndexingController::DIData::getFParVector() const
{
  std::vector<float> fParVector(SizeConstants::FParSize, 0.0f);

  fParVector[22] = maskRadius;
//  fParVector[23] = hipassFilter;

  return fParVector;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<char> DictionaryIndexingController::DIData::getSParVector() const
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
void DictionaryIndexingController::setUpdateProgress(int loopCompleted, int totalLoops)
{
  QString ss = QObject::tr("Average Dot Product: %1 of %2").arg(loopCompleted, totalLoops);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int DictionaryIndexingController::getNumCPUCores()
{
  return QThread::idealThreadCount();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool DictionaryIndexingController::getCancel() const
{
  return m_Cancel;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::setCancel(const bool& value)
{
  m_Cancel = value;
}
