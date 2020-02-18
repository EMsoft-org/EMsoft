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

#include <QtCore/QCoreApplication>
#include <QtCore/QDateTime>
#include <QtCore/QTextStream>

#include "H5Support/H5ScopedSentinel.h"
#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "EbsdLib/LaueOps/LaueOps.h"

#include "SIMPLib/Common/Constants.h"
#include "SIMPLib/Utilities/ColorTable.h"

#include "Common/EbsdLoader.h"

#include "EMOpenCLLib/DIwrappers.h"
#include "EMsoftLib/EMsoftStringConstants.h"

#include "Workbench/Common/FileIOTools.h"

const QString k_ExeName = QString("EMEBSDDI");
const QString k_NMLName = QString("EMEBSDDI.nml");

static size_t k_InstanceKey = 0;
static QMap<size_t, DictionaryIndexingController*> instances;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIProcessOutput(size_t instance, int loopCompleted, int totalLoops, float timeRemaining, int nDict, float** eulerArray, float** dpArray, int32_t** indexArray)
{
  DictionaryIndexingController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->setUpdateProgress(loopCompleted, totalLoops, timeRemaining);
    obj->updateOutput(nDict, *eulerArray, *dpArray, *indexArray);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DIProcessError(size_t instance, int errorCode)
{
  DictionaryIndexingController* obj = instances[instance];
  if(nullptr != obj)
  {
    obj->reportError(errorCode);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
DictionaryIndexingController::DictionaryIndexingController(QObject* parent)
: IProcessController(k_ExeName, k_NMLName, parent)
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
void DictionaryIndexingController::initializeData()
{
  m_Cancel = false;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::setData(const InputDataType& data)
{
  m_InputData = data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::executeWrapper()
{
  initializeData();

  QTemporaryDir tempDir;

  QString nmlFilePath = tempDir.path() + QDir::separator() + k_NMLName;
  generateNMLFile(nmlFilePath);

  m_SpaceGroupNumber = readSpaceGroupNumber(m_InputData.masterFile);
  if(m_SpaceGroupNumber < 0)
  {
    QString ss = QObject::tr("Could not read space group number from master file located at '%1'.").arg(m_InputData.masterFile);
    emit errorMessageGenerated(ss);
    return;
  }

  // the EMsoft call will return two arrays: mLPNH and mLPSH
  // call the EMsoft EMsoftCgetEBSDmaster routine to compute the patterns;
  // m_Executing enables the Cancel button to properly work by passing
  // on a m_Cancel flag to the EMsoft routine; the m_InstanceKey provides
  // a unique label to this particular instantiation of this filter, so that
  // multiple simultaneous instantiations of this filter become possible without
  // incorrect interactions between the callback routines.
  m_Executing = true;
  instances[m_InstanceKey] = this;

  char* nmlFilePathArray = nmlFilePath.toLatin1().data();
  char* appNameArray = QCoreApplication::applicationName().toLatin1().data();
  EBSDDIdriver(nmlFilePathArray, appNameArray, &DIProcessOutput, &DIProcessError, m_InstanceKey, &m_Cancel);

  emit finished();
}

// -----------------------------------------------------------------------------
void DictionaryIndexingController::generateNMLFile(const QString& path)
{
  std::vector<std::string> nml;

  nml.emplace_back(std::string(" &EBSDIndexingdata"));
  nml.emplace_back(std::string("! The line above must not be changed"));
  nml.emplace_back(std::string("! The values below are the default values for this program"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! INDEXING MODE"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! 'dynamic' for on the fly indexing or 'static' for pre calculated dictionary"));

  if(m_InputData.indexingMode == IndexingMode::Dynamic)
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::indexingmode, QString("dynamic")));
  }

  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! DICTIONARY PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! do you want Email or Slack notification when the run has completed?"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(QString("Notify"), QString("Off")));
  nml.emplace_back(std::string("! width of data set in pattern input file"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ipfwd, m_InputData.ipfWidth));
  nml.emplace_back(std::string("! height of data set in pattern input file"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ipfht, m_InputData.ipfHeight));
  nml.emplace_back(std::string("! define the region of interest as x0 y0 w h;  leave all at 0 for full field of view"));
  nml.emplace_back(std::string("! region of interest has the point (x0,y0) as its lower left corner and is w x h patterns"));

  if(m_InputData.useROI)
  {
    QString roi = QString("%1 %2 %3 %4").arg(QString::number(m_InputData.roi_x), QString::number(m_InputData.roi_y), QString::number(m_InputData.roi_w), (QString::number(m_InputData.roi_h)));
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ROI, roi, false, false));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ROI, QString("0 0 0 0"), false, false));
  }
  nml.emplace_back(std::string("! X and Y sampling step sizes"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::stepXVariable, m_InputData.samplingStepSizeX));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::stepYVariable, m_InputData.samplingStepSizeY));
  nml.emplace_back(std::string("! number of top matches to keep from the dot product results"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nnk, m_InputData.nnk));
  nml.emplace_back(std::string("! number of top matches to use for orientation averaging (<nnk)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nnav, m_InputData.nnav));
  nml.emplace_back(std::string("! number of top matches to use for Orientation Similarity Map computation (<nnk)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nosm, m_InputData.nosm));
  nml.emplace_back(std::string("! number of top matches to use for Indexing Success Map computation (<nnk)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nism, m_InputData.nism));
  nml.emplace_back(std::string("! Indexing Success threshold angle (degrees)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::isangle, m_InputData.isangle));
  nml.emplace_back(std::string("! to use a custom mask, enter the mask filename here; leave undefined for standard mask option"));
  if(m_InputData.maskfile.isEmpty())
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskfile, QString("undefined")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskfile, m_InputData.maskfile));
  }
  nml.emplace_back(std::string("! mask or not"));
  if(m_InputData.useMask)
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskpattern, QString("y")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskpattern, QString("n")));
  }
  nml.emplace_back(std::string("! mask radius (in pixels, AFTER application of the binning operation)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::maskradius, m_InputData.maskRadius));
  nml.emplace_back(std::string("! hi pass filter w parameter; 0.05 is a reasonable value"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::hipassw, m_InputData.hipassValue));
  nml.emplace_back(std::string("! number of regions for adaptive histogram equalization"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nregions, m_InputData.numOfRegions));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! ONLY SPECIFY WHEN INDEXINGMODE IS 'DYNAMIC'"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! number of cubochoric points to generate list of orientations"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ncubochoricVariable, m_InputData.nCubochoric));
  nml.emplace_back(std::string("! distance between scintillator and illumination point [microns]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::L, m_InputData.L));
  nml.emplace_back(std::string("! tilt angle of the camera (positive below horizontal, [degrees])"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::thetac, m_InputData.thetac));
  nml.emplace_back(std::string("! CCD pixel size on the scintillator surface [microns]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::delta, m_InputData.delta));
  nml.emplace_back(std::string("! number of CCD pixels along x and y"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numsx, m_InputData.numsx));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numsy, m_InputData.numsy));
  nml.emplace_back(std::string("! pattern center coordinates in units of pixels"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::xpc, m_InputData.xpc));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ypc, m_InputData.ypc));
  nml.emplace_back(std::string("! angle between normal of sample and detector"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::omega, m_InputData.omega));
  nml.emplace_back(std::string("! minimum and maximum energy to use for interpolation [keV]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energymin, m_InputData.energymin));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energymax, m_InputData.energymax));
  nml.emplace_back(std::string("! energy averaging method (0 for exact, 1 for approximate)"));
  switch(m_InputData.averagingMethod)
  {
  case EnergyAveragingMethod::Exact:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energyaverage, 0));
    break;
  case EnergyAveragingMethod::Approximate:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::energyaverage, 1));
    break;
  }
  nml.emplace_back(std::string("! spatial averaging method ('y' or 'n' ;can't be used with approximate energy average)"));
  if(m_InputData.useSpatialAveraging)
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::spatialaverage, QString("y")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::spatialaverage, QString("n")));
  }
  nml.emplace_back(std::string("! incident beam current [nA]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::beamcurrent, m_InputData.beamCurrent));
  nml.emplace_back(std::string("! beam dwell time [micro s]"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dwelltime, m_InputData.dwellTime));
  nml.emplace_back(std::string("! binning mode (1, 2, 4, or 8)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::binning, m_InputData.binning));
  nml.emplace_back(std::string("! intensity scaling mode 'not' = no scaling, 'lin' = linear, 'gam' = gamma correction"));
  switch(m_InputData.scalingMode)
  {
  case IntensityScalingMode::NoScaling:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::scalingmode, QString("not")));
    break;
  case IntensityScalingMode::Linear:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::scalingmode, QString("lin")));
    break;
  case IntensityScalingMode::GammaCorrection:
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::scalingmode, QString("gam")));
    break;
  }
  nml.emplace_back(std::string("! gamma correction factor"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::gammavalue, m_InputData.gammaCorrectionFactor));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! INPUT FILE PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! name of datafile where the patterns are stored; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::exptfile, m_InputData.exptFile));
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

  //    QString hdfStringsStr = " HDFstrings = @@HDF_STRINGS@@,\n";
  //    QString hdfStringsJoined = data.hdfStrings.join("' '");
  //    hdfStringsJoined.prepend("'");
  //    hdfStringsJoined.append("'");
  //    hdfStringsStr.replace("@@HDF_STRINGS@@", hdfStringsJoined);
  //    out << hdfStringsStr;
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! OTHER FILE PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! temporary data storage file name ; will be stored in $HOME/.config/EMsoft/tmp"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::tmpfile, tr("%1/EMEBSDDict_tmp.data").arg(m_TempDir.path())));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::keeptmpfile, QString("n")));
  nml.emplace_back(std::string("! output file ; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::datafile, m_InputData.outputDataFilePath));
  nml.emplace_back(std::string("! ctf output file ; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::ctffile, m_InputData.outputCtfFilePath));
  nml.emplace_back(std::string("! average ctf output file ; path relative to EMdatapathname"));
  if(m_InputData.outputAvgCtfFilePath.isEmpty())
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::avctffile, QString("undefined")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::avctffile, m_InputData.outputAvgCtfFilePath));
  }
  nml.emplace_back(std::string("! ang output file ; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::angfile, m_InputData.outputAngFilePath));
  nml.emplace_back(std::string("! euler angle input file"));
  if(m_InputData.eulerAngleFile.isEmpty())
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::eulerfile, QString("undefined")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::eulerfile, m_InputData.eulerAngleFile));
  }
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! ONLY IF INDEXINGMODE IS STATIC"));
  nml.emplace_back(std::string("!###################################################################"));
  if(m_InputData.eulerAngleFile.isEmpty())
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dictfile, QString("undefined")));
  }
  else
  {
    nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::dictfile, m_InputData.dictFile));
  }
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! ONLY IF INDEXINGMODE IS DYNAMIC"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! master pattern input file; path relative to EMdatapathname"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::masterfile, m_InputData.masterFile));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! SYSTEM PARAMETERS: COMMON TO 'STATIC' AND 'DYNAMIC'"));
  nml.emplace_back(std::string("!###################################################################"));
  nml.emplace_back(std::string("! number of dictionary files arranged in column for dot product on GPU (multiples of 16 perform better)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numdictsingle, m_InputData.numDictSingle));
  nml.emplace_back(std::string("! number of experimental files arranged in column for dot product on GPU (multiples of 16 perform better)"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::numexptsingle, m_InputData.numExptSingle));
  nml.emplace_back(std::string("! number of threads for parallel execution"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::nthreads, m_InputData.numOfThreads));
  nml.emplace_back(std::string("! GPU platform ID selector"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::platid, m_InputData.platId));
  nml.emplace_back(std::string("! GPU device ID selector"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::devid, m_InputData.devId));
  nml.emplace_back(std::string("! if you are running EMEBSDDImem on multiple GPUs, enter their device ids (up to eight) here; leave others at zero"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::multidevid, tr("%1 0 0 0 0 0 0 0").arg(m_InputData.devId), false, false));
  nml.emplace_back(std::string("! how many GPU devices do you want to use?"));
  nml.emplace_back(FileIOTools::CreateNMLEntry(EMsoft::Constants::usenumd, 1));

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
void DictionaryIndexingController::reportError(int errorCode)
{
  QString ss = QObject::tr("Process crashed with error code %1.").arg(errorCode);
  emit errorMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::setUpdateProgress(int loopCompleted, int totalLoops, float timeRemaining)
{
  QString timeLeft = QDateTime::fromTime_t(timeRemaining).toUTC().toString("hh:mm:ss");
  float percent = (static_cast<float>(loopCompleted) / totalLoops) * 100;
  QString ss = QObject::tr("%1% complete - Time Remaining: %3").arg(percent).arg(timeLeft);
  emit stdOutputMessageGenerated(ss);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void DictionaryIndexingController::updateOutput(int nDict, float* dictArray, float* dpArray, int32_t* indexArray)
{
  QImage adpMapImage = m_InputData.adpMap;
  adpMapImage = adpMapImage.convertToFormat(QImage::Format_ARGB32, Qt::MonoOnly);
  QImage ipfColorImage = m_InputData.adpMap;
  ipfColorImage = ipfColorImage.convertToFormat(QImage::Format_ARGB32, Qt::MonoOnly);

  QSize roiSize = getRegionOfInterest(m_InputData);
  size_t roiArraySize = roiSize.width() * roiSize.height();

  std::vector<float> dictVec(nDict * 3, 0.0);
  std::vector<float> dpVec(roiArraySize, 0.0);
  std::vector<int32_t> idxVec(roiArraySize, 0);

  std::copy(dictArray, dictArray + (nDict * 3), dictVec.begin());
  std::copy(dpArray, dpArray + roiArraySize, dpVec.begin());
  std::copy(indexArray, indexArray + roiArraySize, idxVec.begin());

  LaueOps::Pointer laueOps = LaueOps::getOrientationOpsFromSpaceGroupNumber(m_SpaceGroupNumber);

  std::vector<float>::iterator minIter = std::min_element(dpVec.begin(), dpVec.end());
  float minConfidence = *minIter;

  std::vector<float>::iterator maxIter = std::max_element(dpVec.begin(), dpVec.end());
  float maxConfidence = *maxIter;

  double refDir[3] = {0, 0, 1};
  for(int y = 0; y < roiSize.height(); y++)
  {
    for(int x = 0; x < roiSize.width(); x++)
    {
      size_t i = x + y * roiSize.width();

      float confidence = dpArray[i];
      float normalizedValue = (static_cast<float>(confidence - minConfidence)) / (static_cast<float>(maxConfidence - minConfidence));
      float confidenceA = normalizedValue * 255;

      //      if(confidence > 0.81f)
      //      {
      double eulers[3] = {dictVec[(idxVec[i] - 1) * 3], dictVec[(idxVec[i] - 1) * 3 + 1], dictVec[(idxVec[i] - 1) * 3 + 2]};

      SIMPL::Rgb rgb = laueOps->generateIPFColor(eulers, refDir, false);
      QRgb rgba = qRgba(qRed(rgb), qGreen(rgb), qBlue(rgb), confidenceA);
      ipfColorImage.setPixel(m_InputData.roi_x + x - 1, m_InputData.roi_y + y - 1, rgba);
      //      }
    }
  }

  emit diCreated(ipfColorImage);
}

// -----------------------------------------------------------------------------
void DictionaryIndexingController::processFinished()
{
  std::array<float, 3> refDirection = {0.0f, 0.0f, 1.0f};
  std::tuple<QImage, int32_t> ipfColorMapResults = EbsdLoader::CreateIPFColorMap(m_InputData.outputAngFilePath, refDirection);
  QImage imageResult = std::get<0>(ipfColorMapResults);
  int32_t err = std::get<1>(ipfColorMapResults);
  if(!imageResult.isNull() && err == 0)
  {
    emit diCreated(imageResult);
  }
}

// -----------------------------------------------------------------------------
QSize DictionaryIndexingController::getRegionOfInterest(InputDataType inputData) const
{
  QSize roiSize(inputData.ipfWidth, inputData.ipfHeight);
  if(inputData.useROI)
  {
    roiSize.setWidth(inputData.roi_w);
    roiSize.setHeight(inputData.roi_h);
  }

  return roiSize;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int DictionaryIndexingController::readSpaceGroupNumber(const QString& masterFile)
{
  hid_t fileId = QH5Utilities::openFile(masterFile);
  if(fileId < 0)
  {
    return -1;
  }
  H5ScopedFileSentinel sentinel(&fileId, true);

  int sgNumber;
  herr_t err = QH5Lite::readScalarDataset(fileId, "CrystalData/SpaceGroupNumber", sgNumber);
  if(err < 0)
  {
    return -1;
  }

  return sgNumber;
}
