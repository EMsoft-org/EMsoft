/* ============================================================================
* Copyright (c) 2009-2016 BlueQuartz Software, LLC
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

#include "PatternTools.h"

#include <QtCore/QFile>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QTextStream>

#include <QtGui/QImage>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "Common/EigenConversions.hpp"
#include "Common/Constants.h"

#include "EbsdLib/OrientationMath/OrientationTransforms.hpp"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternTools::PatternTools() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternTools::~PatternTools() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::GeneratePattern(PatternTools::IParValues iParValues, PatternTools::FParValues fParValues,
                                                          std::vector<float> &lpnhData, std::vector<float> &lpshData,
                                                          std::vector<int32_t> &monteCarloSquareData, const std::vector<float> &eulerAngles,
                                                          int angleIndex, bool &cancel)
{
  std::vector<int32_t> genericIParPtr(EMsoftWorkbenchConstants::Constants::IParSize);
  std::fill(genericIParPtr.begin(), genericIParPtr.end(), 0);

  std::vector<float> genericFParPtr(EMsoftWorkbenchConstants::Constants::FParSize);
  std::fill(genericFParPtr.begin(), genericFParPtr.end(), 0.0f);

  genericIParPtr[0] = (iParValues.numsx - 1) / 2;
  genericIParPtr[8] = iParValues.numset;
  genericIParPtr[11] = static_cast<int>((iParValues.incidentBeamVoltage - iParValues.minEnergy) / iParValues.energyBinSize) + 1;
  genericIParPtr[16] = iParValues.npx;

  genericIParPtr[18] = static_cast<int32_t>(iParValues.numOfPixelsX);
  genericIParPtr[19] = static_cast<int32_t>(iParValues.numOfPixelsY);

  int binningIndex = 0;
  if (iParValues.detectorBinningValue == 2)
  {
    binningIndex = 1;
  }
  else if (iParValues.detectorBinningValue == 4)
  {
    binningIndex = 2;
  }
  else if (iParValues.detectorBinningValue == 8)
  {
    binningIndex = 3;
  }

  genericIParPtr[20] = static_cast<int32_t>(iParValues.numberOfOrientations); // number of orientations
  genericIParPtr[21] = binningIndex;
  genericIParPtr[22] = static_cast<int32_t>(iParValues.numOfPixelsX / iParValues.detectorBinningValue);
  genericIParPtr[23] = static_cast<int32_t>(iParValues.numOfPixelsY / iParValues.detectorBinningValue);

  // and set all the float input parameters for the EMsoftCgetEBSDPatterns routine
  // some of these have been set in previous filters
  genericFParPtr[0] = fParValues.sigma;
  genericFParPtr[1] = fParValues.omega;

  genericFParPtr[14] = fParValues.pcPixelsX;         // pattern center x component (in pixel units)
  genericFParPtr[15] = fParValues.pcPixelsY;         // pattern center y component (in pixel units)
  genericFParPtr[16] = fParValues.scintillatorPixelSize;       // pixel size (microns) on scintillator surface
  genericFParPtr[17] = fParValues.detectorTiltAngle;      // detector tilt angle (degrees) from horizontal (positive for detector looking upwards)
  genericFParPtr[18] = fParValues.scintillatorDist;           // sample-scintillator distance (microns)
  genericFParPtr[19] = fParValues.beamCurrent; // beam current [nA]
  genericFParPtr[20] = fParValues.dwellTime;   // beam dwell time per pattern [micro-seconds]
  genericFParPtr[21] = fParValues.gammaValue;  // intensity scaling gamma value

  std::vector<float> genericEBSDPatternsPtr;
  genericEBSDPatternsPtr.resize(iParValues.numberOfOrientations * genericIParPtr[22] * genericIParPtr[23]);

  PatternTools::GeneratePattern_Helper(angleIndex, eulerAngles, lpnhData, lpshData, monteCarloSquareData, genericEBSDPatternsPtr, genericIParPtr, genericFParPtr, cancel);

  return genericEBSDPatternsPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternTools::GeneratePattern_Helper(size_t index, const std::vector<float> &eulerAngles, std::vector<float> &genericLPNHPtr, std::vector<float> &genericLPSHPtr, std::vector<int32_t> &genericAccum_ePtr, std::vector<float> &genericEBSDPatternsPtr, std::vector<int32_t> &genericIParPtr, std::vector<float> &genericFParPtr, bool &cancel)
{
  std::vector<float> eulerAngle;
  std::vector<float>::const_iterator iter = eulerAngles.begin() + (index * 3);
  eulerAngle.push_back(*(iter + 0));
  eulerAngle.push_back(*(iter + 1));
  eulerAngle.push_back(*(iter + 2));

  std::vector<float> quat(4);
  OrientationTransforms<std::vector<float>,float>::eu2qu(eulerAngle, quat, QuaternionMath<float>::QuaternionScalarVector);

  EMsoftCgetEBSDPatterns(genericIParPtr.data(), genericFParPtr.data(), genericEBSDPatternsPtr.data(), quat.data(), genericAccum_ePtr.data(), genericLPNHPtr.data(), genericLPSHPtr.data(), nullptr, 0, &cancel);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage PatternTools::ApplyCircularMask(QImage pattern)
{
  size_t xDim = pattern.width();
  size_t yDim = pattern.height();
  size_t centerX = xDim / 2;
  size_t centerY = yDim / 2;

  size_t radius = 0;
  if (xDim < yDim)
  {
    radius = xDim / 2;
  }
  else
  {
    radius = yDim / 2;
  }

  size_t radius_sq = radius * radius;

  for (size_t y = 0; y < yDim; y++)
  {
    for (size_t x = 0; x < xDim; x++)
    {
      size_t dist = (x - centerX) * (x - centerX) + (y - centerY) * (y - centerY);
      if (dist > radius_sq)
      {
        pattern.setPixel(x, y, 0);
      }
    }
  }

  return pattern;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::ApplyHipassFilter(const std::vector<float> &patternData, std::vector<size_t> dims, double lowCutOff, std::vector<double> &hipassData)
{
  if (dims.size() != 2) { return std::vector<float>(); }

  std::vector<double> patternPtr(patternData.size());

  for (int i = 0; i < patternData.size(); i++)
  {
    patternPtr.at(i) = static_cast<double>(patternData.at(i));
  }

  int32_t hiPassDims[2];
  hiPassDims[0] = static_cast<int32_t>(dims[0]);
  hiPassDims[1] = static_cast<int32_t>(dims[1]);

  bool init = false;
  bool destroy = false;
  HiPassFilterC(patternPtr.data(), hiPassDims, &lowCutOff, &init, &destroy, hipassData.data());

  std::vector<float> newPatternData = patternData;
  for (int i = 0; i < hipassData.size(); i++)
  {
    newPatternData.at(i) = static_cast<float>(hipassData.at(i));
  }

  return newPatternData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage PatternTools::CalculateDifference(QImage minuend, QImage subtrahend)
{
  if (minuend.width() != subtrahend.width() || minuend.height() != subtrahend.height()) { return QImage(); }

  uchar* differencePtr = minuend.bits();
  uchar* subtrahendPtr = subtrahend.bits();

  for (int i = 0; i < minuend.width() * minuend.height(); i++)
  {
    differencePtr[i] = std::abs(differencePtr[i] - subtrahendPtr[i]);
  }

  QImage differencePattern(differencePtr, minuend.width(), minuend.height(), minuend.format());
  return differencePattern;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage PatternTools::CalculateComposite(QImage src, QImage dst, double opacity)
{
  if (src.width() <= 0 || src.height() <= 0)
  {
    return dst;
  }
  if (dst.width() <= 0 || dst.height() <= 0)
  {
    return dst;
  }

  if (src.width() != dst.width() || src.height() != dst.height()) {
#ifndef NDEBUG
      std::cerr << "WARNING: ImageEffect::blend : src and destination images are not the same size\n";
#endif
      return dst;
  }

  if (opacity < 0.0 || opacity > 1.0) {
#ifndef NDEBUG
      std::cerr << "WARNING: ImageEffect::blend : invalid opacity. Range [0, 1]\n";
#endif
      return dst;
  }

  if (src.depth() != 32)
  {
    src = src.convertToFormat(QImage::Format_ARGB32);
  }
  if (dst.depth() != 32)
  {
    dst = dst.convertToFormat(QImage::Format_ARGB32);
  }

  int pixels = src.width() * src.height();
  {
#ifdef WORDS_BIGENDIAN   // ARGB (skip alpha)
      unsigned char *data1 = (unsigned char *)dst.bits() + 1;
      unsigned char *data2 = (unsigned char *)src.bits() + 1;
#else                    // BGRA
      unsigned char *data1 = (unsigned char *)dst.bits();
      unsigned char *data2 = (unsigned char *)src.bits();
#endif

      for (int i=0; i<pixels; i++)
      {
#ifdef WORDS_BIGENDIAN
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
#else
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
          *data1 += (unsigned char)((*(data2++) - *data1) * opacity);
          data1++;
#endif
          data1++; // skip alpha
          data2++;
      }
  }

  return dst;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage PatternTools::CalculateColorChannelBlend(QImage src, QImage dst)
{
  QImage newImage(src.size(), QImage::Format_ARGB32);

  QVector<size_t> tDims {static_cast<size_t>(newImage.width()), static_cast<size_t>(newImage.height())};
  uchar* srcBits = src.bits();
  uchar* dstBits = dst.bits();
  for (size_t i = 0; i < tDims[0] * tDims[1]; i++)
  {
    size_t x = 0, y = 0;
    Idx2Coords(tDims, i, x, y);

    newImage.setPixel(x, y, qRgb(srcBits[i], dstBits[i], dstBits[i]));
  }

  return newImage;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QPair<int, int> PatternTools::CalculateMinMaxValue(QImage image)
{
  int min = 255;
  int max = 0;

  uchar* imageBits = image.bits();
  for (int i = 0; i < image.width() * image.height(); i++)
  {
    if (imageBits[i] > max)
    {
      max = imageBits[i];
    }

    if (imageBits[i] < min)
    {
      min = imageBits[i];
    }
  }

  QPair<int, int> pair;
  pair.first = min;
  pair.second = max;
  return pair;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QImage PatternTools::RemoveRamp(QImage image)
{
  // Single image input means that the "background" is just that single image
  // instead of an average of a set of images. That means we can take a shortcut
  // and just copy the input image into the "background" image

  size_t totalPoints = image.width() * image.height();
  uchar* imageBits = image.bits();

  std::vector<double> background(totalPoints, 0.0);
//  std::vector<double> counter(totalPoints, 1.0);

  // For ALL Images....
  for(size_t i = 0; i<totalPoints; i++)
  {

    background[i] = background[i] + static_cast<double>(imageBits[i]);
  }
  // END FOR ALL IMAGES

  // This is a expensive no-op because the counter for all values is 1.0
  //    for (size_t j = 0; j < totalPoints; j++)
  //    {
  //        background[j] = (background[j] /= (counter[j]));
  //    }

  // Fit the background to a first order polynomial
  // p are the coefficients p[0] + p[1]*x + p[2]*y
  Eigen::MatrixXd A(totalPoints, 3);
  Eigen::VectorXd B(totalPoints);

  for(int i = 0; i < totalPoints; ++i)
  {
    int xval = static_cast<int>(i / image.width());
    int yval = static_cast<int>(i % image.width());
    B(i) = background[i];
    A(i, 0) = 1;
    A(i, 1) = xval;
    A(i, 2) = yval;

  }

  //  "Fitting a polynomial to data. May take a while to solve if images are large"
  Eigen::VectorXd p = A.colPivHouseholderQr().solve(B);


  Eigen::VectorXd Bcalc(totalPoints);
  double average = 0;

  Bcalc = A * p;
  average = Bcalc.mean();
  Bcalc = Bcalc - Eigen::VectorXd::Constant(totalPoints, average);

  for(int i = 0; i < totalPoints; ++i)
  {
    background[i] = Bcalc(i);
  }

  int m_lowThresh = 0;
  int m_highThresh = 255;

  for(int64_t t = 0; t < totalPoints; t++)
  {
    if (static_cast<uint8_t>(imageBits[t]) >= m_lowThresh && static_cast<uint8_t>(imageBits[t])  <= m_highThresh)
    {
      int16_t value = static_cast<int16_t>(imageBits[t]) - static_cast<int16_t>(Bcalc(t));

      if (value < 0)
      {
        value = 0;
      }

      if (value > 255)
      {
        value = 255;
      }

      imageBits[t] = static_cast<uchar>(value);
    }
  }

  QImage newImage(imageBits, image.width(), image.height(), image.format());
  newImage.setColorTable(image.colorTable());
  return newImage;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::InverseGaussian(const std::vector<float> &patternData, std::vector<size_t> tDims)
{
  if (tDims.size() != 2) { return std::vector<float>(); }

  std::vector<float> newPatternData = patternData;
  EigenConversions::FloatMapType matrixMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(newPatternData, tDims);

  // Swap the tDims because Eigen is expecting dimensions in column-major
  size_t temp = tDims[1];
  tDims[1] = tDims[0];
  tDims[0] = temp;

  std::vector<float> inverseGaussianMask = CreateInverseGaussianMask(patternData, tDims);
  EigenConversions::FloatMapType inverseGaussianMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(inverseGaussianMask, tDims);
  if (!inverseGaussianMask.empty())
  {
    matrixMap.array() = matrixMap.array() * inverseGaussianMap.array();
  }

  return newPatternData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::CreateInverseGaussianMask(const std::vector<float> &patternData, const std::vector<size_t> &tDims)
{
  if (tDims.size() != 2) { return std::vector<float>(); }

  std::vector<float> inverseGaussian = patternData;
  EigenConversions::FloatMapType inverseGaussianMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(inverseGaussian, tDims);

  std::vector<float> grid = GetInverseGaussianGrid(tDims);
  EigenConversions::FloatMapType gridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(grid, tDims);

  gridMap = -gridMap * 1.5;

  inverseGaussianMap = 1.0 - 0.75 * gridMap.array().exp();

  return inverseGaussian;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::GetInverseGaussianGrid(std::vector<size_t> dims)
{
  size_t dimsSize = std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<size_t>());

  std::vector<float> xLine = GetInverseGaussianLine(dims[0]);

  std::vector<float> xGrid(dimsSize);
  EigenConversions::FloatMapType xGridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(xGrid, dims);

  for (int col = 0; col < dims[1]; col++)
  {
    for (int row = 0; row < dims[0]; row++)
    {
      xGridMap(row, col) = xLine.at(row);
    }
  }

  std::vector<float> yLine = GetInverseGaussianLine(dims[1]);

  std::vector<float> yGrid(dimsSize);
  EigenConversions::FloatMapType yGridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(yGrid, dims);

  for (int row = 0; row < dims[0]; row++)
  {
    for (int col = 0; col < dims[1]; col++)
    {
      yGridMap(row, col) = yLine.at(col);
    }
  }

  xGridMap.array() = (xGridMap.array() * xGridMap.array()) + (yGridMap.array() * yGridMap.array());

  QFile file("/Users/joeykleingers/Desktop/PatternTools_Debug.txt");
  if (file.open(QFile::WriteOnly))
  {
    QTextStream out(&file);
    for (int row = 0; row < dims[0]; row++)
    {
      for (int col = 0; col < dims[1]; col++)
      {
        out << xGridMap(row, col) << "\t";
      }

      out << endl;
    }

    file.close();
  }

  if (dims[0] > dims[1])
  {
    float part = static_cast<float>(dims[0] / 2);
    xGridMap.array() = xGridMap.array() / (part * part);
  }
  else
  {
    float part = static_cast<float>(dims[1] / 2);
    xGridMap.array() = xGridMap.array() / (part * part);
  }

  return xGrid;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::GetInverseGaussianLine(size_t size)
{
  std::vector<float> line = FindGen(size);
  for (float &value : line)
  {
    value = value - static_cast<float>(size / 2);
  }

  return line;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<float> PatternTools::FindGen(size_t size)
{
  std::vector<float> lineArray(size);
  for (int i = 0; i < lineArray.size(); i++)
  {
    lineArray.at(i) = static_cast<float>(i);
  }

  return lineArray;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
size_t PatternTools::Coords2Idx(QVector<size_t> tDims, size_t x, size_t y)
{
  return (tDims[0] * y) + x;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternTools::Idx2Coords(QVector<size_t> tDims, size_t index, size_t& x, size_t& y)
{
  x = (index % tDims[0]);
  y = (index / tDims[0]) % tDims[1];
}


