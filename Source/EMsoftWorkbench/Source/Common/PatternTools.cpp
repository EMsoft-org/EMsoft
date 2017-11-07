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

#include "EMsoftLib/EMsoftLib.h"

#include "Common/EigenConversions.h"

#include "OrientationLib/OrientationMath/OrientationTransforms.hpp"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternTools::PatternTools()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternTools::~PatternTools()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer PatternTools::GeneratePattern(PatternTools::IParValues iParValues, PatternTools::FParValues fParValues,
                                                          FloatArrayType::Pointer lpnhData, FloatArrayType::Pointer lpshData,
                                                          Int32ArrayType::Pointer monteCarloSquareData, FloatArrayType::Pointer eulerAngles,
                                                          int angleIndex, bool &cancel)
{
  Int32ArrayType::Pointer genericIParPtr = Int32ArrayType::CreateArray(40, QVector<size_t>(1, 1), "IPar");
  genericIParPtr->initializeWithZeros();

  FloatArrayType::Pointer  genericFParPtr = FloatArrayType::CreateArray(40, QVector<size_t>(1, 1), "FPar");
  genericFParPtr->initializeWithZeros();

  int32_t* genericIPar = genericIParPtr->getPointer(0);
  float* genericFPar = genericFParPtr->getPointer(0);

  genericIPar[0] = (iParValues.numsx - 1) / 2;
  genericIPar[8] = iParValues.numset;
  genericIPar[11] = static_cast<int>((iParValues.incidentBeamVoltage - iParValues.minEnergy) / iParValues.energyBinSize) + 1;
  genericIPar[16] = iParValues.npx;

  genericIPar[18] = static_cast<size_t>(iParValues.numOfPixelsX);
  genericIPar[19] = static_cast<size_t>(iParValues.numOfPixelsY);

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

  genericIPar[20] = static_cast<size_t>(iParValues.numberOfOrientations); // number of orientations
  genericIPar[21] = binningIndex;
  genericIPar[22] = static_cast<size_t>(iParValues.numOfPixelsX / iParValues.detectorBinningValue);
  genericIPar[23] = static_cast<size_t>(iParValues.numOfPixelsY / iParValues.detectorBinningValue);

  // and set all the float input parameters for the EMsoftCgetEBSDPatterns routine
  // some of these have been set in previous filters
  genericFPar[0] = fParValues.sigma;
  genericFPar[1] = fParValues.omega;

  genericFPar[14] = fParValues.pcPixelsX;         // pattern center x component (in pixel units)
  genericFPar[15] = fParValues.pcPixelsY;         // pattern center y component (in pixel units)
  genericFPar[16] = fParValues.scintillatorPixelSize;       // pixel size (microns) on scintillator surface
  genericFPar[17] = fParValues.detectorTiltAngle;      // detector tilt angle (degrees) from horizontal (positive for detector looking upwards)
  genericFPar[18] = fParValues.scintillatorDist;           // sample-scintillator distance (microns)
  genericFPar[19] = fParValues.beamCurrent; // beam current [nA]
  genericFPar[20] = fParValues.dwellTime;   // beam dwell time per pattern [micro-seconds]
  genericFPar[21] = fParValues.gammaValue;  // intensity scaling gamma value

  QVector<size_t> cDims(2);
  cDims[0] = genericIPar[22];
  cDims[1] = genericIPar[23];

  FloatArrayType::Pointer genericEBSDPatternsPtr = FloatArrayType::CreateArray(iParValues.numberOfOrientations, cDims, "ebsdPatterns");

  PatternTools::GeneratePattern_Helper(angleIndex, eulerAngles, lpnhData, lpshData, monteCarloSquareData, genericEBSDPatternsPtr, genericIParPtr, genericFParPtr, cancel);

  return genericEBSDPatternsPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternTools::GeneratePattern_Helper(size_t index, FloatArrayType::Pointer eulerAngles, FloatArrayType::Pointer genericLPNHPtr, FloatArrayType::Pointer genericLPSHPtr, Int32ArrayType::Pointer genericAccum_ePtr, FloatArrayType::Pointer genericEBSDPatternsPtr, Int32ArrayType::Pointer genericIParPtr, FloatArrayType::Pointer genericFParPtr, bool &cancel)
{
  int32_t* genericIPar = genericIParPtr->getPointer(0);
  float* genericFPar = genericFParPtr->getPointer(0);
  float* genericEBSDPatterns = genericEBSDPatternsPtr->getPointer(0);
  int32_t* genericAccum_e = genericAccum_ePtr->getPointer(0);
  float* genericLPNH = genericLPNHPtr->getPointer(0);
  float* genericLPSH = genericLPSHPtr->getPointer(0);

  FloatArrayType::Pointer genericQuaternionsPtr = FloatArrayType::CreateArray(1, QVector<size_t>(1, 4), "Quats");
  float* genericQuaternions = genericQuaternionsPtr->getPointer(0);

  QVector<float> eulerAngle;
  eulerAngle.push_back(eulerAngles->getComponent(index, 0));
  eulerAngle.push_back(eulerAngles->getComponent(index, 1));
  eulerAngle.push_back(eulerAngles->getComponent(index, 2));

  QVector<float> quat(4);
  OrientationTransforms<QVector<float>,float>::eu2qu(eulerAngle, quat, QuaternionMath<float>::QuaternionScalarVector);
  genericQuaternionsPtr->setComponent(0, 0, quat[0]);
  genericQuaternionsPtr->setComponent(0, 1, quat[1]);
  genericQuaternionsPtr->setComponent(0, 2, quat[2]);
  genericQuaternionsPtr->setComponent(0, 3, quat[3]);

  EMsoftCgetEBSDPatterns(genericIPar, genericFPar, genericEBSDPatterns, genericQuaternions, genericAccum_e, genericLPNH, genericLPSH, nullptr, 0, &cancel);
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
FloatArrayType::Pointer PatternTools::ApplyHipassFilter(FloatArrayType::Pointer patternData, QVector<size_t> dims, double lowCutOff, DoubleArrayType::Pointer hipassData)
{
  if (dims.size() != 2) { return FloatArrayType::NullPointer(); }

  DoubleArrayType::Pointer patternPtr = DoubleArrayType::CreateArray(patternData->getNumberOfComponents(), "patternPtr");

  for (int i = 0; i < patternData->getNumberOfComponents(); i++)
  {
    patternPtr->setValue(i, static_cast<double>(patternData->getComponent(0, i)));
  }

  int32_t hiPassDims[2];
  hiPassDims[0] = dims[0];
  hiPassDims[1] = dims[1];

  bool init = false;
  bool destroy = false;
  HiPassFilterC(patternPtr->getPointer(0), hiPassDims, &lowCutOff, &init, &destroy, hipassData->getPointer(0));

  FloatArrayType::Pointer newPatternData = std::dynamic_pointer_cast<FloatArrayType>(patternData->deepCopy());
  for (int i = 0; i < hipassData->getNumberOfTuples(); i++)
  {
    newPatternData->setComponent(0, i, static_cast<float>(hipassData->getValue(i)));
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
      return dst;
  if (dst.width() <= 0 || dst.height() <= 0)
      return dst;

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

  if (src.depth() != 32) src = src.convertToFormat(QImage::Format_ARGB32);
  if (dst.depth() != 32) dst = dst.convertToFormat(QImage::Format_ARGB32);

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
FloatArrayType::Pointer PatternTools::InverseGaussian(FloatArrayType::Pointer patternData, QVector<size_t> tDims)
{
  if (tDims.size() != 2) { return FloatArrayType::NullPointer(); }

  FloatArrayType::Pointer newPatternData = std::dynamic_pointer_cast<FloatArrayType>(patternData->deepCopy());
  EigenConversions::FloatMapType matrixMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(newPatternData, tDims);

  // Swap the tDims because Eigen is expecting dimensions in column-major
  size_t temp = tDims[1];
  tDims[1] = tDims[0];
  tDims[0] = temp;

  FloatArrayType::Pointer inverseGaussianMask = CreateInverseGaussianMask(patternData, tDims);
  EigenConversions::FloatMapType inverseGaussianMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(inverseGaussianMask, tDims);
  if (inverseGaussianMask != FloatArrayType::NullPointer())
  {
    matrixMap.array() = matrixMap.array() * inverseGaussianMap.array();
  }

  return newPatternData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer PatternTools::CreateInverseGaussianMask(FloatArrayType::Pointer patternData, QVector<size_t> tDims)
{
  if (tDims.size() != 2) { return FloatArrayType::NullPointer(); }

  FloatArrayType::Pointer inverseGaussian = std::dynamic_pointer_cast<FloatArrayType>(patternData->deepCopy());
  EigenConversions::FloatMapType inverseGaussianMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(inverseGaussian, tDims);

  FloatArrayType::Pointer grid = GetInverseGaussianGrid(tDims);
  EigenConversions::FloatMapType gridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(grid, tDims);

  gridMap = -gridMap * 1.5;

  inverseGaussianMap = 1.0 - 0.75 * gridMap.array().exp();

  return inverseGaussian;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer PatternTools::GetInverseGaussianGrid(QVector<size_t> dims)
{
  FloatArrayType::Pointer xLine = GetInverseGaussianLine(dims[0]);

  FloatArrayType::Pointer xGrid = FloatArrayType::CreateArray(dims, QVector<size_t>(1, 1), "xGrid");
  EigenConversions::FloatMapType xGridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(xGrid, dims);

  for (int col = 0; col < dims[1]; col++)
  {
    for (int row = 0; row < dims[0]; row++)
    {
      xGridMap(row, col) = xLine->getValue(row);
    }
  }

  FloatArrayType::Pointer yLine = GetInverseGaussianLine(dims[1]);

  FloatArrayType::Pointer yGrid = FloatArrayType::CreateArray(dims, QVector<size_t>(1, 1), "yGrid");
  EigenConversions::FloatMapType yGridMap = EigenConversions::DataArrayToEigenMatrixMap<float, Eigen::RowMajor>(yGrid, dims);

  for (int row = 0; row < dims[0]; row++)
  {
    for (int col = 0; col < dims[1]; col++)
    {
      yGridMap(row, col) = yLine->getValue(col);
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
FloatArrayType::Pointer PatternTools::GetInverseGaussianLine(size_t size)
{
  FloatArrayType::Pointer line = FindGen(size);
  for (int i = 0; i < line->getNumberOfTuples(); i++)
  {
    float value = line->getValue(i) - static_cast<float>(size / 2);
    line->setValue(i, value);
  }

  return line;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
FloatArrayType::Pointer PatternTools::FindGen(size_t size)
{
  FloatArrayType::Pointer lineArray = FloatArrayType::CreateArray(size, "Line");
  for (int i = 0; i < lineArray->getNumberOfTuples(); i++)
  {
    lineArray->setValue(i, static_cast<float>(i));
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


