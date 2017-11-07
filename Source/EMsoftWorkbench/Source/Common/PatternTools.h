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


#ifndef _PatternTools_h_
#define _PatternTools_h_

#include <QtGui/QColor>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"
#include "SIMPLib/DataArrays/DataArray.hpp"

class PatternTools
{
  public:
    virtual ~PatternTools();

    struct IParValues
    {
        int numsx;
        int numset;
        float incidentBeamVoltage;
        float minEnergy;
        float energyBinSize;
        int npx;
        double numOfPixelsX;
        double numOfPixelsY;
        int detectorBinningValue;
        size_t numberOfOrientations;
    };

    struct FParValues
    {
        float omega;
        float sigma;
        double pcPixelsX;
        double pcPixelsY;
        double scintillatorPixelSize;
        double scintillatorDist;
        double detectorTiltAngle;
        double beamCurrent;
        double dwellTime;
        double gammaValue;
    };

    /**
     * @brief GeneratePattern
     * @param iParValues
     * @param fParValues
     * @param lpnhData
     * @param lpshData
     * @param monteCarloSquareData
     * @param eulerAngles
     * @param angleIndex
     * @param cancel
     * @return
     */
    static FloatArrayType::Pointer GeneratePattern(PatternTools::IParValues iParValues, PatternTools::FParValues fParValues,
                                  FloatArrayType::Pointer lpnhData, FloatArrayType::Pointer lpshData,
                                  Int32ArrayType::Pointer monteCarloSquareData, FloatArrayType::Pointer eulerAngles,
                                  int angleIndex, bool &cancel);

    /**
     * @brief ApplyCircularMask
     * @param pattern
     * @return
     */
    static QImage ApplyCircularMask(QImage pattern);

    /**
     * @brief ApplyHipassFilter
     * @param patternData
     * @param dims
     * @param lowCutOff
     * @param resultData
     */
    static FloatArrayType::Pointer ApplyHipassFilter(FloatArrayType::Pointer patternData, QVector<size_t> dims, double lowCutOff, DoubleArrayType::Pointer hipassData);

    /**
     * @brief CalculateDifference
     * @param minuend
     * @param subtrahend
     * @return
     */
    static QImage CalculateDifference(QImage minuend, QImage subtrahend);

    /**
     * @brief CalculateOverlap
     * @param src
     * @param dst
     * @param opacity
     * @return
     */
    static QImage CalculateComposite(QImage src, QImage dst, double opacity);

    /**
     * @brief CalculateColorChannelBlend
     * @param src
     * @param dst
     * @return
     */
    static QImage CalculateColorChannelBlend(QImage src, QImage dst);

    /**
     * @brief CalculateMinMaxValue
     * @param image
     * @return
     */
    static QPair<int, int> CalculateMinMaxValue(QImage image);

    /**
     * @brief RemoveRamp
     * @param image
     * @return
     */
    static QImage RemoveRamp(QImage image);

    /**
     * @brief InverseGaussian
     * @param patternData
     * @param tDims
     * @return
     */
    static FloatArrayType::Pointer InverseGaussian(FloatArrayType::Pointer patternData, QVector<size_t> tDims);

  protected:
    PatternTools();

  private:

    /**
     * @brief GeneratePattern_Helper
     * @param index
     * @param eulerAngles
     * @param genericLPNHPtr
     * @param genericLPSHPtr
     * @param genericAccum_ePtr
     * @param genericEBSDPatternsPtr
     * @param genericIParPtr
     * @param genericFParPtr
     * @param cancel
     * @return
     */
    static void GeneratePattern_Helper(size_t index, FloatArrayType::Pointer eulerAngles, FloatArrayType::Pointer genericLPNHPtr, FloatArrayType::Pointer genericLPSHPtr, Int32ArrayType::Pointer genericAccum_ePtr, FloatArrayType::Pointer genericEBSDPatternsPtr, Int32ArrayType::Pointer genericIParPtr, FloatArrayType::Pointer genericFParPtr, bool &cancel);

    /**
     * @brief Sub2Ind
     * @param tDims
     * @param x
     * @param y
     * @return
     */
    static size_t Coords2Idx(QVector<size_t> tDims, size_t x, size_t y);

    /**
     * @brief Ind2Sub
     * @param tDims
     * @param index
     * @param x
     * @param y
     */
    static void Idx2Coords(QVector<size_t> tDims, size_t index, size_t& x, size_t& y);

    /**
     * @brief FindGen
     * @param size
     * @return
     */
    static FloatArrayType::Pointer FindGen(size_t size);


    // Inverse Gaussian Private Helper Methods
    /**
     * @brief CreateInverseGaussianMask
     * @param patternData
     * @param tDims
     * @return
     */
    static FloatArrayType::Pointer CreateInverseGaussianMask(FloatArrayType::Pointer patternData, QVector<size_t> tDims);

    /**
     * @brief GetInverseGaussianLine
     * @param size
     * @return
     */
    static FloatArrayType::Pointer GetInverseGaussianLine(size_t size);

    /**
     * @brief GetInverseGaussianGrid
     * @param xDim
     * @param yDim
     * @return
     */
    static FloatArrayType::Pointer GetInverseGaussianGrid(QVector<size_t> dims);

    PatternTools(const PatternTools&); // Copy Constructor Not Implemented
    void operator=(const PatternTools&); // Operator '=' Not Implemented
};

#endif /* _PatternTools_h_ */
