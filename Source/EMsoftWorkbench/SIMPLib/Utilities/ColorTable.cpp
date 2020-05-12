/* ============================================================================
 * Copyright (c) 2009-2019 BlueQuartz Software, LLC
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

#include "ColorTable.h"

#include <algorithm>
#include <iostream>

#include <QtCore/QJsonArray>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SIMPLColorTable::SIMPLColorTable() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SIMPLColorTable::~SIMPLColorTable() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SIMPLColorTable::GetColorTable(int numColors, QVector<float>& colorsOut)
{
  static const int numColorNodes = 8;
  float color[numColorNodes][3] = {
      {0.0f, 0.0f / 255.0f, 255.0f / 255.0f},            // blue
      {105.0f / 255.0f, 145.0f / 255.0f, 2.0f / 255.0f}, // yellow
      {0.0f / 255.0f, 255.0f / 255.0f, 29.0f / 255.0f},  // Green
      {180.0f / 255.0f, 255.0f / 255.0f, 0.0f / 255.0f},
      {255.0f / 255.0f, 215.0f / 255.0f, 6.0f / 255.0f},
      {255.0f / 255.0f, 143.0f / 255.0f, 1.0f / 255.0f},
      {255.0f / 255.0f, 69.0f / 255.0f, 0.0f / 255.0f},
      {255.0f / 255.0f, 0.0f / 255.0f, 0.0f / 255.0f} // red
  };

  static const int maxNodeIndex = numColorNodes - 1;
  const float stepSize = 1.0f / numColors;
  const float nodeStepSize = 1.0f / (maxNodeIndex);
  for(int i = 0; i < numColors; i++)
  {
    float pos = i * stepSize; // [0, 1] range
    int currColorBin = static_cast<int>(pos / nodeStepSize);
    float currFraction = (pos / nodeStepSize) - currColorBin;

    float r;
    float g;
    float b;
    currColorBin = std::min(currColorBin, maxNodeIndex);
    // currColorBin + 1 causes this to step out of color[] bounds when currColorBin == (numColorNodes - 1)
    if(i < numColors - 1)
    {
      r = color[currColorBin][0] * (1.0f - currFraction) + color[currColorBin + 1][0] * currFraction;
      g = color[currColorBin][1] * (1.0f - currFraction) + color[currColorBin + 1][1] * currFraction;
      b = color[currColorBin][2] * (1.0f - currFraction) + color[currColorBin + 1][2] * currFraction;
    }
    else
    {
      r = color[currColorBin][0];
      g = color[currColorBin][1];
      b = color[currColorBin][2];
    }
    colorsOut[3 * i] = r;
    colorsOut[3 * i + 1] = g;
    colorsOut[3 * i + 2] = b;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<unsigned char> SIMPLColorTable::GetColorTable(size_t numColors, QJsonArray colorControlPoints)
{
  const size_t controlColorsCount = colorControlPoints.count() / 4;
  const size_t numComponents = 4;
  std::vector<std::vector<double>> controlPoints(controlColorsCount, std::vector<double>(numComponents));

  // Migrate colorControlPoints values from QJsonArray to 2D array.  Store A-values in binPoints vector.
  std::vector<float> binPoints;
  for(size_t i = 0; i < controlColorsCount; i++)
  {
    for(size_t j = 0; j < numComponents; j++)
    {
      controlPoints[i][j] = static_cast<float>(colorControlPoints[numComponents * i + j].toDouble());
      if(j == 0)
      {
        binPoints.push_back(controlPoints[i][j]);
      }
    }
  }

  // Normalize binPoints values
  const float min = binPoints[0];
  const float max = binPoints[binPoints.size() - 1];
  for(size_t i = 0; i < binPoints.size(); i++)
  {
    binPoints[i] = (binPoints[i] - min) / (max - min);
  }

  std::vector<unsigned char> generatedColors(numColors * 3);
  size_t currentBinIndex = 0;
  const float colorStepSize = 1.0f / numColors;
  for(size_t i = 0; i < numColors; i++)
  {
    // Calculate what point we are at in the entire color range
    const float allColorVal = static_cast<float>(i) * colorStepSize;

    // If we have crossed into the next color bin, increment the currentBinIndex variable.
    if(currentBinIndex + 1 < binPoints.size() && allColorVal > binPoints[currentBinIndex + 1])
    {
      // We have crossed into the next bin
      currentBinIndex++;
    }

    // Find the fractional distance traveled between the beginning and end of the current color bin
    float currFraction = 0.0f;
    if(currentBinIndex + 1 < binPoints.size())
    {
      currFraction = (allColorVal - binPoints[currentBinIndex]) / (binPoints[currentBinIndex + 1] - binPoints[currentBinIndex]);
    }
    else
    {
      currFraction = (allColorVal - binPoints[currentBinIndex]) / (1 - binPoints[currentBinIndex]);
    }

    // If the current color bin index is larger than the total number of control colors, automatically set the currentBinIndex
    // to the last control color.
    currentBinIndex = std::min(currentBinIndex, controlColorsCount - 1);

    // Calculate the RGB values
    unsigned char r = 0;
    unsigned char g = 0;
    unsigned char b = 0;
    if(currentBinIndex < controlColorsCount - 1)
    {
      r = (controlPoints[currentBinIndex][1] * (1.0 - currFraction) + controlPoints[currentBinIndex + 1][1] * currFraction) * 255;
      g = (controlPoints[currentBinIndex][2] * (1.0 - currFraction) + controlPoints[currentBinIndex + 1][2] * currFraction) * 255;
      b = (controlPoints[currentBinIndex][3] * (1.0 - currFraction) + controlPoints[currentBinIndex + 1][3] * currFraction) * 255;
    }
    else
    {
      r = controlPoints[currentBinIndex][1] * 255;
      g = controlPoints[currentBinIndex][2] * 255;
      b = controlPoints[currentBinIndex][3] * 255;
    }

    // Store the RGB values in the RGB generatedColors vector
    generatedColors[3 * i] = r;
    generatedColors[3 * i + 1] = g;
    generatedColors[3 * i + 2] = b;
  }

  return generatedColors;
}
