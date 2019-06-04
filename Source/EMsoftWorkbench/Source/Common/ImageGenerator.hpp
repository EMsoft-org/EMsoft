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

#pragma once

#include <QtGui/QImage>

#include <H5public.h>

#include "Common/AbstractImageGenerator.h"

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

template <typename T>
class ImageGenerator : public AbstractImageGenerator
{
  public:
    SIMPL_SHARED_POINTERS(ImageGenerator<T>)
    SIMPL_TYPE_MACRO(ImageGenerator<T>)

    static Pointer New(const std::vector<T> &data, hsize_t xDim, hsize_t yDim, int zSlice,
                       bool mirroredHorizontal = false, bool mirroredVertical = false)
    {
      Pointer sharedPtr (new ImageGenerator(data, xDim, yDim, zSlice, mirroredHorizontal, mirroredVertical));
      return sharedPtr;
    }

    ~ImageGenerator() override = default;

    void createImage() override
    {
      m_GeneratedImage = QImage(m_XDim, m_YDim, QImage::Format_Indexed8);
      if (m_Data.empty())
      {
        return;
      }

      QVector<QRgb> colorTable;
      for (int i = 0; i <= 255; i++)
      {
        colorTable.push_back(qRgb(i, i, i));
      }

      m_GeneratedImage.setColorTable(colorTable);

      int firstIndex = m_YDim*m_XDim*m_ZSlice + m_XDim*0 + 0;
      int lastIndex = m_YDim*m_XDim*m_ZSlice + m_XDim*(m_YDim - 1) + (m_XDim - 1);
      T min = std::numeric_limits<T>::max(), max = std::numeric_limits<T>::min();
      for (int i = firstIndex; i <= lastIndex; i++)
      {
        T value = m_Data.at(i);
        if (value < min)
        {
          min = m_Data.at(i);
        }
        if (value > max)
        {
          max = m_Data.at(i);
        }
      }

      m_MinMaxPair.first = min;
      m_MinMaxPair.second = max;

      for (int y = 0; y < m_YDim; y++)
      {
        for (int x = 0; x < m_XDim; x++)
        {
          int index = m_YDim*m_XDim*m_ZSlice + m_XDim*y + x;
          T value = m_Data.at(index);
          if (max == min)
          {
            m_GeneratedImage.setPixel(x, y, value);
          }
          else
          {
            float normalizedValue = (static_cast<float>(value - min)) / (static_cast<float>(max - min));
            normalizedValue = normalizedValue * 255;
            m_GeneratedImage.setPixel(x, y, normalizedValue);
          }
        }
      }

      m_GeneratedImage = m_GeneratedImage.mirrored(m_MirroredHorizontal, m_MirroredVertical);
    }

  protected:
    ImageGenerator(std::vector<T> data, hsize_t xDim, hsize_t yDim, int zSlice,
                   bool mirroredHorizontal = false, bool mirroredVertical = false) :
      AbstractImageGenerator(),
      m_Data(data),
      m_XDim(xDim),
      m_YDim(yDim),
      m_ZSlice(zSlice),
      m_MirroredHorizontal(mirroredHorizontal),
      m_MirroredVertical(mirroredVertical)
    {

    }

  private:
    std::vector<T> m_Data;
    hsize_t m_XDim;
    hsize_t m_YDim;
    int m_ZSlice;
    bool m_MirroredHorizontal;
    bool m_MirroredVertical;

  public:
    ImageGenerator(const ImageGenerator&) = delete; // Copy Constructor Not Implemented
    ImageGenerator(ImageGenerator&&) = delete;      // Move Constructor Not Implemented
    ImageGenerator& operator=(const ImageGenerator&) = delete; // Copy Assignment Not Implemented
    ImageGenerator& operator=(ImageGenerator&&) = delete;      // Move Assignment Not Implemented
};
