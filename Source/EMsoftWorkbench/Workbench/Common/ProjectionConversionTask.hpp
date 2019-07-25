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

#include "Common/ImageGenerationTask.hpp"
#include "Common/ProjectionConversions.hpp"

#include "EbsdLib/Utilities/ModifiedLambertProjection.h"

template <typename P, typename I>
class ProjectionConversionTask : public ImageGenerationTask<I>
{
  public:
    ProjectionConversionTask(const std::vector<P> &data, size_t xDim, size_t yDim, size_t projDim, ModifiedLambertProjection::ProjectionType projType,
                             size_t zValue, ModifiedLambertProjection::Square square, QVector<AbstractImageGenerator::Pointer> &imageGenerators, QSemaphore &sem,
                             size_t vectorIdx, bool horizontalMirror = false, bool verticalMirror = false) :
      ImageGenerationTask<I>(xDim, yDim, zValue, imageGenerators, sem, vectorIdx, horizontalMirror, verticalMirror),
      m_Data(data),
      m_ProjDim(projDim),
      m_ProjType(projType),
      m_Square(square)
    {

    }

    ~ProjectionConversionTask() override = default;

    void beforeImageGeneration() override
    {
      ProjectionConversions projConversion;
      std::vector<float> ptr = projConversion.convertLambertSquareData<P>(m_Data, m_ProjDim, m_ProjType, this->getVectorIndex(), m_Square);
      this->setImageData(ptr);
    }

  private:
    std::vector<P> m_Data;
    size_t m_ProjDim;
    ModifiedLambertProjection::ProjectionType m_ProjType;
    ModifiedLambertProjection::Square m_Square = ModifiedLambertProjection::Square::NorthSquare;

  public:
    ProjectionConversionTask(const ProjectionConversionTask&) = delete; // Copy Constructor Not Implemented
    ProjectionConversionTask(ProjectionConversionTask&&) = delete;      // Move Constructor Not Implemented
    ProjectionConversionTask& operator=(const ProjectionConversionTask&) = delete; // Copy Assignment Not Implemented
    ProjectionConversionTask& operator=(ProjectionConversionTask&&) = delete;      // Move Assignment Not Implemented
};
