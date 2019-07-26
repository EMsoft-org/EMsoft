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

#include <QtCore/QRunnable>
#include <QtCore/QSemaphore>


#include "Common/ImageGenerator.hpp"

template <typename T>
class ImageGenerationTask : public QRunnable
{
  public:
    using Self = ImageGenerationTask;
  using Pointer = std::shared_ptr<Self>;
  using ConstPointer = std::shared_ptr<const Self>;
  using WeakPointer = std::weak_ptr<Self>;
  using ConstWeakPointer = std::weak_ptr<Self>;
  
  static Pointer NullPointer()
  {
    return Pointer(static_cast<Self*>(nullptr));
  }

    ImageGenerationTask(const std::vector<T> &data, size_t xDim, size_t yDim, size_t zSlice, QVector<AbstractImageGenerator::Pointer> &imageGenerators,
                        QSemaphore &sem, size_t listIdx, bool horizontalMirror = false, bool verticalMirror = false) :
      QRunnable(),
      m_Data(data),
      m_zSlice(zSlice),
      m_VectorIdx(listIdx),
      m_xDim(xDim),
      m_yDim(yDim),
      m_HorizontalMirror(horizontalMirror),
      m_VerticalMirror(verticalMirror),
      m_ImageGenerators(&imageGenerators),
      m_Semaphore(&sem)
    {

    }

    ~ImageGenerationTask() override = default;

    virtual void beforeImageGeneration() {}

    virtual void afterImageGeneration() {}

    void run() override
    {
      beforeImageGeneration();

      AbstractImageGenerator::Pointer imgGen = ImageGenerator<T>::New(m_Data, m_xDim, m_yDim, m_zSlice, m_HorizontalMirror, m_VerticalMirror);
      imgGen->createImage();

      m_Semaphore->acquire();
      (*m_ImageGenerators)[m_VectorIdx] = imgGen;
      m_Semaphore->release();

      afterImageGeneration();
    }

  protected:
    ImageGenerationTask(size_t xDim, size_t yDim, size_t zSlice, QVector<AbstractImageGenerator::Pointer> &imageGenerators,
                        QSemaphore &sem, size_t vectorIdx, bool horizontalMirror = false, bool verticalMirror = false) :
      QRunnable(),
      m_zSlice(zSlice),
      m_VectorIdx(vectorIdx),
      m_xDim(xDim),
      m_yDim(yDim),
      m_HorizontalMirror(horizontalMirror),
      m_VerticalMirror(verticalMirror),
      m_ImageGenerators(&imageGenerators),
      m_Semaphore(&sem)
    {

    }

    void setImageData(const std::vector<T> &data)
    {
      m_Data = data;
    }

    size_t getVectorIndex() const
    {
      return m_VectorIdx;
    }

  private:
    std::vector<T> m_Data;

    size_t m_zSlice;
    size_t m_VectorIdx;
    size_t m_xDim;
    size_t m_yDim;
    bool m_HorizontalMirror;
    bool m_VerticalMirror;
    QVector<AbstractImageGenerator::Pointer>* m_ImageGenerators;
    QSemaphore* m_Semaphore;

  public:
    ImageGenerationTask(const ImageGenerationTask&) = delete; // Copy Constructor Not Implemented
    ImageGenerationTask(ImageGenerationTask&&) = delete;      // Move Constructor Not Implemented
    ImageGenerationTask& operator=(const ImageGenerationTask&) = delete; // Copy Assignment Not Implemented
    ImageGenerationTask& operator=(ImageGenerationTask&&) = delete;      // Move Assignment Not Implemented
};
