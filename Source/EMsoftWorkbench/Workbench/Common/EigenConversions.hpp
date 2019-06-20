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

#include <Eigen/Dense>

class EigenConversions
{
  public:
    EigenConversions() = default;
    virtual ~EigenConversions() = default;

    using FloatArrayType = Eigen::Array<float, Eigen::Dynamic, 1, Eigen::RowMajor>;
    using FloatMatrixType = Eigen::Matrix<float,Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using FloatMapType = Eigen::Map<FloatMatrixType>;

    template <typename T, int RowCount, int Major>
    using TemplateArrayType = Eigen::Array<T, RowCount, 1, Major>;

    template <typename T, int RowCount, int ColumnCount, int Major>
    using TemplateMatrixType = Eigen::Matrix<T, RowCount, ColumnCount, Major>;

    template <typename T, int RowCount, int ColumnCount, int Major>
    using TemplateMapType = Eigen::Map<TemplateMatrixType<T, RowCount, ColumnCount, Major> >;

    template <typename T, int Major>
    static Eigen::Map< Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Major> > DataArrayToEigenMatrixMap(std::vector<T> &dataPtr, const std::vector<size_t> &dims)
    {
      if (dims.size() != 2) { return Eigen::Map< Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Major> >(nullptr, 0, 0); }

      Eigen::Map< Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Major> > matrixMap(dataPtr.data(), dims[0], dims[1]);
      return matrixMap;
    }

    template <typename T, int Major>
    static Eigen::Map< Eigen::Array<T, Eigen::Dynamic, 1, Major> > DataArrayToEigenArrayMap(std::vector<T> &dataPtr, const std::vector<size_t> &dims)
    {
      if (dims.size() != 1) { return Eigen::Map< Eigen::Array<T, Eigen::Dynamic, 1, Major> >(nullptr); }

      Eigen::Map< Eigen::Array<T, Eigen::Dynamic, 1, Major> > arrayMap(dataPtr.data(), dims[0]);
      return arrayMap;
    }

    template <typename T, int RowCount, int ColumnCount, int Major>
    static Eigen::Map< Eigen::Matrix<T, RowCount, ColumnCount, Major> > ImageToEigenMatrixMap(QImage image)
    {
      Eigen::Map< Eigen::Matrix<uchar, Eigen::Dynamic, Eigen::Dynamic, Major> > matrixMap(image.bits(), image.height(), image.width());
      return matrixMap;
    }

  private:


    static size_t Coords2Idx(QVector<size_t> tDims, size_t x, size_t y)
    {
      return (tDims[0] * y) + x;
    }

  public:
    EigenConversions(const EigenConversions&) = delete; // Copy Constructor Not Implemented
    EigenConversions(EigenConversions&&) = delete;      // Move Constructor Not Implemented
    EigenConversions& operator=(const EigenConversions&) = delete; // Copy Assignment Not Implemented
    EigenConversions& operator=(EigenConversions&&) = delete;      // Move Assignment Not Implemented
};
