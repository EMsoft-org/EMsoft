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

#ifndef _emsoftcontroller_h_
#define _emsoftcontroller_h_



#include <QtCore/QObject>
#include <QtCore/QPair>
#include <QtCore/QSemaphore>
#include <QtCore/QFutureWatcher>
#include <QtGui/QImage>

#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"
#include "H5Support/HDF5ScopedFileSentinel.h"

#include "SIMPLib/Math/SIMPLibMath.h"
#include "SIMPLib/Common/SIMPLibSetGetMacros.h"
#include "SIMPLib/DataArrays/DataArray.hpp"


#include "EMsoftWorkbench/MPMCDisplayWidget.h"
#include "EMsoftWorkbench/PatternDisplayWidget.h"

class EMsoftController : public QObject
{
    Q_OBJECT

  public:
    EMsoftController(QObject* parent = nullptr);
    ~EMsoftController();

    SIMPL_INSTANCE_PROPERTY(PatternDisplayWidget*, PatternDisplayWidget)

    struct HeaderData
    {
      // EMheader/EBSDmaster
      QString mpProgramName;
      QString mpVersionId;

      // EMheader/MCOpenCL
      QString mcProgramName;
      QString mcVersionId;

      // EMData/EBSDmaster
      int numMPEnergyBins;
      int numset;

      // EMData/MCOpenCL
      int numDepthBins;
      int numMCEnergyBins;

      // NMLparameters/EBSDMasterNameList
      int npx;

      // NMLparameters/MCCLNameList
      QString mcStructureFileName;
      float incidentBeamVoltage;
      QString mcMode;
      float omega;
      float sigma;
      float minEnergy;
      float maxEnergy;
      float energyBinSize;
      float maxDepth;
      float depthStep;
      int totalNumIncidentEl;
      int numsx;
    };

    struct DetectorData
    {
        double scintillatorDist;
        double detectorTiltAngle;
        double detectorOmegaAngle;
        double scintillatorPixelSize;
        double pixelNumX;
        double pixelNumY;
        double pcPixelsX;
        double pcPixelsY;
        double pixelCoordinateX;
        double pixelCoordinateY;
        double samplingStepSizeX;
        double samplingStepSizeY;
        double beamCurrent;
        double dwellTime;
        double barrelDistortion;
        double energyMin;
        double energyMax;
    };

    /**
     * @brief setMasterFilePath Sets a new master file.  Automatically reads the data from the master file
     * into the EMsoftController
     * @param masterFilePath
     */
    void setMasterFilePath(QString masterFilePath);

    typedef QPair<float, float> FloatPair;
    typedef QPair<int, int> IntPair;

  public slots:
    /**
     * @brief generatePatternImage
     * @param patternData
     * @param detectorData
     */
    void generatePatternImages(PatternDisplayWidget::PatternDisplayData patternData, EMsoftController::DetectorData detectorData);

    /**
     * @brief addPriorityIndex
     * @param index
     */
    void addPriorityIndex(size_t index);

  signals:
    void updateEkeVs(FloatArrayType::Pointer ekeVs);
    void mpImageNeedsDisplayed(GLImageDisplayWidget::GLImageData);
    void mcImageNeedsDisplayed(GLImageDisplayWidget::GLImageData);
    void mpKeVNeedsDisplayed(float keV);
    void mcKeVNeedsDisplayed(float keV);
    void imageRangeChanged(int min, int max);
    void statusMsgGenerated(QString msg);
    void splashScreenMsgGenerated(const QString &msg);
    void newProgressBarMaximumValue(int value);
    void newProgressBarValue(int value);
    void rowDataChanged(const QModelIndex &, const QModelIndex &);
    void generationFinished();

  private slots:
    void updateMPImage(int value, MPMCDisplayWidget::ProjectionMode mode);
    void updateMCImage(int value, MPMCDisplayWidget::ProjectionMode mode);

    void threadFinished();

    void cancelGeneration();

  private:
    QString                                   m_MasterFilePath;
    HeaderData                                m_HeaderData;
    bool                                      m_Cancel = false;
    QSemaphore                                m_NumOfFinishedPatternsLock;
    size_t                                    m_NumOfFinishedPatterns = 0;
    size_t                                    m_NumOfFinishedThreads = 0;

    QList<size_t>                             m_CurrentOrder;
    QList<size_t>                             m_PriorityOrder;
    QSemaphore                                m_CurrentOrderLock;

    FloatArrayType::Pointer                   m_MasterLPNHData;
    std::vector<QImage>                       m_MasterLPNH;
    std::vector<FloatPair>                    m_MasterLPNHPairs;

    FloatArrayType::Pointer                   m_MasterLPSHData;
    std::vector<QImage>                       m_MasterLPSH;
    std::vector<FloatPair>                    m_MasterLPSHPairs;

    FloatArrayType::Pointer                   m_MasterSPNHData;
    std::vector<QImage>                       m_MasterSPNH;
    std::vector<FloatPair>                    m_MasterSPNHPairs;

    FloatArrayType::Pointer                   m_MasterCircleData;
    std::vector<QImage>                       m_MasterCircle;
    std::vector<FloatPair>                    m_MasterCirclePairs;

    Int32ArrayType::Pointer                   m_MonteCarloSquareData;
    std::vector<QImage>                       m_MonteCarloSquare;
    std::vector<IntPair>                      m_MonteCarloSquarePairs;

    Int32ArrayType::Pointer                   m_MonteCarloCircleData;
    std::vector<QImage>                       m_MonteCarloCircle;
    std::vector<IntPair>                      m_MonteCarloCirclePairs;

    Int32ArrayType::Pointer                   m_MonteCarloStereoData;
    std::vector<QImage>                       m_MonteCarloStereo;
    std::vector<IntPair>                      m_MonteCarloStereoPairs;

    FloatArrayType::Pointer                   m_EkeVs;

    QVector< QSharedPointer<QFutureWatcher<void>> >           m_Watchers;

    /**
     * @brief readDatasetDimensions
     * @param parentId
     * @param objectName
     * @return
     */
    std::vector<hsize_t> readDatasetDimensions(hid_t parentId, QString objectName);

    /**
     * @brief readMasterFile Reads data from the current master file
     */
    void readMasterFile();

    /**
     * @brief readHeaderData Helper function that reads all the header data in the master file
     * @param fileId
     */
    void readHeaderData(hid_t fileId);

    /**
     * @brief readMasterPatternData Helper function that reads all the master pattern data in the master file
     * @param fileId
     */
    void readMasterPatternData(hid_t fileId, size_t &currentCount, size_t &totalItems);

    /**
     * @brief readMonteCarloData Helper function that reads all the monte carlo data in the master file
     * @param fileId
     */
    void readMonteCarloData(hid_t fileId, size_t &currentCount, size_t &totalItems);

    /**
     * @brief deHyperSlabData
     * @param data
     */
    template <typename T>
    typename DataArray<T>::Pointer deHyperSlabData(typename DataArray<T>::Pointer data, hsize_t xDim, hsize_t yDim, hsize_t zDim)
    {
      typename DataArray<T>::Pointer newData = std::dynamic_pointer_cast<DataArray<T>>(data->deepCopy());
      size_t currentIdx = 0;

      for (int z = 0; z < zDim; z++)
      {
        for (int y = yDim - 1; y >= 0; y--)   // We count down in the y-direction so that the image isn't flipped
        {
          for (int x = 0; x < xDim; x++)
          {
            int index = (xDim*zDim*y) + (zDim*x) + z;
            T value = data->getValue(index);
            newData->setValue(currentIdx, value);
            currentIdx++;
          }
        }
      }

      return newData;
    }

    /**
     * @brief readStringDataset
     * @param parentId
     * @param objectName
     * @return
     */
    QString readStringDataset(const hid_t &parentId, const QString &objectName)
    {
      QString value = "";
      if (QH5Lite::readStringDataset(parentId, objectName, value) < 0)
      {
        QString objPath = QH5Utilities::getObjectPath(parentId);
        emit statusMsgGenerated(tr("Error: Unable to read string dataset '%1' at path '%2'").arg(objectName).arg(objPath));
      }

      return value;
    }

    template <typename T>
    /**
     * @brief readScalarDataset
     * @param parentId
     * @param objectName
     * @return
     */
    T readScalarDataset(const hid_t &parentId, const QString &objectName)
    {
      T value = -1;
      if (QH5Lite::readScalarDataset(parentId, objectName, value) < 0)
      {
        QString objPath = QH5Utilities::getObjectPath(parentId);
        emit statusMsgGenerated(tr("Error: Unable to read scalar dataset '%1' at path '%2'").arg(objectName).arg(objPath));
      }

      return value;
    }

    /**
     * @brief readDataset
     * @param parentId
     * @param objectName
     * @return
     */
    template <typename T>
    typename DataArray<T>::Pointer readArrayDataset(hid_t parentId, QString objectName)
    {
      std::vector<hsize_t> dims = readDatasetDimensions(parentId, objectName);
      if (dims.size() <= 0) { return DataArray<T>::NullPointer(); }

      size_t numTuples = dims[0];
      for (int i = 1; i < dims.size(); i++)
      {
        numTuples = numTuples * dims[i];
      }

      typename DataArray<T>::Pointer dataArray = DataArray<T>::CreateArray(numTuples, QVector<size_t>(1, 1), objectName);
      herr_t mLPNH_id = QH5Lite::readPointerDataset(parentId, objectName, dataArray->getPointer(0));
      if (mLPNH_id < 0)
      {
        emit statusMsgGenerated(tr("Error: Could not read object '%1'").arg(objectName));
        return DataArray<T>::NullPointer();
      }

      return dataArray;
    }

    /**
     * @brief createImages
     * @param data
     * @param xDim
     * @param yDim
     * @param zDim
     * @return
     */
    template <typename T>
    std::vector<QImage> createImages(typename DataArray<T>::Pointer data, hsize_t xDim, hsize_t yDim, hsize_t zDim, std::vector<QPair<T,T> > &minMaxPairs)
    {
      std::vector<QImage> images;

      if (data->getNumberOfTuples() <= 0) { return images; }

      for (int z = 0; z < zDim; z++)
      {
        QPair<T,T> pair;
        QImage image = createImage<T>(data, xDim, yDim, z, pair);
        images.push_back(image);
        minMaxPairs.push_back(pair);
      }

      return images;
    }

    /**
     * @brief createImages
     * @param data
     * @param xDim
     * @param yDim
     * @param zDim
     * @return
     */
    template <typename T>
    QImage createImage(typename DataArray<T>::Pointer data, hsize_t xDim, hsize_t yDim, hsize_t zValue, QPair<T,T> &minMaxPair)
    {
      T* dataPtr = data->getPointer(0);

      QImage image(xDim, yDim, QImage::Format_Grayscale8);
      if (data->getNumberOfTuples() <= 0) { return image; }

      int firstIndex = yDim*xDim*zValue + xDim*0 + 0;
      int lastIndex = yDim*xDim*zValue + xDim*(yDim - 1) + (xDim - 1);
      T min = std::numeric_limits<T>::max(), max = std::numeric_limits<T>::min();
      for (int i = firstIndex; i <= lastIndex; i++)
      {
        T value = dataPtr[i];
        if (value < min)
        {
          min = dataPtr[i];
        }
        if (value > max)
        {
          max = dataPtr[i];
        }
      }

      minMaxPair.first = min;
      minMaxPair.second = max;

      for (int y = 0; y < yDim; y++)
      {
        for (int x = 0; x < xDim; x++)
        {
          int index = yDim*xDim*zValue + xDim*y + x;
          T value = data->getValue(index);
          if (max == min)
          {
            image.setPixel(x, y, qRgb(value, value, value));
          }
          else
          {
            float normalizedValue = (static_cast<float>(value - min)) / (static_cast<float>(max - min));
            normalizedValue = normalizedValue * 255;
            image.setPixel(x, y, qRgb(normalizedValue, normalizedValue, normalizedValue));
          }
        }
      }

      return image;
    }

    /**
     * @brief generatePatternImagesUsingThread
     * @param patternData
     * @param detectorData
     * @param indexOrder
     */
    void generatePatternImagesUsingThread(PatternDisplayWidget::PatternDisplayData patternData, EMsoftController::DetectorData detectorData);

    /**
     * @brief generatePatternImage
     * @param index
     * @param eulerAngles
     * @param genericLPNHPtr
     * @param genericLPSHPtr
     * @param genericAccum_ePtr
     * @param genericEBSDPatternsPtr
     * @param genericIParPtr
     * @param genericFParPtr
     * @param patternOrigin
     * @return
     */
    bool generatePatternImage(size_t index, FloatArrayType::Pointer eulerAngles, FloatArrayType::Pointer genericLPNHPtr, FloatArrayType::Pointer genericLPSHPtr, Int32ArrayType::Pointer genericAccum_ePtr, FloatArrayType::Pointer genericEBSDPatternsPtr, Int32ArrayType::Pointer genericIParPtr, FloatArrayType::Pointer genericFParPtr, QString patternOrigin);

    EMsoftController(const EMsoftController&);    // Copy Constructor Not Implemented
    void operator=(const EMsoftController&);  // Operator '=' Not Implemented
};

Q_DECLARE_METATYPE(EMsoftController::DetectorData)

#endif /* _emsoftcontroller_h_ */
