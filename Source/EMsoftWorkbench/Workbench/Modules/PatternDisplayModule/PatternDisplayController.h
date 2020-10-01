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

#pragma once

#include <QtCore/QFutureWatcher>
#include <QtCore/QObject>
#include <QtCore/QPair>
#include <QtCore/QSemaphore>
#include <QtCore/QThreadPool>
#include <QtGui/QImage>

#include "H5Support/H5ScopedSentinel.h"
#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "Common/AbstractImageGenerator.hpp"
#include "Common/MasterPatternFileReader.h"
#include "Common/ProjectionConversionTask.hpp"

#include "EbsdLib/Math/EbsdLibMath.h"

#include "EbsdLib/Utilities/ModifiedLambertProjection.h"

#include "Modules/PatternDisplayModule/MPMCDisplayWidget.h"
#include "Modules/PatternDisplayModule/SimulatedPatternDisplayWidget.h"

class PatternDisplayController : public QObject
{
  Q_OBJECT

public:
  PatternDisplayController(QObject* parent = nullptr);
  ~PatternDisplayController() override;

    /**
    * @brief Setter property for PatternDisplayWidget
    */
    void setPatternDisplayWidget(SimulatedPatternDisplayWidget* value);

    /**
    * @brief Getter property for PatternDisplayWidget
    * @return Value of PatternDisplayWidget
    */
    SimulatedPatternDisplayWidget* getPatternDisplayWidget() const;
    /**
    * @brief Setter property for Observer
    */
    void setObserver(IObserver* value);

    /**
    * @brief Getter property for Observer
    * @return Value of Observer
    */
    IObserver* getObserver() const;

  struct DetectorData
  {
    double scintillatorDist;
    double detectorTiltAngle;
    double detectorOmegaAngle;
    double scintillatorPixelSize;
    double numOfPixelsX;
    double numOfPixelsY;
    double patternCenterX;
    double patternCenterY;
    double pixelCoordinateX;
    double pixelCoordinateY;
    double samplingStepSizeX;
    double samplingStepSizeY;
    double beamCurrent;
    double dwellTime;
    double barrelDistortion;
    double energyMin;
    double energyMax;
    QString masterFilePath;
  };

  /**
   * @brief validateDetectorValues
   * @param data
   * @return
   */
  bool validateDetectorValues(PatternDisplayController::DetectorData data) const;

  /**
   * @brief setMasterFilePath Sets a new master file.  Automatically reads the data from the master file
   * into the EMsoftController
   * @param masterFilePath
   */
  void setMasterFilePath(const QString &masterFilePath);

  using VariantPair = QPair<QVariant, QVariant>;
  using FloatPair = QPair<float, float>;
  using IntPair = QPair<int, int>;

public slots:
  /**
   * @brief generatePatternImage
   * @param patternData
   * @param detectorData
   */
  void generatePatternImages(SimulatedPatternDisplayWidget::PatternDisplayData patternData, const PatternDisplayController::DetectorData &detectorData);

  /**
   * @brief addPriorityIndex
   * @param index
   */
  void addPriorityIndex(size_t index);

signals:
  void minMaxEnergyLevelsChanged(const std::vector<float> &ekeVs) const;
  void mpImageNeedsDisplayed(PatternImageViewer::ImageData) const;
  void mcImageNeedsDisplayed(PatternImageViewer::ImageData) const;
  void energyMinChanged(int min) const;
  void energyMaxChanged(int max) const;
  void imageRangeChanged(int min, int max) const;
  void newProgressBarMaximumValue(int value) const;
  void newProgressBarValue(int value) const;
  void rowDataChanged(const QModelIndex&, const QModelIndex&) const;
  void mpmcGenerationFinished() const;
  void patternGenerationFinished() const;
  void mpInitializationFinished() const;

  void errorMessageGenerated(const QString& msg) const;
  void warningMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;

private slots:
  void updateMPImage(MPMCDisplayWidget::MPMCData mpData) const;
  void updateMCImage(MPMCDisplayWidget::MPMCData mcData) const;

  void checkImageGenerationCompletion() const;

  void patternThreadFinished(int maxThreadCount);

  void cancelGeneration();

private:
  SimulatedPatternDisplayWidget* m_PatternDisplayWidget;
  IObserver* m_Observer;

  const QString m_MasterLPNHName = "masterLPNH";
  const QString m_MasterLPSHName = "masterLPSH";
  const QString m_MasterCircleName = "masterCircle";
  const QString m_MasterSPNHName = "masterSPNH";
  const QString m_MonteCarloSquareName = "monteCarloSquare";
  const QString m_MonteCarloCircleName = "monteCarloCircle";
  const QString m_MonteCarloStereoName = "monteCarloStereo";

  QString m_MasterFilePath;
  bool m_Cancel = false;
  QSemaphore m_NumOfFinishedPatternsLock;
  size_t m_NumOfFinishedPatterns = 0;

  QList<size_t> m_CurrentOrder;
  QList<size_t> m_PriorityOrder;
  QSemaphore m_CurrentOrderLock;

  MasterPatternFileReader::MasterPatternData m_MP_Data;

  std::vector<AbstractImageGenerator::Pointer> m_MasterLPNHImageGenerators;
  QSemaphore m_MasterLPNHImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MasterLPSHImageGenerators;
  QSemaphore m_MasterLPSHImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MasterCircleImageGenerators;
  QSemaphore m_MasterCircleImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MasterStereoImageGenerators;
  QSemaphore m_MasterStereoImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MCSquareImageGenerators;
  QSemaphore m_MCSquareImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MCCircleImageGenerators;
  QSemaphore m_MCCircleImageGenLock;

  std::vector<AbstractImageGenerator::Pointer> m_MCStereoImageGenerators;
  QSemaphore m_MCStereoImageGenLock;

  int32_t m_NumOfFinishedPatternThreads = 0;
  std::vector<std::unique_ptr<QFutureWatcher<void>>> m_PatternWatchers;

  /**
   * @brief createMasterPatternImageGenerators Helper function that creates all the image generators for the master pattern images
   */
  void createMasterPatternImageGenerators();

  /**
   * @brief createMonteCarloImageGenerators Helper function that creates all the image generators for the monte carlo images
   */
  void createMonteCarloImageGenerators();

  /**
   * @brief createImageGeneratorTasks
   * @param data
   * @param xDim
   * @param yDim
   * @param zDim
   * @param imageGenerators
   * @param sem
   * @param horizontalMirror
   * @param verticalMirror
   */
  template <typename T>
  void createImageGeneratorTasks(const std::vector<T>& data, size_t xDim, size_t yDim, size_t zDim, std::vector<AbstractImageGenerator::Pointer>& imageGenerators, QSemaphore& sem,
                                 bool horizontalMirror = false, bool verticalMirror = false)
  {
    for(size_t z = 0; z < zDim; z++)
    {
      ImageGenerationTask<T>* task = new ImageGenerationTask<T>(data, xDim, yDim, z, imageGenerators, sem, z, horizontalMirror, verticalMirror);
      task->setAutoDelete(true);
      QThreadPool::globalInstance()->start(task);
    }
  }

  // -----------------------------------------------------------------------------
  template <typename T, typename U>
  void createProjectionConversionTasks(const std::vector<T>& data, size_t xDim, size_t yDim, size_t zDim, size_t projDim, ModifiedLambertProjection::ProjectionType projType,
                                       ModifiedLambertProjection::Square square, std::vector<AbstractImageGenerator::Pointer>& imageGenerators, QSemaphore& sem, bool horizontalMirror = false,
                                       bool verticalMirror = false)
  {
    for(size_t z = 0; z < zDim; z++)
    {
      ProjectionConversionTask<T, U>* task = new ProjectionConversionTask<T, U>(data, xDim, yDim, projDim, projType, 0, square, imageGenerators, sem, z, horizontalMirror, verticalMirror);
      task->setAutoDelete(true);
      QThreadPool::globalInstance()->start(task);
    }
  }

  /**
   * @brief deHyperSlabData
   * @param data
   */
  template <typename T>
  std::vector<T> deHyperSlabData(const std::vector<T> &data, hsize_t xDim, hsize_t yDim, hsize_t zDim)
  {
    std::vector<T> newData = data;
    size_t currentIdx = 0;

    for(size_t z = 0; z < zDim; z++)
    {
      for(int32_t y = static_cast<int32_t>(yDim) - 1; y >= 0; y--) // We count down in the y-direction so that the image isn't flipped
      {
        for(size_t x = 0; x < xDim; x++)
        {
          size_t index = (xDim * zDim * static_cast<size_t>(y)) + (zDim * x) + z;
          T value = data.at(index);
          newData.at(currentIdx) = value;
          currentIdx++;
        }
      }
    }

    return newData;
  }

  /**
   * @brief generatePatternImagesUsingThread
   * @param patternData
   * @param detectorData
   * @param indexOrder
   */
  void generatePatternImagesUsingThread(const SimulatedPatternDisplayWidget::PatternDisplayData &patternData, const DetectorData &detectorData);

  /**
   * @brief generatePatternImage
   * @param imageData
   * @param pattern
   * @param xDim
   * @param yDim
   * @param zValue
   * @return
   */
  bool generatePatternImage(PatternImageViewer::ImageData& imageData, const std::vector<float> &pattern, hsize_t xDim, hsize_t yDim, hsize_t zValue) const;

public:
  PatternDisplayController(const PatternDisplayController&) = delete;            // Copy Constructor Not Implemented
  PatternDisplayController(PatternDisplayController&&) = delete;                 // Move Constructor Not Implemented
  PatternDisplayController& operator=(const PatternDisplayController&) = delete; // Copy Assignment Not Implemented
  PatternDisplayController& operator=(PatternDisplayController&&) = delete;      // Move Assignment Not Implemented
};

Q_DECLARE_METATYPE(PatternDisplayController::DetectorData)
