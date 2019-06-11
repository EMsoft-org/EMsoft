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
#include <QtGui/QImage>

#include "H5Support/HDF5ScopedFileSentinel.h"
#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "Common/MasterPatternFileReader.h"

#include "SIMPLib/Math/SIMPLibMath.h"

#include "Modules/PatternFitModule/PatternFitViewer.h"

class PatternFitController : public QObject
{
  Q_OBJECT

public:
  PatternFitController(QObject* parent = nullptr);
  ~PatternFitController() override;

    /**
    * @brief Setter property for Observer
    */
    void setObserver(IObserver* value); 

    /**
    * @brief Getter property for Observer
    * @return Value of Observer
    */
    IObserver* getObserver() const;
    /**
    * @brief Setter property for MPFileData
    */
    void setMPFileData(const MasterPatternFileReader::MasterPatternData& value); 

    /**
    * @brief Getter property for MPFileData
    * @return Value of MPFileData
    */
    MasterPatternFileReader::MasterPatternData getMPFileData() const;

  struct SimulationData
  {
    double scintillatorDist;
    double detectorTiltAngle;
    double scintillatorPixelSize;
    int numOfPixelsX;
    int numOfPixelsY;
    int patternCenterX;
    int patternCenterY;
    double beamCurrent;
    double dwellTime;
    double sampleOmegaAngle;
    std::vector<float> angles;
    double gammaValue;
    bool useCircularMask;
    bool useHipassFilter;
    bool useLinearRampSubtraction;
    bool useInverseGaussian;
    double hipassFilterLowCutOff;
    QString masterFilePath;
    QString expPatternFilePath;
  };

  /**
   * @brief generatePattern
   * @param detectorData
   * @return
   */
  std::vector<float> generatePattern(PatternFitController::SimulationData detectorData);

  /**
   * @brief generatePatternImage
   * @param detectorData
   * @return
   */
  PatternImageViewer::ImageData generatePatternImage(PatternFitController::SimulationData detectorData);

  /**
   * @brief generatePatternImage
   * @param patternData
   * @param xDim
   * @param yDim
   * @return
   */
  PatternImageViewer::ImageData generatePatternImage(const std::vector<float> &patternData, size_t xDim, size_t yDim, size_t zValue = 0);

  /**
   * @brief validateSimulationValues
   * @param data
   * @return
   */
  bool validateSimulationValues(PatternFitController::SimulationData data) const;

  /**
   * @brief setMasterFilePath Sets a new master file.  Automatically reads the data from the master file
   * into the EMsoftController
   * @param masterFilePath
   */
  void setMasterFilePath(const QString &masterFilePath);

  using VariantPair = QPair<QVariant, QVariant>;
  using FloatPair = QPair<float, float>;
  using IntPair = QPair<int, int>;

signals:
  void updateEkeVs(std::vector<float> ekeVs) const;
  void mpImageNeedsDisplayed(PatternImageViewer::ImageData) const;
  void mcImageNeedsDisplayed(PatternImageViewer::ImageData) const;
  void mpKeVNeedsDisplayed(float keV) const;
  void mcKeVNeedsDisplayed(float keV) const;
  void imageRangeChanged(int min, int max) const;
  void splashScreenMsgGenerated(const QString& msg) const;
  void newProgressBarMaximumValue(int value) const;
  void newProgressBarValue(int value) const;
  void rowDataChanged(const QModelIndex&, const QModelIndex&) const;
  void generationFinished() const;

  void errorMessageGenerated(const QString& msg) const;
  void warningMessageGenerated(const QString& msg) const;
  void stdOutputMessageGenerated(const QString& msg) const;

private:
    IObserver* m_Observer;
    MasterPatternFileReader::MasterPatternData m_MPFileData;

  QString m_MasterFilePath;
  bool m_Cancel = false;

  QVector<QSharedPointer<QFutureWatcher<void>>> m_Watchers;

public:
  PatternFitController(const PatternFitController&) = delete; // Copy Constructor Not Implemented
  PatternFitController(PatternFitController&&) = delete;      // Move Constructor Not Implemented
  PatternFitController& operator=(const PatternFitController&) = delete; // Copy Assignment Not Implemented
  PatternFitController& operator=(PatternFitController&&) = delete;      // Move Assignment Not Implemented
};

Q_DECLARE_METATYPE(PatternFitController::SimulationData)
