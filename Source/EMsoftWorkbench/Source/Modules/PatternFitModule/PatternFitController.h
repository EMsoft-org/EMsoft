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

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"
#include "SIMPLib/DataArrays/DataArray.hpp"
#include "SIMPLib/Math/SIMPLibMath.h"

#include "Modules/PatternFitModule/PatternFitViewer.h"

class PatternFitController : public QObject
{
  Q_OBJECT

public:
  PatternFitController(QObject* parent = nullptr);
  ~PatternFitController() override;

  SIMPL_INSTANCE_PROPERTY(IObserver*, Observer)
  SIMPL_INSTANCE_PROPERTY(MasterPatternFileReader::MasterPatternData, MPFileData)

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
    FloatArrayType::Pointer angles;
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
  FloatArrayType::Pointer generatePattern(PatternFitController::SimulationData detectorData);

  /**
   * @brief generatePatternImage
   * @param detectorData
   * @return
   */
  GLImageViewer::GLImageData generatePatternImage(PatternFitController::SimulationData detectorData);

  /**
   * @brief generatePatternImage
   * @param patternData
   * @param xDim
   * @param yDim
   * @return
   */
  GLImageViewer::GLImageData generatePatternImage(const FloatArrayType::Pointer& patternData, size_t xDim, size_t yDim, size_t zValue = 0);

  /**
   * @brief validateSimulationValues
   * @param data
   * @return
   */
  bool validateSimulationValues(PatternFitController::SimulationData data);

  /**
   * @brief setMasterFilePath Sets a new master file.  Automatically reads the data from the master file
   * into the EMsoftController
   * @param masterFilePath
   */
  void setMasterFilePath(const QString& masterFilePath);

  using VariantPair = QPair<QVariant, QVariant>;
  using FloatPair = QPair<float, float>;
  using IntPair = QPair<int, int>;

signals:
  void updateEkeVs(FloatArrayType::Pointer ekeVs);
  void mpImageNeedsDisplayed(GLImageViewer::GLImageData);
  void mcImageNeedsDisplayed(GLImageViewer::GLImageData);
  void mpKeVNeedsDisplayed(float keV);
  void mcKeVNeedsDisplayed(float keV);
  void imageRangeChanged(int min, int max);
  void splashScreenMsgGenerated(const QString& msg);
  void newProgressBarMaximumValue(int value);
  void newProgressBarValue(int value);
  void rowDataChanged(const QModelIndex&, const QModelIndex&);
  void generationFinished();

  void errorMessageGenerated(const QString& msg);
  void warningMessageGenerated(const QString& msg);
  void stdOutputMessageGenerated(const QString& msg);

private:
  QString m_MasterFilePath;
  bool m_Cancel = false;

  QVector<QSharedPointer<QFutureWatcher<void>>> m_Watchers;

public:
  PatternFitController(const PatternFitController&) = delete;            // Copy Constructor Not Implemented
  PatternFitController(PatternFitController&&) = delete;                 // Move Constructor Not Implemented
  PatternFitController& operator=(const PatternFitController&) = delete; // Copy Assignment Not Implemented
  PatternFitController& operator=(PatternFitController&&) = delete;      // Move Assignment Not Implemented
};

Q_DECLARE_METATYPE(PatternFitController::SimulationData)
