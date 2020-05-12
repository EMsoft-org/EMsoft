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

#include <QtCore/QString>

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
namespace DictionaryIndexingModuleConstants
{
  const QString ModuleName("Dictionary Indexing");

  namespace ArraySizes
  {
    static const size_t IParSize = 80;
    static const size_t FParSize = 80;
    static const size_t SParSize = 80;
    static const size_t SParStringSize = 512;
  }

  namespace IOStrings
  {
    const QString DIModule = "Dictionary Indexing Module";
    const QString InputType = "Input Type";

    const QString ADPMapParams = "Average Dot Product Map Parameters";
    const QString PPParameters = "Pattern Preprocessing Parameters";
    const QString DIParams = "Dictionary Indexing Parameters";
    const QString PatternHeight = "Pattern Height";
    const QString PatternWidth = "Pattern Width";
    const QString UseROI = "Use ROI";
    const QString ROI_X = "ROI_X";
    const QString ROI_Y = "ROI_Y";
    const QString ROI_W = "ROI_W";
    const QString ROI_H = "ROI_H";
    const QString BinningFactor = "Binning Factor";
    const QString BinningX = "Binning X";
    const QString BinningY = "Binning Y";
    const QString IPFHeight = "IPF Height";
    const QString IPFWidth = "IPF Width";
    const QString MaskPattern = "Mask Pattern";
    const QString MaskRadius = "Mask Radius";
    const QString HipassFilter = "Hipass Filter";
    const QString NumberOfRegions = "Number Of Regions";
    const QString NumberOfThreads = "Number Of Threads";
    const QString SelectedADPCoordX = "Selected ADP Coord X";
    const QString SelectedADPCoordY = "Selected ADP Coord Y";

    const QString HipassValue("Hipass Value");
    const QString HipassNumOfSteps("Hipass Number of Steps");
    const QString MinNumOfRegions("Minimum Number of Regions");
    const QString MaxNumOfRegions("Maximum Number of Regions");
    const QString NumOfRegionsStepSize("Number of Regions Step Size");
    const QString SelectedPatternImageX = "Selected Pattern Image X";
    const QString SelectedPatternImageY = "Selected Pattern Image Y";

    const QString IndexingMode = "Indexing Mode";
    const QString EulerAngleFile = "Euler Angle File";
    const QString DictionaryFile = "Dictionary File";
    const QString MasterPatternFile = "Master Pattern File";
    const QString SamplingStepSizeX = "Sampling Step Size X";
    const QString SamplingStepSizeY = "Sampling Step Size Y";
    const QString ADPMatches = "ADP Matches";
    const QString OrientationSimilarityMM = "Orientation Similarity Map Matches";
    const QString OrientationAveragingMM = "Orientation Averaging Matches";
    const QString IndexingSuccessMM = "Orientation Averaging Matches";
    const QString IndexingSuccessThreshAngle = "Indexing Success Thresh Angle";
    const QString UseCustomMask = "Use Custom Mask";
    const QString MaskFile = "Mask File";
    const QString NumDictSingle = "NumDictSingle";
    const QString NumExptSingle = "NumExptSingle";
    const QString GPUPlatform = "GPU Platform";
    const QString GPUDevice = "GPU Device";
    const QString L = "L";
    const QString CubochoricPoints = "Cubochoric Points";
    const QString CameraTiltAngle = "Camera Tilt Angle";
    const QString Delta = "Delta";
    const QString NumsX = "NumsX";
    const QString NumsY = "NumsY";
    const QString PatternCenterX = "Pattern Center X";
    const QString PatternCenterY = "Pattern Center Y";
    const QString Omega = "Omega";
    const QString EnergyMin = "Energy Min";
    const QString EnergyMax = "Energy Max";
    const QString EnergyAveragingMethod = "Energy Averaging Method";
    const QString SpatialAveragingMethod = "Spatial Averaging Method";
    const QString BeamCurrent = "Beam Current";
    const QString DwellTime = "Dwell Time";
    const QString BinningMode = "Binning Mode";
    const QString IntensityScalingMode = "Intensity Scaling Mode";
    const QString GammaCorrectionFactor = "Gamma Correction Factor";
    const QString OutputDataFile = "Output Data File";
    const QString OutputCtfFile = "Output Ctf File";
    const QString OutputAngFile = "Output Ang File";
    const QString OutputAvgCtfFile = "Output Avg Ctf File";
  }
}

