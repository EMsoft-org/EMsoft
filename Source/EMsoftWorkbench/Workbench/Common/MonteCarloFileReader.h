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

#include <QtCore/QObject>
#include <QtCore/QString>

#include <H5Support/QH5Lite.h>
#include <H5Support/QH5Utilities.h>


#include "Common/XtalFileReader.h"

#include <hdf5.h>

class MonteCarloFileReader : public XtalFileReader
{
    Q_OBJECT

  public:
    MonteCarloFileReader();
    ~MonteCarloFileReader() override;

    /**
     * @brief closeFile
     * @return
     */
    bool closeFile() override;

    bool getSampleTiltAngleSigma(double &sampleTiltAngleSigma);
    bool getSampleRotationalAngleOmega(double &sampleRotAngleOmega);
    bool getSampleStartTiltAngle(double &sampleStartTiltAngle);
    bool getSampleEndTiltAngle(double &sampleEndTiltAngle);
    bool getSampleTiltStepSize(double &sampleTiltStepSize);
    bool getAcceleratingVoltage(double &accelVoltage);
    bool getMinimumEnergyToConsider(double &minEnergy);
    bool getEnergyBinSize(double &energyBinSize);
    bool getMaximumDepthToConsider(double &maxDepth);
    bool getDepthStepSize(double &depthStepSize);
    bool getNumberOfPixelsN(int &numOfPixelsN);
    bool getNumberOfElectronsPerWorkitem(int &numOfEPerWorkitem);
    bool getTotalNumberOfElectronsConsidered(int &totEConsidered);
    bool getMultiplierForTotalNumberOfElectrons(int &multiplier);
    bool getGPUPlatformID(int &gpuPlatformID);
    bool getGPUDeviceID(int &gpuDeviceID);
    bool getGlobalWorkgroupSize(int &globalWorkgrpSize);
    bool getMonteCarloMode(int &mcMode);
    bool getXtalFileName(QString &xtalFileName);
    std::vector<int32_t> getAccumzPtr();
    std::vector<int32_t> getAccumePtr();
    std::vector<int32_t> getIParPtr();
    std::vector<float> getFParPtr();

  private:
    double m_SampleTiltAngleSig;
    double m_SampleRotAngleOmega;
    double m_SampleStartTiltAngle;
    double m_SampleEndTiltAngle;
    double m_SampleTiltStepSize;
    double m_AcceleratingVoltage;
    double m_MinEnergyConsider;
    double m_EnergyBinSize;
    double m_MaxDepthConsider;
    double m_DepthStepSize;
    int m_NumOfPixelsN;
    int m_NumOfEPerWorkitem;
    int m_TotalNumOfEConsidered;
    int m_MultiplierForTotalNumOfE;
    int m_GPUPlatformID;
    int m_GPUDeviceID;
    int m_GlobalWorkGroupSize;
    int m_MonteCarloMode;
    QString m_XtalFileName;
    std::vector<int32_t> m_AccumePtr;
    std::vector<int32_t> m_AccumzPtr;
    std::vector<int32_t> m_IParVector;
    std::vector<float> m_FParVector;

    /**
     * @brief initializeData
     */
    void initializeData();

  public:
    MonteCarloFileReader(const MonteCarloFileReader&) = delete; // Copy Constructor Not Implemented
    MonteCarloFileReader(MonteCarloFileReader&&) = delete;      // Move Constructor Not Implemented
    MonteCarloFileReader& operator=(const MonteCarloFileReader&) = delete; // Copy Assignment Not Implemented
    MonteCarloFileReader& operator=(MonteCarloFileReader&&) = delete;      // Move Assignment Not Implemented
};
