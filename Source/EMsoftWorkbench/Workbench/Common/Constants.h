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

#include "EbsdLib/Math/EbsdLibMath.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
namespace EMsoftWorkbenchConstants
{ 
  using EnumType = unsigned int;

  enum class InfoStringFormat : EnumType
  {
    HtmlFormat = 0,
    //      JsonFormat,
    //      TextFormat,
    //      XmlFormat,
    UnknownFormat
  };

  enum class InputType : EnumType
  {
    Binary = 0,
    TSLup1,
    TSLup2,
    TSLHDF,
    OxfordBinary,
    OxfordHDF,
    EMEBSD,
    BrukerHDF
  };

  namespace Constants
  {
    static const size_t IParSize = 80;
    static const size_t FParSize = 80;
    static const size_t SParSize = 80;
    static const size_t SParStringSize = 512;

    static const int AnorthicType = 0; // Triclinic
    static const int CyclicType = 1;
    static const int DihedralType = 2;
    static const int TetrahedralType = 3;
    static const int OctahedralType = 4;

    static const int NoAxisOrder = 0;
    static const int TwoFoldAxisOrder = 2;
    static const int ThreeFoldAxisOrder = 3;
    static const int FourFoldAxisOrder = 4;
    static const int SixFoldAxisOrder = 6;

    static const int FZtarray[32] = { AnorthicType, AnorthicType,CyclicType,CyclicType,CyclicType,
                                      DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,CyclicType,
                                      DihedralType,DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,
                                      DihedralType,DihedralType,DihedralType,CyclicType,CyclicType,CyclicType,
                                      DihedralType,DihedralType,DihedralType,DihedralType,TetrahedralType,
                                      TetrahedralType,OctahedralType,TetrahedralType,OctahedralType };

    static const int FZoarray[32] = { NoAxisOrder,NoAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,
                                      TwoFoldAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,TwoFoldAxisOrder,FourFoldAxisOrder,
                                      FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,FourFoldAxisOrder,
                                      FourFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,ThreeFoldAxisOrder,
                                      ThreeFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,SixFoldAxisOrder,
                                      SixFoldAxisOrder,SixFoldAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder,NoAxisOrder};
  }

  namespace StringConstants
  {
    const QString GBCD("GBCD");
    const QString Modules("Modules");

    const QString WindowSettings = "Window Settings";
    const QString WindowGeometry = "Window Geometry";
    const QString WindowState = "Window State";
    const QString OverlayState = "Overlay State";

    const QString Statistics("Statistics");
    const QString StatsType("StatsType");
  }

  namespace ModuleNames
  {
    const QString PatternDisplay("Pattern Display");
    const QString PatternFit("Pattern Fit");
    const QString CrystalStructureCreation("Crystal Structure Creation");
    const QString MonteCarloSimulation("Monte Carlo Simulation");
    const QString MasterPatternSimulation("Master Pattern Simulation");
  }

  namespace ImageViewerConstants
  {
    const QString ImageViewer = "Image Viewer";
    const QString ZoomFactor = "Zoom Factor";
    const QString PanningOffset = "Panning Offset";
    const QString IsPannable = "isPannable";
    const QString LastPos = "Last Position";
    const QString ViewportWidth = "Viewport Width";
    const QString ViewportHeight = "Viewport Height";
    const QString DefaultControls = "Default Controls";
  }

  namespace IOStrings
  {
    const QString CrystalSystem = "Crystal System";
    const QString SpaceGroup = "Space Group";
    const QString AtomCoordinates = "Atom Coordinates";
    const QString OutputCrystalFileName = "Output Crystal File Name";

    const QString XtalFolder("XtalFolder");

    const QString CrystalSystemSelection = "Crystal System Selection";
    const QString A = "a";
    const QString B = "b";
    const QString C = "c";
    const QString Alpha = "alpha";
    const QString Beta = "beta";
    const QString Gamma = "gamma";

    const QString SpaceGroupNumber = "Space Group Number";
    const QString SpaceGroupSetting = "Space Group Setting";

    const QString AsymmetricUnitData = "Asymmetric Unit Data";

    const QString OutputFileName = "Output File Name";
    const QString WriteToFile = "Write To File";

    const QString MonteCarlo = "Monte Carlo";
    const QString MonteCarloMode = "Monte Carlo Mode";
    const QString SampleTiltAngleSigma = "Sample Tilt Angle Sigma";
    const QString SampleRotationAngleOmega = "Sample Rotation Angle Omega";
    const QString SampleStartTiltAngle = "Sample Start Tilt Angle";
    const QString SampleEndTiltAngle = "Sample End Tilt Angle";
    const QString SampleTiltStepSize = "Sample Tilt Step Size";

    const QString AcceleratingVoltage = "Accelerating Voltage";
    const QString MinEnergyToConsider = "Minimum Energy To Consider";
    const QString EnergyBinSize = "Energy Bin Size";
    const QString MaxDepthToConsider = "Maximum Depth To Consider";
    const QString DepthStepSize = "Depth Step Size";
    const QString NumberOfXPixelsN = "Number of X Pixels N";
    const QString InteractionVolumeX = "Interaction Volume X";
    const QString InteractionVolumeY = "Interaction Volume Y";
    const QString InteractionVolumeZ = "Interaction Volume Z";
    const QString InteractionStepX = "Interaction Vol Step X";
    const QString InteractionStepY = "Interaction Vol Step Y";
    const QString InteractionStepZ = "Interaction Vol Step Z";

    const QString GPU = "GPU";
    const QString NumberOfElectronsPerWorkitem = "Number of Electrons Per Workitem";
    const QString TotalNumOfElectronsToBeConsidered = "Total Number of Electrons To Be Considered";
    const QString MultiplierForTotalNumberOfElectrons = "Multiplier For Total Number of Electrons";
    const QString GPUPlatformID = "GPU Platform ID";
    const QString GPUDeviceID = "GPU Device ID";
    const QString GlobalWorkGroupSize = "Global Work Group Size";

    const QString WriteJsonFile = "Write .JSON File";
    const QString WriteEMsoftH5DataFile = "Write EMsoft *.h5 Data File";
    const QString MonteCarloDataFileName = "Monte Carlo Data File Name";

    const QString MCDataContainerName("MonteCarloDataContainer");
    const QString CrystalData("CrystalData");
    const QString AtomData("AtomData");
    const QString Atomtypes("Atomtypes");
    const QString LatticeParameters("LatticeParameters");
    const QString Natomtypes("Natomtypes");
    const QString CreationDate("CreationDate");
    const QString CreationTime("CreationTime");
    const QString Creator("Creator");
    const QString ProgramName("ProgramName");

    const QString InputCrystalFileName = "Crystal Structure File Name";
    const QString InputMonteCarloFileName = "Monte Carlo File Name";
    const QString EnergyFileName = "Energy File Name";

    const QString CompParam = "Computational Parameters";
    const QString SmallestDSpacing = "Smallest D-Spacing To Consider";
    const QString NumOfMasterPatternPxls = "Number of Master Pattern Pixels";
    const QString BetheParameters = "Bethe Parameters";
    const QString Bethe_X = "X";
    const QString Bethe_Y = "Y";
    const QString Bethe_Z = "Z";
    const QString CPUPlatform = "CPU Platform";
    const QString NumOfOpenMPThreads = "Number of OpenMP Threads";

    const QString MasterPatternDataFileName = "Master Pattern Data File Name";

    const QString MasterPatternWidget = "Master Pattern Display Widget";
    const QString MonteCarloWidget = "Monte Carlo Display Widget";
    const QString DMParameters = "Detector and Microscope Parameters";
    const QString AngleType = "Angle Type";
    const QString AngleModeParameters = "Angle Mode Parameters";
    const QString CurrentDisplayTab = "Current Pattern Display Tab";

    const QString AngleFilePath = "Angle File Path";
    const QString PartialFile = "Partial File";
    const QString MinLineNum = "Minimum Line Number";
    const QString MaxLineNum = "Maximum Line Number";

    const QString SamplingMode = "Sampling Mode";
    const QString PointGroupNum = "Point Group Number";
    const QString MisorientationAngle = "Misorientation Angle";
    const QString ReferenceOrientation = "Reference Orientation";
    const QString OffsetSamplingGrid = "Offset Sampling Grid From Origin";
    const QString NumberOfSamplingPoints = "Number of Sampling Points Along Cube Semi-Axis";

    const QString Phi1 = "phi1";
    const QString Phi = "phi";
    const QString Phi2 = "phi2";
    const QString Start = "Start";
    const QString End = "End";
    const QString Step = "Step";
    const QString Angle = "Angle";
    const QString IsChecked = "isChecked";
    const QString DegreesChecked = "Degrees Checked";

    const QString EulerAngle = "Euler Angle";
    const QString CircularMask = "Circular Mask";

    const QString MasterPatternFilePath = "Master Pattern File Path";

    const QString EnergyBin = "Energy Bin";
    const QString ProjectionMode = "Projection Mode";

    const QString ScintillatorDistance = "Scintillator Distance";
    const QString DetectorTiltAngle = "Detector Tilt Angle";
    const QString DetectorOmegaAngle = "Detector Omega Angle";
    const QString ScintillatorPixelSize = "Scintillator Pixel Size";
    const QString NumberOfPixels = "Number of Pixels";
    const QString X = "X";
    const QString Y = "Y";
    const QString Z = "Z";
    const QString PatternCenter = "Pattern Center";
    const QString PixelCoordinates = "Pixel Coordinates";
    const QString SamplingStepSize = "Sampling Step Size";
    const QString BeamCurrent = "Beam Current";
    const QString DwellTime = "Dwell Time";
    const QString BarrelDistortion = "Barrel Distortion";
    const QString Energy = "Energy";
    const QString Minimum = "Min";
    const QString Maximum = "Max";

    const QString ExperimentalPatternFilePath = "Experimental Pattern File Path";

    const QString PatternControls = "Pattern Controls";
    const QString Opacity = "Opacity";
    const QString RotationStepSize = "Rotation Step Size";
    const QString PatternSelection = "Pattern Selection";

    const QString PatternFitViewer = "Pattern Fit Viewer";
    const QString FlickerInterval = "Flicker Interval";

    const QString PatternFit = "Pattern Fit";
    const QString NonRefinableParameters = "Non-Refinable Parameters";
    const QString RefinableDetectorParameters = "Refinable Detector Parameters";
    const QString RefinableSampleParameters = "Refinable Sample Parameters";
    const QString FitParameters = "Fit Parameters";

    const QString HipassFilter = "Hipass Filter";
    const QString HipassFilterLCF = "Hipass Filter Low Cut Off";
    const QString LinearRampSubtraction = "Linear Ramp Subtraction";
    const QString InverseGaussian = "Inverse Gaussian";

    const QString SampleOmegaAngle = "Sample Omega Angle";
    const QString PatternCenterX = "Pattern Center X";
    const QString PatternCenterY = "Pattern Center Y";

    const QString IntensityGamma = "Intensity Gamma";

    const QString Value = "Value";
    const QString MStepSize = "Manual Step Size";
    const QString FStepSize = "Fit Step Size";

    const QString FitCriterion = "Fit Criterion";
    const QString FitMode = "Fit Mode";

    const QString HDF5DatasetSelectionSettings = "HDF5 Dataset Selection Settings";
    const QString HDF5DatasetSelectionInputFile = "HDF5 Dataset Selection Input File";
    const QString HDF5DatasetSelection = "HDF5 Dataset Selection";

    const QString PatternsInputType = "Patterns Input Type";
  }
}

