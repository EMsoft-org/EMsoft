

#include "EbsdLoader.h"

#include "EbsdLib/Core/EbsdDataArray.hpp"
#include "EbsdLib/Core/EbsdLibConstants.h"
#include "EbsdLib/IO/TSL/AngReader.h"
#include "EbsdLib/LaueOps/LaueOps.h"
#include "EbsdLib/Math/EbsdMatrixMath.h"
#include "EbsdLib/Utilities/ColorTable.h"

#include <QtGui/QImage>

// -----------------------------------------------------------------------------
EbsdLoader::EbsdLoader() = default;

// -----------------------------------------------------------------------------
EbsdLoader::~EbsdLoader() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
EbsdDataArray<uint32_t>::Pointer loadCrystalStructures(AngReader* reader)
{
  std::vector<AngPhase::Pointer> phases = reader->getPhaseVector();

  // Initialize the zero'th element to unknowns. The other elements will
  // be filled in based on values from the data file
  EbsdDataArray<uint32_t>::Pointer crystalStructures = EbsdDataArray<uint32_t>::CreateArray(phases.size() + 1, EbsdLib::AngFile::CrystalStructures, true);
  crystalStructures->setValue(0, EbsdLib::CrystalStructure::UnknownCrystalStructure);

  if(phases.empty())
  {
    return crystalStructures;
  }

  for(const AngPhase::Pointer& phase : phases)
  {
    int32_t phaseID = phase->getPhaseIndex();
    crystalStructures->setValue(static_cast<size_t>(phaseID), phase->determineLaueGroup());
  }

  return crystalStructures;
}

// -----------------------------------------------------------------------------
std::tuple<QImage, int32_t> EbsdLoader::CreateIPFColorMap(const std::string& filepath, std::array<float, 3>& refDirection)
{

  AngReader reader;
  reader.setFileName(filepath);
  int32_t err = reader.readHeaderOnly();
  if(err < 0)
  {
    std::cout << "Error reading the header from input Ang file" << std::endl;
    return {QImage(), -1};
  }

  int32_t xDim = reader.getXDimension();
  int32_t yDim = reader.getYDimension();
  // size_t totalPoints = static_cast<size_t>(xDim * yDim);

  std::cout << "X Dim: " << xDim << std::endl;
  std::cout << "Y Dim: " << yDim << std::endl;

  // float xStep = reader.getXStep();
  // float yStep = reader.getYStep();

  err = reader.readFile();
  if(err < 0)
  {
    std::cout << "Error reading the data from input Ang file" << std::endl;
    return {QImage(), -2};
  }

  float* phi1 = reader.getPhi1Pointer();
  float* phi = reader.getPhiPointer();
  float* phi2 = reader.getPhi2Pointer();

  int32_t* phases = reader.getPhaseDataPointer();

  std::vector<AngPhase::Pointer> ensembles(1);
  // Add a dummy Ang Phase to the front of the vector
  ensembles[0] = AngPhase::New();
  {
    std::vector<AngPhase::Pointer> ens = reader.getPhaseVector();
    for(const auto& e : ens)
    {
      ensembles.push_back(e);
    }
  }

  std::array<float, 3> normRefDir = refDirection; // Make a copy of the reference Direction

  EbsdMatrixMath::Normalize3x1(normRefDir[0], normRefDir[1], normRefDir[2]);

  /* ******** Begin the generation of the IPFColors *************/

  EbsdDataArray<uint32_t>::Pointer crystalStructuresPtr = loadCrystalStructures(&reader);
  EbsdDataArray<uint32_t>& crystalStructures = *crystalStructuresPtr;

  std::vector<LaueOps::Pointer> ops = LaueOps::GetAllOrientationOps();
  std::array<double, 3> refDir = {normRefDir[0], normRefDir[1], normRefDir[2]};
  std::array<double, 3> dEuler = {0.0, 0.0, 0.0};
  EbsdLib::Rgb argb = 0x00000000;
  int32_t phase = 0;
  bool calcIPF = false;

  QImage ipfImage(xDim, yDim, QImage::Format_RGBA8888);
  for(int32_t y = 0; y < yDim; y++)
  {
    for(int32_t x = 0; x < xDim; x++)
    {
      size_t idx = static_cast<size_t>((y * xDim) + x);
      phase = phases[idx];

      ipfImage.setPixelColor(x, y, QRgb(0x00000000));

      dEuler[0] = static_cast<double>(phi1[idx]);
      dEuler[1] = static_cast<double>(phi[idx]);
      dEuler[2] = static_cast<double>(phi2[idx]);

      // Make sure we are using a valid Euler Angles with valid crystal symmetry
      calcIPF = true;

      // Sanity check the phase data to make sure we do not walk off the end of the array
      if(phase >= ensembles.size())
      {
        std::cout << "Scan Point (" << x << ", " << y << ") had a phase value of " << phase << " which is greater then " << ensembles.size() << std::endl;
      }

      if(phase < ensembles.size() && calcIPF && crystalStructures[phase] < EbsdLib::CrystalStructure::LaueGroupEnd)
      {
        argb = ops[crystalStructures[phase]]->generateIPFColor(dEuler.data(), refDir.data(), false);
        ipfImage.setPixelColor(x, y, argb);
      }
    }
  }

  return {ipfImage, 0};
}
