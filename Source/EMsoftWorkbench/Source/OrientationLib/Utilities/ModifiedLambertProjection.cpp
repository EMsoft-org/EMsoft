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


#include "ModifiedLambertProjection.h"

#include <cmath>

#include <QtCore/QSet>

#include "SIMPLib/Math/MatrixMath.h"

#define WRITE_LAMBERT_SQUARE_COORD_VTK 0

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModifiedLambertProjection::ModifiedLambertProjection() :
  m_Dimension(0),
  m_StepSize(0.0f),
  m_SphereRadius(1.0f),
  m_MaxCoord(0.0),
  m_MinCoord(0.0)
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModifiedLambertProjection::~ModifiedLambertProjection() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModifiedLambertProjection::Pointer ModifiedLambertProjection::NullPointer()
{
  return Pointer(static_cast<Self*>(nullptr));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModifiedLambertProjection::Pointer ModifiedLambertProjection::New()
{
  Pointer sharedPtr (new Self);
  return sharedPtr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ModifiedLambertProjection::Pointer ModifiedLambertProjection::CreateProjectionFromXYZCoords(const std::vector<float> &coords, int dimension, float sphereRadius)
{

  bool nhCheck = false;
  float sqCoord[2];
  //int sqIndex = 0;
  ModifiedLambertProjection::Pointer squareProj = ModifiedLambertProjection::New();
  squareProj->initializeSquares(dimension, sphereRadius);


#if WRITE_LAMBERT_SQUARE_COORD_VTK
  QString ss;
  QString filename("/tmp/");
  filename.append("ModifiedLambert_Square_Coords_").append(coords->getName()).append(".vtk");
  FILE* f = nullptr;
  f = fopen(filename.toLatin1().data(), "wb");
  if(nullptr == f)
  {
    ss.str("");
    QString ss = QObject::tr("Could not open vtk viz file %1 for writing. Please check access permissions and the path to the output location exists").arg(filename);
    return squareProj;
  }

  // Write the correct header
  fprintf(f, "# vtk DataFile Version 2.0\n");
  fprintf(f, "data set from DREAM3D\n");
  fprintf(f, "ASCII");
  fprintf(f, "\n");

  fprintf(f, "DATASET UNSTRUCTURED_GRID\nPOINTS %lu float\n", coords->getNumberOfTuples() );
#endif

  for(std::vector<float>::const_iterator it = coords.begin(); it != coords.end(); it + 3)
  {
    sqCoord[0] = 0.0;
    sqCoord[1] = 0.0;
    //get coordinates in square projection of crystal normal parallel to boundary normal
    nhCheck = squareProj->getSquareCoord(&(*it), sqCoord);
#if WRITE_LAMBERT_SQUARE_COORD_VTK
    fprintf(f, "%f %f 0\n", sqCoord[0], sqCoord[1]);
#endif

    // Based on the XY coordinate, get the pointer index that the value corresponds to in the proper square
//    sqIndex = squareProj->getSquareIndex(sqCoord);
    if (nhCheck)
    {
      //north increment by 1
//      squareProj->addValue(ModifiedLambertProjection::Square::NorthSquare, sqIndex, 1.0);
      squareProj->addInterpolatedValues(ModifiedLambertProjection::Square::NorthSquare, sqCoord, 1.0);
    }
    else
    {
      // south increment by 1
//      squareProj->addValue(ModifiedLambertProjection::SouthSquare, sqIndex, 1.0);
      squareProj->addInterpolatedValues(ModifiedLambertProjection::Square::SouthSquare, sqCoord, 1.0);
    }
  }
#if WRITE_LAMBERT_SQUARE_COORD_VTK
  fclose(f);
#endif

  return squareProj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::initializeSquares(int dims, float sphereRadius)
{
  m_Dimension = dims;
  m_SphereRadius = sphereRadius;
  // We want half the sphere area for each square because each square represents a hemisphere.
  float halfSphereArea = 4 * M_PI * sphereRadius * sphereRadius / 2.0;
  // The length of a side of the square is the square root of the area
  float squareEdge = sqrt(halfSphereArea);

  m_StepSize = squareEdge / static_cast<float>(m_Dimension);

  m_MaxCoord = squareEdge / 2.0;
  m_MinCoord = -squareEdge / 2.0;
  m_HalfDimension = static_cast<float>(m_Dimension) / 2.0;
  m_HalfDimensionTimesStepSize = m_HalfDimension * m_StepSize;

  m_NorthSquare.resize(m_Dimension*m_Dimension);
  std::fill(m_NorthSquare.begin(), m_NorthSquare.end(), 0.0);

  m_SouthSquare.resize(m_Dimension*m_Dimension);
  std::fill(m_SouthSquare.begin(), m_SouthSquare.end(), 0.0);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int ModifiedLambertProjection::writeHDF5Data(hid_t groupId)
{
  int err = 0;
  return err;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int ModifiedLambertProjection::readHDF5Data(hid_t groupId)
{
  int err = 0;
  return err;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::addInterpolatedValues(Square square, const float* sqCoord, double value)
{
  int abin1 = 0, bbin1 = 0;
  int abin2 = 0, bbin2 = 0;
  int abin3 = 0, bbin3 = 0;
  int abin4 = 0, bbin4 = 0;
  int abinSign, bbinSign;
  float modX = (sqCoord[0] + m_HalfDimensionTimesStepSize ) / m_StepSize;
  float modY = (sqCoord[1] + m_HalfDimensionTimesStepSize ) / m_StepSize;
  int abin = (int) modX;
  int bbin = (int) modY;
  modX -= abin;
  modY -= bbin;
  modX -= 0.5;
  modY -= 0.5;
  if(modX == 0.0)
  {
    abinSign = 1;
  }
  else
  {
    abinSign = modX / fabs(modX);
  }
  if(modY == 0.0)
  {
    bbinSign = 1;
  }
  else
  {
    bbinSign = modY / fabs(modY);
  }
  abin1 = abin;
  bbin1 = bbin;
  abin2 = abin + abinSign;
  bbin2 = bbin;
  if(abin2 < 0 || abin2 > m_Dimension - 1)
  {
    abin2 = abin2 - (abinSign * m_Dimension), bbin2 = m_Dimension - bbin2 - 1;
  }
  abin3 = abin;
  bbin3 = bbin + bbinSign;
  if(bbin3 < 0 || bbin3 > m_Dimension - 1)
  {
    abin3 = m_Dimension - abin3 - 1, bbin3 = bbin3 - (bbinSign * m_Dimension);
  }
  abin4 = abin + abinSign;
  bbin4 = bbin + bbinSign;
  if((abin4 < 0 || abin4 > m_Dimension - 1) && (bbin4 >= 0 && bbin4 <= m_Dimension - 1))
  {
    abin4 = abin4 - (abinSign * m_Dimension), bbin4 = m_Dimension - bbin4 - 1;
  }
  else if((abin4 >= 0 && abin4 <= m_Dimension - 1) && (bbin4 < 0 || bbin4 > m_Dimension - 1))
  {
    abin4 = m_Dimension - abin4 - 1, bbin4 = bbin4 - (bbinSign * m_Dimension);
  }
  else if((abin4 < 0 || abin4 > m_Dimension - 1) && (bbin4 < 0 || bbin4 > m_Dimension - 1))
  {
    abin4 = abin4 - (abinSign * m_Dimension), bbin4 = bbin4 - (bbinSign * m_Dimension);
  }
  modX = fabs(modX);
  modY = fabs(modY);

  int index1 = bbin1 * m_Dimension + abin1;
  int index2 = bbin2 * m_Dimension + abin2;
  int index3 = bbin3 * m_Dimension + abin3;
  int index4 = bbin4 * m_Dimension + abin4;
  if (square == ModifiedLambertProjection::Square::NorthSquare)
  {
    double v1 = m_NorthSquare.at(index1) + value * (1.0 - modX) * (1.0 - modY);
    double v2 = m_NorthSquare.at(index2) + value * (modX) * (1.0 - modY);
    double v3 = m_NorthSquare.at(index3) + value * (1.0 - modX) * (modY);
    double v4 = m_NorthSquare.at(index4) + value * (modX) * (modY);
    m_NorthSquare.at(index1) = v1;
    m_NorthSquare.at(index2) = v2;
    m_NorthSquare.at(index3) = v3;
    m_NorthSquare.at(index4) = v4;
  }
  else
  {
    double v1 = m_SouthSquare.at(index1) + value * (1.0 - modX) * (1.0 - modY);
    double v2 = m_SouthSquare.at(index2) + value * (modX) * (1.0 - modY);
    double v3 = m_SouthSquare.at(index3) + value * (1.0 - modX) * (modY);
    double v4 = m_SouthSquare.at(index4) + value * (modX) * (modY);
    m_SouthSquare.at(index1) = v1;
    m_SouthSquare.at(index2) = v2;
    m_SouthSquare.at(index3) = v3;
    m_SouthSquare.at(index4) = v4;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::addValue(Square square, int index, double value)
{
  if (square == ModifiedLambertProjection::Square::NorthSquare)
  {
    double v = m_NorthSquare.at(index) + value;
    m_NorthSquare.at(index) = v;
  }
  else
  {
    double v = m_SouthSquare.at(index) + value;
    m_SouthSquare.at(index) = v;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::setValue(Square square, int index, double value)
{
  if (square == ModifiedLambertProjection::Square::NorthSquare)
  {
    m_NorthSquare.at(index) = value;
  }
  else
  {
    m_SouthSquare.at(index) = value;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
double ModifiedLambertProjection::getValue(Square square, int index)
{
  if (square == ModifiedLambertProjection::Square::NorthSquare)
  {
    return m_NorthSquare.at(index);
  }

  return m_SouthSquare.at(index);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
double ModifiedLambertProjection::getInterpolatedValue(Square square, const float* sqCoord)
{
// float sqCoord[2] = { sqCoord0[0] - 0.5*m_StepSize, sqCoord0[1] - 0.5*m_StepSize};
  int abin1, bbin1;
  int abin2, bbin2;
  int abin3, bbin3;
  int abin4, bbin4;
  int abinSign, bbinSign;
  float modX = (sqCoord[0] + m_HalfDimensionTimesStepSize ) / m_StepSize;
  float modY = (sqCoord[1] + m_HalfDimensionTimesStepSize ) / m_StepSize;
  int abin = (int) modX;
  int bbin = (int) modY;
  modX -= abin;
  modY -= bbin;
  modX -= 0.5;
  modY -= 0.5;
  if(modX == 0.0)
  {
    abinSign = 1;
  }
  else
  {
    abinSign = modX / fabs(modX);
  }
  if(modY == 0.0)
  {
    bbinSign = 1;
  }
  else
  {
    bbinSign = modY / fabs(modY);
  }
  abin1 = abin;
  bbin1 = bbin;
  abin2 = abin + abinSign;
  bbin2 = bbin;
  if(abin2 < 0 || abin2 > m_Dimension - 1)
  {
    abin2 = abin2 - (abinSign * m_Dimension), bbin2 = m_Dimension - bbin2 - 1;
  }
  abin3 = abin;
  bbin3 = bbin + bbinSign;
  if(bbin3 < 0 || bbin3 > m_Dimension - 1)
  {
    abin3 = m_Dimension - abin3 - 1, bbin3 = bbin3 - (bbinSign * m_Dimension);
  }
  abin4 = abin + abinSign;
  bbin4 = bbin + bbinSign;
  if((abin4 < 0 || abin4 > m_Dimension - 1) && (bbin4 >= 0 && bbin4 <= m_Dimension - 1))
  {
    abin4 = abin4 - (abinSign * m_Dimension), bbin4 = m_Dimension - bbin4 - 1;
  }
  else if((abin4 >= 0 && abin4 <= m_Dimension - 1) && (bbin4 < 0 || bbin4 > m_Dimension - 1))
  {
    abin4 = m_Dimension - abin4 - 1, bbin4 = bbin4 - (bbinSign * m_Dimension);
  }
  else if((abin4 < 0 || abin4 > m_Dimension - 1) && (bbin4 < 0 || bbin4 > m_Dimension - 1))
  {
    abin4 = abin4 - (abinSign * m_Dimension), bbin4 = bbin4 - (bbinSign * m_Dimension);
  }
  modX = fabs(modX);
  modY = fabs(modY);
  if (square == ModifiedLambertProjection::Square::NorthSquare)
  {
    float intensity1 = m_NorthSquare.at((abin1) + (bbin1 * m_Dimension));
    float intensity2 = m_NorthSquare.at((abin2) + (bbin2 * m_Dimension));
    float intensity3 = m_NorthSquare.at((abin3) + (bbin3 * m_Dimension));
    float intensity4 = m_NorthSquare.at((abin4) + (bbin4 * m_Dimension));
    float interpolatedIntensity = ((intensity1 * (1 - modX) * (1 - modY)) + (intensity2 * (modX) * (1 - modY)) + (intensity3 * (1 - modX) * (modY)) + (intensity4 * (modX) * (modY)));
    return interpolatedIntensity;
  }

  float intensity1 = m_SouthSquare.at((abin1) + (bbin1 * m_Dimension));
  float intensity2 = m_SouthSquare.at((abin2) + (bbin2 * m_Dimension));
  float intensity3 = m_SouthSquare.at((abin3) + (bbin3 * m_Dimension));
  float intensity4 = m_SouthSquare.at((abin4) + (bbin4 * m_Dimension));
  float interpolatedIntensity = ((intensity1 * (1 - modX) * (1 - modY)) + (intensity2 * (modX) * (1 - modY)) + (intensity3 * (1 - modX) * (modY)) + (intensity4 * (modX) * (modY)));
  return interpolatedIntensity;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool ModifiedLambertProjection::getSquareCoord(const float* xyz, float* sqCoord)
{
  bool nhCheck = false;
  float adjust = 1.0;
  float xCoord = xyz[0];
  float yCoord = xyz[1];
  float zCoord = xyz[2];

  if(zCoord >= 0.0)
  {
    adjust = -1.0;
    nhCheck = true;
  }
  if(xCoord == 0 && yCoord == 0)
  {
    sqCoord[0] = 0.0;
    sqCoord[1] = 0.0;
    return nhCheck;
  }
  if(fabs(xCoord) >= fabs(yCoord))
  {
    sqCoord[0] = (xCoord / fabs(xCoord) ) * sqrt(2.0 * m_SphereRadius * (m_SphereRadius + (zCoord * adjust) ) ) * SIMPLib::Constants::k_HalfOfSqrtPi;
    sqCoord[1] = (xCoord / fabs(xCoord) ) * sqrt(2.0 * m_SphereRadius * (m_SphereRadius + (zCoord * adjust) ) ) * ((SIMPLib::Constants::k_2OverSqrtPi) * atan(yCoord / xCoord));
  }
  else
  {
    sqCoord[0] = (yCoord / fabs(yCoord)) * sqrt(2.0 * m_SphereRadius * (m_SphereRadius + (zCoord * adjust))) * ((SIMPLib::Constants::k_2OverSqrtPi) * atan(xCoord / yCoord));
    sqCoord[1] = (yCoord / fabs(yCoord)) * sqrt(2.0 * m_SphereRadius * (m_SphereRadius + (zCoord * adjust))) * (SIMPLib::Constants::k_HalfOfSqrtPi);
  }

  if (sqCoord[0] >= m_MaxCoord)
  {
    sqCoord[0] = (m_MaxCoord) - .0001;
  }
  if (sqCoord[1] >= m_MaxCoord)
  {
    sqCoord[1] = (m_MaxCoord) - .0001;
  }
  return nhCheck;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int ModifiedLambertProjection::getSquareIndex(const float* sqCoord)
{
  int x = (int)( (sqCoord[0] + m_MaxCoord) / m_StepSize);
  if (x >= m_Dimension)
  {
    x = m_Dimension - 1;
  }
  if (x < 0)
  {
    x = 0;
  }
  int y = (int)( (sqCoord[1] + m_MaxCoord) / m_StepSize);
  if (y >= m_Dimension)
  {
    y = m_Dimension - 1;
  }
  if (y < 0)
  {
    y = 0;
  }
  int index = y * m_Dimension + x;
  Q_ASSERT(index < m_Dimension * m_Dimension);
  return index;
}


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::normalizeSquares()
{

  size_t npoints = m_NorthSquare.size();
  double nTotal = 0;
  double sTotal = 0;

  double* north = m_NorthSquare.data();
  double* south = m_SouthSquare.data();

  // Get the Sum of all the bins
  for(size_t i = 0; i < npoints; ++i)
  {
    nTotal = nTotal + north[i];
    sTotal = sTotal + south[i];
  }
  double oneOverNTotal = 1.0 / nTotal;
  double oneOverSTotal = 1.0 / sTotal;

  // Divide each bin by the total of all the bins for that Hemisphere
  for(size_t i = 0; i < npoints; ++i)
  {
    north[i] = (north[i] * oneOverNTotal);
    south[i] = (south[i] * oneOverSTotal);
  }

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::normalizeSquaresToMRD()
{
  // First Normalize the squares
  normalizeSquares();
  size_t npoints = m_NorthSquare.size();
  double* north = m_NorthSquare.data();
  double* south = m_SouthSquare.data();
  int dimSqrd = m_Dimension * m_Dimension;

  // Multiply Each Bin by the total number of bins
  for(size_t i = 0; i < npoints; ++i)
  {
    north[i] = north[i] * dimSqrd;
    south[i] = south[i] * dimSqrd;
  }

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ModifiedLambertProjection::createProjection(int dim, std::vector<float> &stereoIntensity, ModifiedLambertProjection::ProjectionType projType)
{
  int xpoints = dim;
  int ypoints = dim;

  int xpointshalf = xpoints / 2;
  int ypointshalf = ypoints / 2;

  float span;
  float unitRadius = 1.0;
  if (projType == ModifiedLambertProjection::ProjectionType::Stereographic)
  {
  }
  else if(projType == ModifiedLambertProjection::ProjectionType::Circular)
  {
    unitRadius = std::sqrt(2.0f);
  }
  else
  {
    return;
  }

  span = unitRadius - (-unitRadius);


  float xres = span / static_cast<float>(xpoints);
  float yres = span / static_cast<float>(ypoints);
  float xtmp, ytmp;
  float sqCoord[2];
  float xyz[3];
  bool nhCheck = false;

  std::fill(stereoIntensity.begin(), stereoIntensity.end(), 0.0f);
  float* intensity = stereoIntensity.data();
// int sqIndex = 0;

  for (int64_t y = 0; y < ypoints; y++)
  {
    for (int64_t x = 0; x < xpoints; x++)
    {
      //get (x,y) for stereographic projection pixel
      xtmp = static_cast<float>(x - xpointshalf) * xres + (xres * 0.5f);
      ytmp = static_cast<float>(y - ypointshalf) * yres + (yres * 0.5f);
      size_t index = static_cast<size_t>(y * xpoints + x);
      if((xtmp * xtmp + ytmp * ytmp) <= unitRadius * unitRadius)
      {
        //project xy from stereo projection to the unit sphere
        if (projType == ModifiedLambertProjection::ProjectionType::Stereographic)
        {
          xyz[2] = -((xtmp * xtmp + ytmp * ytmp) - 1) / ((xtmp * xtmp + ytmp * ytmp) + 1);
          xyz[0] = xtmp * (1 + xyz[2]);
          xyz[1] = ytmp * (1 + xyz[2]);
        }
        else if (projType == ModifiedLambertProjection::ProjectionType::Circular)
        {
          float q = xtmp*xtmp + ytmp*ytmp;
          float t = std::sqrt(1.0f - (q / 4.0f));

          xyz[0] = xtmp * t;
          xyz[1] = ytmp * t;
          xyz[2] = (q / 2.0f) - 1.0f;
        }
        else
        {
          // Not a valid projection type
          return;
        }


        for( int64_t m = 0; m < 2; m++)
        {
          if(m == 1)
          {
            xyz[0] *= -1.0;
            xyz[1] *= -1.0;
            xyz[2] *= -1.0;
          }
          nhCheck = getSquareCoord(xyz, sqCoord);
          //sqIndex = getSquareIndex(sqCoord);
          if (nhCheck)
          {
            //get Value from North square
            intensity[index] += getInterpolatedValue(ModifiedLambertProjection::Square::NorthSquare, sqCoord);
            //intensity[index] += getValue(ModifiedLambertProjection::Square::NorthSquare, sqIndex);
          }
          else
          {
            //get Value from South square
            intensity[index] += getInterpolatedValue(ModifiedLambertProjection::Square::SouthSquare, sqCoord);
            //intensity[index] += getValue(ModifiedLambertProjection::SouthSquare, sqIndex);
          }
        }
        intensity[index]  = intensity[index] * 0.5f;
      }
    }
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
typename std::vector<float> ModifiedLambertProjection::createProjection(int dim, ModifiedLambertProjection::ProjectionType projType)
{
  QString arrayName;
  if (projType == ModifiedLambertProjection::ProjectionType::Stereographic)
  {
    arrayName = "ModifiedLambertProjection_StereographicProjection";
  }
  else if (projType == ModifiedLambertProjection::ProjectionType::Circular)
  {
    arrayName = "ModifiedLambertProjection_CircularProjection";
  }
  else
  {
    // Not a valid projection type
    return std::vector<float>();
  }

  typename std::vector<float> intensity(dim*dim, 0.0);
  createProjection(dim, intensity, projType);
  return intensity;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int ModifiedLambertProjection::getDimension() const
{
  return m_Dimension;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
float ModifiedLambertProjection::getStepSize() const
{
  return m_StepSize;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
float ModifiedLambertProjection::getSphereRadius() const
{
  return m_SphereRadius;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<double> ModifiedLambertProjection::getNorthSquare() const
{
  return m_NorthSquare;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<double> ModifiedLambertProjection::getSouthSquare() const
{
  return m_SouthSquare;
}
