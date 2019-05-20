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

#include "CrystalStructureCreationController.h"

#include <functional>
#include <math.h>

#include <QtCore/QDateTime>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

#include "EMsoftLib/EMsoftStringConstants.h"

#include "Common/EMsoftFileWriter.h"

#include "H5Support/HDF5ScopedFileSentinel.h"
#include "H5Support/QH5Utilities.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreationController::CrystalStructureCreationController(QObject* parent)
: QObject(parent)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreationController::~CrystalStructureCreationController()
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreationController::createCrystalStructureFile(CrystalStructureCreationController::CrystalStructureCreationData data)
{
  QString outputFilePath = data.outputFilePath;
  outputFilePath = QDir::toNativeSeparators(outputFilePath);

  QString tmpOutputFilePath = outputFilePath + ".tmp";
  QFileInfo tmpFi(tmpOutputFilePath);

  if(tmpFi.exists())
  {
    if(!QFile::remove(tmpOutputFilePath))
    {
      QString ss = QObject::tr("Error creating temporary output file '%1'").arg(tmpFi.fileName());
      emit errorMessageGenerated(ss);
      return;
    }
  }

  QFileInfo fi(tmpOutputFilePath);
  QString parentPath = fi.path();
  QDir dir;
  if(!dir.mkpath(parentPath))
  {
    QString ss = QObject::tr("Error creating parent path '%1'").arg(parentPath);
    emit errorMessageGenerated(ss);
    return;
  }

  QSharedPointer<EMsoftFileWriter> writer = QSharedPointer<EMsoftFileWriter>(new EMsoftFileWriter());
  connect(writer.data(), &EMsoftFileWriter::errorMessageGenerated, [=](const QString& msg) { emit errorMessageGenerated(msg); });

  // open the crystal structure HDF5 file
  if(!writer->openFile(tmpOutputFilePath))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // Create the CrystalData group
  if(!writer->openGroup(EMsoft::Constants::CrystalData))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // and fill it with datasets

  // Crystal system (process this number to make sure it falls in the EMsoft range)
  int crystalSystem = static_cast<int>(data.crystalSystem);
  int iv = crystalSystem;
  if(iv < 5)
  {
    iv += 1;
  }
  if(!writer->writeScalarDataset(EMsoft::Constants::CrystalSystem, iv))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // lattice parameters
  QVector<hsize_t> dims;
  dims.push_back(6);
  QVector<double> lp(6);
  lp[3] = 90.0;
  lp[4] = 90.0;
  lp[5] = 90.0;

  switch(crystalSystem)
  {
  case 0: // cubic
    lp[0] = data.a;
    lp[1] = lp[0];
    lp[2] = lp[0];
    break;
  case 1: // tetragonal
    lp[0] = data.a;
    lp[1] = lp[0];
    lp[2] = data.c;
    break;
  case 2: // orthorhombic
    lp[0] = data.a;
    lp[1] = data.b;
    lp[2] = data.c;
    break;
  case 3: // hexagonal
    lp[0] = data.a;
    lp[1] = lp[0];
    lp[2] = data.c;
    lp[5] = 120.0;
    break;
  case 4: // trigonalH
    lp[0] = data.a;
    lp[1] = lp[0];
    lp[2] = data.c;
    lp[5] = 120.0;
    break;
  case 5: // trigonalR
    lp[0] = data.a;
    lp[1] = lp[0];
    lp[2] = lp[0];
    lp[3] = data.alpha;
    lp[4] = lp[3];
    lp[5] = lp[3];
    break;
  case 6: // monoclinic
    lp[0] = data.a;
    lp[1] = data.b;
    lp[2] = data.c;
    lp[4] = data.beta;
    break;
  case 7: // anorthic
    lp[0] = data.a;
    lp[1] = data.b;
    lp[2] = data.c;
    lp[3] = data.alpha;
    lp[4] = data.beta;
    lp[5] = data.gamma;
    break;
    //  default : ;
  }

  if(!writer->writeVectorDataset(EMsoft::Constants::LatticeParameters, lp, dims))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // CreationDate  (use QDataTime)
  QString date = QDateTime::currentDateTime().date().toString();
  if(!writer->writeStringDataset(EMsoft::Constants::CreationDate, date))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // CreationTime
  QString time = QDateTime::currentDateTime().time().toString();
  if(!writer->writeStringDataset(EMsoft::Constants::CreationTime, time))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // Creator
  if(!writer->writeStringDataset(EMsoft::Constants::Creator, "EMsoftWorkbench"))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // ProgramName
  if(!writer->writeStringDataset(EMsoft::Constants::ProgramName, "EMsoftWorkbench - Crystal Structure Creation Module"))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // SpaceGroupNumber
  if(!writer->writeScalarDataset(EMsoft::Constants::SpaceGroupNumber, data.spaceGroupNumber))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // SpaceGroupSetting
  if(!writer->writeScalarDataset(EMsoft::Constants::SpaceGroupSetting, data.spaceGroupSetting))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // Natomtypes
  if(!writer->writeScalarDataset(EMsoft::Constants::Natomtypes, data.atomCoordinates.size()))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // AtomTypes
  std::vector<std::vector<double>> td = data.atomCoordinates;
  size_t numOfAtoms = td.size();
  QVector<hsize_t> dims2;
  dims2.push_back(numOfAtoms);
  QVector<int> atps(numOfAtoms);
  for(int i = 0; i < numOfAtoms; i++)
  {
    atps[i] = (int)td[i][0];
  }
  if(!writer->writeVectorDataset(EMsoft::Constants::Atomtypes, atps, dims2))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  // write AtomData; also perform checks to make sure the fractional coordinates are in [0,1]
  QVector<double> apos(numOfAtoms * 5);
  double tmp;
  double intpart;
  for(int i = 0; i < numOfAtoms; i++)
  {
    tmp = modf(td[i][1] + 100.0, &intpart); // make sure the coordinates are in [0,1]
    apos[i] = tmp;
    tmp = modf(td[i][2] + 100.0, &intpart); // make sure the coordinates are in [0,1]
    apos[numOfAtoms + i] = tmp;
    tmp = modf(td[i][3] + 100.0, &intpart); // make sure the coordinates are in [0,1]
    apos[2 * numOfAtoms + i] = tmp;
    apos[3 * numOfAtoms + i] = td[i][4];
    apos[4 * numOfAtoms + i] = td[i][5];
  }
  QVector<hsize_t> dims3;
  dims3.push_back(5);
  dims3.push_back(numOfAtoms);
  if(!writer->writeVectorDataset(EMsoft::Constants::AtomData, apos, dims3))
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  if(!writer->closeGroup())
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  if(!writer->closeFile())
  {
    QFile::remove(tmpOutputFilePath);
    return;
  }

  QFileInfo outFi(outputFilePath);
  if(outFi.exists())
  {
    if(!QFile::remove(outputFilePath))
    {
      QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
      emit errorMessageGenerated(ss);
      QFile::remove(tmpOutputFilePath);
      return;
    }
  }

  if(!QFile::rename(tmpOutputFilePath, outputFilePath))
  {
    QString ss = QObject::tr("Error replacing output file '%1'").arg(outFi.fileName());
    emit errorMessageGenerated(ss);
    QFile::remove(tmpOutputFilePath);
    return;
  }

  emit stdOutputMessageGenerated("Crystal Structure File Generation Complete");
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool CrystalStructureCreationController::validateCrystalStructureValues(CrystalStructureCreationController::CrystalStructureCreationData data)
{
  if(data.outputFilePath.isEmpty())
  {
    QString ss = QObject::tr("The crystal structure output file path must be set.");
    emit errorMessageGenerated(ss);
    return false;
  }

  // don't allow the user to overwrite an existing structure file
  QString outputFilePath = data.outputFilePath;
  QFileInfo fi2(outputFilePath);
  if(fi2.completeSuffix() != "xtal")
  {
    QString ss = QObject::tr("The crystal structure output file at path '%1' needs a '.xtal' suffix.").arg(outputFilePath);
    emit errorMessageGenerated(ss);
    return false;
  }

  // check the space group number bounds for each crystal system
  if((data.spaceGroupNumber < 1) || (data.spaceGroupNumber > 237))
  {
    QString ss = QObject::tr("Space group number must fall in interval [1,237]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Cubic) && ((data.spaceGroupNumber < 195) || (data.spaceGroupNumber > 230)))
  {
    QString ss = QObject::tr("Cubic space group number must fall in interval [195,230]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Tetragonal) && ((data.spaceGroupNumber < 75) || (data.spaceGroupNumber > 142)))
  {
    QString ss = QObject::tr("Tetragonal space group number must fall in interval [75,142]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Orthorhombic) && ((data.spaceGroupNumber < 16) || (data.spaceGroupNumber > 74)))
  {
    QString ss = QObject::tr("Orthorhombic space group number must fall in interval [16,74]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Hexagonal) && ((data.spaceGroupNumber < 168) || (data.spaceGroupNumber > 194)))
  {
    QString ss = QObject::tr("Hexagonal space group number must fall in interval [168,194]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::TrigonalH) && ((data.spaceGroupNumber < 143) || (data.spaceGroupNumber > 167)))
  {
    QString ss = QObject::tr("Trigonal/H space group number must fall in interval [143,167]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::TrigonalR) && ((data.spaceGroupNumber < 231) || (data.spaceGroupNumber > 237)))
  {
    QString ss = QObject::tr("Trigonal/R space group number must fall in interval [231,237]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Monoclinic) && ((data.spaceGroupNumber < 3) || (data.spaceGroupNumber > 15)))
  {
    QString ss = QObject::tr("Monoclinic space group number must fall in interval [3,15]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }
  if((data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Anorthic) && ((data.spaceGroupNumber < 1) || (data.spaceGroupNumber > 2)))
  {
    QString ss = QObject::tr("Anorthic (triclinic) space group number must fall in interval [1,2]. (see help page)");
    emit errorMessageGenerated(ss);
    return false;
  }

  // better way to do the following block of lines (to be modified)
  //  QList<int> v;
  //  v.contains(10);

  // check whether or not the space group has a second setting (there are only 24 of those);
  // we do this by taking the 24 space group numbers, subtracting the selected space group number, and
  // taking the absolute value; if the minimum element of this vector equals zero, then the space group does
  // have a second setting.
  {
    std::vector<int> v = {48, 50, 59, 68, 70, 85, 86, 88, 125, 126, 129, 130, 133, 134, 137, 138, 141, 142, 201, 203, 222, 224, 227, 228};
    std::transform(v.begin(), v.end(), v.begin(), std::bind2nd(std::plus<int>(), -data.spaceGroupNumber));
    for(unsigned int i = 0; i < v.size(); i++)
    {
      if(v[i] < 0)
        v[i] *= -1;
    }
    double minp = *std::min_element(v.begin(), v.end());
    if((minp > 0) && (data.spaceGroupSetting == 2))
    { // this is the second setting
      QString ss = QObject::tr("This space group does not have a second setting.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  // make sure all lattice parameters are strictly positive; for angles, they must be strictly positive and strictly less than 180°
  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Cubic)
  {
    if(data.a <= 0)
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Tetragonal)
  {
    if((data.a <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Orthorhombic)
  {
    if((data.a <= 0) || (data.b <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Hexagonal)
  {
    if((data.a <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::TrigonalH)
  {
    if((data.a <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::TrigonalR)
  {
    if(data.a <= 0)
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
    if((data.alpha <= 0) || (data.alpha >= 180))
    {
      QString ss = QObject::tr("Angle must be in interval ]0,180[.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Monoclinic)
  {
    if((data.a <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
    if((data.beta <= 0) || (data.beta >= 180))
    {
      QString ss = QObject::tr("Angle must be in interval ]0,180[.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  if(data.crystalSystem == CrystalStructureCreationController::CrystalSystem::Anorthic)
  {
    if((data.a <= 0) || (data.b <= 0) || (data.c <= 0))
    {
      QString ss = QObject::tr("Lattice parameter must be positive.");
      emit errorMessageGenerated(ss);
      return false;
    }
    if((data.alpha <= 0) || (data.alpha >= 180) || (data.beta <= 0) || (data.beta >= 180) || (data.gamma <= 0) || (data.gamma >= 180))
    {
      QString ss = QObject::tr("Angle must be in interval ]0,180[.");
      emit errorMessageGenerated(ss);
      return false;
    }
  }

  { // all the following use the table data
    // get the table in a form that can be parsed
    int iv = data.atomCoordinates.size();
    std::vector<std::vector<double>> td = data.atomCoordinates;

    // make sure the site occupation parameters are between 0 and 1
    std::vector<double> row(iv);
    if(0 != iv)
    {
      for(int i = 0; i < iv; i++)
      {
        row[i] = td[i][4];
      }
      double minp = *std::min_element(row.begin(), row.end());
      double maxp = *std::max_element(row.begin(), row.end());
      if((minp <= 0.0) || (maxp > 1.0))
      {
        QString ss = QObject::tr("Site occupation parameter must be in interval ]0,1].");
        emit errorMessageGenerated(ss);
        return false;
      }
    }

    // make sure that Debye-Waller factors are all strictly larger than 0
    if(0 != iv)
    {
      for(int i = 0; i < iv; i++)
      {
        row[i] = td[i][5];
      }
      double minp = *std::min_element(row.begin(), row.end());
      if(minp <= 0.0)
      {
        QString ss = QObject::tr("Debye-Waller factors must be strictly positive.");
        emit errorMessageGenerated(ss);
        return false;
      }
    }

    // make sure that the atom types are between 1 and 92
    if(0 != iv)
    {
      for(int i = 0; i < iv; i++)
      {
        row[i] = td[i][0];
      }
      double minp = *std::min_element(row.begin(), row.end());
      double maxp = *std::max_element(row.begin(), row.end());
      if((minp < 1) || (maxp > 92))
      {
        QString ss = QObject::tr("Atomic number must be in range [1,92].");
        emit errorMessageGenerated(ss);
        return false;
      }
    }
  }

  return true;
}
