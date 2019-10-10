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

#include "CrystalStructureCreation_UI.h"

#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonDocument>

#include <QtWidgets/QFileDialog>

#include "EMsoftLib/EMsoftStringConstants.h"
#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "EMsoftApplication.h"

#include "Common/Constants.h"
#include "Common/FileIOTools.h"
#include "Common/PatternTools.h"

#include "QtSupport/QtSSettings.h"

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreation_UI::CrystalStructureCreation_UI(QWidget* parent)
: IModuleUI(parent)
{
  setupUi(this);

  m_Controller = new CrystalStructureCreationController(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreation_UI::~CrystalStructureCreation_UI() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::setupGui()
{
  // Create and set the validators on all the line edits
  createValidators();

  // Create all signal/slot connections between this widget and its sub-widgets.
  createWidgetConnections();

  // Create all signal/slot connections that will update the simulated pattern when parameters are changed
  createModificationConnections();

  bLabel->hide();
  bLE->hide();
  cLabel->hide();
  cLE->hide();
  alphaLabel->hide();
  alphaLE->hide();
  betaLabel->hide();
  betaLE->hide();
  gammaLabel->hide();
  gammaLE->hide();
  spaceGrpNumberSB->setRange(195, 230);

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::createValidators() const
{
  QDoubleValidator* doubleValidator = new QDoubleValidator(aLE);
  aLE->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(bLE);
  bLE->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(cLE);
  cLE->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(alphaLE);
  alphaLE->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(betaLE);
  betaLE->setValidator(doubleValidator);

  doubleValidator = new QDoubleValidator(gammaLE);
  gammaLE->setValidator(doubleValidator);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::createModificationConnections()
{
  connect(asymmetricUnitTable, &AsymmetricUnitTableWidget::parametersChanged, [=] { parametersChanged(); });

  // Spin Boxes
  connect(aLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(bLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(cLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(alphaLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(betaLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(gammaLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
  connect(spaceGrpNumberSB, static_cast<void (QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] { parametersChanged(); });

  // Combo Boxes
  connect(csCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });
  connect(spaceGrpSettingCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { parametersChanged(); });

  // Line Edits
  connect(csFilePathLE, &QLineEdit::textChanged, [=] { parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::createWidgetConnections()
{
  connect(createCrystalStructureBtn, &QPushButton::clicked, this, [=] {
    setRunning(true);
    // Sanity Check the input/output Files
    QString absPath = FileIOTools::GetAbsolutePath(csFilePathLE->text());
    csFilePathLE->setText(absPath);

    CrystalStructureCreationController::CrystalStructureCreationData data = getCreationData();
    m_Controller->createCrystalStructureFile(data);
    emit validationOfOtherModulesNeeded(this);
    setRunning(false);
  });

  // Pass errors, warnings, and std output messages up to the user interface
  connect(m_Controller, &CrystalStructureCreationController::errorMessageGenerated, this, &CrystalStructureCreation_UI::notifyErrorMessage);
  connect(m_Controller, &CrystalStructureCreationController::warningMessageGenerated, this, &CrystalStructureCreation_UI::notifyWarningMessage);
  connect(m_Controller, SIGNAL(stdOutputMessageGenerated(QString)), this, SLOT(appendToStdOut(QString)));

  connect(selectOutputFileBtn, &QPushButton::clicked, [=] {
    QString proposedFile = emSoftApp->getOpenDialogLastDirectory() + QDir::separator() + "Untitled.xtal";
    QString filePath = FileIOTools::GetSavePathFromDialog("Select Output File", "Crystal Structure File (*.xtal);;All Files (*.*)", proposedFile);
    if(filePath.isEmpty())
    {
      return;
    }

    filePath = QDir::toNativeSeparators(filePath);
    csFilePathLE->setText(filePath);
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::parametersChanged()
{
  validateData();
  emit moduleParametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::validateData()
{
  clearModuleIssues();

  CrystalStructureCreationController::CrystalStructureCreationData data = getCreationData();
  if(m_Controller->validateCrystalStructureValues(data))
  {
    createCrystalStructureBtn->setEnabled(true);
  }
  else
  {
    createCrystalStructureBtn->setDisabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::changeEvent(QEvent* event)
{
  if(event->type() == QEvent::ActivationChange)
  {
    emit moduleChangedState(this);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::readModuleSession(QJsonObject& obj)
{
  QJsonObject csObj = obj[ioConstants::CrystalSystem].toObject();
  QJsonObject spaceGrpObj = obj[ioConstants::SpaceGroup].toObject();
  QJsonObject atomCoordsParamObj = obj[ioConstants::AtomCoordinates].toObject();

  if(!csObj.isEmpty())
  {
    readCrystalSystemParameters(obj);
  }

  if(!spaceGrpObj.isEmpty())
  {
    readSpaceGroupParameters(obj);
  }

  csFilePathLE->setText(obj[ioConstants::OutputCrystalFileName].toString());

  asymmetricUnitTable->readSession(atomCoordsParamObj);

  validateData();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::readCrystalSystemParameters(QJsonObject& obj)
{
  QJsonObject crystalSystemParamObj = obj[ioConstants::CrystalSystem].toObject();

  if(!crystalSystemParamObj.isEmpty())
  {
    csCB->setCurrentIndex(crystalSystemParamObj[ioConstants::CrystalSystemSelection].toInt());
    aLE->setText(crystalSystemParamObj[ioConstants::A].toString());
    bLE->setText(crystalSystemParamObj[ioConstants::B].toString());
    cLE->setText(crystalSystemParamObj[ioConstants::C].toString());
    alphaLE->setText(crystalSystemParamObj[ioConstants::Alpha].toString());
    betaLE->setText(crystalSystemParamObj[ioConstants::Beta].toString());
    gammaLE->setText(crystalSystemParamObj[ioConstants::Gamma].toString());
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::readSpaceGroupParameters(QJsonObject& obj)
{
  QJsonObject spaceGroupParamObj = obj[ioConstants::SpaceGroup].toObject();

  if(!spaceGroupParamObj.isEmpty())
  {
    spaceGrpNumberSB->setValue(spaceGroupParamObj[ioConstants::SpaceGroupNumber].toInt());
    spaceGrpSettingCB->setCurrentIndex(spaceGroupParamObj[ioConstants::SpaceGroupSetting].toInt());
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::writeModuleSession(QJsonObject& obj) const
{
  QJsonObject crystalSystemParamObj;
  QJsonObject spaceGroupParamObj;
  QJsonObject atomCoordsParamObj;

  writeCrystalSystemParameters(crystalSystemParamObj);
  writeSpaceGroupParameters(spaceGroupParamObj);

  asymmetricUnitTable->writeSession(atomCoordsParamObj);

  obj[ioConstants::CrystalSystem] = crystalSystemParamObj;
  obj[ioConstants::SpaceGroup] = spaceGroupParamObj;
  obj[ioConstants::AtomCoordinates] = atomCoordsParamObj;
  obj[ioConstants::OutputCrystalFileName] = csFilePathLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::writeCrystalSystemParameters(QJsonObject& obj) const
{
  obj[ioConstants::CrystalSystemSelection] = csCB->currentIndex();
  obj[ioConstants::A] = aLE->text();
  obj[ioConstants::B] = bLE->text();
  obj[ioConstants::C] = cLE->text();
  obj[ioConstants::Alpha] = alphaLE->text();
  obj[ioConstants::Beta] = betaLE->text();
  obj[ioConstants::Gamma] = gammaLE->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::writeSpaceGroupParameters(QJsonObject& obj) const
{
  obj[ioConstants::SpaceGroupNumber] = spaceGrpNumberSB->value();
  obj[ioConstants::SpaceGroupSetting] = spaceGrpSettingCB->currentIndex();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::on_csCB_currentIndexChanged(int index) const
{
  CrystalStructureCreationController::CrystalSystem cs = static_cast<CrystalStructureCreationController::CrystalSystem>(index);

  aLabel->hide();
  aLE->hide();
  bLabel->hide();
  bLE->hide();
  cLabel->hide();
  cLE->hide();
  alphaLabel->hide();
  alphaLE->hide();
  betaLabel->hide();
  betaLE->hide();
  gammaLabel->hide();
  gammaLE->hide();

  if(cs == CrystalStructureCreationController::CrystalSystem::Cubic)
  {
    aLabel->show();
    aLE->show();
    spaceGrpNumberSB->setRange(195, 230);
  }
  else if(cs == CrystalStructureCreationController::CrystalSystem::Tetragonal || cs == CrystalStructureCreationController::CrystalSystem::Hexagonal ||
          cs == CrystalStructureCreationController::CrystalSystem::TrigonalH)
  {
    aLabel->show();
    aLE->show();
    cLabel->show();
    cLE->show();
    if(cs == CrystalStructureCreationController::CrystalSystem::Tetragonal)
    {
      spaceGrpNumberSB->setRange(75, 142);
    }
    else if(cs == CrystalStructureCreationController::CrystalSystem::Hexagonal)
    {
      spaceGrpNumberSB->setRange(168, 194);
    }
    else
    {
      spaceGrpNumberSB->setRange(143, 167);
    }
  }
  else if(cs == CrystalStructureCreationController::CrystalSystem::Orthorhombic)
  {
    aLabel->show();
    aLE->show();
    bLabel->show();
    bLE->show();
    cLabel->show();
    cLE->show();
    spaceGrpNumberSB->setRange(16, 74);
  }
  else if(cs == CrystalStructureCreationController::CrystalSystem::TrigonalR)
  {
    aLabel->show();
    aLE->show();
    alphaLabel->show();
    alphaLE->show();
    spaceGrpNumberSB->setRange(231, 237);
  }
  else if(cs == CrystalStructureCreationController::CrystalSystem::Monoclinic)
  {
    aLabel->show();
    aLE->show();
    bLabel->show();
    bLE->show();
    cLabel->show();
    cLE->show();
    betaLabel->show();
    betaLE->show();
    spaceGrpNumberSB->setRange(3, 15);
  }
  else if(cs == CrystalStructureCreationController::CrystalSystem::Anorthic)
  {
    aLabel->show();
    aLE->show();
    bLabel->show();
    bLE->show();
    cLabel->show();
    cLE->show();
    alphaLabel->show();
    alphaLE->show();
    betaLabel->show();
    betaLE->show();
    gammaLabel->show();
    gammaLE->show();
    spaceGrpNumberSB->setRange(1, 2);
  }
  else
  {
  }

#if 0
 int TRIG[7] = { 146,148,155,160,161,166,167};
 bool skip = false;

 switch(cs)
 {
 case (0):
  sgmin = 195; sgmax = 230
 case (1):
 sgmin =  75; sgmax = 142
 case (2):
 sgmin =  16; sgmax =  74
 case (3):
 sgmin = 168; sgmax = 194
 case (4):
  if (cell%SG%SYM_second)
   {
             call Message('The space groups below correspond to the ', frm = "(/A)")
             call Message('second (rhombohedral) setting.', frm = "(A/)")
             call Message('Please select one of these space groups.', frm = "(A/)")
             for (int i=0, i < 7; i++)
             {
              if ((mod(i,4) == 0) || (i == 7)) {
                write (6,"(1x,i3,':',A11,5x)") TRIG(i),SYM_SGname(TRIG(i))
              } else {
                write (6,"(1x,i3,':',A11,5x,$)") TRIG(i),SYM_SGname(TRIG(i))
              }

             }
             call Message(' -------------------------- ', frm = "(A)")
             call ReadValue(' Enter space group number : ', io_int, 1)
             cell%SYM_SGnum = io_int(1)

// check for rhombohedral settings of rhombohedral space groups
             if (cell%SG%SYM_second) {
               if (cell%SYM_SGnum == 146) cell%SYM_SGnum=231;
               if (cell%SYM_SGnum == 148) cell%SYM_SGnum=232;
               if (cell%SYM_SGnum == 155) cell%SYM_SGnum=233;
               if (cell%SYM_SGnum == 160) cell%SYM_SGnum=234;
               if (cell%SYM_SGnum == 161) cell%SYM_SGnum=235;
               if (cell%SYM_SGnum == 166) cell%SYM_SGnum=236;
               if (cell%SYM_SGnum == 167) cell%SYM_SGnum=237;
             }
             skip = true;
           } else {
            sgmin = 143;
            sgmax = 167;
           }
 case (5):
  sgmin =   3; sgmax =  15;
 case (6):
  sgmin =   1; sgmax =   2;
}

// print out all the relevant space group names and numbers
 if (!skip) {
  call Message(' ', frm = "(/A/)")
  for(int i=sgmin;i < sgmax; i++) {
   j=i-sgmin+1;
   if ((mod(j,4) == 0) || (i == sgmax)) {
    write (6,"(1x,i3,':',A11,5x)") i,SYM_SGname(i)
   } else {
    write (6,"(1x,i3,':',A11,5x,$)") i,SYM_SGname(i)
   }
  }
  cell%SYM_SGnum = sgmin-1
   while ((cell%SYM_SGnum.lt.sgmin) || (cell%SYM_SGnum.gt.sgmax))
   {
   call Message(' -------------------------- ', frm = "(A)")
   call ReadValue(' Enter space group number : ', io_int, 1)
   cell%SYM_SGnum = io_int(1)
   if ((cell%SYM_SGnum.lt.sgmin) || (cell%SYM_SGnum.gt.sgmax))
   {
    call Message('Error in space group number ', frm = "(A)")
    call Message('Crystal system / space group mismatch ', frm = "(A)")
   }
  }
 }
#endif
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreationController::CrystalStructureCreationData CrystalStructureCreation_UI::getCreationData() const
{
  CrystalStructureCreationController::CrystalStructureCreationData data;
  data.a = aLE->text().toDouble();
  data.alpha = alphaLE->text().toDouble();
  data.atomCoordinates = asymmetricUnitTable->getData();
  data.b = bLE->text().toDouble();
  data.beta = betaLE->text().toDouble();
  data.c = cLE->text().toDouble();
  data.crystalSystem = static_cast<CrystalStructureCreationController::CrystalSystem>(csCB->currentIndex());
  data.gamma = gammaLE->text().toDouble();
  data.outputFilePath = csFilePathLE->text();
  data.spaceGroupNumber = spaceGrpNumberSB->value();
  data.spaceGroupSetting = spaceGrpSettingCB->currentText().toInt();

  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void CrystalStructureCreation_UI::setController(CrystalStructureCreationController* value)
{
  m_Controller = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
CrystalStructureCreationController* CrystalStructureCreation_UI::getController() const
{
  return m_Controller;
}

