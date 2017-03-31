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

#include "PatternDisplayWidget.h"

#include <QtCore/QSignalMapper>

#include <QtWidgets/QMenu>
#include <QtWidgets/QFileDialog>

#include "EMsoftLib/EMsoftLib.h"

#include "EMsoftWorkbench/EMsoftApplication.h"
#include "EMsoftWorkbench/AngleWidgets/AbstractAngleWidget.h"
#include "EMsoftWorkbench/PatternListModel.h"

const QString PatternDisplayWidget::UpperLeftOrigin = "UL";
const QString PatternDisplayWidget::LowerLeftOrigin = "LL";
const QString PatternDisplayWidget::UpperRightOrigin = "UR";
const QString PatternDisplayWidget::LowerRightOrigin = "LR";
const QString PatternDisplayWidget::LinearScaling = "Linear";
const QString PatternDisplayWidget::GammaScaling = "Gamma";
const QString PatternDisplayWidget::DetBin_1 = "1";
const QString PatternDisplayWidget::DetBin_2 = "2";
const QString PatternDisplayWidget::DetBin_4 = "4";
const QString PatternDisplayWidget::DetBin_8 = "8";
const QString PatternDisplayWidget::DetBinLabel = "Detector Binning";
const QString PatternDisplayWidget::PatternOriginLabel = "Pattern Origin";
const QString PatternDisplayWidget::PatternScalingLabel = "Pattern Scaling";
const QString PatternDisplayWidget::GenerateText = "Generate";
const QString PatternDisplayWidget::CancelText = "Cancel...";

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplayWidget::PatternDisplayWidget(QWidget* parent, Qt::WindowFlags windowFlags) :
  GLImageDisplayWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplayWidget::~PatternDisplayWidget()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::setupGui()
{
  PatternListModel* model = PatternListModel::Instance();
  patternListView->setModel(model);

  connect(patternListView->selectionModel(), SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)), this, SLOT(patternListView_itemSelectionChanged(const QItemSelection &, const QItemSelection &)));

  connect(patternListView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(patternListView_doubleClicked(const QModelIndex &)));

  m_MinSBValue = gammaSpinBox->minimum();
  m_MaxSBValue = gammaSpinBox->maximum();

  // Create Detector Binning Button Menu
  {
    QMenu* detectorBinningMenu = new QMenu(detectorBinningBtn);
    m_DetectorBinningMenuActionGroup = new QActionGroup(this);

    m_DetectorBinning_1 = detectorBinningMenu->addAction(PatternDisplayWidget::DetBin_1, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_1->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_1);

    m_DetectorBinning_2 = detectorBinningMenu->addAction(PatternDisplayWidget::DetBin_2, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_2->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_2);

    m_DetectorBinning_4 = detectorBinningMenu->addAction(PatternDisplayWidget::DetBin_4, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_4->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_4);

    m_DetectorBinning_8 = detectorBinningMenu->addAction(PatternDisplayWidget::DetBin_8, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_8->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_8);

    detectorBinningBtn->setMenu(detectorBinningMenu);

    m_DetectorBinning_1->setChecked(true);
  }

  // Create Pattern Origin Button Menu
  {
    QMenu* patternOriginMenu = new QMenu(patternOriginBtn);
    m_PatternOriginMenuActionGroup = new QActionGroup(this);

    m_PatternOrigin_UL = patternOriginMenu->addAction(PatternDisplayWidget::UpperLeftOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_UL->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_UL);

    m_PatternOrigin_LL = patternOriginMenu->addAction(PatternDisplayWidget::LowerLeftOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_LL->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_LL);

    m_PatternOrigin_UR = patternOriginMenu->addAction(PatternDisplayWidget::UpperRightOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_UR->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_UR);

    m_PatternOrigin_LR = patternOriginMenu->addAction(PatternDisplayWidget::LowerRightOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_LR->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_LR);

    patternOriginBtn->setMenu(patternOriginMenu);

    m_PatternOrigin_UL->setChecked(true);
  }

  // Create Pattern Scaling Button Menu
  {
    QMenu* patternScalingMenu = new QMenu(patternScalingBtn);
    m_PatternScalingMenuActionGroup = new QActionGroup(this);

    m_PatternScaling_Linear = patternScalingMenu->addAction(PatternDisplayWidget::LinearScaling, this, SLOT(patternScaling_selectionChanged()));
    m_PatternScaling_Linear->setCheckable(true);
    m_PatternScalingMenuActionGroup->addAction(m_PatternScaling_Linear);

    m_PatternScaling_Gamma = patternScalingMenu->addAction(PatternDisplayWidget::GammaScaling, this, SLOT(patternScaling_selectionChanged()));
    m_PatternScaling_Gamma->setCheckable(true);
    m_PatternScalingMenuActionGroup->addAction(m_PatternScaling_Gamma);

    patternScalingBtn->setMenu(patternScalingMenu);

    m_PatternScaling_Linear->setChecked(true);
  }

  gammaSpinBox->blockSignals(true);
  gammaSpinBox->setValue(m_MinSBValue);
  gammaSpinBox->blockSignals(false);

  detectorBinningBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::DetBinLabel).arg(getDetectorBinningValue()));
  patternOriginBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::PatternOriginLabel).arg(getPatternOriginValue()));
  patternScalingBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::PatternScalingLabel).arg(getPatternScalingValue()));

  connect(zoomIn, SIGNAL(pressed()), imageWidget, SLOT(zoomInBtn_pressed()));
  connect(zoomOut, SIGNAL(pressed()), imageWidget, SLOT(zoomOutBtn_pressed()));
  connect(fitToScreen, SIGNAL(pressed()), imageWidget, SLOT(fitToScreenBtn_pressed()));

  splitter->setSizes(QList<int>() << 100 << 400);

  PatternDisplayData data;
  data.detectorBinningValue = PatternDisplayWidget::DetBin_1.toInt();
  data.patternOrigin = PatternDisplayWidget::UpperLeftOrigin;
  data.patternScaling = PatternDisplayWidget::LinearScaling;
  data.gammaValue = m_MinSBValue;
  data.currentRow = patternListView->currentIndex().row();
  m_CurrentPatternDisplayData = data;

  percentLabel->hide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::detectorBinning_selectionChanged()
{
  size_t detectorBinValue = getDetectorBinningValue();
  detectorBinningBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::DetBinLabel).arg(detectorBinValue));

  displayImage(patternListView->currentIndex().row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::patternOrigin_selectionChanged()
{  
  QString patternOrigin = getPatternOriginValue();
  patternOriginBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::PatternOriginLabel).arg(patternOrigin));

  displayImage(patternListView->currentIndex().row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::patternScaling_selectionChanged()
{
  QString patternScaling = getPatternScalingValue();

  patternScalingBtn->setText(tr("%1: %2").arg(PatternDisplayWidget::PatternScalingLabel).arg(patternScaling));

  bool gammaIsOn = (patternScaling == PatternDisplayWidget::GammaScaling);
  gammaSpinBox->setEnabled(gammaIsOn);
  gammaLabel->setEnabled(gammaIsOn);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::generateImages()
{
  m_NewPatternDisplayData = getPatternDisplayData();
  on_generateBtn_pressed();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternDisplayWidget::PatternDisplayData PatternDisplayWidget::getPatternDisplayData()
{
  size_t detectorBinningValue = getDetectorBinningValue();
  if (detectorBinningValue == 0)
  {
    // Error: Invalid detector binning value
  }

  QString patternOrigin = getPatternOriginValue();
  if (patternOrigin.isEmpty())
  {
    // Error: Invalid pattern origin
  }

  QString patternScaling = getPatternScalingValue();
  if (patternScaling.isEmpty())
  {
    // Error: Invalid pattern scaling
  }

  double gamma;
  if (patternScaling == PatternDisplayWidget::LinearScaling)
  {
    gamma = 1;
    gammaLabel->setDisabled(true);
    gammaSpinBox->setDisabled(true);
  }
  else
  {
    gamma = gammaSpinBox->value();
    gammaLabel->setEnabled(true);
    gammaSpinBox->setEnabled(true);
  }

  PatternDisplayData data;
  data.detectorBinningValue = detectorBinningValue;
  data.patternOrigin = patternOrigin;
  data.patternScaling = patternScaling;
  data.gammaValue = gamma;
  data.currentRow = patternListView->currentIndex().row();
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::on_generateBtn_pressed()
{
  if (generateBtn->text() == GenerateText)
  {
    PatternListModel* model = PatternListModel::Instance();

    int imageCount = m_LoadedImageData.size();
    m_LoadedImageData.clear();
    m_LoadedImageData.resize(imageCount);

    generateBtn->setText(CancelText);
    percentLabel->setText(tr("%1/%2").arg(0).arg(model->rowCount()));
    percentLabel->show();
    m_NewPatternDisplayData = getPatternDisplayData();
    displayImage(m_NewPatternDisplayData.currentRow);
    m_CurrentPatternDisplayData = m_NewPatternDisplayData;
    emit dataChanged(m_NewPatternDisplayData);
  }
  else
  {
    emit cancelRequested();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::on_gammaSpinBox_valueChanged(double value)
{
  m_NewPatternDisplayData.gammaValue = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::on_saveBtn_pressed()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Image"),
    proposedDir, tr("JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if (filePath.isEmpty()) { return; }

  QImage image = imageWidget->getCurrentImage();
  if (!image.save(filePath))
  {
//    emit statusMsgGenerated(tr("Error: Unable to save image to file path '%1'").arg(filePath));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::setExpectedPatterns(FloatArrayType::Pointer eulerAngles)
{
  PatternListModel* model = PatternListModel::Instance();

  patternListView->selectionModel()->clear();
  model->clear();

  m_LoadedImageData.clear();
  m_LoadedImageData.resize(eulerAngles->getNumberOfTuples());

  for (int i = 0; i < eulerAngles->getNumberOfTuples(); i++)
  {
    float a1 = eulerAngles->getComponent(i, 0);
    float a2 = eulerAngles->getComponent(i, 1);
    float a3 = eulerAngles->getComponent(i, 2);
    a1 = AbstractAngleWidget::ConvertToDegrees(a1);
    a2 = AbstractAngleWidget::ConvertToDegrees(a2);
    a3 = AbstractAngleWidget::ConvertToDegrees(a3);

    QString name = tr("Pattern %1: (%2, %3, %4)").arg(QString::number(i+1), QString::number(a1), QString::number(a2), QString::number(a3));

    model->insertItem(i, name);
  }

  patternListView->setCurrentIndex(model->index(0, PatternListItem::DefaultColumn));
  patternListView->setFocus();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::setProgressBarValue(int value)
{
  progressBar->setValue(value);
  int maximum = progressBar->maximum();

  percentLabel->setText(tr("%1/%2").arg(value).arg(maximum));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::setProgressBarMaximum(int value)
{
  progressBar->setMaximum(value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::loadImage(int index, GLImageDisplayWidget::GLImageData data)
{
  if (index > m_LoadedImageData.size() - 1 || index < 0) { return; }

  m_LoadedImageData[index] = data;

  if (patternListView->currentIndex().row() == index)
  {
    displayImage(index);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::updateImageViewer()
{
  imageWidget->update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::displayImage(int index)
{
  displayImage(m_LoadedImageData[index]);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::displayImage(GLImageDisplayWidget::GLImageData imageData)
{
  PatternDisplayData displayData = getPatternDisplayData();

  if (imageData.image.isNull() == false)
  {
    if (displayData.patternOrigin == PatternDisplayWidget::UpperLeftOrigin)
    {
      imageData.image = imageData.image.mirrored();
    }
    else if (displayData.patternOrigin == PatternDisplayWidget::UpperRightOrigin)
    {
      imageData.image = imageData.image.mirrored(true, true);
    }
    else if (displayData.patternOrigin == PatternDisplayWidget::LowerRightOrigin)
    {
      imageData.image = imageData.image.mirrored(true, false);
    }

    imageData.image = imageData.image.scaled(imageData.image.width() / displayData.detectorBinningValue, imageData.image.height() / displayData.detectorBinningValue);
  }

  minLabel->setText(QString::number(imageData.minValue));
  maxLabel->setText(QString::number(imageData.maxValue));

  imageWidget->loadImage(imageData.image);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::patternListView_itemSelectionChanged(const QItemSelection &current, const QItemSelection &previous)
{
  Q_UNUSED(previous)

  if (current.indexes().size() == 1)
  {
    GLImageDisplayWidget::GLImageData imageData = m_LoadedImageData[current.indexes()[0].row()];
    displayImage(imageData);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::patternListView_doubleClicked(const QModelIndex &index)
{
  PatternListModel* model = PatternListModel::Instance();
  model->setPatternStatus(index.row(), PatternListItem::PatternStatus::Priority);

  emit patternNeedsPriority(index.row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString PatternDisplayWidget::getPatternOriginValue()
{
  QAction* checkedAction = m_PatternOriginMenuActionGroup->checkedAction();
  QString value = "";

  if (checkedAction != nullptr)
  {
    value = checkedAction->text();
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString PatternDisplayWidget::getPatternScalingValue()
{
  QAction* checkedAction = m_PatternScalingMenuActionGroup->checkedAction();
  QString value = "";

  if (checkedAction != nullptr)
  {
    value = checkedAction->text();
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
size_t PatternDisplayWidget::getDetectorBinningValue()
{
  QAction* checkedAction = m_DetectorBinningMenuActionGroup->checkedAction();
  size_t value = 0;

  if (checkedAction != nullptr)
  {
    bool ok = false;
    value = checkedAction->text().toInt(&ok);
    if (ok == false) { value = 0; }
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
double PatternDisplayWidget::getGammaValue()
{
  return gammaSpinBox->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternDisplayWidget::generationFinished()
{
  progressBar->setValue(0);
  percentLabel->hide();
  generateBtn->setText(GenerateText);
}

