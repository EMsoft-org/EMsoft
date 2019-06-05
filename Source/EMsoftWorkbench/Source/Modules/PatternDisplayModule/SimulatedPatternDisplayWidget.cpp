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

#include "SimulatedPatternDisplayWidget.h"

#include <QtCore/QSignalMapper>

#include <QtWidgets/QActionGroup>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>

#include "EMsoftWrapperLib/SEM/EMsoftSEMwrappers.h"

#include "EMsoftWorkbench/EMsoftApplication.h"

#include "Common/FileIOTools.h"
#include "Common/PatternTools.h"

#include "Modules/PatternDisplayModule/AngleWidgets/AbstractAngleWidget.h"
#include "Modules/PatternDisplayModule/PatternListModel.h"

const QString SimulatedPatternDisplayWidget::UpperLeftOrigin = "UL";
const QString SimulatedPatternDisplayWidget::LowerLeftOrigin = "LL";
const QString SimulatedPatternDisplayWidget::UpperRightOrigin = "UR";
const QString SimulatedPatternDisplayWidget::LowerRightOrigin = "LR";
const QString SimulatedPatternDisplayWidget::LinearScaling = "Linear";
const QString SimulatedPatternDisplayWidget::GammaScaling = "Gamma";
const QString SimulatedPatternDisplayWidget::DetBin_1 = "1";
const QString SimulatedPatternDisplayWidget::DetBin_2 = "2";
const QString SimulatedPatternDisplayWidget::DetBin_4 = "4";
const QString SimulatedPatternDisplayWidget::DetBin_8 = "8";
const QString SimulatedPatternDisplayWidget::DetBinLabel = "Detector Binning";
const QString SimulatedPatternDisplayWidget::PatternOriginLabel = "Pattern Origin";
const QString SimulatedPatternDisplayWidget::PatternScalingLabel = "Pattern Scaling";
const QString SimulatedPatternDisplayWidget::GenerateText = "Generate";
const QString SimulatedPatternDisplayWidget::CancelText = "Cancel...";

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SimulatedPatternDisplayWidget::SimulatedPatternDisplayWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: QWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SimulatedPatternDisplayWidget::~SimulatedPatternDisplayWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::setupGui()
{
  PatternListModel* model = PatternListModel::Instance();
  patternListView->setModel(model);

  connect(patternListView->selectionModel(), SIGNAL(selectionChanged(QItemSelection,QItemSelection)), this,
          SLOT(patternListView_itemSelectionChanged(QItemSelection,QItemSelection)));

  connect(patternListView, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(patternListView_doubleClicked(QModelIndex)));

  m_MinSBValue = gammaSpinBox->minimum();
  m_MaxSBValue = gammaSpinBox->maximum();

  // Create Detector Binning Button Menu
  {
    QMenu* detectorBinningMenu = new QMenu(detectorBinningBtn);
    m_DetectorBinningMenuActionGroup = new QActionGroup(this);

    m_DetectorBinning_1 = detectorBinningMenu->addAction(SimulatedPatternDisplayWidget::DetBin_1, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_1->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_1);

    m_DetectorBinning_2 = detectorBinningMenu->addAction(SimulatedPatternDisplayWidget::DetBin_2, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_2->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_2);

    m_DetectorBinning_4 = detectorBinningMenu->addAction(SimulatedPatternDisplayWidget::DetBin_4, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_4->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_4);

    m_DetectorBinning_8 = detectorBinningMenu->addAction(SimulatedPatternDisplayWidget::DetBin_8, this, SLOT(detectorBinning_selectionChanged()));
    m_DetectorBinning_8->setCheckable(true);
    m_DetectorBinningMenuActionGroup->addAction(m_DetectorBinning_8);

    detectorBinningBtn->setMenu(detectorBinningMenu);

    m_DetectorBinning_1->setChecked(true);
  }

  // Create Pattern Origin Button Menu
  {
    QMenu* patternOriginMenu = new QMenu(patternOriginBtn);
    m_PatternOriginMenuActionGroup = new QActionGroup(this);

    m_PatternOrigin_UL = patternOriginMenu->addAction(SimulatedPatternDisplayWidget::UpperLeftOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_UL->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_UL);

    m_PatternOrigin_LL = patternOriginMenu->addAction(SimulatedPatternDisplayWidget::LowerLeftOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_LL->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_LL);

    m_PatternOrigin_UR = patternOriginMenu->addAction(SimulatedPatternDisplayWidget::UpperRightOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_UR->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_UR);

    m_PatternOrigin_LR = patternOriginMenu->addAction(SimulatedPatternDisplayWidget::LowerRightOrigin, this, SLOT(patternOrigin_selectionChanged()));
    m_PatternOrigin_LR->setCheckable(true);
    m_PatternOriginMenuActionGroup->addAction(m_PatternOrigin_LR);

    patternOriginBtn->setMenu(patternOriginMenu);

    m_PatternOrigin_UL->setChecked(true);
  }

  // Create Pattern Scaling Button Menu
  {
    QMenu* patternScalingMenu = new QMenu(patternScalingBtn);
    m_PatternScalingMenuActionGroup = new QActionGroup(this);

    m_PatternScaling_Linear = patternScalingMenu->addAction(SimulatedPatternDisplayWidget::LinearScaling, this, SLOT(patternScaling_selectionChanged()));
    m_PatternScaling_Linear->setCheckable(true);
    m_PatternScalingMenuActionGroup->addAction(m_PatternScaling_Linear);

    m_PatternScaling_Gamma = patternScalingMenu->addAction(SimulatedPatternDisplayWidget::GammaScaling, this, SLOT(patternScaling_selectionChanged()));
    m_PatternScaling_Gamma->setCheckable(true);
    m_PatternScalingMenuActionGroup->addAction(m_PatternScaling_Gamma);

    patternScalingBtn->setMenu(patternScalingMenu);

    m_PatternScaling_Linear->setChecked(true);
  }

  gammaSpinBox->blockSignals(true);
  gammaSpinBox->setValue(m_MinSBValue);
  gammaSpinBox->blockSignals(false);

  detectorBinningBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::DetBinLabel, getDetectorBinningValue()));
  patternOriginBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::PatternOriginLabel, getPatternOriginValue()));
  patternScalingBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::PatternScalingLabel, getPatternScalingValue()));

  connect(zoomIn, &QPushButton::pressed, [=] { imageWidget->zoomIn(); });
  connect(zoomOut, &QPushButton::pressed, [=] { imageWidget->zoomOut(); });
  connect(fitToScreen, &QPushButton::pressed, [=] { imageWidget->fitToScreen(); });

  connect(overlayCB, &QCheckBox::stateChanged, [=](int state) {
    imageWidget->setUseStatsOverlay(state == Qt::Checked);
    imageWidget->update();
  });

  imageWidget->fitToScreen();

  splitter->setSizes(QList<int>() << 100 << 400);

  PatternDisplayData data;
  data.detectorBinningValue = SimulatedPatternDisplayWidget::DetBin_1.toInt();
  data.patternOrigin = SimulatedPatternDisplayWidget::UpperLeftOrigin;
  data.patternScaling = SimulatedPatternDisplayWidget::LinearScaling;
  data.gammaValue = m_MinSBValue;
  data.currentRow = patternListView->currentIndex().row();
  m_CurrentPatternDisplayData = data;

  percentLabel->hide();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::closeEvent(QCloseEvent* event)
{
  emit cancelRequested();

  QWidget::closeEvent(event);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::detectorBinning_selectionChanged() const
{
  size_t detectorBinValue = getDetectorBinningValue();
  detectorBinningBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::DetBinLabel).arg(detectorBinValue));

  displayImage(patternListView->currentIndex().row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::patternOrigin_selectionChanged() const
{
  QString patternOrigin = getPatternOriginValue();
  patternOriginBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::PatternOriginLabel, patternOrigin));

  displayImage(patternListView->currentIndex().row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::patternScaling_selectionChanged() const
{
  QString patternScaling = getPatternScalingValue();

  patternScalingBtn->setText(tr("%1: %2").arg(SimulatedPatternDisplayWidget::PatternScalingLabel, patternScaling));

  bool gammaIsOn = (patternScaling == SimulatedPatternDisplayWidget::GammaScaling);
  gammaSpinBox->setEnabled(gammaIsOn);
  gammaLabel->setEnabled(gammaIsOn);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::generateImages()
{
  m_NewPatternDisplayData = getPatternDisplayData();
  on_generateBtn_clicked();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
SimulatedPatternDisplayWidget::PatternDisplayData SimulatedPatternDisplayWidget::getPatternDisplayData() const
{
  size_t detectorBinningValue = getDetectorBinningValue();
  if(detectorBinningValue == 0)
  {
    // Error: Invalid detector binning value
  }

  QString patternOrigin = getPatternOriginValue();
  if(patternOrigin.isEmpty())
  {
    // Error: Invalid pattern origin
  }

  QString patternScaling = getPatternScalingValue();
  if(patternScaling.isEmpty())
  {
    // Error: Invalid pattern scaling
  }

  double gamma;
  if(patternScaling == SimulatedPatternDisplayWidget::LinearScaling)
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
  data.useCircularMask = useCircularMask->isChecked();
  data.currentRow = patternListView->currentIndex().row();
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::on_generateBtn_clicked()
{
  if(generateBtn->text() == GenerateText)
  {
    emit generationStarted();
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
    emit generationFinished();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::on_useCircularMask_toggled(bool checked)
{
  m_NewPatternDisplayData.useCircularMask = checked;
  displayImage(m_NewPatternDisplayData.currentRow);
  m_CurrentPatternDisplayData = m_NewPatternDisplayData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::on_gammaSpinBox_valueChanged(double value)
{
  m_NewPatternDisplayData.gammaValue = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::on_saveBtn_clicked() const
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QFileInfo fi(proposedDir);
  QString parentPath = fi.path();
  parentPath = parentPath + "/Untitled.jpeg";
  parentPath = QDir::toNativeSeparators(parentPath);
  QString filePath = FileIOTools::GetSavePathFromDialog("Save Pattern Image", "JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)", parentPath);
  if(filePath.isEmpty())
  {
    return;
  }

  QImage image = imageWidget->getCurrentImage();
  if(!image.save(filePath))
  {
    //    emit stdOutputMessageGenerated(tr("Error: Unable to save image to file path '%1'").arg(filePath));
  }
  else
  {
    emSoftApp->setOpenDialogLastDirectory(proposedDir);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::setExpectedPatterns(const std::vector<float> &eulerAngles)
{
  PatternListModel* model = PatternListModel::Instance();

  patternListView->selectionModel()->clear();
  model->clear();

  m_LoadedImageData.clear();
  m_LoadedImageData.resize(eulerAngles.size() / 3);

  int index = 0;
  for(std::vector<float>::const_iterator iter = eulerAngles.begin(); iter < eulerAngles.end(); iter + 3)
  {
    float a1 = eulerAngles.at(*(iter + 0));
    float a2 = eulerAngles.at(*(iter + 1));
    float a3 = eulerAngles.at(*(iter + 2));
    a1 = AbstractAngleWidget::ConvertToDegrees(a1);
    a2 = AbstractAngleWidget::ConvertToDegrees(a2);
    a3 = AbstractAngleWidget::ConvertToDegrees(a3);

    QString name = tr("Pattern %1: (%2, %3, %4)").arg(QString::number(index + 1), QString::number(a1), QString::number(a2), QString::number(a3));

    model->insertItem(index, name);
    index++;
  }

  patternListView->setCurrentIndex(model->index(0, PatternListItem::DefaultColumn));
  patternListView->setFocus();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::setProgressBarValue(int value) const
{
  progressBar->setValue(value);
  int maximum = progressBar->maximum();

  percentLabel->setText(tr("%1/%2").arg(value).arg(maximum));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::setProgressBarMaximum(int value) const
{
  progressBar->setMaximum(value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::loadImage(int index, const GLImageViewer::GLImageData& data)
{
  if(index > m_LoadedImageData.size() - 1 || index < 0)
  {
    return;
  }

  m_LoadedImageData[index] = data;

  if(patternListView->currentIndex().row() == index)
  {
    displayImage(index);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::displayImage(int index) const
{
  displayImage(m_LoadedImageData[index]);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::displayImage(GLImageViewer::GLImageData imageData) const
{
  PatternDisplayData displayData = getPatternDisplayData();

  if(!imageData.image.isNull())
  {
    if(displayData.patternOrigin == SimulatedPatternDisplayWidget::UpperLeftOrigin)
    {
      imageData.image = imageData.image.mirrored();
    }
    else if(displayData.patternOrigin == SimulatedPatternDisplayWidget::UpperRightOrigin)
    {
      imageData.image = imageData.image.mirrored(true, true);
    }
    else if(displayData.patternOrigin == SimulatedPatternDisplayWidget::LowerRightOrigin)
    {
      imageData.image = imageData.image.mirrored(true, false);
    }

    if(useCircularMask->isChecked())
    {
      imageData.image = PatternTools::ApplyCircularMask(imageData.image);
    }

    imageData.image = imageData.image.scaled(imageData.image.width() / displayData.detectorBinningValue, imageData.image.height() / displayData.detectorBinningValue);
  }

  imageWidget->loadImage(imageData);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::patternListView_itemSelectionChanged(const QItemSelection& current, const QItemSelection& previous) const
{
  Q_UNUSED(previous)

  if(current.indexes().size() == 1)
  {
    GLImageViewer::GLImageData imageData = m_LoadedImageData[current.indexes()[0].row()];
    displayImage(imageData);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::patternListView_doubleClicked(const QModelIndex& index) const
{
  PatternListModel* model = PatternListModel::Instance();
  model->setPatternStatus(index.row(), PatternListItem::PatternStatus::Priority);

  emit patternNeedsPriority(index.row());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SimulatedPatternDisplayWidget::getPatternOriginValue() const
{
  QAction* checkedAction = m_PatternOriginMenuActionGroup->checkedAction();
  QString value = "";

  if(checkedAction != nullptr)
  {
    value = checkedAction->text();
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString SimulatedPatternDisplayWidget::getPatternScalingValue() const
{
  QAction* checkedAction = m_PatternScalingMenuActionGroup->checkedAction();
  QString value = "";

  if(checkedAction != nullptr)
  {
    value = checkedAction->text();
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
size_t SimulatedPatternDisplayWidget::getDetectorBinningValue() const
{
  QAction* checkedAction = m_DetectorBinningMenuActionGroup->checkedAction();
  size_t value = 0;

  if(checkedAction != nullptr)
  {
    bool ok = false;
    value = checkedAction->text().toInt(&ok);
    if(!ok)
    {
      value = 0;
    }
  }

  return value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
double SimulatedPatternDisplayWidget::getGammaValue() const
{
  return gammaSpinBox->value();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void SimulatedPatternDisplayWidget::patternGenerationFinished() const
{
  progressBar->setValue(0);
  percentLabel->hide();
  generateBtn->setText(GenerateText);
  emit generationFinished();
}
