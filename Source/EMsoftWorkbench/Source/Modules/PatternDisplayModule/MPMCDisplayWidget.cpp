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

#include "MPMCDisplayWidget.h"

#include <QtCore/QSignalMapper>

#include <QtWidgets/QActionGroup>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>

#include "Modules/PatternDisplayModule/PatternDisplay_UI.h"

#include "Common/Constants.h"

#include "EMsoftWorkbench/EMsoftApplication.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MPMCDisplayWidget::MPMCDisplayWidget(QWidget* parent, Qt::WindowFlags windowFlags)
: QWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MPMCDisplayWidget::~MPMCDisplayWidget() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setupGui()
{
  connect(zoomIn, &QPushButton::pressed, [=] { imageViewer->zoomIn(); });
  connect(zoomOut, &QPushButton::pressed, [=] { imageViewer->zoomOut(); });
  connect(fitToScreen, &QPushButton::pressed, [=] { imageViewer->fitToScreen(); });

  imageViewer->setHasKevValue(true);

  QMenu* projModeMenu = new QMenu(projModeBtn);
  m_ProjModeMenuActionGroup = new QActionGroup(this);

  QSignalMapper* mapper = new QSignalMapper(this);
  connect(mapper, SIGNAL(mapped(int)), this, SLOT(projModeChanged(int)));

  m_LambertSquareAction = projModeMenu->addAction("Lambert (square)", mapper, SLOT(map()));
  mapper->setMapping(m_LambertSquareAction, static_cast<int>(MPMCDisplayWidget::ProjectionMode::Lambert_Square));
  m_LambertSquareAction->setCheckable(true);
  m_ProjModeMenuActionGroup->addAction(m_LambertSquareAction);

  m_LambertCircleAction = projModeMenu->addAction("Lambert (circle)", mapper, SLOT(map()));
  mapper->setMapping(m_LambertCircleAction, static_cast<int>(MPMCDisplayWidget::ProjectionMode::Lambert_Circle));
  m_LambertCircleAction->setCheckable(true);
  m_ProjModeMenuActionGroup->addAction(m_LambertCircleAction);

  m_StereographicAction = projModeMenu->addAction("Stereographic P.", mapper, SLOT(map()));
  mapper->setMapping(m_StereographicAction, static_cast<int>(MPMCDisplayWidget::ProjectionMode::Stereographic));
  m_StereographicAction->setCheckable(true);
  m_ProjModeMenuActionGroup->addAction(m_StereographicAction);

  projModeBtn->setMenu(projModeMenu);

  zoomIn->setDisabled(true);
  zoomOut->setDisabled(true);
  fitToScreen->setDisabled(true);
  saveBtn->setDisabled(true);
  projModeBtn->setDisabled(true);
  dispModeBtn->setDisabled(true);
  energyBinSpinBox->setDisabled(true);
  overlayCB->setDisabled(true);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setEnergyBinSpinBoxRange(int min, int max) const
{
  energyBinSpinBox->setRange(min, max);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setEnergyValue(int value) const
{
  if(energyBinSpinBox->maximum() < value || energyBinSpinBox->minimum() > value)
  {
    return;
  }

  energyBinSpinBox->setValue(value);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::on_energyBinSpinBox_valueChanged(int value) const
{
  emit controlsChanged(getMPMCData());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::on_saveBtn_clicked()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Image"), proposedDir, tr("JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if(filePath.isEmpty())
  {
    return;
  }

  QImage image = imageViewer->getCurrentImage();
  if(!image.save(filePath))
  {
    emit stdOutputMessageGenerated(tr("Error: Unable to save image to file path '%1'").arg(filePath));
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::projModeChanged(int mode)
{
  m_ProjectionMode = static_cast<MPMCDisplayWidget::ProjectionMode>(mode);
  on_energyBinSpinBox_valueChanged(energyBinSpinBox->value());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::updateSliderRange(int min, int max) const
{
  energyBinSpinBox->setRange(min, max);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::loadImage(GLImageViewer::GLImageData data) const
{
  QImage img = data.image;

  zoomIn->setDisabled(img.isNull());
  zoomOut->setDisabled(img.isNull());
  fitToScreen->setDisabled(img.isNull());
  saveBtn->setDisabled(img.isNull());
  projModeBtn->setDisabled(img.isNull());
  dispModeBtn->setDisabled(img.isNull());
  energyBinSpinBox->setDisabled(img.isNull());
  overlayCB->setDisabled(img.isNull());
  imageViewer->loadImage(data);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setProjectionMode(MPMCDisplayWidget::ProjectionMode mode)
{
  m_ProjectionMode = mode;

  if(mode == ProjectionMode::Lambert_Square)
  {
    m_LambertSquareAction->trigger();
  }
  else if(mode == ProjectionMode::Lambert_Circle)
  {
    m_LambertCircleAction->trigger();
  }
  else if(mode == ProjectionMode::Stereographic)
  {
    m_StereographicAction->trigger();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::readSession(QJsonObject& obj)
{
  m_ProjectionMode = static_cast<MPMCDisplayWidget::ProjectionMode>(obj[EMsoftWorkbenchConstants::IOStrings::ProjectionMode].toInt());

  energyBinSpinBox->blockSignals(true);
  energyBinSpinBox->setValue(obj[EMsoftWorkbenchConstants::IOStrings::EnergyBin].toInt());
  energyBinSpinBox->blockSignals(false);

  overlayCB->setCheckState(static_cast<Qt::CheckState>(obj[EMsoftWorkbenchConstants::StringConstants::OverlayState].toInt(Qt::Checked)));

  QJsonObject imageViewerObj = obj[EMsoftWorkbenchConstants::ImageViewerConstants::ImageViewer].toObject();
  imageViewer->readSession(imageViewerObj);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::writeSession(QJsonObject& obj) const
{
  obj[EMsoftWorkbenchConstants::IOStrings::EnergyBin] = energyBinSpinBox->value();
  obj[EMsoftWorkbenchConstants::IOStrings::ProjectionMode] = static_cast<int>(m_ProjectionMode);

  obj[EMsoftWorkbenchConstants::StringConstants::OverlayState] = static_cast<int>(overlayCB->checkState());

  QJsonObject imageViewerObj;
  imageViewer->writeSession(imageViewerObj);
  obj[EMsoftWorkbenchConstants::ImageViewerConstants::ImageViewer] = imageViewerObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::createConnections(PatternDisplay_UI* ui) const
{
  // If anything causes a new image to be generated, modify the window
  connect(this, &MPMCDisplayWidget::controlsChanged, [=] { emit ui->moduleParametersChanged(); });

  connect(overlayCB, &QCheckBox::stateChanged, [=](int state) {
    imageViewer->setUseStatsOverlay(state == Qt::Checked);
    imageViewer->update();
    emit ui->moduleParametersChanged();
  });

  // Image Viewer modification connections
  imageViewer->createModificationConnections(ui);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MPMCDisplayWidget::MPMCData MPMCDisplayWidget::getMPMCData() const
{
  MPMCDisplayWidget::MPMCData data;
  data.energyBin = energyBinSpinBox->value();
  data.mode = m_ProjectionMode;
  return data;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setImageVector(const std::vector<QImage>& value)
{
  m_ImageVector = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::vector<QImage> MPMCDisplayWidget::getImageVector() const
{
  return m_ImageVector;
}

