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

#include <QtWidgets/QMenu>
#include <QtWidgets/QFileDialog>

#include "EMsoftWorkbench/EMsoftApplication.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MPMCDisplayWidget::MPMCDisplayWidget(QWidget* parent, Qt::WindowFlags windowFlags) :
  GLImageDisplayWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
MPMCDisplayWidget::~MPMCDisplayWidget()
{

}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setupGui()
{
  connect(zoomIn, SIGNAL(pressed()), imageWidget, SLOT(zoomInBtn_pressed()));
  connect(zoomOut, SIGNAL(pressed()), imageWidget, SLOT(zoomOutBtn_pressed()));
  connect(fitToScreen, SIGNAL(pressed()), imageWidget, SLOT(fitToScreenBtn_pressed()));

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
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setKeV(float keV)
{
  keVDisplay->setText(QString::number(keV));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setEnergyBinSpinBoxRange(int min, int max)
{
  energyBinSpinBox->setRange(min, max);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::on_energyBinSpinBox_valueChanged(int value)
{
  emit controlsChanged(energyBinSpinBox->value(), m_ProjectionMode);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::on_saveBtn_pressed()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QString filePath = QFileDialog::getSaveFileName(this, tr("Save Image"),
    proposedDir, tr("JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)"));
  emSoftApp->setOpenDialogLastDirectory(filePath);
  if (filePath.isEmpty()) { return; }

  QImage image = imageWidget->getCurrentImage();
  if (!image.save(filePath))
  {
    emit statusMsgGenerated(tr("Error: Unable to save image to file path '%1'").arg(filePath));
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
void MPMCDisplayWidget::updateSliderRange(int min, int max)
{
  energyBinSpinBox->setRange(min, max);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::loadImage(GLImageDisplayWidget::GLImageData data)
{
  QImage img = data.image;
  float minValue = data.minValue;
  float maxValue = data.maxValue;

  zoomIn->setDisabled(img.isNull());
  zoomOut->setDisabled(img.isNull());
  fitToScreen->setDisabled(img.isNull());
  saveBtn->setDisabled(img.isNull());
  projModeBtn->setDisabled(img.isNull());
  dispModeBtn->setDisabled(img.isNull());
  energyBinSpinBox->setDisabled(img.isNull());

  imageWidget->loadImage(img);
  minLabel->setText(QString::number(minValue));
  maxLabel->setText(QString::number(maxValue));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void MPMCDisplayWidget::setProjectionMode(MPMCDisplayWidget::ProjectionMode mode)
{
  m_ProjectionMode = mode;

  if (mode == ProjectionMode::Lambert_Square)
  {
    m_LambertSquareAction->trigger();
  }
  else if (mode == ProjectionMode::Lambert_Circle)
  {
    m_LambertCircleAction->trigger();
  }
  else if (mode == ProjectionMode::Stereographic)
  {
    m_StereographicAction->trigger();
  }
}

