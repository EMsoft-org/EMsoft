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

#include "PatternFitViewer.h"

#include <QtCore/QDir>
#include <QtCore/QFileInfo>

#include "Common/Constants.h"
#include "Common/FileIOTools.h"

#include "EMsoftWorkbench/EMsoftApplication.h"

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitViewer::PatternFitViewer(QWidget* parent, Qt::WindowFlags windowFlags)
: QWidget(parent, windowFlags)
{
  setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
PatternFitViewer::~PatternFitViewer() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::setupGui()
{
  // Turn the zooming feature off
  imageWidget->setZoomable(false);

  flickerIntervalSB->setMaximum(std::numeric_limits<int>::max());
  flickerIntervalSB->setValue(300);

  connect(flickerChkBox, &QCheckBox::stateChanged, this, &PatternFitViewer::flickerBoxChecked);

  connect(overlayCB, &QCheckBox::stateChanged, [=](int state) {
    imageWidget->setUseStatsOverlay(state == Qt::Checked);
    imageWidget->update();
    emit controlsChanged();
  });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::loadImage(GLImageViewer::GLImageData imageData)
{
  m_CurrentImage = imageData.image;
  imageWidget->loadImage(imageData);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::clearImage()
{
  imageWidget->loadImage(GLImageViewer::GLImageData());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::on_saveBtn_clicked()
{
  QString proposedDir = emSoftApp->getOpenDialogLastDirectory();
  QFileInfo fi(proposedDir);
  QString parentPath = fi.path();
  parentPath = parentPath + "/Untitled";
  parentPath = QDir::toNativeSeparators(parentPath);
  QString filePath = FileIOTools::GetSavePathFromDialog("Save Pattern Image", "JPEG File (*.jpeg);;PNG File(*.png);;TIFF File(*.tiff);;All Files (*.*)", parentPath);
  if(filePath.isEmpty())
  {
    return;
  }

  if(m_CurrentImage.save(filePath))
  {
    emSoftApp->setOpenDialogLastDirectory(proposedDir);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::readSession(QJsonObject& obj)
{
  flickerIntervalSB->setValue(obj[EMsoftWorkbenchConstants::IOStrings::FlickerInterval].toInt());
  overlayCB->setCheckState(static_cast<Qt::CheckState>(obj[EMsoftWorkbenchConstants::StringConstants::OverlayState].toInt(Qt::Checked)));
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void PatternFitViewer::writeSession(QJsonObject& obj)
{
  obj[EMsoftWorkbenchConstants::IOStrings::FlickerInterval] = flickerIntervalSB->value();
  obj[EMsoftWorkbenchConstants::StringConstants::OverlayState] = static_cast<int>(overlayCB->checkState());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int PatternFitViewer::getFlickerInterval()
{
  return flickerIntervalSB->value();
}
