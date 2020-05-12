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

#include "ChoosePatternsDataset_UI.h"

#include <QtCore/QJsonObject>

using InputType = EMsoftWorkbenchConstants::InputType;

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ChoosePatternsDataset_UI::ChoosePatternsDataset_UI(QWidget* parent, Qt::WindowFlags flags)
: QWidget(parent, flags)
, m_Ui(new Ui::ChoosePatternsDataset_UI())
{
  m_Ui->setupUi(this);

  setupGui();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
ChoosePatternsDataset_UI::~ChoosePatternsDataset_UI() = default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ChoosePatternsDataset_UI::setupGui()
{
  m_Ui->hdf5DatasetSelectionWidget->setInputFileLabelText("Pattern Data File");
  m_Ui->hdf5DatasetSelectionWidget->setOneSelectionOnly(true);

  connect(m_Ui->inputTypeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=](int index) { emit inputTypeChanged(static_cast<InputType>(index)); });
  connect(m_Ui->inputTypeCB, static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged), [=] { emit parametersChanged(); });
  connect(m_Ui->hdf5DatasetSelectionWidget, &HDF5DatasetSelectionWidget::selectedHDF5PathsChanged, this, &ChoosePatternsDataset_UI::selectedHDF5PathsChanged);
  connect(m_Ui->hdf5DatasetSelectionWidget, &HDF5DatasetSelectionWidget::selectedHDF5PathsChanged, [=] { emit parametersChanged(); });
  connect(m_Ui->hdf5DatasetSelectionWidget, &HDF5DatasetSelectionWidget::patternDataFilePathChanged, this, &ChoosePatternsDataset_UI::patternDataFilePathChanged);
  connect(m_Ui->hdf5DatasetSelectionWidget, &HDF5DatasetSelectionWidget::patternDataFilePathChanged, [=] { emit parametersChanged(); });
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5DatasetSelectionWidget* ChoosePatternsDataset_UI::getHDF5DatasetSelectionWidget() const
{
  return m_Ui->hdf5DatasetSelectionWidget;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ChoosePatternsDataset_UI::readSession(QJsonObject& obj)
{
  m_Ui->inputTypeCB->setCurrentIndex(obj[ioConstants::PatternsInputType].toInt());

  m_Ui->hdf5DatasetSelectionWidget->readParameters(obj);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void ChoosePatternsDataset_UI::writeSession(QJsonObject& obj) const
{
  m_Ui->hdf5DatasetSelectionWidget->writeParameters(obj);

  obj[ioConstants::PatternsInputType] = m_Ui->inputTypeCB->currentIndex();
}
