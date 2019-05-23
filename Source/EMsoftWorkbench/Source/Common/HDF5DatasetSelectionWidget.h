/* ============================================================================
 * Copyright (c) 2017 BlueQuartz Software, LLC
 * All rights reserved.
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
 * Neither the names of any of the BlueQuartz Software contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
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
 *
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#pragma once

#include <QtWidgets/QWidget>

#include "hdf5.h"

//-- UIC generated Header
#include "ui_HDF5DatasetSelectionWidget.h"

#define READ_HDF5_EXTRA_FEATURES 1

class IH5DataWindow;
class QDockWidget;
class QtSLineEdit;
class QKeyEvent;
class QAction;
class QTableWidget;

/**
 * @brief The HDF5DatasetSelectionWidget class
 */
class HDF5DatasetSelectionWidget : public QWidget
{
  Q_OBJECT

public:
  HDF5DatasetSelectionWidget(QWidget* parent = nullptr);

  ~HDF5DatasetSelectionWidget() override;

  /**
   * @brief getCurrentFile
   * @return
   */
  QString getCurrentFile() const;

  Q_PROPERTY(QPixmap Icon READ getIcon WRITE setIcon)
  void setIcon(const QPixmap& path);
  QPixmap getIcon();

  /**
   * @brief setInputFileLabelText
   * @param text
   */
  void setInputFileLabelText(const QString &text);

  /**
   * @brief readParameters
   * @param obj
   */
  void readParameters(QJsonObject& obj);

  /**
   * @brief writeParameters
   * @param obj
   */
  void writeParameters(QJsonObject& obj);

  /**
   * @brief setOneSelectionOnly
   * @param value
   */
  void setOneSelectionOnly(bool value);

  /**
   * @brief getSelectedHDF5Paths
   * @return
   */
  QStringList getSelectedHDF5Paths() const;

  /**
   * @brief setOneSelectionOnly
   * @param value
   */
  void setOneSelectionOnly(bool value);

public slots:

  /**
   * @brief listenSelectBtnClicked
   */
  void listenSelectBtnClicked();

  void listenFileDropped(const QString& text);
  void listenFileEditingFinished();
  void listenFileTextChanged(const QString& text);
  void listenFileReturnPressed();

protected:
  /**
   * @brief
   * @param event
   */
  void keyPressEvent(QKeyEvent* event) override;

  /**
   * @brief setupMenuField
   */
  void setupMenuField();

  /**
   * @brief Drag and drop implementation
   */
  void dragEnterEvent(QDragEnterEvent* dragEvent) override;

  /**
   * @brief Drag and drop implementation
   */
  void dropEvent(QDropEvent* dropEvent) override;

  /**
   * @brief Initializes some of the GUI elements with selections or other GUI related items
   */
  void setupGui();

  /**
   * @brief initWithFile Initializes the window by trying to open and populate the window
   * with values from the passed in hdf5 file
   * @param hdf5File
   */
  bool initWithFile(const QString& hdf5File);

  /**
  * @brief Returns the best guess at component dimensions for the given path.  This requires a valid AttributeMatrix, ImageGeometry, and HDF5 path
  * @param path
  * @return
  */
  std::tuple<herr_t, QString> bestGuessCDims(const QString& path);

  /**
   * @brief setValue
   * @param val
   */
  void setValue(const QString& val);

  /**
   * @brief getValue
   * @return
   */
  QString getValue();

  /**
   * @brief setErrorText
   * @param value
   */
  void setErrorText(const QString& value);

  /**
   * @brief getErrorText
   * @return
   */
  QString getErrorText() const;

signals:
  void parametersChanged();
  void selectedHDF5PathsChanged(QStringList selectedHDF5Paths);

private slots:
  /**
   * @brief Slot to catch events when the DataRecord TreeView selection is changed
   * @param current The currently selected Index
   * @param previous The previously selected Index
   */
  void listenHDFTreeViewCurrentChanged(const QModelIndex& current, const QModelIndex& previous);

private:
  QSharedPointer<Ui::HDF5DatasetSelectionWidget> m_Ui;
  static QString m_OpenDialogLastDirectory; // Stores the last directory the user visited
  QString m_CurrentOpenFile;                // Stores the currently open HDF5 File
  std::string m_CurrentHDFDataPath;         // Stores the currently viewed HDF data path
  hid_t m_FileId;
  QStringList m_CurrentPathsWithErrors;
  QPixmap m_Icon = QPixmap(QLatin1String(":/SIMPL/icons/images/caret-bottom.png"));
  QAction* m_ShowFileAction = nullptr;
  QString m_CurrentText = "";
  bool m_OneSelectionOnly = false;

  /**
   * @brief Updates the QGraphicsView based on the current Data Dimension and Data record values
   * @param path The path to the HDF data set
   */
  void _updateViewFromHDFPath(const std::string& path);

  /**
   * @brief updateAttributeTable
   * @param datasetPath
   * @return
   */
  herr_t updateAttributeTable(const QString& datasetPath);

  /**
   * @brief updateGeneralTable
   * @param path
   * @return
   */
  herr_t updateGeneralTable(const QString& path);

  /**
   * @brief updateComponentDimensions
   * @param datasetPath
   * @return
   */
  herr_t updateComponentDimensions(const QString& datasetPath);

  /**
   * @brief addRow
   * @param table
   * @param row
   * @param key
   * @param value
   */
  void addRow(QTableWidget* table, int row, const QString& key, const QString& value);

  /**
   * @brief setHDF5Path
   * @param hdf5Path
   */
  void setHDF5Path(const QString &hdf5Path);

  /**
   * @brief Breaks the specified number, n, down into its prime factors.  These are numbers that are
   * only divisible by 1 and themselves.
   * @param n The number to calculate the prime factors of.
   * @param primeFactors The vector to store the prime factors into.
   * @return
   */
  void calculatePrimeFactors(int n, QVector<int>& primeFactors);

public:
  HDF5DatasetSelectionWidget(const HDF5DatasetSelectionWidget&) = delete; // Copy Constructor Not Implemented
  HDF5DatasetSelectionWidget(HDF5DatasetSelectionWidget&&) = delete;      // Move Constructor Not Implemented
  HDF5DatasetSelectionWidget& operator=(const HDF5DatasetSelectionWidget&) = delete; // Copy Assignment Not Implemented
  HDF5DatasetSelectionWidget& operator=(HDF5DatasetSelectionWidget&&) = delete;      // Move Assignment Not Implemented
};
