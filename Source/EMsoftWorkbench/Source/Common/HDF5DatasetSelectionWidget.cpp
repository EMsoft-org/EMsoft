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

#include "HDF5DatasetSelectionWidget.h"

#include <cmath>

#include <QtCore/QJsonObject>
#include <QtCore/QDir>
#include <QtCore/QMimeData>

#include <QtGui/QKeyEvent>
#include <QtGui/QPainter>

#include <QtWidgets/QMenu>
#include <QtWidgets/QFileDialog>

#include "Common/HDF5FileTreeModel.h"
#include "Common/SVStyle.h"
#include "Common/Constants.h"

#include "H5Support/QH5Lite.h"
#include "H5Support/QH5Utilities.h"

#include "QtSupport/QtSFileCompleter.h"
#include "QtSupport/QtSFileUtils.h"

#include "SIMPLib/Utilities/SIMPLDataPathValidator.h"

namespace ioConstants = EMsoftWorkbenchConstants::IOStrings;

// Initialize private static member variable
QString HDF5DatasetSelectionWidget::m_OpenDialogLastDirectory = "";

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5DatasetSelectionWidget::HDF5DatasetSelectionWidget(QWidget* parent)
: QWidget(parent)
, m_Ui(new Ui::HDF5DatasetSelectionWidget())
, m_CurrentOpenFile("")
, m_CurrentHDFDataPath("")
, m_FileId(-1)
{
  m_Ui->setupUi(this);
  setupGui();

  m_OpenDialogLastDirectory = QDir::homePath();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5DatasetSelectionWidget::~HDF5DatasetSelectionWidget()
{
  if(m_FileId > 0)
  {
    H5Fclose(m_FileId);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setIcon(const QPixmap& path)
{
  m_Icon = path;
  setupMenuField();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QPixmap HDF5DatasetSelectionWidget::getIcon()
{
  return m_Icon;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString HDF5DatasetSelectionWidget::getCurrentFile() const
{
  return m_CurrentOpenFile;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setupGui()
{
  setErrorText("");

  QtSFileCompleter* com = new QtSFileCompleter(this, false);
  m_Ui->value->setCompleter(com);
  QObject::connect(com, SIGNAL(activated(QString)), this, SLOT(listenFileTextChanged(QString)));

  connect(m_Ui->selectBtn, &QPushButton::clicked, this, &HDF5DatasetSelectionWidget::listenSelectBtnClicked);
  connect(m_Ui->value, &QtSLineEdit::fileDropped, this, &HDF5DatasetSelectionWidget::listenFileDropped);
  connect(m_Ui->value, &QtSLineEdit::textChanged, this, &HDF5DatasetSelectionWidget::listenFileTextChanged);
  connect(m_Ui->value, &QtSLineEdit::returnPressed, this, &HDF5DatasetSelectionWidget::listenFileReturnPressed);
  connect(m_Ui->value, &QtSLineEdit::editingFinished, this, &HDF5DatasetSelectionWidget::listenFileEditingFinished);

  setupMenuField();

  QFont inputFileFont;
  inputFileFont.setBold(true);
  inputFileFont.setItalic(true);
  inputFileFont.setWeight(75);
  inputFileFont.setStyleStrategy(QFont::PreferAntialias);
#if defined(Q_OS_MAC)
  inputFileFont.setPointSize(12);
#elif defined(Q_OS_WIN)
  inputFileFont.setPointSize(9);
#else
  inputFileFont.setPointSize(10);
#endif

  m_Ui->value->setFont(inputFileFont);

  m_CurrentText = m_Ui->value->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::readParameters(QJsonObject& obj)
{
  QJsonObject widgetObj = obj[ioConstants::HDF5DatasetSelectionSettings].toObject();

  m_Ui->value->blockSignals(true);

  m_Ui->value->setText(widgetObj[ioConstants::HDF5DatasetSelectionInputFile].toString());
  setValue(m_Ui->value->text());
  parametersChanged();

  m_Ui->value->blockSignals(false);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::writeParameters(QJsonObject& obj)
{
  QJsonObject widgetObj;

  widgetObj[ioConstants::HDF5DatasetSelectionInputFile] = m_Ui->value->text();

  obj[ioConstants::HDF5DatasetSelectionSettings] = widgetObj;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::keyPressEvent(QKeyEvent* event)
{
  if(event->key() == Qt::Key_Escape)
  {
    SVStyle* style = SVStyle::Instance();
    m_Ui->value->setText(m_CurrentText);
    style->LineEditClearStyle(m_Ui->value);
    m_Ui->value->setToolTip("");
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setupMenuField()
{
  QFileInfo fi(m_Ui->value->text());

  QMenu* lineEditMenu = new QMenu(m_Ui->value);
  m_Ui->value->setButtonMenu(QtSLineEdit::Left, lineEditMenu);

  m_Ui->value->setButtonVisible(QtSLineEdit::Left, true);

  QPixmap pixmap(8, 8);
  pixmap.fill(Qt::transparent);
  QPainter painter(&pixmap);
  painter.drawPixmap(0, (pixmap.height() - m_Icon.height()) / 2, m_Icon);
  m_Ui->value->setButtonPixmap(QtSLineEdit::Left, pixmap);

  {
    m_ShowFileAction = new QAction(lineEditMenu);
    m_ShowFileAction->setObjectName(QString::fromUtf8("showFileAction"));
#if defined(Q_OS_WIN)
    m_ShowFileAction->setText("Show in Windows Explorer");
#elif defined(Q_OS_MAC)
    m_ShowFileAction->setText("Show in Finder");
#else
    m_ShowFileAction->setText("Show in File System");
#endif
    lineEditMenu->addAction(m_ShowFileAction);
//    connect(m_ShowFileAction, SIGNAL(triggered()), this, SLOT(showFileInFileSystem()));
  }

  if(!m_Ui->value->text().isEmpty() && fi.exists())
  {
    m_ShowFileAction->setEnabled(true);
  }
  else
  {
    m_ShowFileAction->setDisabled(true);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenFileEditingFinished()
{
  setValue(m_Ui->value->text());
  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenFileReturnPressed()
{
  setValue(m_Ui->value->text());
  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenFileTextChanged(const QString& text)
{
  SIMPLDataPathValidator* validator = SIMPLDataPathValidator::Instance();
  QString inputPath = validator->convertToAbsolutePath(text);

  QFileInfo fi(text);
  if(fi.isRelative())
  {
    m_Ui->absPathLabel->setText(inputPath);
  }

  if(QtSFileUtils::HasValidFilePath(inputPath))
  {
    m_ShowFileAction->setEnabled(true);
  }
  else
  {
    m_ShowFileAction->setDisabled(true);
  }

  SVStyle* style = SVStyle::Instance();

  if(text != m_CurrentText)
  {
    style->LineEditBackgroundErrorStyle(m_Ui->value);
    m_Ui->value->setToolTip("Press the 'Return' key to apply your changes");
  }
  else
  {
    style->LineEditClearStyle(m_Ui->value);
    m_Ui->value->setToolTip("");
  }

  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenFileDropped(const QString& text)
{
  setValue(text);
  parametersChanged();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setHDF5Path(const QString &hdf5Path)
{
  // Automatically expand HDF groups in the viewer
  HDF5FileTreeModel* treeModel = dynamic_cast<HDF5FileTreeModel*>(m_Ui->hdfTreeView->model());
  if(treeModel != nullptr)
  {
    QStringList hdf5PathTokens = hdf5Path.split("/", QString::SkipEmptyParts);
    QModelIndex parentIdx = treeModel->index(0, 0);
    m_Ui->hdfTreeView->expand(parentIdx);
    treeModel->setData(parentIdx, Qt::Checked, Qt::CheckStateRole);
    while(!hdf5PathTokens.empty())
    {
      QString hdf5PathToken = hdf5PathTokens.front();
      QString dsetToken = hdf5PathTokens.back();
      hdf5PathTokens.pop_front();
      QModelIndexList idxList = treeModel->match(treeModel->index(0, 0, parentIdx), Qt::DisplayRole, hdf5PathToken);
      if(!idxList.empty())
      {
        QModelIndex foundIdx = idxList[0];
        m_Ui->hdfTreeView->expand(foundIdx);

        if(treeModel->data(foundIdx, Qt::DisplayRole).toString() == dsetToken)
        {
          treeModel->setData(foundIdx, Qt::Checked, Qt::CheckStateRole);
        }

        parentIdx = foundIdx;
      }
      else
      {
        hdf5PathTokens.clear();
      }
    }

    // Select the dataset
    m_Ui->hdfTreeView->setCurrentIndex(parentIdx);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenSelectBtnClicked()
{
  QString s = QString("HDF5 Files (*.hdf5 *.h5);;All Files(*.*)");
  QString file = QFileDialog::getOpenFileName(this, tr("Select %1").arg(m_Ui->inputFileLabel->text()), m_OpenDialogLastDirectory, s);

  if(file.isEmpty())
  {
    return;
  }

  file = QDir::toNativeSeparators(file);

  if(initWithFile(file))
  {
    m_Ui->value->blockSignals(true);
    m_Ui->value->setText(file);
    m_Ui->value->blockSignals(false);
    parametersChanged();
  }

  // Store the last used directory into the private instance variable
  QFileInfo fi(file);
  m_OpenDialogLastDirectory = fi.path();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::dragEnterEvent(QDragEnterEvent* e)
{
  const QMimeData* dat = e->mimeData();
  QList<QUrl> urls = dat->urls();
  QString file = urls.count() != 0 ? urls[0].toLocalFile() : QString();
  QDir parent(file);
  m_OpenDialogLastDirectory = parent.dirName();
  QFileInfo fi(file);
  QString ext = fi.suffix();
  if(fi.exists() && fi.isFile() && ((ext.compare("dream3d") != 0) || (ext.compare("h5") != 0) || (ext.compare("hdf5") != 0)))
  {
    e->accept();
  }
  else
  {
    e->ignore();
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::dropEvent(QDropEvent* e)
{
  const QMimeData* dat = e->mimeData();
  QList<QUrl> urls = dat->urls();
  QString file = urls.count() != 0 ? urls[0].toLocalFile() : QString();
  QDir parent(file);
  m_OpenDialogLastDirectory = parent.dirName();
  QFileInfo fi(file);
  QString ext = fi.suffix();
  file = QDir::toNativeSeparators(file);
  if(fi.exists() && fi.isFile() && ((ext.compare("h5") != 0) || (ext.compare("hdf5") != 0) || (ext.compare("dream3d") != 0)))
  {
    QDir parent(file);
    m_OpenDialogLastDirectory = parent.dirName();
    initWithFile(file);
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool HDF5DatasetSelectionWidget::initWithFile(const QString& hdf5File)
{
  if(hdf5File.isNull())
  {
    return false;
  }

  // If there is current file open, close it.
  if(m_FileId >= 0)
  {
    herr_t err = H5Utilities::closeFile(m_FileId);
    if(err < 0)
    {
      QString msg;
      QTextStream out(&msg);
      out << "Error closing HDF5 file: " << m_CurrentOpenFile;
      setErrorText(msg);
      return false;
    }
  }

  setErrorText("");
  m_FileId = H5Utilities::openFile(hdf5File.toStdString(), true);
  if(m_FileId < 0)
  {
    QString msg;
    QTextStream out(&msg);
    out << "Error Reading HDF5 file: " << hdf5File;
    setErrorText(msg);
    return false;
  }
  // Delete the old model
  QAbstractItemModel* oldModel = m_Ui->hdfTreeView->model();

  delete oldModel;

  // Save the last place the user visited while opening the file
  QString nativeHdf5File = QDir::toNativeSeparators(hdf5File);

  QFileInfo fileInfo(nativeHdf5File);
  m_OpenDialogLastDirectory = fileInfo.path();
  m_CurrentOpenFile = nativeHdf5File;

  // Set the Window Title to the file name
  setWindowTitle(fileInfo.fileName());

  // Get the HDF5FileTreeModel and set the Root Node
  HDF5FileTreeModel* treeModel = new HDF5FileTreeModel(m_FileId, m_Ui->hdfTreeView);
  treeModel->setOneSelectionOnly(m_OneSelectionOnly);
  connect(treeModel, SIGNAL(selectedHDF5PathsChanged()), this, SIGNAL(parametersChanged()));
  m_Ui->hdfTreeView->setModel(treeModel);
#if defined(Q_OS_MAC)
  m_Ui->hdfTreeView->setAttribute(Qt::WA_MacShowFocusRect, false);
#endif

  QModelIndex rootIndex = treeModel->index(0, 0);
  if(rootIndex.isValid())
  {
    m_Ui->hdfTreeView->setExpanded(rootIndex, true);
  }

  // Connect the Tree View selection Model to a method in this class
  connect(m_Ui->hdfTreeView->selectionModel(), &QItemSelectionModel::currentChanged, this, &HDF5DatasetSelectionWidget::listenHDFTreeViewCurrentChanged);

  m_Ui->attributesTable->horizontalHeader()->setStretchLastSection(true); // Stretch the last column to fit to the viewport

//  initializeHDF5Paths();
  return true;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::_updateViewFromHDFPath(const std::string& dataPath)
{
  m_CurrentHDFDataPath = dataPath;
  QString message("Current Dataset:");
  message.append(dataPath.c_str());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::listenHDFTreeViewCurrentChanged(const QModelIndex& current, const QModelIndex& previous)
{
  setErrorText("");

  // Check to make sure we have a data file opened
  if(m_FileId < 0)
  {
    QString msg;
    QTextStream out(&msg);
    out << "No data file is opened";
    setErrorText(msg);
    return;
  }

  HDF5FileTreeModel* model = static_cast<HDF5FileTreeModel*>(m_Ui->hdfTreeView->model());
  QString path = model->indexToHDF5Path(current);

  m_CurrentHDFDataPath = path.toStdString();

  updateGeneralTable(path);
  updateAttributeTable(path);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t HDF5DatasetSelectionWidget::updateGeneralTable(const QString& path)
{
  setErrorText("");
  std::string datasetPath = path.toStdString();
  std::string objName = H5Utilities::extractObjectName(datasetPath);
  QString objType;

  herr_t err = 0;
  if(m_FileId < 0)
  {
    QString msg;
    QTextStream out(&msg);
    out << "Error: FileId is Invalid: " << m_FileId;
    setErrorText(msg);
    return m_FileId;
  }
  // Test for Dataset Existance
  H5O_info_t statbuf;
  err = H5Oget_info_by_name(m_FileId, datasetPath.c_str(), &statbuf, H5P_DEFAULT);
  bool isGroup = false;
  if(err < 0)
  {
    m_Ui->generalTable->clearContents(); // Clear the attributes Table
    QString msg;
    QTextStream out(&msg);
    out << "Data Set Does NOT exist at Path: " << path << "\nFileId: " << m_FileId;
    setErrorText(msg);
    return err;
  }
  m_Ui->generalTable->clearContents(); // Clear the attributes Table
  // Put the basic info in first
  m_Ui->generalTable->setRowCount(4);
  m_Ui->generalTable->setColumnCount(2);
  m_Ui->generalTable->verticalHeader()->hide();
  m_Ui->generalTable->horizontalHeader()->hide();

#if 0
  QStringList headerLabels;
  headerLabels.insert(0, tr("Key"));
  headerLabels.insert(1, tr("m_Ui->value"));
  m_Ui->generalTable->setHorizontalHeaderLabels( headerLabels );
#endif

  switch(statbuf.type)
  {
  case H5O_TYPE_GROUP:
    isGroup = true;
    objType = "HDF5 Group";
    break;
  case H5O_TYPE_DATASET:
    isGroup = false;
    objType = "HDF5 Dataset";
    break;
  case H5O_TYPE_NAMED_DATATYPE:
    isGroup = false;
    objType = "Named DataType";
    break;
  default:
    isGroup = false;
    objType = "UNKNOWN";
  }
  // Update the attributes table
  int row = 0;

  addRow(m_Ui->generalTable, row, "Name", QString::fromStdString(objName));
  ++row;
  addRow(m_Ui->generalTable, row, "Path", path);
  ++row;
  addRow(m_Ui->generalTable, row, "Type", objType);
  ++row;

  QString objectIdString = QString::number(statbuf.addr);
  addRow(m_Ui->generalTable, row, "Object ID", objectIdString);
  ++row;

  if(!isGroup)
  {
    m_Ui->generalTable->setRowCount(7);
    H5T_class_t classType;
    size_t classSize;
    std::vector<hsize_t> dims;
    err = H5Lite::getDatasetInfo(m_FileId, datasetPath, dims, classType, classSize);
    if(err < 0)
    {
      QString msg;
      QTextStream out(&msg);
      out << "Could not get dataset info for " << QString::fromStdString(datasetPath);
      setErrorText(msg);
      return err;
    }

    addRow(m_Ui->generalTable, row, "No. of Dimension(s)", QString::number(dims.size()));
    ++row;

    QString key;
    int index = 0;
    for(hsize_t dim : dims)
    {
      if(index > 0)
      {
        key += " x ";
      }
      key += QString::number(dim);

      ++index;
    }
    addRow(m_Ui->generalTable, row, "Dimensions Size(s)", key);
    row++;
    hid_t typeId = H5Lite::getDatasetType(m_FileId, datasetPath);
    QString theType = QString::fromStdString(H5Lite::StringForHDFClassType(classType));
    err = H5Tclose(typeId);
    addRow(m_Ui->generalTable, row, "Data Type", theType);
    row++;
  }
  else
  {
    m_Ui->generalTable->setRowCount(5);
    hid_t grpId = H5Gopen(m_FileId, datasetPath.c_str(), H5P_DEFAULT);
    hsize_t num_objs = 0;

    H5G_info_t group_info;
    err = H5Gget_info(grpId, &group_info);
    if(err < 0)
    {
      QString msg;
      QTextStream out(&msg);
      out << "Error getting number of objects for group: " << grpId;
      setErrorText(msg);
      return err;
    }
    num_objs = group_info.nlinks;

    addRow(m_Ui->generalTable, row, "Number of Members", QString::number(num_objs));
    ++row;

    H5Gclose(grpId);
  }

  m_Ui->generalTable->resizeColumnsToContents();
  m_Ui->generalTable->horizontalHeader()->setStretchLastSection(true);

  return err;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::addRow(QTableWidget* table, int row, const QString& key, const QString& value)
{

  QTableWidgetItem* keyItem = new QTableWidgetItem(key);
  table->setItem(row, 0, keyItem);
  QTableWidgetItem* valueItem = new QTableWidgetItem(value);
  table->setItem(row, 1, valueItem);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t HDF5DatasetSelectionWidget::updateAttributeTable(const QString& path)
{
  QString objName = QH5Utilities::extractObjectName(path);
  setErrorText("");
  herr_t err = 0;
  if(m_FileId < 0)
  {
    QString msg;
    QTextStream out(&msg);
    out << "Error: FileId is Invalid: " << m_FileId;
    setErrorText(msg);
    return m_FileId;
  }
  // Test for Dataset Existance
  H5O_info_t statbuf;
  err = H5Oget_info_by_name(m_FileId, path.toStdString().c_str(), &statbuf, H5P_DEFAULT);
  if(err < 0)
  {
    m_Ui->attributesTable->clearContents(); // Clear the attributes Table
    QString msg;
    QTextStream out(&msg);
    out << "Data Set Does NOT exist at Path: " << path << "\nFileId: " << m_FileId;
    setErrorText(msg);
    return err;
  }

  // Update the attributes table
  if(nullptr != m_Ui->attributesTable)
  {
    m_Ui->attributesTable->clearContents(); // Clear the attributes Table
    std::list<std::string> attributes;
    err = H5Utilities::getAllAttributeNames(m_FileId, path.toStdString(), attributes);
    if(err < 0)
    {
      QString msg;
      QTextStream out(&msg);
      out << "Error Reading Attributes for datasetPath: " << path;
      setErrorText(msg);
    }
    else
    {
      m_Ui->attributesTable->setRowCount(attributes.size());
      m_Ui->attributesTable->setColumnCount(2);
      m_Ui->attributesTable->verticalHeader()->hide();
      QStringList headerLabels;
      headerLabels.insert(0, tr("Name"));
      headerLabels.insert(1, tr("Value"));
      m_Ui->attributesTable->setHorizontalHeaderLabels(headerLabels);
      qint32 row = 0;
      // for(std::list<std::string>::iterator iter = attributes.begin(); iter != attributes.end(); iter++)
      for(const auto& keyStdStr : attributes)
      {
        QString key = QString::fromStdString(keyStdStr);

        QString parentPath = "";
        if(path == "/")
        {
          parentPath = path;
        }
        else
        {
          parentPath = QH5Utilities::getParentPath(path);
        }

        hid_t parentLoc = QH5Utilities::openHDF5Object(m_FileId, parentPath);

        QVector<hsize_t> dims;
        H5T_class_t classType;
        size_t typeSize;
        hid_t attributeId;
        QH5Lite::getAttributeInfo(parentLoc, objName, key, dims, classType, typeSize, attributeId);

        QString strData = "";
        herr_t err = 0;

        if(classType == H5T_NO_CLASS)
        {
          strData = QString("H5T_NO_CLASS: Not Parsed");
        }
        if(classType == H5T_INTEGER)
        {
          int data = 0;
          err = QH5Lite::readScalarAttribute(parentLoc, objName, key, data);
          strData = QString::number(data);
        }
        if(classType == H5T_FLOAT)
        {
          float data = 0.0f;
          err = QH5Lite::readScalarAttribute(parentLoc, objName, key, data);
          strData = QString::number(data);
        }
        if(classType == H5T_TIME)
        {
          strData = QString("H5T_TIME: Not Parsed");
        }
        if(classType == H5T_STRING)
        {
          err = QH5Lite::readStringAttribute(parentLoc, objName, key, strData);
          if(err < 0)
          {
            strData = QString("H5T_STRING: Not Parsed");
          }
        }
        if(classType == H5T_BITFIELD)
        {
          strData = QString("H5T_BITFIELD: Not Parsed");
        }
        if(classType == H5T_OPAQUE)
        {
          strData = QString("H5T_OPAQUE: Not Parsed");
        }
        if(classType == H5T_COMPOUND)
        {
          strData = QString("H5T_COMPOUND: Not Parsed");
        }
        if(classType == H5T_REFERENCE)
        {
          strData = QString("H5T_REFERENCE: Not Parsed");
        }
        if(classType == H5T_ENUM)
        {
          strData = QString("H5T_ENUM: Not Parsed");
        }
        if(classType == H5T_VLEN)
        {
          strData = QString("H5T_VLEN: Not Parsed");
        }
        if(classType == H5T_ARRAY)
        {
          strData = QString("H5T_ARRAY: Not Parsed");
        }

        QH5Utilities::closeHDF5Object(parentLoc);

        QTableWidgetItem* keyItem = new QTableWidgetItem(key);
        m_Ui->attributesTable->setItem(row, 0, keyItem);

        QTableWidgetItem* valueItem = new QTableWidgetItem(strData);
        m_Ui->attributesTable->setItem(row, 1, valueItem);

        ++row;
      }
    }
  }
  return err;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::calculatePrimeFactors(int n, QVector<int>& primeFactors)
{
  // Find and store all the 2s that n is divisible by
  while(n % 2 == 0)
  {
    primeFactors.push_back(2);
    n = n / 2;
  }

  // At this point, n must be odd.  So we can skip one element (Note: i = i + 2)
  for(int i = 3; i <= std::sqrt(n); i = i + 2)
  {
    // Store i if n is divisible by i
    while(n % i == 0)
    {
      primeFactors.push_back(i);
      n = n / i;
    }
  }

  // Handle the edge case when the remaining n m_Ui->value is a prime number greater than 2
  if(n > 2)
  {
    primeFactors.push_back(n);
  }
}

//// -----------------------------------------------------------------------------
////
//// -----------------------------------------------------------------------------
//void HDF5DatasetSelectionWidget::filterNeedsInputParameters(AbstractFilter* filter)
//{
//  Q_UNUSED(filter)

//  m_Filter->setHDF5FilePath(m_Ui->value->text());

//  HDF5FileTreeModel* treeModel = dynamic_cast<HDF5FileTreeModel*>(m_Ui->hdfTreeView->model());
//  if(treeModel != nullptr)
//  {
//    QStringList dsetPaths = treeModel->getSelectedHDF5Paths();
//    QList<ImportHDF5Dataset::DatasetImportInfo> importInfoList;
//    for(int i = 0; i < dsetPaths.size(); i++)
//    {
//      ImportHDF5Dataset::DatasetImportInfo importInfo;
//      importInfo.dataSetPath = dsetPaths[i];
//      importInfo.componentDimensions = m_ComponentDimsMap[dsetPaths[i]];
//      importInfoList.push_back(importInfo);
//    }
//    m_Filter->setDatasetImportInfoList(importInfoList);
//  }
//}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setValue(const QString& text)
{
  SIMPLDataPathValidator* validator = SIMPLDataPathValidator::Instance();
  QString inputPath = validator->convertToAbsolutePath(text);
  m_CurrentText = text;

  // Set/Remove the red outline if the file does exist
  if(!QtSFileUtils::VerifyPathExists(inputPath, m_Ui->value))
  {
    return;
  }

  QFileInfo fi(m_Ui->value->text());
  if(fi.isRelative())
  {
    m_Ui->absPathLabel->setText(inputPath);
    m_Ui->absPathLabel->show();
  }
  else
  {
    m_Ui->absPathLabel->hide();
  }

  QString file = QDir::toNativeSeparators(inputPath);

  if(initWithFile(file))
  {
    m_Ui->value->setText(file);
    // Store the last used directory into the private instance variable
    QFileInfo fi(file);
    m_OpenDialogLastDirectory = fi.path();
    setErrorText("");
  }
  m_Ui->errorLabel->update();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString HDF5DatasetSelectionWidget::getValue()
{
  if(m_Ui->value->text().isEmpty())
  {
    return QDir::homePath();
  }
  return m_Ui->value->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setErrorText(const QString& value)
{
  m_Ui->errorLabel->setText(value);
  m_Ui->errorLabel->setVisible(!value.isEmpty());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString HDF5DatasetSelectionWidget::getErrorText() const
{
  return m_Ui->errorLabel->text();
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setInputFileLabelText(const QString &text)
{
  m_Ui->inputFileLabel->setText(text);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool HDF5DatasetSelectionWidget::isDatasetSelectionEnabled() const
{
  return (m_Ui->hdfTreeView->isEnabled() && m_Ui->importHDF5DatasetTabWidget->isEnabled());
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setDatasetSelectionEnabled(bool enabled)
{
  m_Ui->hdfTreeView->setEnabled(enabled);
  m_Ui->importHDF5DatasetTabWidget->setEnabled(enabled);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5DatasetSelectionWidget::setOneSelectionOnly(bool value)
{
  HDF5FileTreeModel* model = dynamic_cast<HDF5FileTreeModel*>(m_Ui->hdfTreeView->model());
  if (model != nullptr)
  {
    model->setOneSelectionOnly(value);
  }

  m_OneSelectionOnly = value;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QStringList HDF5DatasetSelectionWidget::getSelectedHDF5Paths() const
{
  QStringList hdf5Paths;
  HDF5FileTreeModel* model = dynamic_cast<HDF5FileTreeModel*>(m_Ui->hdfTreeView->model());
  if (model != nullptr)
  {
    hdf5Paths = model->getSelectedHDF5Paths();
  }

  return hdf5Paths;
}
