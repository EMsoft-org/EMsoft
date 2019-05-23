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

#include "HDF5FileTreeModelItem.h"

#include <QtCore/QStringList>

#include "H5Support/H5Utilities.h"

HDF5FileTreeModelItem::HDF5FileTreeModelItem(hid_t fileId, const QString& data, HDF5FileTreeModelItem* parent)
: m_ItemData(QVariant(data))
, m_ParentItem(parent)
, m_FileId(fileId)
{
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModelItem::~HDF5FileTreeModelItem()
{
  qDeleteAll(m_ChildItems);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModelItem::appendChild(HDF5FileTreeModelItem* item)
{
  m_ChildItems.append(item);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModelItem* HDF5FileTreeModelItem::child(int row)
{
  if(m_ChildItemsInitialized == 0)
  {
    initializeChildItems();
  }
  return m_ChildItems.value(row);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModelItem::childCount()
{
  if(m_ChildCount < 0)
  {
    initializeChildCount();
  }
  return m_ChildCount;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModelItem::columnCount()
{
  return 1;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QVariant HDF5FileTreeModelItem::data(int column)
{
  return m_ItemData;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
HDF5FileTreeModelItem* HDF5FileTreeModelItem::parent()
{
  return m_ParentItem;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModelItem::row()
{
  if(m_ParentItem != nullptr)
  {
    return m_ParentItem->m_ChildItems.indexOf(const_cast<HDF5FileTreeModelItem*>(this));
  }

  return 0;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool HDF5FileTreeModelItem::isGroup()
{
  if(m_ChildCount < 0)
  {
    initializeChildCount();
  }
  return m_IsGroup;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
Qt::CheckState HDF5FileTreeModelItem::getCheckState()
{
  return m_CheckState;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModelItem::setCheckState(Qt::CheckState checkState)
{
  m_CheckState = checkState;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
int HDF5FileTreeModelItem::numAttributes()
{
  if(m_NumAttrs < 0)
  {
    initializeChildCount();
  }
  return m_NumAttrs;
}

int HDF5FileTreeModelItem::numDimensions()
{
  return m_NumDims;
}

bool HDF5FileTreeModelItem::isImage()
{
  if(m_ChildItemsInitialized == 0)
  {
    initializeChildItems();
  }
  return m_IsImage;
}

bool HDF5FileTreeModelItem::isString()
{
  if(m_ChildItemsInitialized == 0)
  {
    initializeChildItems();
  }
  return m_IsString;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QIcon HDF5FileTreeModelItem::icon()
{
  QString iconName = "";

  if(isGroup())
  {
    iconName = ":/SIMPL/icons/images/folder_blue.png";
  }
  else
  {
    iconName = ":/SIMPL/icons/images/cube_molecule.png";
  }

  return QIcon(iconName);
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModelItem::initializeChildCount()
{
  if(m_FileId < 0)
  {
    return;
  }
  // std::cout << "HDF5FileTreeModelItem::initializeChildCount()" << std::endl;
  herr_t err = 0;
  // Build up an HDF path to the current group
  HDF5FileTreeModelItem* currentParent = m_ParentItem;
  if(nullptr == currentParent)
  {
    QString name("/"); // Add the "HDF5 Root Directory"
    HDF5FileTreeModelItem* item = new HDF5FileTreeModelItem(m_FileId, name, const_cast<HDF5FileTreeModelItem*>(this));
    m_ChildItems.append(item);
    m_ChildCount = 1;
    m_NumAttrs = 0;
    m_NumDims = 0;
    m_IsGroup = true;
    return;
  }

  QString path = generateHDFPath();

  hid_t obj_id = H5Utilities::openHDF5Object(m_FileId, path.toStdString());
  if(obj_id > 0)
  {
    H5O_info_t object_info;
    err = H5Oget_info(obj_id, &object_info);
    m_NumAttrs = object_info.num_attrs;
  }
  H5Utilities::closeHDF5Object(obj_id);

  if(H5Utilities::isGroup(m_FileId, path.toStdString()))
  {
    m_IsGroup = true;
    hid_t groupId = H5Gopen(m_FileId, path.toStdString().c_str(), H5P_DEFAULT);
    if(groupId < 0)
    {
      std::cout << "Could not open Group '" << path.toStdString() << "'" << __FILE__ << ":" << __LINE__ << std::endl;
      return;
    }

    H5G_info_t group_info;
    err = H5Gget_info(groupId, &group_info);
    m_ChildCount = group_info.nlinks;

    err = H5Gclose(groupId);
  }
  else
  {
    m_ChildCount = 0;
  }
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void HDF5FileTreeModelItem::initializeChildItems()
{
  if(m_FileId < 0)
  {
    return;
  }
  // std::cout << "HDF5FileTreeModelItem::initializeChildItems()" << std::endl;
  herr_t err = 0;
  // Build up an HDF path to the current group
  HDF5FileTreeModelItem* currentParent = m_ParentItem;
  if(nullptr == currentParent)
  {
    QString name("/"); // Add the "HDF5 Root Directory"
    HDF5FileTreeModelItem* item = new HDF5FileTreeModelItem(m_FileId, name, const_cast<HDF5FileTreeModelItem*>(this));
    m_ChildItems.append(item);
    m_ChildItemsInitialized = 1;
    return;
  }

  QString path = generateHDFPath();

  // std::cout << "HDF5FileTreeModelItem::initializeChildItems() - Generated Path as: " << path.toStdString() << std::endl;
  // Check to see if the path is a group or data set
  if(H5Utilities::isGroup(m_FileId, path.toStdString()))
  {
    m_IsGroup = true;
    hid_t groupId = H5Gopen(m_FileId, path.toStdString().c_str(), H5P_DEFAULT);
    if(groupId < 0)
    {
      std::cout << "Could not open Group '" << path.toStdString() << "'" << __FILE__ << ":" << __LINE__ << std::endl;
      return;
    }

    H5O_info_t object_info;
    err = H5Oget_info(groupId, &object_info);
    m_NumAttrs = object_info.num_attrs;

    std::list<std::string> itemList;
    herr_t err = H5Utilities::getGroupObjects(groupId, H5Utilities::H5Support_ANY, itemList);
    if(err < 0)
    {
      std::cout << "Error getting group objects. " << __FILE__ << ":" << __LINE__ << std::endl;
      err = H5Gclose(groupId);
      return;
    }

    for(const std::string &nameStdString : itemList)
    {
      // std::cout << "Adding Child with name '" << *iter << "'" << std::endl;
      QString name(nameStdString.c_str());
      HDF5FileTreeModelItem* item = new HDF5FileTreeModelItem(m_FileId, name, const_cast<HDF5FileTreeModelItem*>(this));
      m_ChildItems.append(item);
    }
    m_ChildCount = static_cast<int32_t>(itemList.size());
    err = H5Gclose(groupId);
  }
  else // Get some basic information about the data set
  {
    // std::cout << "TreeModelItem is a DataSet" << std::endl;
    hid_t obj_id = H5Utilities::openHDF5Object(m_FileId, path.toStdString());
    if(obj_id > 0)
    {
      H5O_info_t object_info;
      err = H5Oget_info(obj_id, &object_info);
      m_NumAttrs = object_info.num_attrs;
    }
    if(m_NumAttrs > 0)
    {
      // Test for Image Class attribute
      std::string data;
      err = H5Lite::readStringAttribute(m_FileId, path.toStdString(), "CLASS", data);
      if(err >= 0)
      {
        m_IsImage = true;
      }
    }

    std::vector<hsize_t> dims;
    H5T_class_t data_type;
    size_t type_size;
    err = H5Lite::getDatasetInfo(m_FileId, path.toStdString(), dims, data_type, type_size);
    m_NumDims = static_cast<int32_t>(dims.size());

    switch(data_type)
    {
    case H5T_STRING:
      m_IsString = true;
      m_DataType = "H5T_STRING";
      break;
    default:
      m_DataType = "OTHER";
    }

    err = H5Utilities::closeHDF5Object(obj_id);
    if(err < 0)
    {
      // TODO: Catch this error
    }
  }
  m_ChildItemsInitialized = 1;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
QString HDF5FileTreeModelItem::generateHDFPath()
{
  if(m_FileId < 0)
  {
    return QString();
  }

  // Build up an HDF path to the current group
  HDF5FileTreeModelItem* currentParent = m_ParentItem;
  QString path = m_ItemData.toString();
  // std::cout << "Current Item Data: " << path.toStdString() << std::endl;
  if(currentParent != nullptr && currentParent->m_ItemData.toString().compare("HEADER") == 0)
  {
    // std::cout << "path=" << path.toStdString() << std::endl;
    currentParent = nullptr; // We are at the top
  }
  while(currentParent != nullptr)
  {
    QString parentName = currentParent->data(0).toString();
    if(parentName.compare("/") == 0)
    {
      path = parentName + path;
    }
    else
    {
      path = parentName + "/" + path;
    }
    // std::cout << "path=" << path.toStdString() << std::endl;
    // Get the parents Parent..
    currentParent = currentParent->parent();
    if(currentParent->parent() == nullptr)
    {
      currentParent = nullptr; // We are at the top
    }
  }
  return path;
}
