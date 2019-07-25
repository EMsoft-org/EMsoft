/* ============================================================================
* Copyright (c) 2009-2016 BlueQuartz Software, LLC
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

#include <H5Support/H5Lite.h>

#include <cstring>

#if defined(H5Support_NAMESPACE)
using namespace H5Support_NAMESPACE;
#endif

/*-------------------------------------------------------------------------
 * Function: find_dataset
 *
 * Purpose: operator function used by H5LTfind_dataset
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 21, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t find_dataset(hid_t  /*loc_id*/, const char* name, void* op_data)
{
  H5SUPPORT_MUTEX_LOCK()
  /* Define a default zero value for return. This will cause the iterator to continue if
   * the dataset is not found yet.
   */

  int32_t ret = 0;

  char* dset_name = reinterpret_cast<char*>(op_data);

  /* Shut the compiler up */
  // loc_id=loc_id;

  /* Define a positive value for return value if the dataset was found. This will
   * cause the iterator to immediately return that positive value,
   * indicating short-circuit success
   */

  if(strcmp(name, dset_name) == 0)
  {
    ret = 1;
  }

  return ret;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t find_attr(hid_t  /*loc_id*/, const char* name, const H5A_info_t*  /*info*/, void* op_data)
{
  H5SUPPORT_MUTEX_LOCK()
  /* Define a default zero value for return. This will cause the iterator to continue if
   * the palette attribute is not found yet.
   */

  int32_t ret = 0;

  char* attr_name = reinterpret_cast<char*>(op_data);

  /* Shut the compiler up */
  // loc_id=loc_id;

  /* Define a positive value for return value if the attribute was found. This will
   * cause the iterator to immediately return that positive value,
   * indicating short-circuit success
   */

  if(strcmp(name, attr_name) == 0)
  {
    ret = 1;
  }

  return ret;
}

// -----------------------------------------------------------------------------
//  Protected Constructor
// -----------------------------------------------------------------------------
H5Lite::H5Lite()
= default;

// -----------------------------------------------------------------------------
//  Protected Destructor
// -----------------------------------------------------------------------------
H5Lite::~H5Lite()
= default;

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
void H5Lite::disableErrorHandlers()
{
  H5SUPPORT_MUTEX_LOCK()

  HDF_ERROR_HANDLER_OFF;
}

// -----------------------------------------------------------------------------
//  Opens an ID for HDF5 operations
// -----------------------------------------------------------------------------
hid_t H5Lite::openId(hid_t loc_id, const std::string& obj_name, H5O_type_t obj_type)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t obj_id = -1;

  switch(obj_type)
  {
  case H5O_TYPE_DATASET:

    /* Open the dataset. */
    if((obj_id = H5Dopen(loc_id, obj_name.c_str(), H5P_DEFAULT)) < 0)
    {
      return -1;
    }
    break;

  case H5O_TYPE_GROUP:

    /* Open the group. */
    if((obj_id = H5Gopen(loc_id, obj_name.c_str(), H5P_DEFAULT)) < 0)
    {
      return -1;
    }
    break;

  default:
    return -1;
  }

  return obj_id;
}

// -----------------------------------------------------------------------------
//  Closes the given ID
// -----------------------------------------------------------------------------
herr_t H5Lite::closeId(hid_t obj_id, int32_t obj_type)
{
  H5SUPPORT_MUTEX_LOCK()

  switch(obj_type)
  {
  case H5O_TYPE_DATASET:
    /* Close the dataset. */
    if(H5Dclose(obj_id) < 0)
    {
      return -1;
    }
    break;

  case H5O_TYPE_GROUP:
    /* Close the group. */
    if(H5Gclose(obj_id) < 0)
    {
      return -1;
    }
    break;

  default:
    return -1;
  }

  return 1;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::string H5Lite::StringForHDFClassType(H5T_class_t classType)
{
  if(classType == H5T_NO_CLASS)
  {
    return "H5T_NO_CLASS";
  }
  if(classType == H5T_INTEGER)
  {
    return "H5T_INTEGER";
  }
  if(classType == H5T_FLOAT)
  {
    return "H5T_FLOAT";
  }
  if(classType == H5T_TIME)
  {
    return "H5T_TIME";
  }
  if(classType == H5T_STRING)
  {
    return "H5T_STRING";
  }
  if(classType == H5T_BITFIELD)
  {
    return "H5T_BITFIELD";
  }
  if(classType == H5T_OPAQUE)
  {
    return "H5T_OPAQUE";
  }
  if(classType == H5T_COMPOUND)
  {
    return "H5T_COMPOUND";
  }
  if(classType == H5T_REFERENCE)
  {
    return "H5T_REFERENCE";
  }
  if(classType == H5T_ENUM)
  {
    return "H5T_ENUM";
  }
  if(classType == H5T_VLEN)
  {
    return "H5T_VLEN";
  }
  if(classType == H5T_ARRAY)
  {
    return "H5T_ARRAY";
  }

  return "UNKNOWN";
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hid_t H5Lite::HDFTypeFromString(const std::string& value)
{
  H5SUPPORT_MUTEX_LOCK()

  if(value == "H5T_STRING")
  {
    return H5T_STRING;
  }

  if(value == "H5T_NATIVE_INT8")
  {
    return H5T_NATIVE_INT8;
  }
  if(value == "H5T_NATIVE_UINT8")
  {
    return H5T_NATIVE_UINT8;
  }

  if(value == "H5T_NATIVE_INT16")
  {
    return H5T_NATIVE_INT16;
  }
  if(value == "H5T_NATIVE_UINT16")
  {
    return H5T_NATIVE_UINT16;
  }

  if(value == "H5T_NATIVE_INT32")
  {
    return H5T_NATIVE_INT32;
  }
  if(value == "H5T_NATIVE_UINT32")
  {
    return H5T_NATIVE_UINT32;
  }

  if(value == "H5T_NATIVE_INT64")
  {
    return H5T_NATIVE_INT64;
  }
  if(value == "H5T_NATIVE_UINT64")
  {
    return H5T_NATIVE_UINT64;
  }

  if(value == "H5T_NATIVE_FLOAT")
  {
    return H5T_NATIVE_FLOAT;
  }
  if(value == "H5T_NATIVE_DOUBLE")
  {
    return H5T_NATIVE_DOUBLE;
  }

  std::cout << "Error: HDFTypeFromString - Unknown Type: " << value << std::endl;
  return -1;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
std::string H5Lite::StringForHDFType(hid_t dataTypeIdentifier)
{
  H5SUPPORT_MUTEX_LOCK()

  if(dataTypeIdentifier == H5T_STRING)
  {
    return "H5T_STRING";
  }

  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_INT8) != 0)
  {
    return "H5T_NATIVE_INT8";
  }
  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_UINT8) != 0)
  {
    return "H5T_NATIVE_UINT8";
  }

  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_INT16) != 0)
  {
    return "H5T_NATIVE_INT16";
  }
  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_UINT16) != 0)
  {
    return "H5T_NATIVE_UINT16";
  }

  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_INT32) != 0)
  {
    return "H5T_NATIVE_INT32";
  }
  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_UINT32) != 0)
  {
    return "H5T_NATIVE_UINT32";
  }

  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_INT64) != 0)
  {
    return "H5T_NATIVE_INT64";
  }
  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_UINT64) != 0)
  {
    return "H5T_NATIVE_UINT64";
  }

  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_FLOAT) != 0)
  {
    return "H5T_NATIVE_FLOAT";
  }
  if(H5Tequal(dataTypeIdentifier, H5T_NATIVE_DOUBLE) != 0)
  {
    return "H5T_NATIVE_DOUBLE";
  }

  std::cout << "Error: HDFTypeForPrimitiveAsStr - Unknown Type: " << dataTypeIdentifier << std::endl;
  return "Unknown";
}

// -----------------------------------------------------------------------------
//  Finds an Attribute given an object to look in
// -----------------------------------------------------------------------------
herr_t H5Lite::findAttribute(hid_t loc_id, const std::string& attrName)
{
  H5SUPPORT_MUTEX_LOCK()

  hsize_t attr_num;
  herr_t ret = 0;

  attr_num = 0;
  ret = H5Aiterate(loc_id, H5_INDEX_NAME, H5_ITER_INC, &attr_num, find_attr, (void*)(attrName.c_str()));

  return ret;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
bool H5Lite::datasetExists(hid_t loc_id, const std::string& dsetName)
{
  H5SUPPORT_MUTEX_LOCK()

  H5O_info_t ginfo{};
  HDF_ERROR_HANDLER_OFF
  herr_t err = H5Oget_info_by_name(loc_id, dsetName.c_str(), &ginfo, H5P_DEFAULT);
  HDF_ERROR_HANDLER_ON
  return err >= 0;
}

// -----------------------------------------------------------------------------
//  We assume a null terminated string
// -----------------------------------------------------------------------------
herr_t H5Lite::writeStringDataset(hid_t loc_id, const std::string& dsetName, size_t size, const char* data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did = -1;
  hid_t sid = -1;
  hid_t tid = -1;
  // size_t  size = 0;
  herr_t err = -1;
  herr_t retErr = 0;

  /* create a string data type */
  if((tid = H5Tcopy(H5T_C_S1)) >= 0)
  {
    if(H5Tset_size(tid, size) >= 0)
    {
      if(H5Tset_strpad(tid, H5T_STR_NULLTERM) >= 0)
      {
        /* Create the data space for the dataset. */
        if((sid = H5Screate(H5S_SCALAR)) >= 0)
        {
          /* Create the dataset. */
          if((did = H5Dcreate(loc_id, dsetName.c_str(), tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0)
          {
            if(nullptr != data)
            {
              err = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
              if(err < 0)
              {
                std::cout << "Error Writing String Data" << std::endl;
                retErr = err;
              }
            }
          }
          else
          {
            //retErr = did;
            retErr = 0;
          }
          CloseH5D(did, err, retErr);
        }
        CloseH5S(sid, err, retErr);
      }
    }
    CloseH5T(tid, err, retErr);
  }
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::writeVectorOfStringsDataset(hid_t loc_id, const std::string& dsetName, const std::vector<std::string>& data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t sid = -1;
  hid_t memspace = -1;
  hid_t datatype = -1;
  hid_t did = -1;
  herr_t err = -1;
  herr_t retErr = 0;

  hsize_t dims[1] = {data.size()};
  if((sid = H5Screate_simple(sizeof(dims) / sizeof(*dims), dims, nullptr)) >= 0)
  {
    dims[0] = 1;

    if((memspace = H5Screate_simple(sizeof(dims) / sizeof(*dims), dims, nullptr)) >= 0)
    {

      datatype = H5Tcopy(H5T_C_S1);
      H5Tset_size(datatype, H5T_VARIABLE);

      if((did = H5Dcreate(loc_id, dsetName.c_str(), datatype, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0)
      {

        //
        // Select the "memory" to be written out - just 1 record.
        hsize_t offset[] = {0};
        hsize_t count[] = {1};
        H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset, nullptr, count, nullptr);
        hsize_t m_pos = 0;
        for(auto v : data)
        {
          // Select the file position, 1 record at position 'pos'
          hsize_t count[] = {1};
          hsize_t offset[] = {m_pos++};
          H5Sselect_hyperslab(sid, H5S_SELECT_SET, offset, nullptr, count, nullptr);
          const char* s = v.c_str();
          err = H5Dwrite(did, datatype, memspace, sid, H5P_DEFAULT, &s);
          if(err < 0)
          {
            std::cout << "Error Writing String Data: " __FILE__ << "(" << __LINE__ << ")" << std::endl;
            retErr = err;
          }
        }
        CloseH5D(did, err, retErr);
      }
      H5Tclose(datatype);
      CloseH5S(memspace, err, retErr);
    }

    CloseH5S(sid, err, retErr);
  }
  return retErr;
}

// -----------------------------------------------------------------------------
//  Writes a string to a HDF5 dataset
// -----------------------------------------------------------------------------
herr_t H5Lite::writeStringDataset(hid_t loc_id, const std::string& dsetName, const std::string& data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did = -1;
  hid_t sid = -1;
  hid_t tid = -1;
  size_t size = 0;
  herr_t err = -1;
  herr_t retErr = 0;

  /* create a string data type */
  if((tid = H5Tcopy(H5T_C_S1)) >= 0)
  {
    size = data.size() + 1;
    if(H5Tset_size(tid, size) >= 0)
    {
      if(H5Tset_strpad(tid, H5T_STR_NULLTERM) >= 0)
      {
        /* Create the data space for the dataset. */
        if((sid = H5Screate(H5S_SCALAR)) >= 0)
        {
          /* Create or open the dataset. */
          HDF_ERROR_HANDLER_OFF
          did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT);
          HDF_ERROR_HANDLER_ON
          if(did < 0) // dataset does not exist so create it
          {
            did = H5Dcreate(loc_id, dsetName.c_str(), tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
          }

          if(did >= 0)
          {
            if(!data.empty())
            {
              err = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data.c_str());
              if(err < 0)
              {
                std::cout << "Error Writing String Data" << std::endl;
                retErr = err;
              }
            }
          }
          else
          {
            //retErr = did;
            retErr = 0;
          }
          CloseH5D(did, err, retErr);
          //          err = H5Dclose(did);
          //          if (err < 0) {
          //            std::cout << "Error Closing Dataset." << std::endl;
          //            retErr = err;
          //          }
        }
        CloseH5S(sid, err, retErr);
        //        err = H5Sclose(sid);
        //        if ( err < 0) {
        //          std::cout << "Error closing Dataspace." << std::endl;
        //          retErr = err;
        //        }
      }
    }
    CloseH5T(tid, err, retErr);
    //    err = H5Tclose(tid);
    //    if (err < 0 ) {
    //     std::cout << "Error closing DataType" << std::endl;
    //     retErr = err;
    //    }
  }
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::writeStringAttributes(hid_t loc_id, const std::string& objName, const std::map<std::string, std::string>& attributes)
{
  H5SUPPORT_MUTEX_LOCK()

  herr_t err = 0;
  for(const auto & attribute : attributes)
  {
    err = H5Lite::writeStringAttribute(loc_id, objName, attribute.first, attribute.second);
    if(err < 0)
    {
      return err;
    }
  }
  return err;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
hsize_t H5Lite::getNumberOfElements(hid_t loc_id, const std::string& dsetName)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did;
  herr_t err = 0;
  herr_t retErr = 0;
  hid_t spaceId;
  hsize_t numElements = 0;
  did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT);
  if(did < 0)
  {
    std::cout << "H5Lite.cpp::getNumberOfElements(" << __LINE__ << ") Error opening Dataset at loc_id (" << loc_id << ") with object name (" << dsetName << ")" << std::endl;
    return -1;
  }
  if(did >= 0)
  {
    spaceId = H5Dget_space(did);
    if(spaceId > 0)
    {
      int32_t rank = H5Sget_simple_extent_ndims(spaceId);
      if(rank > 0)
      {
        std::vector<hsize_t> dims;
        dims.resize(rank); // Allocate enough room for the dims
        err = H5Sget_simple_extent_dims(spaceId, &(dims.front()), nullptr);
        numElements = 1;
        for(auto iter = dims.begin(); iter < dims.end(); ++iter)
        {
          numElements = numElements * (*iter);
        }
      }
      err = H5Sclose(spaceId);
      if(err < 0)
      {
        std::cout << "Error Closing Data Space" << std::endl;
        retErr = err;
      }
    }
    else
    {
      std::cout << "Error Opening SpaceID" << std::endl;
      //retErr = spaceId;
    }
    err = H5Dclose(did);
    if(err < 0)
    {
      std::cout << "Error Closing Dataset" << std::endl;
      retErr = err;
    }
  }
  return numElements;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::writeStringAttribute(hid_t loc_id, const std::string& objName, const std::string& attrName, hsize_t size, const char* data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t attr_type;
  hid_t attr_space_id;
  hid_t attr_id;
  hid_t obj_id;
  int32_t has_attr;
  H5O_info_t statbuf{};
  size_t attr_size;
  herr_t err = 0;
  herr_t retErr = 0;

  /* Get the type of object */
  retErr = H5Oget_info_by_name(loc_id, objName.c_str(), &statbuf, H5P_DEFAULT);
  if(retErr >= 0)
  {
    /* Open the object */
    obj_id = H5Lite::openId(loc_id, objName, statbuf.type);
    if(obj_id >= 0)
    {
      /* Create the attribute */
      attr_type = H5Tcopy(H5T_C_S1);
      if(attr_type >= 0)
      {
        attr_size = size; /* extra null term */
        err = H5Tset_size(attr_type, attr_size);
        if(err < 0)
        {
          std::cout << "Error Setting H5T Size" << std::endl;
          retErr = err;
        }
        if(err >= 0)
        {
          err = H5Tset_strpad(attr_type, H5T_STR_NULLTERM);
          if(err < 0)
          {
            std::cout << "Error adding a null terminator." << std::endl;
            retErr = err;
          }
          if(err >= 0)
          {
            attr_space_id = H5Screate(H5S_SCALAR);
            if(attr_space_id >= 0)
            {
              /* Verify if the attribute already exists */
              has_attr = H5Lite::findAttribute(obj_id, attrName);
              /* The attribute already exists, delete it */
              if(has_attr == 1)
              {
                err = H5Adelete(obj_id, attrName.c_str());
                if(err < 0)
                {
                  std::cout << "Error Deleting Attribute '" << attrName << "' from Object '" << objName << "'" << std::endl;
                  retErr = err;
                }
              }
              if(err >= 0)
              {
                /* Create and write the attribute */
                attr_id = H5Acreate(obj_id, attrName.c_str(), attr_type, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
                if(attr_id >= 0)
                {
                  err = H5Awrite(attr_id, attr_type, data);
                  if(err < 0)
                  {
                    std::cout << "Error Writing String attribute." << std::endl;

                    retErr = err;
                  }
                }
                CloseH5A(attr_id, err, retErr);
              }
              CloseH5S(attr_space_id, err, retErr);
            }
          }
        }
        CloseH5T(attr_type, err, retErr);
      }
      else
      {
        //retErr = attr_type;
      }
      /* Close the object */
      err = H5Lite::closeId(obj_id, statbuf.type);
      if(err < 0)
      {
        std::cout << "Error Closing Object Id" << std::endl;
        retErr = err;
      }
    }
  }
  return retErr;
}

// -----------------------------------------------------------------------------
//  Writes a string to an HDF5 Attribute
// -----------------------------------------------------------------------------
herr_t H5Lite::writeStringAttribute(hid_t loc_id, const std::string& objName, const std::string& attrName, const std::string& data)
{
  return H5Lite::writeStringAttribute(loc_id, objName, attrName, data.size() + 1, data.data());
}

// -----------------------------------------------------------------------------
//  Reads a String dataset into a std::string
// -----------------------------------------------------------------------------
herr_t H5Lite::readStringDataset(hid_t loc_id, const std::string& dsetName, std::string& data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did; // dataset id
  hid_t tid; // type id
  herr_t err = 0;
  herr_t retErr = 0;
  hsize_t size;
  data.clear();
  did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT);
  if(did < 0)
  {
    std::cout << "H5Lite.cpp::readStringDataset(" << __LINE__ << ") Error opening Dataset at loc_id (" << loc_id << ") with object name (" << dsetName << ")" << std::endl;
    return -1;
  }
  /*
  * Get the datatype.
  */
  tid = H5Dget_type(did);
  if(tid >= 0)
  {
    htri_t isVariableString = H5Tis_variable_str(tid); // Test if the string is variable length

    if(isVariableString == 1)
    {
      std::vector<std::string> strings;
      err = readVectorOfStringDataset(loc_id, dsetName, strings); // Read the string
      if(err < 0 || (strings.size() > 1 && !strings.empty()))
      {
        std::cout << "Error Reading string dataset. There were multiple Strings and the program asked for a single string." << std::endl;
        retErr = err;
      }
      else
      {
        data.assign(strings[0]);
      }
    }
    else
    {
      size = H5Dget_storage_size(did);
      std::vector<char> buf(static_cast<int>(size + 1), 0x00); // Allocate and Zero and array
      err = H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &(buf.front()));
      if(err < 0)
      {
        std::cout << "Error Reading string dataset." << std::endl;
        retErr = err;
      }
      else
      {
        data.append(&(buf.front())); // Append the string to the given string
      }
    }
  }
  CloseH5D(did, err, retErr);
  CloseH5T(tid, err, retErr);
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::readStringDataset(hid_t loc_id, const std::string& dsetName, char* data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did; // dataset id
  hid_t tid; // type id
  herr_t err = 0;
  herr_t retErr = 0;

  did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT);
  if(did < 0)
  {
    std::cout << "H5Lite.cpp::readStringDataset(" << __LINE__ << ") Error opening Dataset at loc_id (" << loc_id << ") with object name (" << dsetName << ")" << std::endl;
    return -1;
  }
  tid = H5Dget_type(did);
  if(tid >= 0)
  {
    err = H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    if(err < 0)
    {
      std::cout << "Error Reading string dataset." << std::endl;
      retErr = err;
    }
    CloseH5T(tid, err, retErr);
  }
  CloseH5D(did, err, retErr);
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::readVectorOfStringDataset(hid_t loc_id, const std::string& dsetName, std::vector<std::string>& data)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did; // dataset id
  hid_t tid; // type id
  herr_t err = 0;
  herr_t retErr = 0;

  did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT);
  if(did < 0)
  {
    std::cout << "H5Lite.cpp::readVectorOfStringDataset(" << __LINE__ << ") Error opening Dataset at loc_id (" << loc_id << ") with object name (" << dsetName << ")" << std::endl;
    return -1;
  }
  /*
  * Get the datatype.
  */
  tid = H5Dget_type(did);
  if(tid >= 0)
  {
    hsize_t dims[1] = {0};
    /*
    * Get dataspace and allocate memory for read buffer.
    */
    hid_t sid = H5Dget_space(did);
    int ndims = H5Sget_simple_extent_dims(sid, dims, nullptr);
    if(ndims != 1)
    {
      CloseH5S(sid, err, retErr);
      CloseH5T(tid, err, retErr);
      std::cout << "H5Lite.cpp::readVectorOfStringDataset(" << __LINE__ << ") Number of dims should be 1 but it was " << ndims << ". Returning early. Is your data file correct?" << std::endl;
      return -2;
    }
    std::vector<char*> rdata(dims[0]);
    for(int i = 0; i < dims[0]; i++)
    {
      rdata[i] = nullptr;
    }

    /*
    * Create the memory datatype.
    */
    hid_t memtype = H5Tcopy(H5T_C_S1);
    herr_t status = H5Tset_size(memtype, H5T_VARIABLE);

    /*
    * Read the data.
    */
    status = H5Dread(did, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, &(rdata.front()));
    if(status < 0)
    {
      status = H5Dvlen_reclaim(memtype, sid, H5P_DEFAULT, &(rdata.front()));
      CloseH5S(sid, err, retErr);
      CloseH5T(tid, err, retErr);
      CloseH5T(memtype, err, retErr);
      std::cout << "H5Lite.cpp::readVectorOfStringDataset(" << __LINE__ << ") Error reading Dataset at loc_id (" << loc_id << ") with object name (" << dsetName << ")" << std::endl;
      return -3;
    }
    /*
    * copy the data into the vector of strings
    */
    data.resize(dims[0]);
    for(int i = 0; i < dims[0]; i++)
    {
      // printf("%s[%d]: %s\n", "VlenStrings", i, rdata[i].p);
      std::string str = std::string(rdata[i]);
      data[i] = str;
    }
    /*
    * Close and release resources.  Note that H5Dvlen_reclaim works
    * for variable-length strings as well as variable-length arrays.
    * Also note that we must still free the array of pointers stored
    * in rdata, as H5Tvlen_reclaim only frees the data these point to.
    */
    status = H5Dvlen_reclaim(memtype, sid, H5P_DEFAULT, &(rdata.front()));
    CloseH5S(sid, err, retErr);
    CloseH5T(tid, err, retErr);
    CloseH5T(memtype, err, retErr);
  }

  CloseH5D(did, err, retErr);

  return retErr;
}

// -----------------------------------------------------------------------------
//  Reads a string Attribute from the HDF file
// -----------------------------------------------------------------------------
herr_t H5Lite::readStringAttribute(hid_t loc_id, const std::string& objName, const std::string& attrName, std::string& data)
{
  H5SUPPORT_MUTEX_LOCK()

  /* identifiers */
  hid_t obj_id;
  H5O_info_t statbuf{};
  hid_t attr_id;
  hid_t attr_type;
  std::vector<char> attr_out;
  hsize_t size;
  herr_t err = 0;
  herr_t retErr = 0;
  data.clear();
  HDF_ERROR_HANDLER_OFF;

  /* Get the type of object */
  err = H5Oget_info_by_name(loc_id, objName.c_str(), &statbuf, H5P_DEFAULT);
  if(err < 0)
  {
    return err;
  }

  /* Open the object */
  obj_id = H5Lite::openId(loc_id, objName, statbuf.type);
  if(obj_id >= 0)
  {
    attr_id = H5Aopen_by_name(loc_id, objName.c_str(), attrName.c_str(), H5P_DEFAULT, H5P_DEFAULT);
    hid_t attrTypeId = H5Aget_type(attr_id);
    htri_t isVariableString = H5Tis_variable_str(attrTypeId); // Test if the string is variable length
    H5Tclose(attrTypeId);
    if(isVariableString == 1)
    {
      data.clear();
      retErr = -1;
      CloseH5A(attr_id, err, retErr);
      return retErr;
    }
    if(attr_id >= 0)
    {
      size = H5Aget_storage_size(attr_id);
      attr_out.resize(static_cast<int>(size)); // Resize the vector to the proper length
      attr_type = H5Aget_type(attr_id);
      if(attr_type >= 0)
      {
        err = H5Aread(attr_id, attr_type, &(attr_out.front()));
        if(err < 0)
        {
          std::cout << "Error Reading Attribute." << std::endl;
          retErr = err;
        }
        else
        {
          if(attr_out[size - 1] == 0) // nullptr Terminated string
          {
            size = size - 1;
          }
          data.append(&(attr_out.front()), size); // Append the data to the passed in string
        }
        CloseH5T(attr_type, err, retErr);
      }
      CloseH5A(attr_id, err, retErr);
    }
    else
    {
      //retErr = attr_id;
    }
    err = H5Lite::closeId(obj_id, statbuf.type);
    if(err < 0)
    {
      std::cout << "Error Closing Object ID" << std::endl;
      retErr = err;
    }
  }
  HDF_ERROR_HANDLER_ON;
  return retErr;
}

// -----------------------------------------------------------------------------
//  Reads a string Attribute from the HDF file
// -----------------------------------------------------------------------------
herr_t H5Lite::readStringAttribute(hid_t loc_id, const std::string& objName, const std::string& attrName, char* data)
{
  H5SUPPORT_MUTEX_LOCK()

  /* identifiers */
  hid_t obj_id;
  H5O_info_t statbuf{};
  hid_t attr_id;
  hid_t attr_type;
  herr_t err = 0;
  herr_t retErr = 0;

  HDF_ERROR_HANDLER_OFF;

  /* Get the type of object */
  err = H5Oget_info_by_name(loc_id, objName.c_str(), &statbuf, H5P_DEFAULT);
  if(err < 0)
  {
    return err;
  }

  /* Open the object */
  obj_id = H5Lite::openId(loc_id, objName, statbuf.type);
  if(obj_id >= 0)
  {
    attr_id = H5Aopen_by_name(loc_id, objName.c_str(), attrName.c_str(), H5P_DEFAULT, H5P_DEFAULT);
    if(attr_id >= 0)
    {
      attr_type = H5Aget_type(attr_id);
      if(attr_type >= 0)
      {
        err = H5Aread(attr_id, attr_type, data);
        if(err < 0)
        {
          std::cout << "Error Reading Attribute." << std::endl;
          retErr = err;
        }
        CloseH5T(attr_type, err, retErr);
      }
      CloseH5A(attr_id, err, retErr);
    }
    else
    {
      //retErr = attr_id;
    }
    err = H5Lite::closeId(obj_id, statbuf.type);
    if(err < 0)
    {
      std::cout << "Error Closing Object ID" << std::endl;
      retErr = err;
    }
  }
  HDF_ERROR_HANDLER_ON;
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::getDatasetNDims(hid_t loc_id, const std::string& dsetName, hid_t& rank)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did;
  hid_t sid;
  herr_t err = 0;
  herr_t retErr = 0;
  rank = 0;

  /* Open the dataset. */
  if((did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT)) < 0)
  {
    return -1;
  }

  /* Get the dataspace handle */
  sid = H5Dget_space(did);
  if(sid >= 0)
  {

    /* Get rank */
    rank = H5Sget_simple_extent_ndims(sid);
    if(rank < 0)
    {
      //retErr = rank;
      rank = 0;
      std::cout << "Error Getting the rank of the dataset:" << std::endl;
    }

    /* Terminate access to the dataspace */
    CloseH5S(sid, err, retErr);
  }

  /* End access to the dataset */
  err = H5Dclose(did);
  if(err < 0)
  {
    retErr = err;
    rank = 0;
  }
  return retErr;
}

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
herr_t H5Lite::getAttributeNDims(hid_t loc_id, const std::string& objName, const std::string& attrName, hid_t& rank)
{
  H5SUPPORT_MUTEX_LOCK()

  /* identifiers */
  hid_t obj_id;
  H5O_info_t statbuf{};
  hid_t attr_id;
  herr_t err = 0;
  herr_t retErr = 0;
  hid_t sid;
  rank = -1;
  /* Get the type of object */
  err = H5Oget_info_by_name(loc_id, objName.c_str(), &statbuf, H5P_DEFAULT);
  if(err < 0)
  {
    return err;
  }
  /* Open the object */
  obj_id = H5Lite::openId(loc_id, objName, statbuf.type);
  if(obj_id >= 0)
  {
    attr_id = H5Aopen_by_name(loc_id, objName.c_str(), attrName.c_str(), H5P_DEFAULT, H5P_DEFAULT);
    if(attr_id >= 0)
    {
      sid = H5Aget_space(attr_id);
      if(sid >= 0)
      {
        rank = H5Sget_simple_extent_ndims(sid);
        CloseH5S(sid, err, retErr);
      }
      CloseH5A(attr_id, err, retErr);
    }
    else
    {
      //retErr = attr_id;
    }
    err = H5Lite::closeId(obj_id, statbuf.type);
    if(err < 0)
    {
      std::cout << "Error Closing Object ID" << std::endl;
      retErr = err;
    }
  }

  return retErr;
}

// -----------------------------------------------------------------------------
//  Returns the type of data stored in the dataset. You MUST use H5Tclose(tid)
//  on the returned value or resource leaks will occur.
// -----------------------------------------------------------------------------
hid_t H5Lite::getDatasetType(hid_t loc_id, const std::string& dsetName)
{
  H5SUPPORT_MUTEX_LOCK()

  herr_t err = 0;
  herr_t retErr = 0;
  hid_t did = -1;
  /* Open the dataset. */
  if((did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT)) < 0)
  {
    return -1;
  }
  /* Get an identifier for the datatype. */
  hid_t tid = H5Dget_type(did);
  CloseH5D(did, err, retErr);
  if(retErr < 0)
  {
    return static_cast<hid_t>(retErr);
  }
  return tid;
}

// -----------------------------------------------------------------------------
//  Get the dataset information
// -----------------------------------------------------------------------------
herr_t H5Lite::getDatasetInfo(hid_t loc_id, const std::string& dsetName, std::vector<hsize_t>& dims, H5T_class_t& classType, size_t& sizeType)
{
  H5SUPPORT_MUTEX_LOCK()

  hid_t did;
  hid_t tid;
  hid_t sid;
  herr_t err = 0;
  herr_t retErr = 0;
  hid_t rank = 0;

  /* Open the dataset. */
  if((did = H5Dopen(loc_id, dsetName.c_str(), H5P_DEFAULT)) < 0)
  {
    return -1;
  }

  /* Get an identifier for the datatype. */
  tid = H5Dget_type(did);
  if(tid >= 0)
  {
    /* Get the class. */
    classType = H5Tget_class(tid);
    /* Get the size. */
    sizeType = H5Tget_size(tid);
    /* Release the datatype. */
    err = H5Tclose(tid);
    if(err < 0)
    {
      std::cout << "Error Closing H5Type" << std::endl;
      retErr = err;
    }
  }
  /* Get the dataspace handle */
  sid = H5Dget_space(did);
  if(sid >= 0)
  {
    /* Get the Number of Dimensions */
    rank = H5Sget_simple_extent_ndims(sid);
    if(rank > 0)
    {
      // hsize_t _dims[rank]; // allocate space for the dimensions
      std::vector<hsize_t> _dims(rank, 0);
      /* Get dimensions */
      err = H5Sget_simple_extent_dims(sid, &(_dims.front()), nullptr);
      if(err < 0)
      {
        std::cout << "Error Getting Simple Extents for dataset" << std::endl;
        retErr = err;
      }
      // Copy the dimensions into the dims vector
      dims.clear(); // Erase everything in the Vector
      dims.resize(rank);
      for(hid_t i = 0; i < rank; ++i)
      {
        dims[i] = static_cast<uint64_t>(_dims[i]);
      }
    }
    else if(classType == H5T_STRING)
    {
      dims.clear(); // Erase everything in the Vector
      dims.resize(1);
      dims[0] = sizeType;
    }
    /* Terminate access to the dataspace */
    CloseH5S(sid, err, retErr);
  }

  /* End access to the dataset */
  CloseH5D(did, err, retErr);
  return retErr;
}

// -----------------------------------------------------------------------------
//  You must close the attributeType argument or resource leaks will occur. Use
//  H5Tclose(tid); after your call to this method if you do not need the id for
//   anything.
// -----------------------------------------------------------------------------
herr_t H5Lite::getAttributeInfo(hid_t loc_id, const std::string& objName, const std::string& attrName, std::vector<hsize_t>& dims, H5T_class_t& type_class, size_t& type_size, hid_t& tid)
{
  H5SUPPORT_MUTEX_LOCK()

  /* identifiers */
  hid_t obj_id;
  H5O_info_t statbuf{};
  hid_t attr_id;
  herr_t err = 0;
  herr_t retErr = 0;
  hid_t sid;
  hid_t rank = -1;

  err = H5Oget_info_by_name(loc_id, objName.c_str(), &statbuf, H5P_DEFAULT);
  if(err < 0)
  {
    return err;
  }

  /* Open the object */
  obj_id = H5Lite::openId(loc_id, objName, statbuf.type);
  if(obj_id >= 0)
  {
    attr_id = H5Aopen_by_name(loc_id, objName.c_str(), attrName.c_str(), H5P_DEFAULT, H5P_DEFAULT);
    if(attr_id >= 0)
    {
      /* Get an identifier for the datatype. */
      tid = H5Aget_type(attr_id);
      if(tid > 0)
      {
        /* Get the class. */
        type_class = H5Tget_class(tid);
        /* Get the size. */
        type_size = H5Tget_size(tid);
        sid = H5Aget_space(attr_id);
        if(sid >= 0)
        {
          if(type_class == H5T_STRING)
          {
            rank = 1;
            dims.resize(1);
            dims[0] = type_size;
          }
          else
          {
            rank = H5Sget_simple_extent_ndims(sid);
            std::vector<hsize_t> _dims(rank, 0);
            /* Get dimensions */
            err = H5Sget_simple_extent_dims(sid, &(_dims.front()), nullptr);
            if(err < 0)
            {
              std::cout << "Error Getting Attribute dims" << std::endl;
              retErr = err;
            }
            // Copy the dimensions into the dims vector
            dims.clear(); // Erase everything in the Vector
            dims.resize(rank);
            for(hid_t i = 0; i < rank; ++i)
            {
              dims[i] = static_cast<uint64_t>(_dims[i]);
            }
          }
          CloseH5S(sid, err, retErr);
          sid = 0;
        }
      }
      CloseH5A(attr_id, err, retErr);
      attr_id = 0;
    }
    else
    {
      retErr = -1;
    }
    err = H5Lite::closeId(obj_id, statbuf.type);
    if(err < 0)
    {
      std::cout << "Error Closing Object ID" << std::endl;
      retErr = err;
    }
  }
  return retErr;
}
