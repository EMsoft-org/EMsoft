#pragma once

#include <hdf5.h>

#include "H5Support/H5Support.h"

#if defined (H5Support_NAMESPACE)
namespace H5Support_NAMESPACE
{
#endif

/**
* @brief This class is meant to disable the normal HDF5 error handlers until the
* instance goes out of scope the original error handlers will be put back in
* place
*/
class H5Support_EXPORT H5ScopedErrorHandler
{

  public:
  
    H5ScopedErrorHandler();
    
    ~H5ScopedErrorHandler();
    
  private:
    herr_t (*_oldHDF_error_func)(hid_t, void *);
    void *_oldHDF_error_client_data;

  public:
    H5ScopedErrorHandler(const H5ScopedErrorHandler&) = delete; // Copy Constructor Not Implemented
    H5ScopedErrorHandler(H5ScopedErrorHandler&&) = delete;      // Move Constructor Not Implemented
    H5ScopedErrorHandler& operator=(const H5ScopedErrorHandler&) = delete; // Copy Assignment Not Implemented
    H5ScopedErrorHandler& operator=(H5ScopedErrorHandler&&) = delete;      // Move Assignment Not Implemented
};


#if defined (H5Support_NAMESPACE)
}
#endif

