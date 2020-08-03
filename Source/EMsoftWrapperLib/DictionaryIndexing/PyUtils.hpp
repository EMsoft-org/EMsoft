#ifndef _PyUtils_C_H_
#define _PyUtils_C_H_

#include <Python.h> // Must be first
#include <vector>
#include <string>
#include <stdexcept>

#ifdef __cplusplus
extern "C" {
#endif

/* Convert float (single precision) vector in C/C++ to float (double precision) list in Python */
PyObject* vectorToList_Float(const std::vector<float> &data);


/* Convert float (single precision) vector in C/C++ to float (double precision) tuple in Python */
PyObject* vectorToTuple_Float(const std::vector<float> &data);


/* Convert 2D float (single precision) array (vector of vectors of float) in C/C++ to
2D float (double precision) tuple (tuple of tuples of float) in Python */
PyObject* vectorVectorToTuple_Float(const std::vector< std::vector< float > > &data);


/* Convert float (double precision) list/tuple in Python to float (single precision) vector in C/C++ */
std::vector<float> listTupleToVector_Float(PyObject* incoming);


/* Convert int list/tuple in Python to int vector in C/C++ */
std::vector<int> listTupleToVector_Int(PyObject* incoming);


/* Convert str list/tuple in Python to str vector in C/C++ */
std::vector<std::string> listTupleToVector_Str(PyObject* incoming);


/* Create large 2D float arrays;
    User calling this function is responsible for freeing the memory.*/
float** alloc_2d_float(int rows, int cols);


/* Create large 2D int32 arrays;
    User calling this function is responsible for freeing the memory.*/
int32_t** alloc_2d_int32_t(int rows, int cols);


/* Create large 2D double arrays;
    User calling this function is responsible for freeing the memory.*/
double** alloc_2d_double(int rows, int cols);

#ifdef __cplusplus
}
#endif

#endif