#ifndef _PyUtils_C_H_
#define _PyUtils_C_H_

#include <Python.h> // Must be first
#include <vector>
#include <string>
#include <stdexcept>

#ifdef __cplusplus
extern "C" {
#endif

using namespace std;

// =====
// LISTS
// =====
PyObject* vectorToList_Float(const vector<float> &data);

// ======
// TUPLES
// ======
PyObject* vectorToTuple_Float(const vector<float> &data);

PyObject* vectorVectorToTuple_Float(const vector< vector< float > > &data);

// PyObject -> Vector
vector<float> listTupleToVector_Float(PyObject* incoming);

// PyObject -> Vector
vector<int> listTupleToVector_Int(PyObject* incoming);

// PyObject -> Vector
vector<string> listTupleToVector_Str(PyObject* incoming);

// Create large 2D arrays
float** alloc_2d_float(int rows, int cols);

// Create large 2D arrays
int32_t** alloc_2d_int32_t(int rows, int cols);

// Create large 2D arrays
double** alloc_2d_double(int rows, int cols);

#ifdef __cplusplus
}
#endif

#endif