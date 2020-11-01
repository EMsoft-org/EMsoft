#ifndef _PyUtils_C_H_
#define _PyUtils_C_H_

#include <Python.h> // Must be first
#include <vector>
#include <string>
#include <stdexcept>

#ifdef __cplusplus
extern "C" {
#endif

/** 
 * Function: vectorToList_Float
 * 
 * @brief Convert stl vector of float (single precision) in C/C++ to list of float (double precision) in Python
 * 
 * @param data: incoming stl vector of float (single precision) in C/C++ to convert
 * 
 * @param output: the pointer to PyObject of list of float (double precision)
 */
PyObject* vectorToList_Float(const std::vector<float> &data);


/** 
 * Function: vectorToTuple_Float
 * 
 * @brief Convert stl vector of float (single precision) in C/C++ to tuple of float (double precision) in Python
 * 
 * @param data: incoming stl vector of float (single precision) in C/C++ to convert
 * 
 * @param output: the pointer to PyObject of tuple of float (double precision)
 */
PyObject* vectorToTuple_Float(const std::vector<float> &data);


/** 
 * Function: vectorVectorToTuple_Float
 * 
 * @brief Convert stl vector of vectors (2D array) of float (single precision) in C/C++ 
 *        to tuple of tuples (2D array) of float (double precision) in Python
 * 
 * @param data: incoming stl vector of vectors of float (single precision) in C/C++ to convert
 * 
 * @param output: the pointer to PyObject of tuple of tuples of float (double precision)
 */
PyObject* vectorVectorToTuple_Float(const std::vector<std::vector<float>> &data);


/** 
 * Function: listTupleToVector_Float
 * 
 * @brief Convert list/tuple of float (double precision) in Python 
 *        to stl vector of float (single precesion) in C/C++
 * 
 * @param incoming: the pointer to PyObject of tuple of float (double precision) to convert
 * 
 * @param output: stl vector of float (double precision) in C/C++
 */
std::vector<float> listTupleToVector_Float(PyObject* incoming);


/** 
 * Function: listTupleToVector_Int
 * 
 * @brief Convert list/tuple of int in Python 
 *        to stl vector of int in C/C++
 * 
 * @param incoming: the pointer to PyObject of tuple of int to convert
 * 
 * @param output: stl vector of int in C/C++
 */
std::vector<int> listTupleToVector_Int(PyObject* incoming);


/** 
 * Function: listTupleToVector_Str
 * 
 * @brief Convert list/tuple of str in Python 
 *        to stl vector of stl string in C++
 * 
 * @param incoming: the pointer to PyObject of tuple of str to convert
 * 
 * @param output: stl vector of stl string in C++
 */
std::vector<std::string> listTupleToVector_Str(PyObject* incoming);


/** 
 * Function: alloc_2d_float
 * 
 * @brief Allocate continuous memory for a 2D array of float in C/C++
 *        User calling this function is responsible for freeing the memory
 * 
 * @param rows: the number of rows, int
 * @param cols: the number of columns, int
 * 
 * @param output: pointer to allocated memory in C++
 */
float** alloc_2d_float(int rows, int cols);


/** 
 * Function: alloc_2d_int32_t
 * 
 * @brief Allocate continuous memory for a 2D array of int32 in C/C++
 *        User calling this function is responsible for freeing the memory
 * 
 * @param rows: the number of rows, int
 * @param cols: the number of columns, int
 * 
 * @param output: pointer to allocated memory in C++
 */
int32_t** alloc_2d_int32_t(int rows, int cols);


/** 
 * Function: alloc_2d_double
 * 
 * @brief Allocate continuous memory for a 2D array of double in C/C++
 *        User calling this function is responsible for freeing the memory
 * 
 * @param rows: the number of rows, int
 * @param cols: the number of columns, int
 * 
 * @param output: pointer to allocated memory in C++
 */
double** alloc_2d_double(int rows, int cols);

#ifdef __cplusplus
}
#endif

#endif