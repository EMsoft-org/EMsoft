#include <Python.h> // Must be first
#include <vector>
#include <string>
#include <stdexcept>
#include "PyUtils.hpp"


/* Convert float (single precision) vector in C/C++ to float (double precision) list in Python */
PyObject* vectorToList_Float(const std::vector<float> &data) {
  PyObject* listObj = PyList_New( data.size() );
    if (!listObj) throw std::logic_error("Unable to allocate memory for Python list");
    for (unsigned int i = 0; i < data.size(); i++) {
        PyObject *num = PyFloat_FromDouble( static_cast<double>(data[i]));
        if (!num) {
            Py_DECREF(listObj);
            throw std::logic_error("Unable to allocate memory for Python list");
        }
        PyList_SET_ITEM(listObj, i, num);
    }
    return listObj;
}


/* Convert float (single precision) vector in C/C++ to float (double precision) tuple in Python */
PyObject* vectorToTuple_Float(const std::vector<float> &data) {
    PyObject* tuple = PyTuple_New( data.size() );
    if (!tuple) throw std::logic_error("Unable to allocate memory for Python tuple");
    for (unsigned int i = 0; i < data.size(); i++) {
        PyObject *num = PyFloat_FromDouble( static_cast<double>(data[i]));
        if (!num) {
            Py_DECREF(tuple);
            throw std::logic_error("Unable to allocate memory for Python tuple");
        }
        PyTuple_SET_ITEM(tuple, i, num);
    }

    return tuple;
}


/* Convert 2D float (single precision) array (vector of vectors of float) in C/C++ to
2D float (double precision) tuple (tuple of tuples of float) in Python */
PyObject* vectorVectorToTuple_Float(const std::vector< std::vector< float > > &data) {
    PyObject* tuple = PyTuple_New( data.size() );
    if (!tuple) throw std::logic_error("Unable to allocate memory for Python tuple");
    for (unsigned int i = 0; i < data.size(); i++) {
        PyObject* subTuple = NULL;
        try {
            subTuple = vectorToTuple_Float(data[i]);
        } catch (std::logic_error &e) {
            throw e;
        }
        if (!subTuple) {
            Py_DECREF(tuple);
            throw std::logic_error("Unable to allocate memory for Python tuple of tuples");
        }
        PyTuple_SET_ITEM(tuple, i, subTuple);
    }

    return tuple;
}


/* Convert float (double precision) list/tuple in Python to float (single precision) vector in C/C++ */
std::vector<float> listTupleToVector_Float(PyObject* incoming) {
    std::vector<float> data;
    if (PyTuple_Check(incoming)) {
        for(Py_ssize_t i = 0; i < PyTuple_Size(incoming); i++) {
            PyObject *value = PyTuple_GetItem(incoming, i);
            data.push_back( static_cast<float>(PyFloat_AsDouble(value)) );
        }
    } else {
        if (PyList_Check(incoming)) {
            for(Py_ssize_t i = 0; i < PyList_Size(incoming); i++) {
                PyObject *value = PyList_GetItem(incoming, i);
                data.push_back( static_cast<float>(PyFloat_AsDouble(value)) );
            }
        } else {
            throw std::logic_error("Passed PyObject pointer was not a list or tuple of float!");
        }
    }
    return data;
}


/* Convert int list/tuple in Python to int vector in C/C++ */
std::vector<int> listTupleToVector_Int(PyObject* incoming) {
    std::vector<int> data;
    if (PyTuple_Check(incoming)) {
        for(Py_ssize_t i = 0; i < PyTuple_Size(incoming); i++) {
            PyObject *value = PyTuple_GetItem(incoming, i);
            data.push_back( static_cast<int>(PyFloat_AsDouble(value)) );
        }
    } else {
        if (PyList_Check(incoming)) {
            for(Py_ssize_t i = 0; i < PyList_Size(incoming); i++) {
                PyObject *value = PyList_GetItem(incoming, i);
                data.push_back( static_cast<int>(PyFloat_AsDouble(value)) );
            }
        } else {
            throw std::logic_error("Passed PyObject pointer was not a list or tuple of int!");
        }
    }
    return data;
}


/* Convert str list/tuple in Python to str vector in C/C++ */
std::vector<std::string> listTupleToVector_Str(PyObject* incoming) {
    std::vector<std::string> data;
    if (PyTuple_Check(incoming)) {
        for(Py_ssize_t i = 0; i < PyTuple_Size(incoming); i++) {
            PyObject *value = PyTuple_GetItem(incoming, i);
            data.push_back((std::string) PyUnicode_AsUTF8(value) );
        }
    } else {
        if (PyList_Check(incoming)) {
            for(Py_ssize_t i = 0; i < PyList_Size(incoming); i++) {
                PyObject *value = PyList_GetItem(incoming, i);
                data.push_back((std::string) PyUnicode_AsUTF8(value) );
            }
        } else {
            throw std::logic_error("Passed PyObject pointer was not a list or tuple of string!");
        }
    }
    return data;
}


/* Create large 2D float arrays;
    User calling this function is responsible for freeing the memory.*/
float** alloc_2d_float(int rows, int cols){
    float *data = (float *)malloc(rows*cols*sizeof(float));
    float **array= (float **)malloc(rows*sizeof(float*));
    for (int i=0; i<rows; i++)
        array[i] = &(data[cols*i]);

    return array;
}


/* Create large 2D int32 arrays;
    User calling this function is responsible for freeing the memory.*/
int32_t** alloc_2d_int32_t(int rows, int cols){
    int32_t *data = (int32_t *)malloc(rows*cols*sizeof(int32_t));
    int32_t **array= (int32_t **)malloc(rows*sizeof(int32_t*));
    for (int i=0; i<rows; i++)
        array[i] = &(data[cols*i]);

    return array;
}


/* Create large 2D double arrays;
    User calling this function is responsible for freeing the memory.*/
double** alloc_2d_double(int rows, int cols){
    double *data = (double *)malloc(rows*cols*sizeof(double));
    double **array= (double **)malloc(rows*sizeof(double*));
    for (int i=0; i<rows; i++)
        array[i] = &(data[cols*i]);

    return array;
}