#include <Python.h>
#define PY_SSIZE_T_CLEAN
#define NPY_NO_DEPRECATED_API NPY_API_VERSION
#include "numpy/arrayobject.h"
#include "numpy/npy_math.h"

#include <stdio.h>
#include <iostream>

#include <stdexcept>
#include <vector>

#include "EMsoftDIwrappers.h"
#include "EMsoftDIwrappersCallbacks.h"
#include "PyUtils.hpp"

// #if defined(__cplusplus)
// extern "C" {void PyEMEBSDDI(int32_t* ipar, float* fpar, char* spar, float* dpatterns, float* epatterns, 
//     float* resultmain, int32_t* indexmain, size_t object, bool* cancel);}
// #endif


/*
*************************************
Fortran/C function description
*************************************
void EMsoftCEBSDDI
	(int32_t* ipar, float* fpar, char* spar, float* dpatterns, float* epatterns, 
	 float* resultmain, int32_t* indexmain, ProgCallBackTypeDI3 callback, ProgCallBackTypeError errorcallback,
     size_t object, bool* cancel); 
*/

/**
* EBSD Dictionary indexing (all in ram) wrapper routine
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param spar array with string input parameters
* @param dpatterns array with pre-processed dictionary patterns
* @param epatterns array with pre-processed experimental patterns
* @param resultmain array with top N dot product values
* @param indexmain array with euler angle indices for top N dot product values
* @param callback callback routine to update progress bar
* @param errorcallback callback routine to report OpenCL error code
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/


PyObject* PyEMEBSDDI(PyObject* self, PyObject* args, PyObject* kw){ 
    /*
    *************************************
    1. Data conversion from Python to C
    *************************************
    */
    static char* kwlist[] = {(char *)"ipar", (char *)"fpar", (char *)"spar", (char *)"dpatterns", (char *)"epatterns", (char *)"obj", (char *)"cancel", NULL};
    PyObject* Py_ipar = NULL;
    PyObject* Py_fpar = NULL;
    PyObject* Py_spar = NULL;

    PyObject* Py_dpatterns = NULL;
    PyObject* Py_epatterns = NULL;
    
    PyObject* Py_object = PyLong_FromLong(0);
    PyObject* Py_cancel = Py_False;

    if (!PyArg_ParseTupleAndKeywords(args, kw, "OOOOO|OO", kwlist,
        &Py_ipar, &Py_fpar, &Py_spar, &Py_dpatterns, &Py_epatterns, &Py_object, &Py_cancel))
    {
        std::cout << "Python arguments parse error" << std::endl;
        return NULL;
    }

    // ipar
    std::vector<int> ipar_vector = listTupleToVector_Int(Py_ipar);
    int32_t* ipar = ipar_vector.data();
    // long ipar = PyLong_AsLong((PyObject*) Py_ipar);
    Py_XDECREF(Py_ipar);
    // std::cout << "ipar done" << std::endl;

    // fpar
    std::vector<float> fpar_vector = listTupleToVector_Float(Py_fpar);
    float* fpar = fpar_vector.data();
    // double fpar = PyFloat_AsDouble((PyObject*) Py_fpar);
    // Py_XDECREF(Py_fpar);
    // std::cout << "fpar done" << std::endl;

    // object
    size_t object = PyLong_AsSize_t((PyObject*) Py_object);
    Py_XDECREF(Py_object);
    // std::cout << "object done" << std::endl;

    // cancel
    bool cancel;
    if(PyObject_IsTrue((PyObject*) Py_cancel)){
        cancel = true;
    }
    else{
        cancel = false;
    }
    Py_XDECREF(Py_cancel);
    // std::cout << "cancel done" << std::endl;

    // spar
    std::vector<std::string> spar_vector = listTupleToVector_Str((PyObject*) Py_spar);
    static char spar[80][512]; // 80 str with at most 512 char each;
    for (int i=0; i<80; ++i){
        memset(&spar[i][0], 0, 512);
    }

    for(int i=0; i<spar_vector.size(); ++i){
        // strcpy(&spar[i*80], spar_vector[i].c_str());
        strcpy(&spar[i][0], spar_vector[i].c_str());
    }
    
    Py_XDECREF(Py_spar);
    // std::cout << "spar done" << std::endl;
    

    // dpatterns
    int typenum = NPY_DOUBLE; // Python float has double precision
    PyArray_Descr *descr = PyArray_DescrFromType(typenum);
    double** dpatterns_double;
    npy_intp dpatterns_dims[2];

    float** dpatterns = alloc_2d_float(ipar[42], ipar[41]);

    // PyObject* dpatterns_obj = PyArray_FROM_OTF(Py_dpatterns, typenum, NPY_ARRAY_C_CONTIGUOUS);

    if (PyArray_AsCArray(&Py_dpatterns, &dpatterns_double, // numpy obj and pointer to array;
                    dpatterns_dims, 2, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, dpatterns");
        return NULL;
    }
    Py_XDECREF(Py_dpatterns);

    for (int i = 0; i < ipar[42]; i++){
        for (int j = 0; j < ipar[41]; j++){
            dpatterns[i][j] = static_cast<float>(dpatterns_double[i][j]);
        }
    }
    // std::cout << "dpatterns done" << std::endl;


    // epatterns
    double** epatterns_double;
    npy_intp epatterns_dims[2];
    float** epatterns = alloc_2d_float(ipar[39], ipar[41]);

    // PyObject* epatterns_obj = PyArray_FROM_OTF(Py_epatterns, typenum, NPY_ARRAY_C_CONTIGUOUS);

    if (PyArray_AsCArray(&Py_epatterns, &epatterns_double, // numpy obj and pointer to array;
                    epatterns_dims, 2, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, epatterns");
        return NULL;
    }
    Py_XDECREF(Py_epatterns);

    for (int i = 0; i < ipar[39]; i++){
        for (int j = 0; j < ipar[41]; j++){
            epatterns[i][j] = static_cast<float>(epatterns_double[i][j]);
        }
    }
    // std::cout << "epatterns done" << std::endl;


    /*
    *************************************
    2. Only for debug in development
    *************************************
    */
    // debug*******************************
    // std::cout << "test" << std::endl;
    // for(int i=0; i<ipar_vector.size(); ++i)
    //     std::cout << ipar[i] << ' ';
    // for(int i=0; i<fpar_vector.size(); ++i)
    //     std::cout << fpar[i] << ' ';
    // std::cout << object << std::endl;
    // std::cout << cancel << std::endl;
    // std::cout << "test done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "spar test" << std::endl;
    // for(int i=0; i<spar_vector.size(); ++i){
    //     std::cout << i+1 << std::endl;
    //     for (int j=0; j<80; ++j){
    //         std::cout << spar[i*80+j];
    //     }
    //     std::cout << std::endl;
    //     std::cout << spar_vector[i] << std::endl;
    // }
    // std::cout << "spar test done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_array" << std::endl;
    // std::cout << (int) PyArray_NDIM((PyArrayObject*) dpatterns_obj) << std::endl;
    // std::cout << (int) PyArray_DIMS((PyArrayObject*) dpatterns_obj)[0] << std::endl;
    // std::cout << dpatterns[0][0] << std::endl;
    // std::cout << dpatterns[1][1] << std::endl;
    // std::cout << "test_array done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_array output" << std::endl;
    // for (int i=0; i<(int) PyArray_DIMS((PyArrayObject*) epatterns_obj)[0]; i++){
    //     for (int j=0; j<(int) PyArray_DIMS((PyArrayObject*) epatterns_obj)[0]; j++){
    //         std::cout << epatterns[i][j];
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_array output done" << std::endl;
    // debug*******************************
    

    /*
    *************************************
    3. Initialize result array
    *************************************
    */
    // resultmain
    // 39 and 41 if starting from 1
    int row = ipar[40];
    int column = ipar[38];
    float** resultmain = alloc_2d_float(row, column);

    // indexmain
    int32_t** indexmain = alloc_2d_int32_t(row, column);


    /*
    *************************************
    4. Initialize callbacks
    *************************************
    */
    ProgCallBackTypeDI3 callback = &DIProcessTiming;
    ProgCallBackTypeError errorcallback = &DIProcessError;


    /*
    *************************************
    5. Call Fortran/C function
    *************************************
    */
    EMsoftCEBSDDI(ipar, fpar, &spar[0][0], &dpatterns[0][0], &epatterns[0][0],
        &resultmain[0][0], &indexmain[0][0], callback, errorcallback, object, &cancel);

    // std::cout << "Fortran running done" << std::endl;

    /*
    *************************************
    6. Collect and return results
    *************************************
    */
    // resultmain (convert to double and PyArrayObject)
    npy_intp resultmain_dims[2] = {row,column};

    double** resultmain_double = alloc_2d_double(row, column);
    for (int i = 0; i < row; ++i) {
        for (int j = 0; j < column; ++j){
            resultmain_double[i][j] = (double) resultmain[i][j];
        }
    }

    // PyArray_SimpleNew allocates the memory needed for the array.
    PyObject* Py_resultmain = PyArray_SimpleNew(2, resultmain_dims, typenum);
    // The pointer to the array data is accessed using PyArray_DATA()
    double* resultmain_p = (double *) PyArray_DATA((PyArrayObject*) Py_resultmain);
    // Copy the data from the "array of arrays" to the contiguous numpy array.
    for (int i = 0; i < row; ++i) {
        memcpy(resultmain_p, resultmain_double[i], sizeof(double) * column);
        resultmain_p += column;
    }


    // indexmain (convert to double and PyArrayObject)
    npy_intp indexmain_dims[2] = {row,column};

    double** indexmain_double = alloc_2d_double(row, column);
    for (int i = 0; i < row; ++i) {
        for (int j = 0; j < column; ++j){
            indexmain_double[i][j] = (double) indexmain[i][j];
        }
    }

    // PyArray_SimpleNew allocates the memory needed for the array.
    PyObject* Py_indexmain = PyArray_SimpleNew(2, indexmain_dims, typenum);
    // The pointer to the array data is accessed using PyArray_DATA()
    double* indexmain_p = (double *) PyArray_DATA((PyArrayObject*) Py_indexmain);
    // Copy the data from the "array of arrays" to the contiguous numpy array.
    for (int i = 0; i < row; ++i) {
        memcpy(indexmain_p, indexmain_double[i], sizeof(double) * column);
        indexmain_p += column;
    }


    // return [Py_resultmain, Py_indexmain]
    PyObject* ReturnList = PyList_New(2);
    if (PyList_SetItem(ReturnList, 0, PyArray_Return((PyArrayObject*) Py_resultmain)) < 0){
        PyErr_SetString(PyExc_Exception, "error setting Py_resultmain(PyArrayObject*) into ReturnList.");
        return NULL;
    }
    if (PyList_SetItem(ReturnList, 1, PyArray_Return((PyArrayObject*) Py_indexmain)) < 0){
        PyErr_SetString(PyExc_Exception, "error setting Py_indexmain(PyArrayObject*) into ReturnList.");
        return NULL;
    }
    
    // free all malloc
    free(dpatterns[0]);
    free(dpatterns);
    free(epatterns[0]);
    free(epatterns);
    free(resultmain[0]);
    free(resultmain);
    free(indexmain[0]);
    free(indexmain);
    free(resultmain_double[0]);
    free(resultmain_double);
    free(indexmain_double[0]);
    free(indexmain_double);

    return ReturnList;
    // Py_RETURN_NONE;
} 


/*
*************************************
Fortran/C function description
*************************************
void EMsoftCEBSDRefine(size_t* ipar, float* fpar, int32_t* accum_e, float* mLPNH, float* mLPSH,
    float* variants, float* epatterns, float* startEulers, float* startdps, float* eumain, 
    float* dpmain, ProgCallBackTypeDI2 callback, size_t object, bool* cancel);
*/

/**
* EBSD indexing refinement (all in ram) wrapper routine
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param accum_e array with Monte Carlo histogram
* @param mLPNH Northern hemisphere master pattern
* @param mLPSH Southern hemisphere master pattern
* @param variants array with quaternions defining the potential pseudosymmetry variants
* @param epatterns array with pre-processed experimental patterns
* @param startEulers array with initial Euler angle triplets
* @param startdps array with initial dot product values
* @param eumain array with refined Euler angle triplets
* @param dpmain array with refined dot product values
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/

PyObject* PyEMEBSDRefine(PyObject* self, PyObject* args, PyObject* kw){
    /*
    *************************************
    1. Data conversion from Python to C
    *************************************
    */
    static char* kwlist[] = {(char *)"ipar", (char *)"fpar", (char *)"accum_e", (char *)"mLPNH", (char *)"mLPSH",
                            (char *)"variants", (char *)"epatterns", (char *)"startEulers", (char *)"startdps",
                            (char *)"obj", (char *)"cancel", NULL};
    PyObject* Py_ipar = NULL;
    PyObject* Py_fpar = NULL;
    PyObject* Py_accum_e = NULL;

    PyObject* Py_mLPNH = NULL;
    PyObject* Py_mLPSH = NULL;

    PyObject* Py_variants = NULL;
    PyObject* Py_epatterns = NULL;

    PyObject* Py_startEulers = NULL;
    PyObject* Py_startdps = NULL;

    PyObject* Py_object = PyLong_FromLong(0);
    PyObject* Py_cancel = Py_False;

    if (!PyArg_ParseTupleAndKeywords(args, kw, "OOOOOOOOO|OO", kwlist,
        &Py_ipar, &Py_fpar, &Py_accum_e, &Py_mLPNH, &Py_mLPSH, &Py_variants, &Py_epatterns,
        &Py_startEulers, &Py_startdps, &Py_object, &Py_cancel))
    {
        std::cout << "Python arguments parse error" << std::endl;
        return NULL;
    }

    // ipar
    std::vector<int> ipar_vector = listTupleToVector_Int(Py_ipar);
    int32_t* ipar = ipar_vector.data();
    // long ipar = PyLong_AsLong((PyObject*) Py_ipar);
    Py_XDECREF(Py_ipar);
    // std::cout << "ipar done" << std::endl;

    // fpar
    std::vector<float> fpar_vector = listTupleToVector_Float(Py_fpar);
    float* fpar = fpar_vector.data();
    // Py_XDECREF(Py_fpar);
    // std::cout << "fpar done" << std::endl;

    // object
    size_t object = PyLong_AsSize_t((PyObject*) Py_object);
    Py_XDECREF(Py_object);
    // std::cout << "object done" << std::endl;

    // cancel
    bool cancel;
    if(PyObject_IsTrue((PyObject*) Py_cancel)){
        cancel = true;
    }
    else{
        cancel = false;
    }
    Py_XDECREF(Py_cancel);
    // std::cout << "cancel done" << std::endl;

    // set data type for Python numpy array conversion
    int typenum = NPY_DOUBLE; // Python float has double precision
    PyArray_Descr *descr = PyArray_DescrFromType(typenum);
    int typenum_int32 = NPY_INT32;
    PyArray_Descr *descr_int32 = PyArray_DescrFromType(typenum_int32);
    int typenum_float32 = NPY_FLOAT32;
    PyArray_Descr *descr_float32 = PyArray_DescrFromType(typenum_float32);

    // accum_e
    int32_t*** accum_e;
    npy_intp accum_e_dims[3];

    if (PyArray_AsCArray(&Py_accum_e, &accum_e, // numpy obj and pointer to array;
                    accum_e_dims, 3, descr_int32) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, accum_e");
        return NULL;
    }
    Py_XDECREF(Py_accum_e);
    // std::cout << "accum_e done" << std::endl;

    // mLPNH
    // use float32 (float) for mLPNH and mLPSH because they are directly read from h5 file
    // with a dtype of numpy.float32
    float*** mLPNH;
    npy_intp mLPNH_dims[3];

    if (PyArray_AsCArray(&Py_mLPNH, &mLPNH, // numpy obj and pointer to array;
                    mLPNH_dims, 3, descr_float32) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, mLPNH");
        return NULL;
    }
    Py_XDECREF(Py_mLPNH);
    // std::cout << "mLPNH done" << std::endl;

    // mLPSH
    float*** mLPSH;
    npy_intp mLPSH_dims[3];

    if (PyArray_AsCArray(&Py_mLPSH, &mLPSH, // numpy obj and pointer to array;
                    mLPSH_dims, 3, descr_float32) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, mLPSH");
        return NULL;
    }
    Py_XDECREF(Py_mLPSH);
    // std::cout << "mLPSH done" << std::endl;

    // variants
    double** variants_double;
    npy_intp variants_dims[2];
    float** variants = alloc_2d_float(ipar[43], 4);

    if (PyArray_AsCArray(&Py_variants, &variants_double, // numpy obj and pointer to array;
                    variants_dims, 2, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, variants");
        return NULL;
    }
    Py_XDECREF(Py_variants);

    for (int i = 0; i < ipar[43]; i++){
        for (int j = 0; j < 4; j++){
            variants[i][j] = static_cast<float>(variants_double[i][j]);
        }
    }
    // std::cout << "variants done" << std::endl;

    // epatterns
    double** epatterns_double;
    npy_intp epatterns_dims[2];
    float** epatterns = alloc_2d_float(ipar[39], ipar[41]);

    // PyObject* epatterns_obj = PyArray_FROM_OTF(Py_epatterns, typenum, NPY_ARRAY_C_CONTIGUOUS);

    if (PyArray_AsCArray(&Py_epatterns, &epatterns_double, // numpy obj and pointer to array;
                    epatterns_dims, 2, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, epatterns");
        return NULL;
    }
    Py_XDECREF(Py_epatterns);

    for (int i = 0; i < ipar[39]; i++){
        for (int j = 0; j < ipar[41]; j++){
            epatterns[i][j] = static_cast<float>(epatterns_double[i][j]);
        }
    }
    // std::cout << "epatterns done" << std::endl;

    // startEulers
    double** startEulers_double;
    npy_intp startEulers_dims[2];
    float** startEulers = alloc_2d_float(ipar[39], 3);

    if (PyArray_AsCArray(&Py_startEulers, &startEulers_double, // numpy obj and pointer to array;
                    startEulers_dims, 2, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, startEulers");
        return NULL;
    }
    Py_XDECREF(Py_startEulers);

    for (int i = 0; i < ipar[39]; i++){
        for (int j = 0; j < 3; j++){
            startEulers[i][j] = static_cast<float>(startEulers_double[i][j]);
        }
    }
    // std::cout << "startEulers done" << std::endl;

    // startdps
    double* startdps_double;
    npy_intp startdps_dims[2];
    float* startdps = (float *)malloc(ipar[39]*sizeof(float));

    if (PyArray_AsCArray(&Py_startdps, &startdps_double, // numpy obj and pointer to array;
                    startdps_dims, 1, descr) < 0) // pointer to numpy array dims output, n-D and data type (PyArray_Descr*);
    {
        PyErr_SetString(PyExc_TypeError, "error converting to c array, startdps");
        return NULL;
    }
    Py_XDECREF(Py_startdps);

    for (int i = 0; i < ipar[39]; i++){
        startdps[i] = static_cast<float>(startdps_double[i]);
    }
    // std::cout << "startdps done" << std::endl;


    /*
    *************************************
    2. Only for debug in development
    *************************************
    */
    // debug*******************************
    // std::cout << "test_ipar_fpar_object_cancel" << std::endl;
    // std::cout << "ipar" << std::endl;
    // for(int i=0; i<ipar_vector.size(); ++i)
    //     std::cout << ipar[i] << ' ';
    // std::cout << std::endl;

    // std::cout << "fpar" << std::endl;
    // for(int i=0; i<fpar_vector.size(); ++i)
    //     std::cout << fpar[i] << ' ';
    // std::cout << std::endl; 

    // std::cout << "object" << std::endl;
    // std::cout << object << std::endl;
    // std::cout << "cancel" << std::endl;
    // std::cout << cancel << std::endl;
    // std::cout << "test_ipar_fpar_object_cancel done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_accum_e" << std::endl;
    // std::cout << accum_e_dims[0] << accum_e_dims[1] << accum_e_dims[2] << std::endl;
    // for(int i=0; i<accum_e_dims[0]; ++i){
    //     for(int j=0; j<accum_e_dims[1]; ++j){
    //         for(int k=0; k<accum_e_dims[2]; ++k){
    //             std::cout << accum_e[i][j][k] << ' ';
    //         }
    //         std::cout << std::endl;
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_accum_e contiguous" << std::endl;
    // for(int i=0; i<accum_e_dims[0]*accum_e_dims[1]*accum_e_dims[2]; ++i)
    //     std::cout << *(&accum_e[0][0][0] + i) << ' ';
    // std::cout << std::endl;
    // std::cout << "test_accum_e done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_mLPNH" << std::endl;
    // for(int i=0; i<mLPNH_dims[0]; ++i){
    //     for(int j=0; j<mLPNH_dims[1]; ++j){
    //         for(int k=0; k<mLPNH_dims[2]; ++k){
    //             std::cout << mLPNH[i][j][k] << ' ';
    //         }
    //         std::cout << std::endl;
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_mLPNH contiguous" << std::endl;
    // for(int i=0; i<mLPNH_dims[0]*mLPNH_dims[1]*mLPNH_dims[2]; ++i)
    //     std::cout << *(&mLPNH[0][0][0] + i) << ' ';
    // std::cout << std::endl;
    // std::cout << "test_mLPNH done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_variants" << std::endl;
    // std::cout << variants_dims[0] << variants_dims[1] << std::endl;
    // for(int i=0; i<variants_dims[0]; ++i){
    //     for(int j=0; j<variants_dims[1]; ++j){
    //         std::cout << variants[i][j] << ' ';
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_variants done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_epatterns" << std::endl;
    // std::cout << epatterns_dims[0] << epatterns_dims[1] << std::endl;
    // for(int i=0; i<epatterns_dims[0]; ++i){
    //     for(int j=0; j<epatterns_dims[1]; ++j){
    //         std::cout << epatterns[i][j] << ' ';
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_epatterns done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_startEulers" << std::endl;
    // std::cout << startEulers_dims[0] << startEulers_dims[1] << std::endl;
    // for(int i=0; i<startEulers_dims[0]; ++i){
    //     for(int j=0; j<startEulers_dims[1]; ++j){
    //         std::cout << startEulers[i][j] << ' ';
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "test_startEulers done" << std::endl;
    // debug*******************************
    // debug*******************************
    // std::cout << "test_startdps" << std::endl;
    // std::cout << startdps_dims[0] << std::endl;
    // for(int i=0; i<startdps_dims[0]; ++i){
    //     std::cout << startdps[i] << ' ';
    // }
    // std::cout << std::endl;
    // std::cout << "test_startdps done" << std::endl;
    // debug*******************************


    /*
    *************************************
    3. Initialize result array
    *************************************
    */
    // eumain
    float** eumain = alloc_2d_float(ipar[39], 3);

    // dpmain
    float* dpmain = (float *)malloc(ipar[39]*sizeof(float));


    /*
    *************************************
    4. Initialize callbacks
    *************************************
    */
    ProgCallBackTypeDI2 callback = &DIProcessRefine;


    /*
    *************************************
    5. Call Fortran/C function
    *************************************
    */
    EMsoftCEBSDRefine(ipar, fpar, &accum_e[0][0][0], &mLPNH[0][0][0], &mLPSH[0][0][0],
       &variants[0][0], &epatterns[0][0], &startEulers[0][0], &startdps[0],
       &eumain[0][0], &dpmain[0], callback, object, &cancel);

    // std::cout << "Fortran running done" << std::endl;

    /*
    *************************************
    6. Collect and return results
    *************************************
    */
    // eumain (convert to double and PyArrayObject)
    npy_intp eumain_dims[2] = {ipar[39], 3};

    double** eumain_double = alloc_2d_double(ipar[39], 3);
    for (int i = 0; i < ipar[39]; ++i) {
        for (int j = 0; j < 3; ++j){
            eumain_double[i][j] = (double) eumain[i][j];
        }
    }

    // PyArray_SimpleNew allocates the memory needed for the array.
    PyObject* Py_eumain = PyArray_SimpleNew(2, eumain_dims, typenum);
    // The pointer to the array data is accessed using PyArray_DATA()
    double* eumain_p = (double *) PyArray_DATA((PyArrayObject*) Py_eumain);
    // Copy the data from the "array of arrays" to the contiguous numpy array.
    for (int i = 0; i < ipar[39]; ++i) {
        memcpy(eumain_p, eumain_double[i], sizeof(double) * 3);
        eumain_p += 3;
    }


    // dpmain (convert to double and PyArrayObject)
    npy_intp dpmain_dims[1] = {ipar[39]};

    double* dpmain_double = (double *)malloc(ipar[39]*sizeof(double));
    for (int i = 0; i < ipar[39]; ++i) {
        dpmain_double[i] = (double) dpmain[i];
    }

    // PyArray_SimpleNew allocates the memory needed for the array.
    PyObject* Py_dpmain = PyArray_SimpleNew(1, dpmain_dims, typenum);
    // The pointer to the array data is accessed using PyArray_DATA()
    double* dpmain_p = (double *) PyArray_DATA((PyArrayObject*) Py_dpmain);
    // Copy the data from the "array of arrays" to the contiguous numpy array.
    memcpy(dpmain_p, dpmain_double, sizeof(double) * ipar[39]);


    // return [Py_eumain, Py_dpmain]
    PyObject* ReturnList = PyList_New(2);
    if (PyList_SetItem(ReturnList, 0, PyArray_Return((PyArrayObject*) Py_eumain)) < 0){
        PyErr_SetString(PyExc_Exception, "error setting Py_eumain(PyArrayObject*) into ReturnList.");
        return NULL;
    }
    if (PyList_SetItem(ReturnList, 1, PyArray_Return((PyArrayObject*) Py_dpmain)) < 0){
        PyErr_SetString(PyExc_Exception, "error setting Py_dpmain(PyArrayObject*) into ReturnList.");
        return NULL;
    }
    
    // free all malloc
    // no need for accum_e, mLPNH, mLPSH (no malloc used)
    if (ipar[43]>0){
        free(variants[0]);
        free(variants);
    }

    free(epatterns[0]);
    free(epatterns);

    free(startEulers[0]);
    free(startEulers);

    free(startdps);

    free(eumain[0]);
    free(eumain);
    free(dpmain);
    free(eumain_double[0]);
    free(eumain_double);
    free(dpmain_double);

    return ReturnList;
    // Py_RETURN_NONE;
}


// module's method table
static PyMethodDef PyEMEBSDDI_methods[] = { 
    {"PyEMEBSDDI", (PyCFunction) PyEMEBSDDI, METH_VARARGS | METH_KEYWORDS, "EMEBSDDI Python API"},
    {"PyEMEBSDRefine", (PyCFunction) PyEMEBSDRefine, METH_VARARGS | METH_KEYWORDS, "EMEBSDRefine(EMFitOrientation) Python API"},
    {NULL, NULL, 0, NULL} 
};


// Module structure
static struct PyModuleDef PyEMEBSDDI_module = {
    PyModuleDef_HEAD_INIT,
    "PyEMEBSDDI", /* name of module */
    "EMEBSDDI and relative functions Python API", /* module documentation, may be NULL */
    -1,          /* size of per-interpreter state of the module, or -1 if the module keeps state in global variables. */
    PyEMEBSDDI_methods /* Method table */
};

PyMODINIT_FUNC PyInit_PyEMEBSDDI(void)
{
    import_array(); // necessary for all functions using PyArray_API
    return PyModule_Create(&PyEMEBSDDI_module);
}