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
#pragma once


#include <cstring>
#include <sstream>
#include <stdexcept>

/**
 * @file SIMPLibSetGetMacros.h
 * @brief This file contains preprocessor macros that can be used to quickly
 * create instance variables, pointer types and other conveniences for a C++ class.
 */

#if defined(QT_CORE_LIB)
#include <QtCore/QSharedPointer>
#include <QtCore/QString>
#include <QtCore/QtDebug>
#endif

//-- C++11 Includes
#include <memory>


#define COPY_ARRAY_3(var, obj)\
  (var)[0] = (obj)->(var)[0]; (var)[1] = (obj)->(var)[1]; (var)[2] = (obj)->(var)[2];

#define COPY_ARRAY_4(var, obj)\
  (var)[0] = (obj)->(var)[0]; (var)[1] = (obj)->(var)[1];\
  (var)[2] = (obj)->(var)[2]; (var)[3] = (obj)->(var)[3];

#define COPY_ARRAY_5(var, obj)\
  (var)[0] = (obj)->(var)[0]; (var)[1] = (obj)->(var)[1];\
  (var)[2] = (obj)->(var)[2]; (var)[3] = (obj)->(var)[3];\
  (var)[4] = (obj)->(var)[4];


/**
 * This will perform a deep copy of the content of the shared vector from
 * target to destination. This is NOT just a simple pointer copy.
 */
#define DEEP_COPY_SHARED_VECTOR(sharedPtr, obj, VType, m_msgType)\
  if (nullptr != (sharedPtr))\
  {\
    (sharedPtr) = VType(static_cast<std::vector<m_msgType>*>(nullptr));\
  }\
  if (nullptr != (obj)->(sharedPtr).get())\
  {\
    (sharedPtr) = VType(new std::vector<m_msgType>(*((obj)->(sharedPtr).get())));\
  }


#define SHARED_IS_NULL(ptr)\
  (  (ptr).get() == nullptr )

/**
 * @brief Creates a static method that returns a nullptr pointer wrapped in a
 * std::shared_ptr<>
 * @param thisClass The name of the class.
 */
#define SIMPL_NULL_SHARED_POINTER(thisClass)\
  static Pointer NullPointer(void)\
  { \
    return Pointer(static_cast<thisClass*>(nullptr));\
  }

#ifndef QT_SHARED_POINTERS
 /* @brief Creates some basic typedefs that can be used throughout the code to
 * reference the class.
 */
#define QT_SHARED_POINTERS(thisClass)\
  typedef thisClass    Self;\
  typedef QSharedPointer< Self > Pointer;\
  typedef QSharedPointer<const Self >  ConstPointer;\
  SIMPL_NULL_SHARED_POINTER(thisClass)

#endif

/**
 * @brief Creates some basic typedefs that can be used throughout the code to
 * reference the class.
 */
#define SIMPL_SHARED_POINTERS(thisClass)\
  typedef thisClass    Self;\
  typedef std::shared_ptr<Self > Pointer;\
  typedef std::shared_ptr<const Self >  ConstPointer;\
  typedef std::weak_ptr<thisClass > WeakPointer;\
  typedef std::weak_ptr<thisClass > ConstWeakPointer;\
  SIMPL_NULL_SHARED_POINTER(thisClass)


#define SIMPL_STATIC_NEW_SUPERCLASS(superclass, theclass)\
  static superclass::Pointer New##superclass()\
  {\
    theclass* ptr = new theclass();\
    superclass::Pointer shared_ptr (dynamic_cast<superclass*>(ptr) );\
    return shared_ptr;\
  }

/**
 * @brief Creates a typedef that points to the superclass of this class
 */
#define SIMPL_SUPERCLASS_POINTER(SuperClass)\
  typedef SuperClass::Pointer SuperClass;


/**
 * @brief Creates a static "New" method that creates an instance of thisClass
 */
#define SIMPL_NEW_SUPERCLASS(thisClass, SuperClass)\
  typedef SuperClass::Pointer SuperClass##Type;\
  static SuperClass##Type New##SuperClass(void) \
  { \
    SuperClass##Type sharedPtr (new (thisClass)); \
    return sharedPtr; \
  }

/**
 * @brief Implements a Static 'New' Method for a class
 */
#define SIMPL_STATIC_NEW_MACRO(thisClass) \
  static Pointer New(void) \
  { \
    Pointer sharedPtr (new (thisClass)); \
    return sharedPtr; \
  }

#define SIMPL_STATIC_NEW_MACRO_WITH_ARGS(thisClass, args) \
  static Pointer New args \
  { \
    Pointer sharedPtr (new (thisClass)); \
    return sharedPtr; \
  }

/** Macro used to add standard methods to all classes, mainly type
 * information. */
#define SIMPL_TYPE_MACRO(thisClass)\
public:     \
  virtual const QString getNameOfClass() const\
  {  \
    return QString(#thisClass);    \
  }  \
  static int IsTypeOf(const char* type)\
  {  \
    if(!strcmp(#thisClass, type))  \
    {\
      return 1;    \
    }\
    return 0;      \
  }

#define SIMPL_TYPE_MACRO_SUPER(thisClass, superclass)    \
public:     \
  virtual const QString getNameOfClass() const\
  {  \
    return QString(#thisClass);    \
  }  \
  static QString ClassName()\
  {  \
    return QString(#thisClass);    \
  }  \
  static int IsTypeOf(const char* type)\
  {  \
    if(!strcmp(#thisClass, type))  \
    {\
      return 1;    \
    }\
    return superclass::IsTypeOf(type); \
  }

#define SIMPL_TYPE_MACRO_SUPER_OVERRIDE(thisClass, superclass)  \
public:     \
  virtual const QString getNameOfClass() const override  \
  {  \
    return QString(#thisClass);    \
  }  \
  static QString ClassName()\
  {  \
    return QString(#thisClass);    \
  }  \
  static int IsTypeOf(const char* type)\
  {  \
    if(!strcmp(#thisClass, type))  \
    {\
      return 1;    \
    }\
    return superclass::IsTypeOf(type); \
  }

#define SIMPL_CLASS_VERSION(vers)\
  virtual int getClassVersion() { return vers; }


//------------------------------------------------------------------------------
// Macros for Properties
/**
* @brief Creates a QString constant for the Property so that the property
* can be retrieved by name.
*/
#define SIMPL_PROPERTY_CONSTANT(prpty)\
  const QString prpty ( #prpty );

/**
* @brief Creates a "setter" method to set the property.
*/
#define SIMPL_SET_PROPERTY(type, prpty)\
  void set##prpty(const type& value) { this->m_##prpty = value; }

/**
* @brief Creates a "getter" method to retrieve the value of the property.
*/
#define SIMPL_GET_PROPERTY(type, prpty)\
  type get##prpty() const { return m_##prpty; }

/**
* @brief
*/
#define SIMPL_SET_FILTER_PARAMETER(type, prpty)\
  void set##prpty(const type& value) { this->m_##prpty = value; emit parametersChanged(); }


#define SIMPL_SET_PROPERTY_DECL(type, prpty)\
  void set##prpty(type value);

#define SIMPL_GET_PROPERTY_DECL(type, prpty)\
  type get##prpty() const;

#define SIMPL_INSTANCE_PROPERTY_DECL(type, prpty)                                                                                                                                                      \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  SIMPL_SET_PROPERTY_DECL(type, prpty)                                                                                                                                                                 \
  SIMPL_GET_PROPERTY_DECL(type, prpty)

#define SIMPL_INSTANCE_PROPERTY_DECL(type, prpty)                                                                                                                                                      \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  SIMPL_SET_PROPERTY_DECL(type, prpty)                                                                                                                                                                 \
  SIMPL_GET_PROPERTY_DECL(type, prpty)

#define SIMPL_VIRTUAL_INSTANCE_PROPERTY_DECL(type, prpty)                                                                                                                                              \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  virtual SIMPL_SET_PROPERTY_DECL(type, prpty) virtual SIMPL_GET_PROPERTY_DECL(type, prpty)

#define SIMPL_SET_PROPERTY_DEF(class, type, prpty)\
  void class::set##prpty(const type& value) { this->m_##prpty = value; }

#define SIMPL_GET_PROPERTY_DEF(class, type, prpty)\
  type class::get##prpty() const { return m_##prpty; }

#define SIMPL_INSTANCE_PROPERTY_DEF(class, type, prpty)\
      SIMPL_SET_PROPERTY_DEF(class, type, prpty)\
      SIMPL_GET_PROPERTY_DEF(class, type, prpty)

#define SIMPL_INSTANCE_PROPERTY_DEF(class, type, prpty)\
      SIMPL_SET_PROPERTY_DEF(class, type, prpty)\
      SIMPL_GET_PROPERTY_DEF(class, type, prpty)

/**
* @brief
*/
#define SIMPL_VIRTUAL_INSTANCE_PROPERTY(type, prpty)                                                                                                                                                   \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  virtual SIMPL_SET_PROPERTY(type, prpty) virtual SIMPL_GET_PROPERTY(type, prpty)

#define SIMPL_INSTANCE_PROPERTY(type, prpty)                                                                                                                                                           \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  SIMPL_SET_PROPERTY(type, prpty)                                                                                                                                                                      \
  SIMPL_GET_PROPERTY(type, prpty)

#define SIMPL_POINTER_PROPERTY(type, prpty)                                                                                                                                                            \
private:                                                                                                                                                                                               \
  type* m_##prpty = nullptr;                                                                                                                                                                           \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  void set##prpty(type* f)                                                                                                                                                                             \
  {                                                                                                                                                                                                    \
    m_##prpty = f;                                                                                                                                                                                     \
  }                                                                                                                                                                                                    \
  SIMPL_GET_PROPERTY(type*, prpty)

#define SIMPL_PRIVATE_INSTANCE_PROPERTY(type, prpty)                                                                                                                                                   \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
  SIMPL_SET_PROPERTY(type, prpty)                                                                                                                                                                      \
public:                                                                                                                                                                                                \
  SIMPL_GET_PROPERTY(type, prpty)

/* *****************************************************************************
 * *****************************************************************************
 * *****************************************************************************
 *
 * PyBind11 Macros that we can use to explicitly define which setters & getters
 * will be exposed to the Python library
 */

/**
* @brief PYB11_CREATE_BINDINGS This macro lets the wrapper know that we want to
* wrap this class with Python bindings. It should only take a single argument
* which is the name of the class. If this line is commented out using the standard
* C++ style of single line comment then <b>NO WRAPPING</b> will be performed
* on the class.
* @code
* PYB11_CREATE_BINDINGS(AbstractFilter)
* @endcode
*/
#define PYB11_CREATE_BINDINGS(...)

/**
* @brief PYB11_NO_BINDINGS This macro will ensure that the class is NOT wrapped
* with python bindings. Only a single argument which is the name of the class
* should be used.
* @code
* PYB11_NO_BINDINGS(AbstractFilter)
* @endcode
*/
#define PYB11_NO_BINDINGS(...)

/**
 * @brief PYB11_STATIC_CREATION This macro will wrap the "static New()" function
 * that most of the SIMPL classes implement as a way to instantiate the class in
 * addition to other static methods that are used for a class. The argument types
 * to the static method should be listed <b>WITHOUT</b> any spaces for each argument.
 *
 * @code
 * PYB11_STATIC_CREATION(Create ARGS std::vector<std::vector<double>> std::list<std::string> std::list<std::string>)
 * @endcode
 *
 * If there are several static creation methods that overload each other then the following form can be used:
 *
 * @code
 * PYB11_STATIC_CREATION(New OVERLOAD QString)
 * PYB11_STATIC_CREATION(New OVERLOAD DataArrayPath)
 * @endcode
 */
#define PYB11_STATIC_CREATION(...)

/**
 * @brief PYB11_CREATION This macro is used for non-static constructors that need
 * to be wrapped. The argument types need to be lists where each argument cannot
 * containe spaces. Use a typedef if needed, but using a typedef also has its
 * own issues. @see DataArrayPath for an example.
 *
 * @code
 * PYB11_CREATION(ARGS QString QString QString)
 * @endcode
 */
#define PYB11_CREATION(...)

/**
* @brief PYB11_ENUMERATION This macro will allow a class enumeration to be wrapped
* in Python an available to the Python side. @see IGeoemtry for an example. The
* macro should only take a single argument.
* @code
* PYB11_ENUMERATION(Type)
* PYB11_ENUMERATION(VtkCellType)
* @endcode
*/
#define PYB11_ENUMERATION(...)

/**
* @brief PYB11_PROPERTY This macro is used to wrap a single class instance property
* that is typically used as an input parameter for a filter. This macro should
* <b>NOT</b> be used for just wrapping instance methods. Use the PYB11_METHOD for
* that. @see AbstractFilter. There are number of arguments depending on if the
* property is a read-only or read-write variable. At least 4 arguments to the
* macro are required when the property is read only.
* @code
  PYB11_PROPERTY(QString NameOfClass READ getNameOfClass)
* @endcode
* and 6 arguments when the property is read-write.
* @code
* PYB11_PROPERTY(bool Cancel READ getCancel WRITE setCancel)
* @endcode
*
* and if there are additional overloads of the Getter that returns a const reference such as
* @code
* const QString& getFoo() const;
* @endcode
*
* then you would add on the CONST_GET_OVERLOAD as the last argument to the macro.
*/
#define PYB11_PROPERTY(...)

/**
 * @brief This macro is used to expose a method to the Python bindings. The signature
 * of the macro should be the following:
 * PYB11_METHOD( _return_type_ _name_of_method_ [ARGS|OVERLOAD] ....)
 * If the ARGS command is used then simply list the variable names for each argument.
 * For example if you have a method "void getFoo(const QString &foo)" that you want
 * to expose:
 * @code
 * PYB11_METHOD(void getFoo ARGS Foo)
 * @endcode
 *
 * If your method does not take any arguments then leave out the ARGS keyword.
 *
 * If you are have overloads of the method that you want to expose to Python then
 * the "OVERLOAD" version of the method should be used. Again, say we have two
 * methods that we want to expose.
 * @code
 * void setPath(const QString &name)
 * void setPath(const DataArrayPath &path)
 * @endcode
 *
 * then we would use the following set of invocations:
 * @code
 * PYB11_METHOD(void setPath OVERLOAD const.QString.&,Name)
 * PYB11_METHOD(void setPath OVERLOAD const.DataArrayPath.&,Path)
 * @endcode
 * Note that in order to get the (const QString &) correct we used the '.' charater
 * to declare the type. This is required as the macro is split using spaces. When
 * then end code is generated the '.' characters will be replaced with spaces.
 *
 * If the method that is being wrapped in python is a 'const' method then the last argument should be CONST_METHOD. In
 * the example below there is a pair of overloaded methods that are both 'const'.
 *
 * @code
 * PYB11_METHOD(bool doesDataContainerExist OVERLOAD const.QString.&,Name CONST_METHOD)
 * PYB11_METHOD(bool doesDataContainerExist OVERLOAD const.DataArrayPath.&,Path CONST_METHOD)
 * @endcode
 */
#define PYB11_METHOD(...)

// End of PYBIND11 Macro Definitions
/* *****************************************************************************
 * *****************************************************************************
 * *****************************************************************************
 */



#define SIMPL_SET_PROPERTY_DECL(type, prpty)\
  void set##prpty(type value);

#define SIMPL_GET_PROPERTY_DECL(type, prpty)\
  type get##prpty() const;


#define SIMPL_PIMPL_PROPERTY_DECL(type, prpty)\
  public:\
  SIMPL_PIMPL_GET_PROPERTY_DECL(type, prpty)\
  SIMPL_PIMPL_SET_PROPERTY_DECL(type, prpty)\

#define SIMPL_PIMPL_GET_PROPERTY_DECL(type, prpty)\
  type get##prpty() const;

#define SIMPL_PIMPL_SET_PROPERTY_DECL(type, prpty)\
  void set##prpty(const type& value);\

#define SIMPL_PIMPL_PROPERTY_DEF(Class, type, prpty)\
  SIMPL_PIMPL_SET_PROPERTY_DEF(Class, type, prpty)\
  SIMPL_PIMPL_GET_PROPERTY_DEF(Class, type, prpty)\

#define SIMPL_PIMPL_GET_PROPERTY_DEF(Class, type, prpty)\
  type Class::get##prpty() const {\
    Q_D(const Class);\
    return d->m_##prpty;\
  }

#define SIMPL_PIMPL_SET_PROPERTY_DEF(Class, type, prpty)                                                                                                                                               \
  void Class::set##prpty(const type& value)                                                                                                                                                            \
  {                                                                                                                                                                                                    \
    Q_D(Class);                                                                                                                                                                                        \
    d->m_##prpty = value;                                                                                                                                                                              \
  }

#define SIMPL_OVERLOAD_PROPERTY(type, prpty, overload)                                                                                                                                                 \
private:                                                                                                                                                                                               \
  type m_##prpty = {};                                                                                                                                                                                 \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  SIMPL_SET_PROPERTY(overload, prpty)                                                                                                                                                                  \
  SIMPL_GET_PROPERTY(overload, prpty)

#define SIMPL_BOOL_PROPERTY(prpty)                                                                                                                                                                     \
private:                                                                                                                                                                                               \
  bool m_##prpty = false;                                                                                                                                                                              \
                                                                                                                                                                                                       \
public:                                                                                                                                                                                                \
  virtual SIMPL_SET_PROPERTY(bool, prpty) SIMPL_GET_BOOL_PROPERTY(prpty)

#define SIMPL_GET_BOOL_PROPERTY(prpty)\
  public:\
  virtual bool is##prpty() { return m_##prpty; }

/**
 * @brief
 */
#define SIMPL_FILTER_NEW_MACRO(Class)                                                                                                                                                                  \
  static std::shared_ptr<Class> New()                                                                                                                                                                  \
  {                                                                                                                                                                                                    \
    struct make_shared_enabler : public Class                                                                                                                                                          \
    {                                                                                                                                                                                                  \
    };                                                                                                                                                                                                 \
    std::shared_ptr<make_shared_enabler> val = std::make_shared<make_shared_enabler>();                                                                                                                \
    val->setupFilterParameters();                                                                                                                                                                      \
    return val;                                                                                                                                                                                        \
  }

/**
* @brief
*/
#define SIMPL_FILTER_PARAMETER(type, prpty)\
  private:\
  type   m_##prpty;\
  public:\
  void set##prpty(const type& value) { this->m_##prpty = value; }\
  type get##prpty() const { return m_##prpty; }

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define SIMPL_SET_2DVECTOR_PROPERTY(type, prpty, varname)\
  void set##prpty(type value[2]) {\
    (varname)[0] = value[0]; (varname)[1] = value[1]; }\
  void set##prpty(const type& value_0, const type& value_1) {\
    (varname)[0] = value_0; (varname)[1] = value_1; }\
  void set##prpty(const std::tuple<type, type> &var) {\
    (varname)[0] = std::get<0>(var); (varname)[1] = std::get<1>(var); }

#define SIMPL_GET_2DVECTOR_PROPERTY(type, prpty, varname)\
  void get##prpty(type value[2]) {\
    value[0] = (varname)[0]; value[1] = (varname)[1]; }\
  void get##prpty((type) &value_0, (type) &value_1) {\
    value_0 = (varname)[0]; value_1 = (varname)[1]; }\
  std::tuple<type, type> get##prpty() const {\
    return std::make_tuple((varname)[0], (varname)[1]); }

#define SIMPL_INSTANCE_VEC2_PROPERTY(type, prpty)\
  private:\
  type   m_##prpty[2];\
  public:\
  SIMPL_SET_2DVECTOR_PROPERTY(type, prpty, m_##prpty)\
  SIMPL_GET_2DVECTOR_PROPERTY(type, prpty, m_##prpty)


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define SIMPL_SET_VEC3_PROPERTY(type, prpty, varname)\
  void set##prpty(type value[3]) {\
    (varname)[0] = value[0]; (varname)[1] = value[1]; (varname)[2] = value[2]; }\
  void set##prpty(type value_0, type value_1, type value_2) {\
    (varname)[0] = value_0; (varname)[1] = value_1; (varname)[2] = value_2; }\
  void set##prpty(const std::tuple<type, type, type> &var) {\
    (varname)[0] = std::get<0>(var); (varname)[1] = std::get<1>(var); (varname)[2] = std::get<2>(var); }

#define SIMPL_GET_VEC3_PROPERTY(type, prpty, varname)\
  void get##prpty(type value[3]) {\
    value[0] = (varname)[0]; value[1] = (varname)[1]; value[2] = (varname)[2]; }\
  void get##prpty(type &value_0, type &value_1, type &value_2) {\
    value_0 = (varname)[0]; value_1 = (varname)[1]; value_2 = (varname)[2]; }\
  std::tuple<type, type, type> get##prpty() const {\
    return std::make_tuple((varname)[0], (varname)[1], (varname)[2]); }

#define SIMPL_INSTANCE_VEC3_PROPERTY(type, prpty)\
  private:\
  type   m_##prpty[3];\
  public:\
  SIMPL_SET_VEC3_PROPERTY(type, prpty, m_##prpty)\
  SIMPL_GET_VEC3_PROPERTY(type, prpty, m_##prpty)

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define SIMPL_SET_VEC3_PROPERTY_VO(type, prpty, varname)\
   void set##prpty(type value[3]) override {\
    (varname)[0] = value[0]; (varname)[1] = value[1]; (varname)[2] = value[2]; }\
   void set##prpty(type value_0, type value_1, type value_2) override {\
    (varname)[0] = value_0; (varname)[1] = value_1; (varname)[2] = value_2; }\
   void set##prpty(const std::tuple<type, type, type> &var) override {\
    (varname)[0] = std::get<0>(var); (varname)[1] = std::get<1>(var); (varname)[2] = std::get<2>(var);}

#define SIMPL_GET_VEC3_PROPERTY_VO(type, prpty, varname)\
  virtual std::tuple<type, type, type> get##prpty() const override\
  {\
    return std::make_tuple((varname)[0], (varname)[1], (varname)[2]);\
  }

#define SIMPL_INSTANCE_VEC3_PROPERTY_VO(type, prpty)\
  private:\
  type   m_##prpty[3];\
  public:\
  SIMPL_SET_VEC3_PROPERTY_VO(type, prpty, m_##prpty)\
  SIMPL_GET_VEC3_PROPERTY_VO(type, prpty, m_##prpty)

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define SIMPL_CONTAINER_TYPE(thisClass, container) \
  typedef (container)<thisClass>     ContainerT; \
  typedef std::shared_ptr<(container)<(thisClass)>> ContainerPType;


/**
* @brief Creates a "setter" method to set the property.
*/
#define SIMPL_SET_STRING_PROPERTY( prpty, varname) \
  void set##prpty(const QString& value) { this->varname = value; }

/**
* @brief Creates a "getter" method to retrieve the value of the property.
*/
#define SIMPL_GET_STRING_PROPERTY( prpty, varname) \
  QString get##prpty() const { return varname; }

/**
* @brief Creates a "setter" method to set the property.
*/
#define SIMPL_SET_STRING_PROPERTY_OVERRIDE(prpty, varname)      \
  void set##prpty(const QString& value) override  \
  {  \
    this->varname = value;  \
  }

/**
* @brief Creates a "getter" method to retrieve the value of the property.
*/
#define SIMPL_GET_STRING_PROPERTY_OVERRIDE(prpty, varname)      \
  QString get##prpty() const override  \
  {  \
    return varname;\
  }

/**
 * @brief Creates setters and getters in the form of 'setXXX()' and 'getXXX()' methods
 */
#define SIMPL_INSTANCE_STRING_PROPERTY(prpty)\
  private:\
  QString      m_##prpty;\
  public:\
  SIMPL_SET_STRING_PROPERTY(prpty,  m_##prpty)\
  SIMPL_GET_STRING_PROPERTY(prpty,  m_##prpty)

/**
* @brief
*/
#define SIMPL_INSTANCE_STRING_PROPERTY_OVERRIDE(prpty)\
private:\
  QString m_##prpty;\
public:\
  SIMPL_SET_STRING_PROPERTY_OVERRIDE(prpty, m_##prpty)\
  SIMPL_GET_STRING_PROPERTY_OVERRIDE(prpty, m_##prpty)

#define SIMPL_VIRTUAL_INSTANCE_STRING_PROPERTY(prpty)\
  private:\
  QString      m_##prpty;\
  public:\
  virtual SIMPL_SET_STRING_PROPERTY(prpty,  m_##prpty)\
  virtual SIMPL_GET_STRING_PROPERTY(prpty,  m_##prpty)


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define SIMPL_DECLARE_ARRAY(Type, ptr, prpty )\
  private:\
  DataArray<Type>::Pointer m_##prpty;\
  Type* (ptr) = nullptr;\
  public:\
  SIMPL_SET_PROPERTY(DataArray<Type>::Pointer, prpty)\
  SIMPL_GET_PROPERTY(DataArray<Type>::Pointer, prpty)


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
#define DEFINE_DATAARRAY_VARIABLE(type, name)\
  private:\
  DataArray<type>::WeakPointer m_##name##Ptr;\
  type* m_##name = nullptr;

#define DEFINE_DATAARRAY_WEAKPTR(type, name)  \
private:    \
  DataArray<type>::WeakPointer m_##name##Ptr;

#define DEFINE_STRINGARRAY_VARIABLE(name)\
  private:\
  StringDataArray::WeakPointer m_##name##Ptr;

#define DEFINE_REQUIRED_VARIABLE(type, name)\
  SIMPL_INSTANCE_STRING_PROPERTY(name##ArrayName);\
  private:\
  type::WeakPointer m_##name##Ptr;\
  (type)* m_##name = nullptr;

//used in place of 'DEFINE_DATAARRAY_VARIABLE' in filter header
#define DEFINE_IDATAARRAY_VARIABLE(varName)\
  private:\
  IDataArray::WeakPointer m_##varName##Ptr;\
  void* m_##varName = nullptr;

#define DEFINE_IDATAARRAY_WEAKPTR(varName)    \
private:    \
  IDataArray::WeakPointer m_##varName##Ptr;

#define SIMPL_COPY_INSTANCEVAR(name)\
  filter->set##name(get##name());


// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
/**
* @brief Macro to silence compiler warnings for unused parameters in methods.
*/
#define SIMPL_NOT_USED(x)

// -----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------


namespace SIMPLTesting
{
class bad_lexical_cast : public std::runtime_error
{
public:
  explicit bad_lexical_cast(const QString& s)
  : std::runtime_error(s.toStdString())
  {
  }
};

class bad_any_cast : public std::runtime_error
{
public:
  explicit bad_any_cast(const QString& s)
  : std::runtime_error(s.toStdString())
  {
  }
};

template <typename T> T lexical_cast(const QString& s)
{
  std::istringstream i(s.toStdString());
  T x;
  if(!(i >> x))
  {
    throw bad_lexical_cast("convertToDouble(\"" + s + "\")");
  }

  return x;
}
} // namespace SIMPLTesting



