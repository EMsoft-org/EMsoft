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
#pragma once

#include <QtCore/QString>
#include <QtWidgets/QWidget>

class QLineEdit;

/**
 * @brief The QtSFileUtils class
 */
class QtSFileUtils
{
public:
  QtSFileUtils();
  virtual ~QtSFileUtils();

  /**
   * @brief Generates a native file system path from the relative path given
   * @param pathEnding
   * @return
   */
  static QString GenerateFileSystemPath(const QString& pathEnding);

  /**
   * @brief Reveals the path in the operating systems UI shell (Windows Explorer or macOS Finder)
   * @param path The path to reveal
   */
  static void ShowPathInGui(QWidget* parent, const QString& path);

  /**
   * @brief verifyPathExists
   * @param filePath
   * @param lineEdit
   * @return
   */
  static bool VerifyPathExists(const QString& filePath, QLineEdit* lineEdit);

  /**
   * @brief hasValidFilePath
   * @param filePath
   * @return
   */
  static bool HasValidFilePath(const QString& filePath);

  /**
   * @brief GetPathSeperator
   * @return Returns the separator character used in the PATH environment variable
   */
  static QString GetPathSeperator();

  /**
   * @brief Finds an executable in the PATH
   * @param exe The executable to find
   * @return The absolute path to the executable or empty if it is not found
   */
  static QString FindInPath(const QString& exe);

  /**
   * @brief Returns an environment variable
   * @param evnVar
   * @return
   */
  static QStringList GetEnvVar(const QString& evnVar);

protected:
private:
};
