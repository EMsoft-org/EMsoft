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

#include <QtCore/QObject>
#include <QtCore/QSet>
#include <QtCore/QString>

#include <QtGui/QFont>
#include <QtGui/QIcon>

#include "SIMPLib/Common/SIMPLibSetGetMacros.h"

class QLineEdit;
class QPushButton;

namespace SVWidgets
{
namespace Styles
{

const QString PushButtonStyleSheet(":/SIMPLPushButton.css");
const QString AddImagePath(":/SIMPL/icons/images/add.png");
const QString DeleteImagePath(":/SIMPL/icons/images/delete.png");
const QString LoadImagePath(":/SIMPL/icons/images/data-transfer-upload.png");
const QString SaveImagePath(":/SIMPL/icons/images/data-transfer-download.png");
const QString ReloadImagePath(":/SIMPL/icons/images/reload.png");
const QString RefreshImagePath(":/SIMPL/icons/images/refresh.png");
const QString CogImagePath(":/SIMPL/icons/images/cog.png");
const QString HDFImagePath(":/SIMPL/icons/images/data-transfer-hdf.png");
const QString InformationImagePath(":/SIMPL/icons/images/information.png");

} // namespace Styles
} // namespace SVWidgets

class SVStyle : public QObject
{
    Q_OBJECT
  public:
    SIMPL_TYPE_MACRO(SVStyle)
    
    ~SVStyle() override;
    
    /**
     * @brief
     * @return
     */
    static SVStyle* Instance();

    SIMPL_GET_PROPERTY(QString, CurrentThemeFilePath)


    SIMPL_INSTANCE_PROPERTY(QColor, Fader_color)
    Q_PROPERTY(QColor Fader_color READ getFader_color WRITE setFader_color)

    SIMPL_INSTANCE_PROPERTY(QColor, Widget_Error_color)    
    Q_PROPERTY(QColor Widget_Error_color READ getWidget_Error_color WRITE setWidget_Error_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, Text_Error_color)    
    Q_PROPERTY(QColor Text_Error_color READ getText_Error_color WRITE setText_Error_color)
        
    SIMPL_INSTANCE_PROPERTY(QColor, CentralWidget_background_color)    
    Q_PROPERTY(QColor CentralWidget_background_color READ getCentralWidget_background_color WRITE setCentralWidget_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QLabel_color)    
    Q_PROPERTY(QColor QLabel_color READ getQLabel_color WRITE setQLabel_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, FilterInputWidget_background_color)    
    Q_PROPERTY(QColor FilterInputWidget_background_color READ getFilterInputWidget_background_color WRITE setFilterInputWidget_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QScrollArea_background_color)    
    Q_PROPERTY(QColor QScrollArea_background_color READ getQScrollArea_background_color WRITE setQScrollArea_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, VariablesTabContents_background_color)    
    Q_PROPERTY(QColor VariablesTabContents_background_color READ getVariablesTabContents_background_color WRITE setVariablesTabContents_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, FilterParameterWidget_background_color)    
    Q_PROPERTY(QColor FilterParameterWidget_background_color READ getFilterParameterWidget_background_color WRITE setFilterParameterWidget_background_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, FilterParameterWidget_border_color)
    Q_PROPERTY(QColor FilterParameterWidget_border_color READ getFilterParameterWidget_border_color WRITE setFilterParameterWidget_border_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, QGroupBoxTitle_background_color)    
    Q_PROPERTY(QColor QGroupBoxTitle_background_color READ getQGroupBoxTitle_background_color WRITE setQGroupBoxTitle_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QGroupBox_background_color)    
    Q_PROPERTY(QColor QGroupBox_background_color READ getQGroupBox_background_color WRITE setQGroupBox_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QDockWidget_border_color)    
    Q_PROPERTY(QColor QDockWidget_border_color READ getQDockWidget_border_color WRITE setQDockWidget_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QDockWidget_color)    
    Q_PROPERTY(QColor QDockWidget_color READ getQDockWidget_color WRITE setQDockWidget_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QDockWidgetTitle_background_color)    
    Q_PROPERTY(QColor QDockWidgetTitle_background_color READ getQDockWidgetTitle_background_color WRITE setQDockWidgetTitle_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QDockWidgetTitle_color)    
    Q_PROPERTY(QColor QDockWidgetTitle_color READ getQDockWidgetTitle_color WRITE setQDockWidgetTitle_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListView_background_color)    
    Q_PROPERTY(QColor QListView_background_color READ getQListView_background_color WRITE setQListView_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListView_color)    
    Q_PROPERTY(QColor QListView_color READ getQListView_color WRITE setQListView_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListViewItemHover_background_color)    
    Q_PROPERTY(QColor QListViewItemHover_background_color READ getQListViewItemHover_background_color WRITE setQListViewItemHover_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListViewItemHover_color)    
    Q_PROPERTY(QColor QListViewItemHover_color READ getQListViewItemHover_color WRITE setQListViewItemHover_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListViewItemSelected_background_color)    
    Q_PROPERTY(QColor QListViewItemSelected_background_color READ getQListViewItemSelected_background_color WRITE setQListViewItemSelected_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QListViewItemSelected_color)    
    Q_PROPERTY(QColor QListViewItemSelected_color READ getQListViewItemSelected_color WRITE setQListViewItemSelected_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMainWindowSeparator_background_color)    
    Q_PROPERTY(QColor QMainWindowSeparator_background_color READ getQMainWindowSeparator_background_color WRITE setQMainWindowSeparator_background_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuBar_background_color)    
    Q_PROPERTY(QColor QMenuBar_background_color READ getQMenuBar_background_color WRITE setQMenuBar_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuBarItem_color )
    Q_PROPERTY(QColor QMenuBarItem_color READ getQMenuBarItem_color WRITE setQMenuBarItem_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuBarItemPressed_color)    
    Q_PROPERTY(QColor QMenuBarItemPressed_color READ getQMenuBarItemPressed_color WRITE setQMenuBarItemPressed_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuBarItemSelected_background_color)    
    Q_PROPERTY(QColor QMenuBarItemSelected_background_color READ getQMenuBarItemSelected_background_color WRITE setQMenuBarItemSelected_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuBarItemSelected_color)    
    Q_PROPERTY(QColor QMenuBarItemSelected_color READ getQMenuBarItemSelected_color WRITE setQMenuBarItemSelected_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenu_background_color)    
    Q_PROPERTY(QColor QMenu_background_color READ getQMenu_background_color WRITE setQMenu_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenu_color)    
    Q_PROPERTY(QColor QMenu_color READ getQMenu_color WRITE setQMenu_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuItemSelected_background_color)    
    Q_PROPERTY(QColor QMenuItemSelected_background_color READ getQMenuItemSelected_background_color WRITE setQMenuItemSelected_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QMenuItemSelected_color)    
    Q_PROPERTY(QColor QMenuItemSelected_color READ getQMenuItemSelected_color WRITE setQMenuItemSelected_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, QMenuItemDisabled_background_color)
    Q_PROPERTY(QColor QMenuItemDisabled_background_color READ getQMenuItemDisabled_background_color WRITE setQMenuItemDisabled_background_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, QPushButton_background_color)    
    Q_PROPERTY(QColor QPushButton_background_color READ getQPushButton_background_color WRITE setQPushButton_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QPushButton_border_color)    
    Q_PROPERTY(QColor QPushButton_border_color READ getQPushButton_border_color WRITE setQPushButton_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonHover_background_color)    
    Q_PROPERTY(QColor QPushButtonHover_background_color READ getQPushButtonHover_background_color WRITE setQPushButtonHover_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonPressed_background_color)
    Q_PROPERTY(QColor QPushButtonPressed_background_color READ getQPushButtonPressed_background_color WRITE setQPushButtonPressed_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonDefault_background_color)
    Q_PROPERTY(QColor QPushButtonDefault_background_color READ getQPushButtonDefault_background_color WRITE setQPushButtonDefault_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonDefaultPressed_background_color)
    Q_PROPERTY(QColor QPushButtonDefaultPressed_background_color READ getQPushButtonDefaultPressed_background_color WRITE setQPushButtonDefaultPressed_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonDefaultHover_background_color)
    Q_PROPERTY(QColor QPushButtonDefaultHover_background_color READ getQPushButtonDefaultHover_background_color WRITE setQPushButtonDefaultHover_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonDefault_border_color)
    Q_PROPERTY(QColor QPushButtonDefault_border_color READ getQPushButtonDefault_border_color WRITE setQPushButtonDefault_border_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QPushButtonDefault_text_color)
    Q_PROPERTY(QColor QPushButtonDefault_text_color READ getQPushButtonDefault_text_color WRITE setQPushButtonDefault_text_color)


    SIMPL_INSTANCE_PROPERTY(QColor, QSplitter_handle_start_color)
    Q_PROPERTY(QColor QSplitter_handle_start_color READ getQSplitter_handle_start_color WRITE setQSplitter_handle_start_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QSplitter_handle_end_color)
    Q_PROPERTY(QColor QSplitter_handle_end_color READ getQSplitter_handle_end_color WRITE setQSplitter_handle_end_color)

    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolButton_background_color)    
    Q_PROPERTY(QColor QToolButton_background_color READ getQToolButton_background_color WRITE setQToolButton_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolButton_color)    
    Q_PROPERTY(QColor QToolButton_color READ getQToolButton_color WRITE setQToolButton_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolButton_border_color)    
    Q_PROPERTY(QColor QToolButton_border_color READ getQToolButton_border_color WRITE setQToolButton_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolButtonChecked_background_color)    
    Q_PROPERTY(QColor QToolButtonChecked_background_color READ getQToolButtonChecked_background_color WRITE setQToolButtonChecked_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolButtonDisabled_background_color)
    Q_PROPERTY(QColor QToolButtonDisabled_background_color READ getQToolButtonDisabled_background_color WRITE setQToolButtonDisabled_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QToolButtonDisabled_color)
    Q_PROPERTY(QColor QToolButtonDisabled_color READ getQToolButtonDisabled_color WRITE setQToolButtonDisabled_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QStatusBar_border_color)    
    Q_PROPERTY(QColor QStatusBar_border_color READ getQStatusBar_border_color WRITE setQStatusBar_border_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTableWidget_color)    
    Q_PROPERTY(QColor QTableWidget_color READ getQTableWidget_color WRITE setQTableWidget_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QTableWidget_selected_background_color)
    Q_PROPERTY(QColor QTableWidget_selected_background_color READ getQTableWidget_selected_background_color WRITE setQTableWidget_selected_background_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderView_background_color)    
    Q_PROPERTY(QColor QHeaderView_background_color READ getQHeaderView_background_color WRITE setQHeaderView_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderView_border_color)    
    Q_PROPERTY(QColor QHeaderView_border_color READ getQHeaderView_border_color WRITE setQHeaderView_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderView_color)    
    Q_PROPERTY(QColor QHeaderView_color READ getQHeaderView_color WRITE setQHeaderView_color)   

    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderViewDisabled_background_color)
    Q_PROPERTY(QColor QHeaderViewDisabled_background_color READ getQHeaderViewDisabled_background_color WRITE setQHeaderViewDisabled_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderViewDisabled_border_color)
    Q_PROPERTY(QColor QHeaderViewDisabled_border_color READ getQHeaderViewDisabled_border_color WRITE setQHeaderViewDisabled_border_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QHeaderViewDisabled_color)
    Q_PROPERTY(QColor QHeaderViewDisabled_color READ getQHeaderViewDisabled_color WRITE setQHeaderViewDisabled_color)
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabWidgetPane_border_color)    
    Q_PROPERTY(QColor QTabWidgetPane_border_color READ getQTabWidgetPane_border_color WRITE setQTabWidgetPane_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabWidgetPane_background_color)    
    Q_PROPERTY(QColor QTabWidgetPane_background_color READ getQTabWidgetPane_background_color WRITE setQTabWidgetPane_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabBarTab_border_color)    
    Q_PROPERTY(QColor QTabBarTab_border_color READ getQTabBarTab_border_color WRITE setQTabBarTab_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabBarTabSelected_background_color)    
    Q_PROPERTY(QColor QTabBarTabSelected_background_color READ getQTabBarTabSelected_background_color WRITE setQTabBarTabSelected_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabBarTabSelected_color)    
    Q_PROPERTY(QColor QTabBarTabSelected_color READ getQTabBarTabSelected_color WRITE setQTabBarTabSelected_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabBarTabNotSelected_background_color)    
    Q_PROPERTY(QColor QTabBarTabNotSelected_background_color READ getQTabBarTabNotSelected_background_color WRITE setQTabBarTabNotSelected_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTabBarTabNotSelectedHover_background_color)    
    Q_PROPERTY(QColor QTabBarTabNotSelectedHover_background_color READ getQTabBarTabNotSelectedHover_background_color WRITE setQTabBarTabNotSelectedHover_background_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTextEdit_background_color)    
    Q_PROPERTY(QColor QTextEdit_background_color READ getQTextEdit_background_color WRITE setQTextEdit_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTextEdit_color)    
    Q_PROPERTY(QColor QTextEdit_color READ getQTextEdit_color WRITE setQTextEdit_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QLineEdit_color)    
    Q_PROPERTY(QColor QLineEdit_color READ getQLineEdit_color WRITE setQLineEdit_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QLineEdit_background_color)    
    Q_PROPERTY(QColor QLineEdit_background_color READ getQLineEdit_background_color WRITE setQLineEdit_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QLineEdit_border_color)    
    Q_PROPERTY(QColor QLineEdit_border_color READ getQLineEdit_border_color WRITE setQLineEdit_border_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QLineEditDisabled_background_color)    
    Q_PROPERTY(QColor QLineEditDisabled_background_color READ getQLineEditDisabled_background_color WRITE setQLineEditDisabled_background_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, QLineEditError_background_color)    
    Q_PROPERTY(QColor QLineEditError_background_color READ getQLineEditError_background_color WRITE setQLineEditError_background_color)    
        
    
    SIMPL_INSTANCE_PROPERTY(QColor, QSpinBoxArrow_background_color)
    Q_PROPERTY(QColor QSpinBoxArrow_background_color READ getQSpinBoxArrow_background_color WRITE setQSpinBoxArrow_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QSpinBoxArrow_hover_background_color)
    Q_PROPERTY(QColor QSpinBoxArrow_hover_background_color READ getQSpinBoxArrow_hover_background_color WRITE setQSpinBoxArrow_hover_background_color)


    SIMPL_INSTANCE_PROPERTY(QColor, QComboBox_border_color)
    Q_PROPERTY(QColor QComboBox_border_color READ getQComboBox_border_color WRITE setQComboBox_border_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBox_background_color)
    Q_PROPERTY(QColor QComboBox_background_color READ getQComboBox_background_color WRITE setQComboBox_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxArrow_background_color)
    Q_PROPERTY(QColor QComboBoxArrow_background_color READ getQComboBoxArrow_background_color WRITE setQComboBoxArrow_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxArrow_hover_background_color)
    Q_PROPERTY(QColor QComboBoxArrow_hover_background_color READ getQComboBoxArrow_hover_background_color WRITE setQComboBoxArrow_hover_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxItem_border_color)
    Q_PROPERTY(QColor QComboBoxItem_border_color READ getQComboBoxItem_border_color WRITE setQComboBoxItem_border_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxItem_selection_color)
    Q_PROPERTY(QColor QComboBoxItem_selection_color READ getQComboBoxItem_selection_color WRITE setQComboBoxItem_selection_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxItem_background_color)
    Q_PROPERTY(QColor QComboBoxItem_background_color READ getQComboBoxItem_background_color WRITE setQComboBoxItem_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxDisabled_background_color)
    Q_PROPERTY(QColor QComboBoxDisabled_background_color READ getQComboBoxDisabled_background_color WRITE setQComboBoxDisabled_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QComboBoxDisabled_color)
    Q_PROPERTY(QColor QComboBoxDisabled_color READ getQComboBoxDisabled_color WRITE setQComboBoxDisabled_color)

    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolBar_background_color)    
    Q_PROPERTY(QColor QToolBar_background_color READ getQToolBar_background_color WRITE setQToolBar_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QToolBar_border_color)    
    Q_PROPERTY(QColor QToolBar_border_color READ getQToolBar_border_color WRITE setQToolBar_border_color)
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeView_background_color)    
    Q_PROPERTY(QColor QTreeView_background_color READ getQTreeView_background_color WRITE setQTreeView_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItem_background_color)    
    Q_PROPERTY(QColor QTreeViewItem_background_color READ getQTreeViewItem_background_color WRITE setQTreeViewItem_background_color)    

    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItem_error_background_color)
    Q_PROPERTY(QColor QTreeViewItem_error_background_color READ getQTreeViewItem_error_background_color WRITE setQTreeViewItem_error_background_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItem_color)
    Q_PROPERTY(QColor QTreeViewItem_color READ getQTreeViewItem_color WRITE setQTreeViewItem_color)

    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItem_error_color)
    Q_PROPERTY(QColor QTreeViewItem_error_color READ getQTreeViewItem_error_color WRITE setQTreeViewItem_error_color)

    SIMPL_INSTANCE_PROPERTY(int, QTreeViewItem_font_size)
    Q_PROPERTY(int QTreeViewItem_font_size READ getQTreeViewItem_font_size WRITE setQTreeViewItem_font_size)

    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemHover_background_color)    
    Q_PROPERTY(QColor QTreeViewItemHover_background_color READ getQTreeViewItemHover_background_color WRITE setQTreeViewItemHover_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemHover_color)    
    Q_PROPERTY(QColor QTreeViewItemHover_color READ getQTreeViewItemHover_color WRITE setQTreeViewItemHover_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemSelectedActive_background_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedActive_background_color READ getQTreeViewItemSelectedActive_background_color WRITE setQTreeViewItemSelectedActive_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemSelectedActive_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedActive_color READ getQTreeViewItemSelectedActive_color WRITE setQTreeViewItemSelectedActive_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemSelectedNotActive_background_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedNotActive_background_color READ getQTreeViewItemSelectedNotActive_background_color WRITE setQTreeViewItemSelectedNotActive_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewItemSelectedNotActive_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedNotActive_color READ getQTreeViewItemSelectedNotActive_color WRITE setQTreeViewItemSelectedNotActive_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewBranch_background_color)    
    Q_PROPERTY(QColor QTreeViewBranch_background_color READ getQTreeViewBranch_background_color WRITE setQTreeViewBranch_background_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewBranchHover_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchHover_background_color READ getQTreeViewBranchHover_background_color WRITE setQTreeViewBranchHover_background_color)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewBranchHover_color)    
    Q_PROPERTY(QColor QTreeViewBranchHover_color READ getQTreeViewBranchHover_color WRITE setQTreeViewBranchHover_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewBranchSelectedActive_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchSelectedActive_background_color READ getQTreeViewBranchSelectedActive_background_color WRITE setQTreeViewBranchSelectedActive_background_color)    

    
    SIMPL_INSTANCE_PROPERTY(QColor, QTreeViewBranchSelectedNotActive_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchSelectedNotActive_background_color READ getQTreeViewBranchSelectedNotActive_background_color WRITE setQTreeViewBranchSelectedNotActive_background_color)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, FilterBackgroundColor)    
    Q_PROPERTY(QColor FilterBackgroundColor READ getFilterBackgroundColor WRITE setFilterBackgroundColor)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, FilterSelectionColor)    
    Q_PROPERTY(QColor FilterSelectionColor READ getFilterSelectionColor WRITE setFilterSelectionColor)    
    
    SIMPL_INSTANCE_PROPERTY(QColor, FilterFontColor)    
    Q_PROPERTY(QColor FilterFontColor READ getFilterFontColor WRITE setFilterFontColor)    
    
    
    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_DataContainer_color)
    Q_PROPERTY(QColor DataArrayPath_DataContainer_color READ getDataArrayPath_DataContainer_color WRITE setDataArrayPath_DataContainer_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_AttributeMatrix_color)
    Q_PROPERTY(QColor DataArrayPath_AttributeMatrix_color READ getDataArrayPath_AttributeMatrix_color WRITE setDataArrayPath_AttributeMatrix_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_DataArray_color)
    Q_PROPERTY(QColor DataArrayPath_DataArray_color READ getDataArrayPath_DataArray_color WRITE setDataArrayPath_DataArray_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_Invalid_color)
    Q_PROPERTY(QColor DataArrayPath_Invalid_color READ getDataArrayPath_Invalid_color WRITE setDataArrayPath_Invalid_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_DataContainer_background_color)
    Q_PROPERTY(QColor DataArrayPath_DataContainer_background_color READ getDataArrayPath_DataContainer_background_color WRITE setDataArrayPath_DataContainer_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_AttributeMatrix_background_color)
    Q_PROPERTY(QColor DataArrayPath_AttributeMatrix_background_color READ getDataArrayPath_AttributeMatrix_background_color WRITE setDataArrayPath_AttributeMatrix_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_DataArray_background_color)
    Q_PROPERTY(QColor DataArrayPath_DataArray_background_color READ getDataArrayPath_DataArray_background_color WRITE setDataArrayPath_DataArray_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_Invalid_background_color)
    Q_PROPERTY(QColor DataArrayPath_Invalid_background_color READ getDataArrayPath_Invalid_background_color WRITE setDataArrayPath_Invalid_background_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_border_normal)
    Q_PROPERTY(QColor DataArrayPath_border_normal READ getDataArrayPath_border_normal WRITE setDataArrayPath_border_normal)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_border_not_found)
    Q_PROPERTY(QColor DataArrayPath_border_not_found READ getDataArrayPath_border_not_found WRITE setDataArrayPath_border_not_found)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_border_drag_enabled)
    Q_PROPERTY(QColor DataArrayPath_border_drag_enabled READ getDataArrayPath_border_drag_enabled WRITE setDataArrayPath_border_drag_enabled)

    SIMPL_INSTANCE_PROPERTY(QColor, DataArrayPath_border_drag_disabled)
    Q_PROPERTY(QColor DataArrayPath_border_drag_disabled READ getDataArrayPath_border_drag_disabled WRITE setDataArrayPath_border_drag_disabled)

    SIMPL_INSTANCE_PROPERTY(QColor, DataPathLabel_color)
    Q_PROPERTY(QColor DataPathLabel_color READ getDataPathLabel_color WRITE setDataPathLabel_color)

    SIMPL_INSTANCE_PROPERTY(QColor, DataPathLabel_Disabled_color)
    Q_PROPERTY(QColor DataPathLabel_Disabled_color READ getDataPathLabel_Disabled_color WRITE setDataPathLabel_Disabled_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, SIMPLViewPipelineDockWidgetTitle_inactive_background_color)
    Q_PROPERTY(QColor SIMPLViewPipelineDockWidgetTitle_inactive_background_color READ getSIMPLViewPipelineDockWidgetTitle_inactive_background_color WRITE setSIMPLViewPipelineDockWidgetTitle_inactive_background_color)
    
    SIMPL_INSTANCE_PROPERTY(QColor, SIMPLViewPipelineDockWidgetTitle_inactive_text_color)
    Q_PROPERTY(QColor SIMPLViewPipelineDockWidgetTitle_inactive_text_color READ getSIMPLViewPipelineDockWidgetTitle_inactive_text_color WRITE setSIMPLViewPipelineDockWidgetTitle_inactive_text_color)
        
    /**
     * @brief loadStyleSheet
     * @param jsonFilePath
     * @return
     */
    bool loadStyleSheet(const QString &jsonFilePath);
    
    /**
     * @brief
     * @return
     */
    QString GetUIFont();
    
    /**
     * @brief GetHumanLabelFont
     * @return
     */
    QFont GetHumanLabelFont();
    
    /**
     * @brief GetBrandingLabelFont
     * @return
     */
    QFont GetBrandingLabelFont();
    
    /**
     * @brief GetCategoryFont
     * @return
     */
    QFont GetCategoryFont();
    
    /**
     * @brief GetTitleFont
     * @return
     */
    QFont GetTitleFont();
    
    /**
     * @brief GetFilterBackgroundColor
     * @return
     */
    QColor GetFilterBackgroundColor();
    
    /**
     * @brief GetFilterSelectionColor
     * @return 
     */
    QColor GetFilterSelectionColor();
    
    /**
     * @brief GetFilterFontColor
     * @return 
     */
    QColor GetFilterFontColor();
    
    /**
     * @brief LineEditErrorStyle
     * @param lineEdit
     */
    void LineEditErrorStyle(QLineEdit* lineEdit);
    
    /**
     * @brief LineEditClearStyle
     * @param lineEdit
     */
    void LineEditClearStyle(QLineEdit* lineEdit);
    
    /**
     * @brief LineEditRedErrorStyle
     * @param lineEdit
     */
    void LineEditBackgroundErrorStyle(QLineEdit* lineEdit);
    
    /**
     * @brief SetErrorColor
     * @param widget
     */
    void SetErrorColor(const QString &widgetType, QWidget* widget);
    
    /**
     * @brief DAPSelectionButtonStyle
     * @param exists
     * @return
     */
    QString QToolSelectionButtonStyle(bool exists);
    
    /**
     * @brief SVStyle::ColorForFilterGroup
     * @param grpName
     * @return
     */
    QColor ColorForFilterGroup(const QString &grpName);
    
    /**
     * @brief SVStyle::IconForGroup
     * @param grpName
     * @return
     */
    QIcon IconForGroup(const QString &grpName);

    /**
     * @brief WrapTextWithHtmlStyle This method will take an existing QString and wrap the contents
     * with some HTML <b> or <span> with an internal "style=..." html snippet of code. This makes
     * the string suitable for a QTextEdit. An option to create bold text is also provided.
     * @param msg
     * @param bold
     * @return
     */
    QString WrapTextWithHtmlStyle(const QString& msg, bool bold) const;

  protected:
    SVStyle();

    /**
     * @brief initialize
     */
    void initialize();

  signals:
    void styleSheetLoaded(const QString &styleFilePath);
    
  private:
    static SVStyle* self;

    QString m_CurrentThemeFilePath = ":/SIMPL/StyleSheets/Default.json";

    QStringList m_ColorProperties;

    /**
     * @brief invalidateColorProperties
     */
    void invalidateColorProperties();

    /**
     * @brief loadStringProperty
     * @return
     */
    QString loadStringProperty(const QString &key, QJsonObject cssRepl, QJsonObject varMapping);

    /**
     * @brief loadIntegerProperty
     * @return
     */
    int loadIntegerProperty(const QString &key, QJsonObject cssRepl, QJsonObject varMapping);
};
