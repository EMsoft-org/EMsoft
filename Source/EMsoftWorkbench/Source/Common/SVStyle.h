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
    ~SVStyle() override;
    
    /**
     * @brief
     * @return
     */
    static SVStyle* Instance();

    Q_PROPERTY(QColor Fader_color READ getFader_color WRITE setFader_color)
    Q_PROPERTY(QColor Widget_Error_color READ getWidget_Error_color WRITE setWidget_Error_color)    
    Q_PROPERTY(QColor Text_Error_color READ getText_Error_color WRITE setText_Error_color)
    Q_PROPERTY(QColor CentralWidget_background_color READ getCentralWidget_background_color WRITE setCentralWidget_background_color)    
    Q_PROPERTY(QColor QLabel_color READ getQLabel_color WRITE setQLabel_color)    
    Q_PROPERTY(QColor FilterInputWidget_background_color READ getFilterInputWidget_background_color WRITE setFilterInputWidget_background_color)    
    Q_PROPERTY(QColor QScrollArea_background_color READ getQScrollArea_background_color WRITE setQScrollArea_background_color)    
    Q_PROPERTY(QColor VariablesTabContents_background_color READ getVariablesTabContents_background_color WRITE setVariablesTabContents_background_color)    
    Q_PROPERTY(QColor FilterParameterWidget_background_color READ getFilterParameterWidget_background_color WRITE setFilterParameterWidget_background_color)    
    Q_PROPERTY(QColor FilterParameterWidget_border_color READ getFilterParameterWidget_border_color WRITE setFilterParameterWidget_border_color)
    Q_PROPERTY(QColor QGroupBoxTitle_background_color READ getQGroupBoxTitle_background_color WRITE setQGroupBoxTitle_background_color)    
    Q_PROPERTY(QColor QGroupBox_background_color READ getQGroupBox_background_color WRITE setQGroupBox_background_color)    
    Q_PROPERTY(QColor QDockWidget_border_color READ getQDockWidget_border_color WRITE setQDockWidget_border_color)    
    Q_PROPERTY(QColor QDockWidget_color READ getQDockWidget_color WRITE setQDockWidget_color)    
    Q_PROPERTY(QColor QDockWidgetTitle_background_color READ getQDockWidgetTitle_background_color WRITE setQDockWidgetTitle_background_color)    
    Q_PROPERTY(QColor QDockWidgetTitle_color READ getQDockWidgetTitle_color WRITE setQDockWidgetTitle_color)    
    Q_PROPERTY(QColor QListView_background_color READ getQListView_background_color WRITE setQListView_background_color)    
    Q_PROPERTY(QColor QListView_color READ getQListView_color WRITE setQListView_color)    
    Q_PROPERTY(QColor QListViewItemHover_background_color READ getQListViewItemHover_background_color WRITE setQListViewItemHover_background_color)    
    Q_PROPERTY(QColor QListViewItemHover_color READ getQListViewItemHover_color WRITE setQListViewItemHover_color)    
    Q_PROPERTY(QColor QListViewItemSelected_background_color READ getQListViewItemSelected_background_color WRITE setQListViewItemSelected_background_color)    
    Q_PROPERTY(QColor QListViewItemSelected_color READ getQListViewItemSelected_color WRITE setQListViewItemSelected_color)    
    Q_PROPERTY(QColor QMainWindowSeparator_background_color READ getQMainWindowSeparator_background_color WRITE setQMainWindowSeparator_background_color)    
    Q_PROPERTY(QColor QMenuBar_background_color READ getQMenuBar_background_color WRITE setQMenuBar_background_color)    
    Q_PROPERTY(QColor QMenuBarItem_color READ getQMenuBarItem_color WRITE setQMenuBarItem_color)    
    Q_PROPERTY(QColor QMenuBarItemPressed_color READ getQMenuBarItemPressed_color WRITE setQMenuBarItemPressed_color)    
    Q_PROPERTY(QColor QMenuBarItemSelected_background_color READ getQMenuBarItemSelected_background_color WRITE setQMenuBarItemSelected_background_color)    
    Q_PROPERTY(QColor QMenuBarItemSelected_color READ getQMenuBarItemSelected_color WRITE setQMenuBarItemSelected_color)    
    Q_PROPERTY(QColor QMenu_background_color READ getQMenu_background_color WRITE setQMenu_background_color)    
    Q_PROPERTY(QColor QMenu_color READ getQMenu_color WRITE setQMenu_color)    
    Q_PROPERTY(QColor QMenuItemSelected_background_color READ getQMenuItemSelected_background_color WRITE setQMenuItemSelected_background_color)    
    Q_PROPERTY(QColor QMenuItemSelected_color READ getQMenuItemSelected_color WRITE setQMenuItemSelected_color)    
    Q_PROPERTY(QColor QMenuItemDisabled_background_color READ getQMenuItemDisabled_background_color WRITE setQMenuItemDisabled_background_color)
    Q_PROPERTY(QColor QPushButton_background_color READ getQPushButton_background_color WRITE setQPushButton_background_color)    
    Q_PROPERTY(QColor QPushButton_border_color READ getQPushButton_border_color WRITE setQPushButton_border_color)    
    Q_PROPERTY(QColor QPushButtonHover_background_color READ getQPushButtonHover_background_color WRITE setQPushButtonHover_background_color)    
    Q_PROPERTY(QColor QPushButtonPressed_background_color READ getQPushButtonPressed_background_color WRITE setQPushButtonPressed_background_color)    
    Q_PROPERTY(QColor QPushButtonDefault_background_color READ getQPushButtonDefault_background_color WRITE setQPushButtonDefault_background_color)
    Q_PROPERTY(QColor QPushButtonDefaultPressed_background_color READ getQPushButtonDefaultPressed_background_color WRITE setQPushButtonDefaultPressed_background_color)
    Q_PROPERTY(QColor QPushButtonDefaultHover_background_color READ getQPushButtonDefaultHover_background_color WRITE setQPushButtonDefaultHover_background_color)
    Q_PROPERTY(QColor QPushButtonDefault_border_color READ getQPushButtonDefault_border_color WRITE setQPushButtonDefault_border_color)
    Q_PROPERTY(QColor QPushButtonDefault_text_color READ getQPushButtonDefault_text_color WRITE setQPushButtonDefault_text_color)
    Q_PROPERTY(QColor QSplitter_handle_start_color READ getQSplitter_handle_start_color WRITE setQSplitter_handle_start_color)
    Q_PROPERTY(QColor QSplitter_handle_end_color READ getQSplitter_handle_end_color WRITE setQSplitter_handle_end_color)
    Q_PROPERTY(QColor QToolButton_background_color READ getQToolButton_background_color WRITE setQToolButton_background_color)    
    Q_PROPERTY(QColor QToolButton_color READ getQToolButton_color WRITE setQToolButton_color)    
    Q_PROPERTY(QColor QToolButton_border_color READ getQToolButton_border_color WRITE setQToolButton_border_color)    
    Q_PROPERTY(QColor QToolButtonChecked_background_color READ getQToolButtonChecked_background_color WRITE setQToolButtonChecked_background_color)    
    Q_PROPERTY(QColor QToolButtonDisabled_background_color READ getQToolButtonDisabled_background_color WRITE setQToolButtonDisabled_background_color)
    Q_PROPERTY(QColor QToolButtonDisabled_color READ getQToolButtonDisabled_color WRITE setQToolButtonDisabled_color)
    Q_PROPERTY(QColor QStatusBar_border_color READ getQStatusBar_border_color WRITE setQStatusBar_border_color)    
    Q_PROPERTY(QColor QTableWidget_color READ getQTableWidget_color WRITE setQTableWidget_color)
    Q_PROPERTY(QColor QTableWidget_selected_background_color READ getQTableWidget_selected_background_color WRITE setQTableWidget_selected_background_color)
    Q_PROPERTY(QColor QHeaderView_background_color READ getQHeaderView_background_color WRITE setQHeaderView_background_color)    
    Q_PROPERTY(QColor QHeaderView_border_color READ getQHeaderView_border_color WRITE setQHeaderView_border_color)    
    Q_PROPERTY(QColor QHeaderView_color READ getQHeaderView_color WRITE setQHeaderView_color)   
    Q_PROPERTY(QColor QHeaderViewDisabled_background_color READ getQHeaderViewDisabled_background_color WRITE setQHeaderViewDisabled_background_color)
    Q_PROPERTY(QColor QHeaderViewDisabled_border_color READ getQHeaderViewDisabled_border_color WRITE setQHeaderViewDisabled_border_color)
    Q_PROPERTY(QColor QHeaderViewDisabled_color READ getQHeaderViewDisabled_color WRITE setQHeaderViewDisabled_color)
    Q_PROPERTY(QColor QTabWidgetPane_border_color READ getQTabWidgetPane_border_color WRITE setQTabWidgetPane_border_color)    
    Q_PROPERTY(QColor QTabWidgetPane_background_color READ getQTabWidgetPane_background_color WRITE setQTabWidgetPane_background_color)    
    Q_PROPERTY(QColor QTabBarTab_border_color READ getQTabBarTab_border_color WRITE setQTabBarTab_border_color)    
    Q_PROPERTY(QColor QTabBarTabSelected_background_color READ getQTabBarTabSelected_background_color WRITE setQTabBarTabSelected_background_color)    
    Q_PROPERTY(QColor QTabBarTabSelected_color READ getQTabBarTabSelected_color WRITE setQTabBarTabSelected_color)    
    Q_PROPERTY(QColor QTabBarTabNotSelected_background_color READ getQTabBarTabNotSelected_background_color WRITE setQTabBarTabNotSelected_background_color)    
    Q_PROPERTY(QColor QTabBarTabNotSelectedHover_background_color READ getQTabBarTabNotSelectedHover_background_color WRITE setQTabBarTabNotSelectedHover_background_color)    
    Q_PROPERTY(QColor QTextEdit_background_color READ getQTextEdit_background_color WRITE setQTextEdit_background_color)    
    Q_PROPERTY(QColor QTextEdit_color READ getQTextEdit_color WRITE setQTextEdit_color)    
    Q_PROPERTY(QColor QLineEdit_color READ getQLineEdit_color WRITE setQLineEdit_color)    
    Q_PROPERTY(QColor QLineEdit_background_color READ getQLineEdit_background_color WRITE setQLineEdit_background_color)    
    Q_PROPERTY(QColor QLineEdit_border_color READ getQLineEdit_border_color WRITE setQLineEdit_border_color)    
    Q_PROPERTY(QColor QLineEditDisabled_background_color READ getQLineEditDisabled_background_color WRITE setQLineEditDisabled_background_color)    
    Q_PROPERTY(QColor QLineEditError_background_color READ getQLineEditError_background_color WRITE setQLineEditError_background_color)    
    Q_PROPERTY(QColor QSpinBoxArrow_background_color READ getQSpinBoxArrow_background_color WRITE setQSpinBoxArrow_background_color)
    Q_PROPERTY(QColor QSpinBoxArrow_hover_background_color READ getQSpinBoxArrow_hover_background_color WRITE setQSpinBoxArrow_hover_background_color)
    Q_PROPERTY(QColor QComboBox_border_color READ getQComboBox_border_color WRITE setQComboBox_border_color)
    Q_PROPERTY(QColor QComboBox_background_color READ getQComboBox_background_color WRITE setQComboBox_background_color)
    Q_PROPERTY(QColor QComboBoxArrow_background_color READ getQComboBoxArrow_background_color WRITE setQComboBoxArrow_background_color)
    Q_PROPERTY(QColor QComboBoxArrow_hover_background_color READ getQComboBoxArrow_hover_background_color WRITE setQComboBoxArrow_hover_background_color)
    Q_PROPERTY(QColor QComboBoxItem_border_color READ getQComboBoxItem_border_color WRITE setQComboBoxItem_border_color)
    Q_PROPERTY(QColor QComboBoxItem_selection_color READ getQComboBoxItem_selection_color WRITE setQComboBoxItem_selection_color)
    Q_PROPERTY(QColor QComboBoxItem_background_color READ getQComboBoxItem_background_color WRITE setQComboBoxItem_background_color)
    Q_PROPERTY(QColor QComboBoxDisabled_background_color READ getQComboBoxDisabled_background_color WRITE setQComboBoxDisabled_background_color)
    Q_PROPERTY(QColor QComboBoxDisabled_color READ getQComboBoxDisabled_color WRITE setQComboBoxDisabled_color)
    Q_PROPERTY(QColor QToolBar_background_color READ getQToolBar_background_color WRITE setQToolBar_background_color)    
    Q_PROPERTY(QColor QToolBar_border_color READ getQToolBar_border_color WRITE setQToolBar_border_color)
    Q_PROPERTY(QColor QTreeView_background_color READ getQTreeView_background_color WRITE setQTreeView_background_color)    
    Q_PROPERTY(QColor QTreeViewItem_background_color READ getQTreeViewItem_background_color WRITE setQTreeViewItem_background_color)    
    Q_PROPERTY(QColor QTreeViewItem_error_background_color READ getQTreeViewItem_error_background_color WRITE setQTreeViewItem_error_background_color)
    Q_PROPERTY(QColor QTreeViewItem_color READ getQTreeViewItem_color WRITE setQTreeViewItem_color)
    Q_PROPERTY(QColor QTreeViewItem_error_color READ getQTreeViewItem_error_color WRITE setQTreeViewItem_error_color)
    Q_PROPERTY(int QTreeViewItem_font_size READ getQTreeViewItem_font_size WRITE setQTreeViewItem_font_size)
    Q_PROPERTY(QColor QTreeViewItemHover_background_color READ getQTreeViewItemHover_background_color WRITE setQTreeViewItemHover_background_color)    
    Q_PROPERTY(QColor QTreeViewItemHover_color READ getQTreeViewItemHover_color WRITE setQTreeViewItemHover_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedActive_background_color READ getQTreeViewItemSelectedActive_background_color WRITE setQTreeViewItemSelectedActive_background_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedActive_color READ getQTreeViewItemSelectedActive_color WRITE setQTreeViewItemSelectedActive_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedNotActive_background_color READ getQTreeViewItemSelectedNotActive_background_color WRITE setQTreeViewItemSelectedNotActive_background_color)    
    Q_PROPERTY(QColor QTreeViewItemSelectedNotActive_color READ getQTreeViewItemSelectedNotActive_color WRITE setQTreeViewItemSelectedNotActive_color)    
    Q_PROPERTY(QColor QTreeViewBranch_background_color READ getQTreeViewBranch_background_color WRITE setQTreeViewBranch_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchHover_background_color READ getQTreeViewBranchHover_background_color WRITE setQTreeViewBranchHover_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchHover_color READ getQTreeViewBranchHover_color WRITE setQTreeViewBranchHover_color)    
    Q_PROPERTY(QColor QTreeViewBranchSelectedActive_background_color READ getQTreeViewBranchSelectedActive_background_color WRITE setQTreeViewBranchSelectedActive_background_color)    
    Q_PROPERTY(QColor QTreeViewBranchSelectedNotActive_background_color READ getQTreeViewBranchSelectedNotActive_background_color WRITE setQTreeViewBranchSelectedNotActive_background_color)    
    Q_PROPERTY(QColor FilterBackgroundColor READ getFilterBackgroundColor WRITE setFilterBackgroundColor)    
    Q_PROPERTY(QColor FilterSelectionColor READ getFilterSelectionColor WRITE setFilterSelectionColor)    
    Q_PROPERTY(QColor FilterFontColor READ getFilterFontColor WRITE setFilterFontColor)    
    Q_PROPERTY(QColor DataArrayPath_DataContainer_color READ getDataArrayPath_DataContainer_color WRITE setDataArrayPath_DataContainer_color)
    Q_PROPERTY(QColor DataArrayPath_AttributeMatrix_color READ getDataArrayPath_AttributeMatrix_color WRITE setDataArrayPath_AttributeMatrix_color)
    Q_PROPERTY(QColor DataArrayPath_DataArray_color READ getDataArrayPath_DataArray_color WRITE setDataArrayPath_DataArray_color)
    Q_PROPERTY(QColor DataArrayPath_Invalid_color READ getDataArrayPath_Invalid_color WRITE setDataArrayPath_Invalid_color)
    Q_PROPERTY(QColor DataArrayPath_DataContainer_background_color READ getDataArrayPath_DataContainer_background_color WRITE setDataArrayPath_DataContainer_background_color)
    Q_PROPERTY(QColor DataArrayPath_AttributeMatrix_background_color READ getDataArrayPath_AttributeMatrix_background_color WRITE setDataArrayPath_AttributeMatrix_background_color)
    Q_PROPERTY(QColor DataArrayPath_DataArray_background_color READ getDataArrayPath_DataArray_background_color WRITE setDataArrayPath_DataArray_background_color)
    Q_PROPERTY(QColor DataArrayPath_Invalid_background_color READ getDataArrayPath_Invalid_background_color WRITE setDataArrayPath_Invalid_background_color)
    Q_PROPERTY(QColor DataArrayPath_border_normal READ getDataArrayPath_border_normal WRITE setDataArrayPath_border_normal)
    Q_PROPERTY(QColor DataArrayPath_border_not_found READ getDataArrayPath_border_not_found WRITE setDataArrayPath_border_not_found)
    Q_PROPERTY(QColor DataArrayPath_border_drag_enabled READ getDataArrayPath_border_drag_enabled WRITE setDataArrayPath_border_drag_enabled)
    Q_PROPERTY(QColor DataArrayPath_border_drag_disabled READ getDataArrayPath_border_drag_disabled WRITE setDataArrayPath_border_drag_disabled)
    Q_PROPERTY(QColor DataPathLabel_color READ getDataPathLabel_color WRITE setDataPathLabel_color)
    Q_PROPERTY(QColor DataPathLabel_Disabled_color READ getDataPathLabel_Disabled_color WRITE setDataPathLabel_Disabled_color)
    Q_PROPERTY(QColor SIMPLViewPipelineDockWidgetTitle_inactive_background_color READ getSIMPLViewPipelineDockWidgetTitle_inactive_background_color WRITE setSIMPLViewPipelineDockWidgetTitle_inactive_background_color)
    Q_PROPERTY(QColor SIMPLViewPipelineDockWidgetTitle_inactive_text_color READ getSIMPLViewPipelineDockWidgetTitle_inactive_text_color WRITE setSIMPLViewPipelineDockWidgetTitle_inactive_text_color)

    /**
    * @brief Setter property for Fader_color
    */
    void setFader_color(const QColor& value); 

    /**
    * @brief Getter property for Fader_color
    * @return Value of Fader_color
    */
    QColor getFader_color() const;

    /**
    * @brief Setter property for Widget_Error_color
    */
    void setWidget_Error_color(const QColor& value); 

    /**
    * @brief Getter property for Widget_Error_color
    * @return Value of Widget_Error_color
    */
    QColor getWidget_Error_color() const;

    /**
    * @brief Setter property for Text_Error_color
    */
    void setText_Error_color(const QColor& value); 

    /**
    * @brief Getter property for Text_Error_color
    * @return Value of Text_Error_color
    */
    QColor getText_Error_color() const;
        
    /**
    * @brief Setter property for CentralWidget_background_color
    */
    void setCentralWidget_background_color(const QColor& value); 

    /**
    * @brief Getter property for CentralWidget_background_color
    * @return Value of CentralWidget_background_color
    */
    QColor getCentralWidget_background_color() const;
    
    /**
    * @brief Setter property for QLabel_color
    */
    void setQLabel_color(const QColor& value); 

    /**
    * @brief Getter property for QLabel_color
    * @return Value of QLabel_color
    */
    QColor getQLabel_color() const;

    /**
    * @brief Setter property for FilterInputWidget_background_color
    */
    void setFilterInputWidget_background_color(const QColor& value); 

    /**
    * @brief Getter property for FilterInputWidget_background_color
    * @return Value of FilterInputWidget_background_color
    */
    QColor getFilterInputWidget_background_color() const;
    
    /**
    * @brief Setter property for QScrollArea_background_color
    */
    void setQScrollArea_background_color(const QColor& value); 

    /**
    * @brief Getter property for QScrollArea_background_color
    * @return Value of QScrollArea_background_color
    */
    QColor getQScrollArea_background_color() const;
    
    /**
    * @brief Setter property for VariablesTabContents_background_color
    */
    void setVariablesTabContents_background_color(const QColor& value); 

    /**
    * @brief Getter property for VariablesTabContents_background_color
    * @return Value of VariablesTabContents_background_color
    */
    QColor getVariablesTabContents_background_color() const;
    
    /**
    * @brief Setter property for FilterParameterWidget_background_color
    */
    void setFilterParameterWidget_background_color(const QColor& value); 

    /**
    * @brief Getter property for FilterParameterWidget_background_color
    * @return Value of FilterParameterWidget_background_color
    */
    QColor getFilterParameterWidget_background_color() const;

    /**
    * @brief Setter property for FilterParameterWidget_border_color
    */
    void setFilterParameterWidget_border_color(const QColor& value); 

    /**
    * @brief Getter property for FilterParameterWidget_border_color
    * @return Value of FilterParameterWidget_border_color
    */
    QColor getFilterParameterWidget_border_color() const;
    
    /**
    * @brief Setter property for QGroupBoxTitle_background_color
    */
    void setQGroupBoxTitle_background_color(const QColor& value); 

    /**
    * @brief Getter property for QGroupBoxTitle_background_color
    * @return Value of QGroupBoxTitle_background_color
    */
    QColor getQGroupBoxTitle_background_color() const;
    
    /**
    * @brief Setter property for QGroupBox_background_color
    */
    void setQGroupBox_background_color(const QColor& value); 

    /**
    * @brief Getter property for QGroupBox_background_color
    * @return Value of QGroupBox_background_color
    */
    QColor getQGroupBox_background_color() const;
    
    /**
    * @brief Setter property for QDockWidget_border_color
    */
    void setQDockWidget_border_color(const QColor& value); 

    /**
    * @brief Getter property for QDockWidget_border_color
    * @return Value of QDockWidget_border_color
    */
    QColor getQDockWidget_border_color() const;
    
    /**
    * @brief Setter property for QDockWidget_color
    */
    void setQDockWidget_color(const QColor& value); 

    /**
    * @brief Getter property for QDockWidget_color
    * @return Value of QDockWidget_color
    */
    QColor getQDockWidget_color() const;
    
    /**
    * @brief Setter property for QDockWidgetTitle_background_color
    */
    void setQDockWidgetTitle_background_color(const QColor& value); 

    /**
    * @brief Getter property for QDockWidgetTitle_background_color
    * @return Value of QDockWidgetTitle_background_color
    */
    QColor getQDockWidgetTitle_background_color() const;
    
    /**
    * @brief Setter property for QDockWidgetTitle_color
    */
    void setQDockWidgetTitle_color(const QColor& value); 

    /**
    * @brief Getter property for QDockWidgetTitle_color
    * @return Value of QDockWidgetTitle_color
    */
    QColor getQDockWidgetTitle_color() const;
    
    /**
    * @brief Setter property for QListView_background_color
    */
    void setQListView_background_color(const QColor& value); 

    /**
    * @brief Getter property for QListView_background_color
    * @return Value of QListView_background_color
    */
    QColor getQListView_background_color() const;
    
    /**
    * @brief Setter property for QListView_color
    */
    void setQListView_color(const QColor& value); 

    /**
    * @brief Getter property for QListView_color
    * @return Value of QListView_color
    */
    QColor getQListView_color() const;
    
    
    /**
    * @brief Setter property for QListViewItemHover_background_color
    */
    void setQListViewItemHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QListViewItemHover_background_color
    * @return Value of QListViewItemHover_background_color
    */
    QColor getQListViewItemHover_background_color() const;
    
    /**
    * @brief Setter property for QListViewItemHover_color
    */
    void setQListViewItemHover_color(const QColor& value); 

    /**
    * @brief Getter property for QListViewItemHover_color
    * @return Value of QListViewItemHover_color
    */
    QColor getQListViewItemHover_color() const;
    
    /**
    * @brief Setter property for QListViewItemSelected_background_color
    */
    void setQListViewItemSelected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QListViewItemSelected_background_color
    * @return Value of QListViewItemSelected_background_color
    */
    QColor getQListViewItemSelected_background_color() const;
    
    /**
    * @brief Setter property for QListViewItemSelected_color
    */
    void setQListViewItemSelected_color(const QColor& value); 

    /**
    * @brief Getter property for QListViewItemSelected_color
    * @return Value of QListViewItemSelected_color
    */
    QColor getQListViewItemSelected_color() const;
    
    
    /**
    * @brief Setter property for QMainWindowSeparator_background_color
    */
    void setQMainWindowSeparator_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMainWindowSeparator_background_color
    * @return Value of QMainWindowSeparator_background_color
    */
    QColor getQMainWindowSeparator_background_color() const;
    
    
    /**
    * @brief Setter property for QMenuBar_background_color
    */
    void setQMenuBar_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuBar_background_color
    * @return Value of QMenuBar_background_color
    */
    QColor getQMenuBar_background_color() const;
    
    /**
    * @brief Setter property for QMenuBarItem_color
    */
    void setQMenuBarItem_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuBarItem_color
    * @return Value of QMenuBarItem_color
    */
    QColor getQMenuBarItem_color() const;
    
    /**
    * @brief Setter property for QMenuBarItemPressed_color
    */
    void setQMenuBarItemPressed_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuBarItemPressed_color
    * @return Value of QMenuBarItemPressed_color
    */
    QColor getQMenuBarItemPressed_color() const;
    
    /**
    * @brief Setter property for QMenuBarItemSelected_background_color
    */
    void setQMenuBarItemSelected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuBarItemSelected_background_color
    * @return Value of QMenuBarItemSelected_background_color
    */
    QColor getQMenuBarItemSelected_background_color() const;
    
    /**
    * @brief Setter property for QMenuBarItemSelected_color
    */
    void setQMenuBarItemSelected_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuBarItemSelected_color
    * @return Value of QMenuBarItemSelected_color
    */
    QColor getQMenuBarItemSelected_color() const;
    
    
    /**
    * @brief Setter property for QMenu_background_color
    */
    void setQMenu_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMenu_background_color
    * @return Value of QMenu_background_color
    */
    QColor getQMenu_background_color() const;
    
    /**
    * @brief Setter property for QMenu_color
    */
    void setQMenu_color(const QColor& value); 

    /**
    * @brief Getter property for QMenu_color
    * @return Value of QMenu_color
    */
    QColor getQMenu_color() const;
    
    /**
    * @brief Setter property for QMenuItemSelected_background_color
    */
    void setQMenuItemSelected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuItemSelected_background_color
    * @return Value of QMenuItemSelected_background_color
    */
    QColor getQMenuItemSelected_background_color() const;
    
    /**
    * @brief Setter property for QMenuItemSelected_color
    */
    void setQMenuItemSelected_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuItemSelected_color
    * @return Value of QMenuItemSelected_color
    */
    QColor getQMenuItemSelected_color() const;

    /**
    * @brief Setter property for QMenuItemDisabled_background_color
    */
    void setQMenuItemDisabled_background_color(const QColor& value); 

    /**
    * @brief Getter property for QMenuItemDisabled_background_color
    * @return Value of QMenuItemDisabled_background_color
    */
    QColor getQMenuItemDisabled_background_color() const;
    
    /**
    * @brief Setter property for QPushButton_background_color
    */
    void setQPushButton_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButton_background_color
    * @return Value of QPushButton_background_color
    */
    QColor getQPushButton_background_color() const;
    
    /**
    * @brief Setter property for QPushButton_border_color
    */
    void setQPushButton_border_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButton_border_color
    * @return Value of QPushButton_border_color
    */
    QColor getQPushButton_border_color() const;
    
    /**
    * @brief Setter property for QPushButtonHover_background_color
    */
    void setQPushButtonHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonHover_background_color
    * @return Value of QPushButtonHover_background_color
    */
    QColor getQPushButtonHover_background_color() const;
    
    /**
    * @brief Setter property for QPushButtonPressed_background_color
    */
    void setQPushButtonPressed_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonPressed_background_color
    * @return Value of QPushButtonPressed_background_color
    */
    QColor getQPushButtonPressed_background_color() const;
    
    /**
    * @brief Setter property for QPushButtonDefault_background_color
    */
    void setQPushButtonDefault_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonDefault_background_color
    * @return Value of QPushButtonDefault_background_color
    */
    QColor getQPushButtonDefault_background_color() const;

    /**
    * @brief Setter property for QPushButtonDefaultPressed_background_color
    */
    void setQPushButtonDefaultPressed_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonDefaultPressed_background_color
    * @return Value of QPushButtonDefaultPressed_background_color
    */
    QColor getQPushButtonDefaultPressed_background_color() const;

    /**
    * @brief Setter property for QPushButtonDefaultHover_background_color
    */
    void setQPushButtonDefaultHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonDefaultHover_background_color
    * @return Value of QPushButtonDefaultHover_background_color
    */
    QColor getQPushButtonDefaultHover_background_color() const;

    /**
    * @brief Setter property for QPushButtonDefault_border_color
    */
    void setQPushButtonDefault_border_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonDefault_border_color
    * @return Value of QPushButtonDefault_border_color
    */
    QColor getQPushButtonDefault_border_color() const;

    /**
    * @brief Setter property for QPushButtonDefault_text_color
    */
    void setQPushButtonDefault_text_color(const QColor& value); 

    /**
    * @brief Getter property for QPushButtonDefault_text_color
    * @return Value of QPushButtonDefault_text_color
    */
    QColor getQPushButtonDefault_text_color() const;


    /**
    * @brief Setter property for QSplitter_handle_start_color
    */
    void setQSplitter_handle_start_color(const QColor& value); 

    /**
    * @brief Getter property for QSplitter_handle_start_color
    * @return Value of QSplitter_handle_start_color
    */
    QColor getQSplitter_handle_start_color() const;

    /**
    * @brief Setter property for QSplitter_handle_end_color
    */
    void setQSplitter_handle_end_color(const QColor& value); 

    /**
    * @brief Getter property for QSplitter_handle_end_color
    * @return Value of QSplitter_handle_end_color
    */
    QColor getQSplitter_handle_end_color() const;

    
    /**
    * @brief Setter property for QToolButton_background_color
    */
    void setQToolButton_background_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButton_background_color
    * @return Value of QToolButton_background_color
    */
    QColor getQToolButton_background_color() const;
    
    /**
    * @brief Setter property for QToolButton_color
    */
    void setQToolButton_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButton_color
    * @return Value of QToolButton_color
    */
    QColor getQToolButton_color() const;
    
    /**
    * @brief Setter property for QToolButton_border_color
    */
    void setQToolButton_border_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButton_border_color
    * @return Value of QToolButton_border_color
    */
    QColor getQToolButton_border_color() const;
    
    /**
    * @brief Setter property for QToolButtonChecked_background_color
    */
    void setQToolButtonChecked_background_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButtonChecked_background_color
    * @return Value of QToolButtonChecked_background_color
    */
    QColor getQToolButtonChecked_background_color() const;
    
    /**
    * @brief Setter property for QToolButtonDisabled_background_color
    */
    void setQToolButtonDisabled_background_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButtonDisabled_background_color
    * @return Value of QToolButtonDisabled_background_color
    */
    QColor getQToolButtonDisabled_background_color() const;

    /**
    * @brief Setter property for QToolButtonDisabled_color
    */
    void setQToolButtonDisabled_color(const QColor& value); 

    /**
    * @brief Getter property for QToolButtonDisabled_color
    * @return Value of QToolButtonDisabled_color
    */
    QColor getQToolButtonDisabled_color() const;

    /**
    * @brief Setter property for QStatusBar_border_color
    */
    void setQStatusBar_border_color(const QColor& value); 

    /**
    * @brief Getter property for QStatusBar_border_color
    * @return Value of QStatusBar_border_color
    */
    QColor getQStatusBar_border_color() const;
    
    
    /**
    * @brief Setter property for QTableWidget_color
    */
    void setQTableWidget_color(const QColor& value); 

    /**
    * @brief Getter property for QTableWidget_color
    * @return Value of QTableWidget_color
    */
    QColor getQTableWidget_color() const;

    /**
    * @brief Setter property for QTableWidget_selected_background_color
    */
    void setQTableWidget_selected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTableWidget_selected_background_color
    * @return Value of QTableWidget_selected_background_color
    */
    QColor getQTableWidget_selected_background_color() const;
    
    /**
    * @brief Setter property for QHeaderView_background_color
    */
    void setQHeaderView_background_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderView_background_color
    * @return Value of QHeaderView_background_color
    */
    QColor getQHeaderView_background_color() const;
    
    /**
    * @brief Setter property for QHeaderView_border_color
    */
    void setQHeaderView_border_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderView_border_color
    * @return Value of QHeaderView_border_color
    */
    QColor getQHeaderView_border_color() const;
    
    /**
    * @brief Setter property for QHeaderView_color
    */
    void setQHeaderView_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderView_color
    * @return Value of QHeaderView_color
    */
    QColor getQHeaderView_color() const;

    /**
    * @brief Setter property for QHeaderViewDisabled_background_color
    */
    void setQHeaderViewDisabled_background_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderViewDisabled_background_color
    * @return Value of QHeaderViewDisabled_background_color
    */
    QColor getQHeaderViewDisabled_background_color() const;

    /**
    * @brief Setter property for QHeaderViewDisabled_border_color
    */
    void setQHeaderViewDisabled_border_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderViewDisabled_border_color
    * @return Value of QHeaderViewDisabled_border_color
    */
    QColor getQHeaderViewDisabled_border_color() const;

    /**
    * @brief Setter property for QHeaderViewDisabled_color
    */
    void setQHeaderViewDisabled_color(const QColor& value); 

    /**
    * @brief Getter property for QHeaderViewDisabled_color
    * @return Value of QHeaderViewDisabled_color
    */
    QColor getQHeaderViewDisabled_color() const;
    
    
    /**
    * @brief Setter property for QTabWidgetPane_border_color
    */
    void setQTabWidgetPane_border_color(const QColor& value); 

    /**
    * @brief Getter property for QTabWidgetPane_border_color
    * @return Value of QTabWidgetPane_border_color
    */
    QColor getQTabWidgetPane_border_color() const;
    
    /**
    * @brief Setter property for QTabWidgetPane_background_color
    */
    void setQTabWidgetPane_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTabWidgetPane_background_color
    * @return Value of QTabWidgetPane_background_color
    */
    QColor getQTabWidgetPane_background_color() const;
    
    /**
    * @brief Setter property for QTabBarTab_border_color
    */
    void setQTabBarTab_border_color(const QColor& value); 

    /**
    * @brief Getter property for QTabBarTab_border_color
    * @return Value of QTabBarTab_border_color
    */
    QColor getQTabBarTab_border_color() const;
    
    /**
    * @brief Setter property for QTabBarTabSelected_background_color
    */
    void setQTabBarTabSelected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTabBarTabSelected_background_color
    * @return Value of QTabBarTabSelected_background_color
    */
    QColor getQTabBarTabSelected_background_color() const;
    
    /**
    * @brief Setter property for QTabBarTabSelected_color
    */
    void setQTabBarTabSelected_color(const QColor& value); 

    /**
    * @brief Getter property for QTabBarTabSelected_color
    * @return Value of QTabBarTabSelected_color
    */
    QColor getQTabBarTabSelected_color() const;
    
    /**
    * @brief Setter property for QTabBarTabNotSelected_background_color
    */
    void setQTabBarTabNotSelected_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTabBarTabNotSelected_background_color
    * @return Value of QTabBarTabNotSelected_background_color
    */
    QColor getQTabBarTabNotSelected_background_color() const;
    
    /**
    * @brief Setter property for QTabBarTabNotSelectedHover_background_color
    */
    void setQTabBarTabNotSelectedHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTabBarTabNotSelectedHover_background_color
    * @return Value of QTabBarTabNotSelectedHover_background_color
    */
    QColor getQTabBarTabNotSelectedHover_background_color() const;
    
    
    /**
    * @brief Setter property for QTextEdit_background_color
    */
    void setQTextEdit_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTextEdit_background_color
    * @return Value of QTextEdit_background_color
    */
    QColor getQTextEdit_background_color() const;
    
    /**
    * @brief Setter property for QTextEdit_color
    */
    void setQTextEdit_color(const QColor& value); 

    /**
    * @brief Getter property for QTextEdit_color
    * @return Value of QTextEdit_color
    */
    QColor getQTextEdit_color() const;
    
    
    /**
    * @brief Setter property for QLineEdit_color
    */
    void setQLineEdit_color(const QColor& value); 

    /**
    * @brief Getter property for QLineEdit_color
    * @return Value of QLineEdit_color
    */
    QColor getQLineEdit_color() const;
    
    /**
    * @brief Setter property for QLineEdit_background_color
    */
    void setQLineEdit_background_color(const QColor& value); 

    /**
    * @brief Getter property for QLineEdit_background_color
    * @return Value of QLineEdit_background_color
    */
    QColor getQLineEdit_background_color() const;
    
    /**
    * @brief Setter property for QLineEdit_border_color
    */
    void setQLineEdit_border_color(const QColor& value); 

    /**
    * @brief Getter property for QLineEdit_border_color
    * @return Value of QLineEdit_border_color
    */
    QColor getQLineEdit_border_color() const;
    
    /**
    * @brief Setter property for QLineEditDisabled_background_color
    */
    void setQLineEditDisabled_background_color(const QColor& value); 

    /**
    * @brief Getter property for QLineEditDisabled_background_color
    * @return Value of QLineEditDisabled_background_color
    */
    QColor getQLineEditDisabled_background_color() const;

    /**
    * @brief Setter property for QLineEditError_background_color
    */
    void setQLineEditError_background_color(const QColor& value); 

    /**
    * @brief Getter property for QLineEditError_background_color
    * @return Value of QLineEditError_background_color
    */
    QColor getQLineEditError_background_color() const;
        
    
    /**
    * @brief Setter property for QSpinBoxArrow_background_color
    */
    void setQSpinBoxArrow_background_color(const QColor& value); 

    /**
    * @brief Getter property for QSpinBoxArrow_background_color
    * @return Value of QSpinBoxArrow_background_color
    */
    QColor getQSpinBoxArrow_background_color() const;

    /**
    * @brief Setter property for QSpinBoxArrow_hover_background_color
    */
    void setQSpinBoxArrow_hover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QSpinBoxArrow_hover_background_color
    * @return Value of QSpinBoxArrow_hover_background_color
    */
    QColor getQSpinBoxArrow_hover_background_color() const;


    /**
    * @brief Setter property for QComboBox_border_color
    */
    void setQComboBox_border_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBox_border_color
    * @return Value of QComboBox_border_color
    */
    QColor getQComboBox_border_color() const;

    /**
    * @brief Setter property for QComboBox_background_color
    */
    void setQComboBox_background_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBox_background_color
    * @return Value of QComboBox_background_color
    */
    QColor getQComboBox_background_color() const;

    /**
    * @brief Setter property for QComboBoxArrow_background_color
    */
    void setQComboBoxArrow_background_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxArrow_background_color
    * @return Value of QComboBoxArrow_background_color
    */
    QColor getQComboBoxArrow_background_color() const;

    /**
    * @brief Setter property for QComboBoxArrow_hover_background_color
    */
    void setQComboBoxArrow_hover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxArrow_hover_background_color
    * @return Value of QComboBoxArrow_hover_background_color
    */
    QColor getQComboBoxArrow_hover_background_color() const;

    /**
    * @brief Setter property for QComboBoxItem_border_color
    */
    void setQComboBoxItem_border_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxItem_border_color
    * @return Value of QComboBoxItem_border_color
    */
    QColor getQComboBoxItem_border_color() const;

    /**
    * @brief Setter property for QComboBoxItem_selection_color
    */
    void setQComboBoxItem_selection_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxItem_selection_color
    * @return Value of QComboBoxItem_selection_color
    */
    QColor getQComboBoxItem_selection_color() const;

    /**
    * @brief Setter property for QComboBoxItem_background_color
    */
    void setQComboBoxItem_background_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxItem_background_color
    * @return Value of QComboBoxItem_background_color
    */
    QColor getQComboBoxItem_background_color() const;

    /**
    * @brief Setter property for QComboBoxDisabled_background_color
    */
    void setQComboBoxDisabled_background_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxDisabled_background_color
    * @return Value of QComboBoxDisabled_background_color
    */
    QColor getQComboBoxDisabled_background_color() const;

    /**
    * @brief Setter property for QComboBoxDisabled_color
    */
    void setQComboBoxDisabled_color(const QColor& value); 

    /**
    * @brief Getter property for QComboBoxDisabled_color
    * @return Value of QComboBoxDisabled_color
    */
    QColor getQComboBoxDisabled_color() const;

    
    /**
    * @brief Setter property for QToolBar_background_color
    */
    void setQToolBar_background_color(const QColor& value); 

    /**
    * @brief Getter property for QToolBar_background_color
    * @return Value of QToolBar_background_color
    */
    QColor getQToolBar_background_color() const;
    
    /**
    * @brief Setter property for QToolBar_border_color
    */
    void setQToolBar_border_color(const QColor& value); 

    /**
    * @brief Getter property for QToolBar_border_color
    * @return Value of QToolBar_border_color
    */
    QColor getQToolBar_border_color() const;
    
    
    /**
    * @brief Setter property for QTreeView_background_color
    */
    void setQTreeView_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeView_background_color
    * @return Value of QTreeView_background_color
    */
    QColor getQTreeView_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewItem_background_color
    */
    void setQTreeViewItem_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItem_background_color
    * @return Value of QTreeViewItem_background_color
    */
    QColor getQTreeViewItem_background_color() const;

    /**
    * @brief Setter property for QTreeViewItem_error_background_color
    */
    void setQTreeViewItem_error_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItem_error_background_color
    * @return Value of QTreeViewItem_error_background_color
    */
    QColor getQTreeViewItem_error_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewItem_color
    */
    void setQTreeViewItem_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItem_color
    * @return Value of QTreeViewItem_color
    */
    QColor getQTreeViewItem_color() const;

    /**
    * @brief Setter property for QTreeViewItem_error_color
    */
    void setQTreeViewItem_error_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItem_error_color
    * @return Value of QTreeViewItem_error_color
    */
    QColor getQTreeViewItem_error_color() const;

    /**
    * @brief Setter property for QTreeViewItem_font_size
    */
    void setQTreeViewItem_font_size(const int& value); 

    /**
    * @brief Getter property for QTreeViewItem_font_size
    * @return Value of QTreeViewItem_font_size
    */
    int getQTreeViewItem_font_size() const;

    /**
    * @brief Setter property for QTreeViewItemHover_background_color
    */
    void setQTreeViewItemHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemHover_background_color
    * @return Value of QTreeViewItemHover_background_color
    */
    QColor getQTreeViewItemHover_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewItemHover_color
    */
    void setQTreeViewItemHover_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemHover_color
    * @return Value of QTreeViewItemHover_color
    */
    QColor getQTreeViewItemHover_color() const;
    
    /**
    * @brief Setter property for QTreeViewItemSelectedActive_background_color
    */
    void setQTreeViewItemSelectedActive_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemSelectedActive_background_color
    * @return Value of QTreeViewItemSelectedActive_background_color
    */
    QColor getQTreeViewItemSelectedActive_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewItemSelectedActive_color
    */
    void setQTreeViewItemSelectedActive_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemSelectedActive_color
    * @return Value of QTreeViewItemSelectedActive_color
    */
    QColor getQTreeViewItemSelectedActive_color() const;
    
    /**
    * @brief Setter property for QTreeViewItemSelectedNotActive_background_color
    */
    void setQTreeViewItemSelectedNotActive_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemSelectedNotActive_background_color
    * @return Value of QTreeViewItemSelectedNotActive_background_color
    */
    QColor getQTreeViewItemSelectedNotActive_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewItemSelectedNotActive_color
    */
    void setQTreeViewItemSelectedNotActive_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewItemSelectedNotActive_color
    * @return Value of QTreeViewItemSelectedNotActive_color
    */
    QColor getQTreeViewItemSelectedNotActive_color() const;
    
    /**
    * @brief Setter property for QTreeViewBranch_background_color
    */
    void setQTreeViewBranch_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewBranch_background_color
    * @return Value of QTreeViewBranch_background_color
    */
    QColor getQTreeViewBranch_background_color() const;
    
    
    /**
    * @brief Setter property for QTreeViewBranchHover_background_color
    */
    void setQTreeViewBranchHover_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewBranchHover_background_color
    * @return Value of QTreeViewBranchHover_background_color
    */
    QColor getQTreeViewBranchHover_background_color() const;
    
    /**
    * @brief Setter property for QTreeViewBranchHover_color
    */
    void setQTreeViewBranchHover_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewBranchHover_color
    * @return Value of QTreeViewBranchHover_color
    */
    QColor getQTreeViewBranchHover_color() const;
    
    
    /**
    * @brief Setter property for QTreeViewBranchSelectedActive_background_color
    */
    void setQTreeViewBranchSelectedActive_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewBranchSelectedActive_background_color
    * @return Value of QTreeViewBranchSelectedActive_background_color
    */
    QColor getQTreeViewBranchSelectedActive_background_color() const;

    
    /**
    * @brief Setter property for QTreeViewBranchSelectedNotActive_background_color
    */
    void setQTreeViewBranchSelectedNotActive_background_color(const QColor& value); 

    /**
    * @brief Getter property for QTreeViewBranchSelectedNotActive_background_color
    * @return Value of QTreeViewBranchSelectedNotActive_background_color
    */
    QColor getQTreeViewBranchSelectedNotActive_background_color() const;
    
    
    /**
    * @brief Setter property for FilterBackgroundColor
    */
    void setFilterBackgroundColor(const QColor& value); 

    /**
    * @brief Getter property for FilterBackgroundColor
    * @return Value of FilterBackgroundColor
    */
    QColor getFilterBackgroundColor() const;
    
    /**
    * @brief Setter property for FilterSelectionColor
    */
    void setFilterSelectionColor(const QColor& value); 

    /**
    * @brief Getter property for FilterSelectionColor
    * @return Value of FilterSelectionColor
    */
    QColor getFilterSelectionColor() const;
    
    /**
    * @brief Setter property for FilterFontColor
    */
    void setFilterFontColor(const QColor& value); 

    /**
    * @brief Getter property for FilterFontColor
    * @return Value of FilterFontColor
    */
    QColor getFilterFontColor() const;
    
    
    /**
    * @brief Setter property for DataArrayPath_DataContainer_color
    */
    void setDataArrayPath_DataContainer_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_DataContainer_color
    * @return Value of DataArrayPath_DataContainer_color
    */
    QColor getDataArrayPath_DataContainer_color() const;

    /**
    * @brief Setter property for DataArrayPath_AttributeMatrix_color
    */
    void setDataArrayPath_AttributeMatrix_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_AttributeMatrix_color
    * @return Value of DataArrayPath_AttributeMatrix_color
    */
    QColor getDataArrayPath_AttributeMatrix_color() const;

    /**
    * @brief Setter property for DataArrayPath_DataArray_color
    */
    void setDataArrayPath_DataArray_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_DataArray_color
    * @return Value of DataArrayPath_DataArray_color
    */
    QColor getDataArrayPath_DataArray_color() const;

    /**
    * @brief Setter property for DataArrayPath_Invalid_color
    */
    void setDataArrayPath_Invalid_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_Invalid_color
    * @return Value of DataArrayPath_Invalid_color
    */
    QColor getDataArrayPath_Invalid_color() const;

    /**
    * @brief Setter property for DataArrayPath_DataContainer_background_color
    */
    void setDataArrayPath_DataContainer_background_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_DataContainer_background_color
    * @return Value of DataArrayPath_DataContainer_background_color
    */
    QColor getDataArrayPath_DataContainer_background_color() const;

    /**
    * @brief Setter property for DataArrayPath_AttributeMatrix_background_color
    */
    void setDataArrayPath_AttributeMatrix_background_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_AttributeMatrix_background_color
    * @return Value of DataArrayPath_AttributeMatrix_background_color
    */
    QColor getDataArrayPath_AttributeMatrix_background_color() const;

    /**
    * @brief Setter property for DataArrayPath_DataArray_background_color
    */
    void setDataArrayPath_DataArray_background_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_DataArray_background_color
    * @return Value of DataArrayPath_DataArray_background_color
    */
    QColor getDataArrayPath_DataArray_background_color() const;

    /**
    * @brief Setter property for DataArrayPath_Invalid_background_color
    */
    void setDataArrayPath_Invalid_background_color(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_Invalid_background_color
    * @return Value of DataArrayPath_Invalid_background_color
    */
    QColor getDataArrayPath_Invalid_background_color() const;

    /**
    * @brief Setter property for DataArrayPath_border_normal
    */
    void setDataArrayPath_border_normal(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_border_normal
    * @return Value of DataArrayPath_border_normal
    */
    QColor getDataArrayPath_border_normal() const;

    /**
    * @brief Setter property for DataArrayPath_border_not_found
    */
    void setDataArrayPath_border_not_found(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_border_not_found
    * @return Value of DataArrayPath_border_not_found
    */
    QColor getDataArrayPath_border_not_found() const;

    /**
    * @brief Setter property for DataArrayPath_border_drag_enabled
    */
    void setDataArrayPath_border_drag_enabled(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_border_drag_enabled
    * @return Value of DataArrayPath_border_drag_enabled
    */
    QColor getDataArrayPath_border_drag_enabled() const;

    /**
    * @brief Setter property for DataArrayPath_border_drag_disabled
    */
    void setDataArrayPath_border_drag_disabled(const QColor& value); 

    /**
    * @brief Getter property for DataArrayPath_border_drag_disabled
    * @return Value of DataArrayPath_border_drag_disabled
    */
    QColor getDataArrayPath_border_drag_disabled() const;

    /**
    * @brief Setter property for DataPathLabel_color
    */
    void setDataPathLabel_color(const QColor& value); 

    /**
    * @brief Getter property for DataPathLabel_color
    * @return Value of DataPathLabel_color
    */
    QColor getDataPathLabel_color() const;

    /**
    * @brief Setter property for DataPathLabel_Disabled_color
    */
    void setDataPathLabel_Disabled_color(const QColor& value); 

    /**
    * @brief Getter property for DataPathLabel_Disabled_color
    * @return Value of DataPathLabel_Disabled_color
    */
    QColor getDataPathLabel_Disabled_color() const;
    
    /**
    * @brief Setter property for SIMPLViewPipelineDockWidgetTitle_inactive_background_color
    */
    void setSIMPLViewPipelineDockWidgetTitle_inactive_background_color(const QColor& value); 

    /**
    * @brief Getter property for SIMPLViewPipelineDockWidgetTitle_inactive_background_color
    * @return Value of SIMPLViewPipelineDockWidgetTitle_inactive_background_color
    */
    QColor getSIMPLViewPipelineDockWidgetTitle_inactive_background_color() const;
    
    /**
    * @brief Setter property for SIMPLViewPipelineDockWidgetTitle_inactive_text_color
    */
    void setSIMPLViewPipelineDockWidgetTitle_inactive_text_color(const QColor& value); 

    /**
    * @brief Getter property for SIMPLViewPipelineDockWidgetTitle_inactive_text_color
    * @return Value of SIMPLViewPipelineDockWidgetTitle_inactive_text_color
    */
    QColor getSIMPLViewPipelineDockWidgetTitle_inactive_text_color() const;

    /**
    * @brief Getter property for CurrentThemeFilePath
    * @return Value of CurrentThemeFilePath
     */
    QString getCurrentThemeFilePath() const;
        
    /**
     * @brief loadStyleSheet
     * @param filePath
     * @return
     */
    bool loadStyleSheet(const QString &filePath);
    
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

    QColor m_Fader_color;
    QColor m_Widget_Error_color;
    QColor m_Text_Error_color;
    QColor m_CentralWidget_background_color;
    QColor m_QLabel_color;
    QColor m_FilterInputWidget_background_color;
    QColor m_QScrollArea_background_color;
    QColor m_VariablesTabContents_background_color;
    QColor m_FilterParameterWidget_background_color;
    QColor m_FilterParameterWidget_border_color;
    QColor m_QGroupBoxTitle_background_color;
    QColor m_QGroupBox_background_color;
    QColor m_QDockWidget_border_color;
    QColor m_QDockWidget_color;
    QColor m_QDockWidgetTitle_background_color;
    QColor m_QDockWidgetTitle_color;
    QColor m_QListView_background_color;
    QColor m_QListView_color;
    QColor m_QListViewItemHover_background_color;
    QColor m_QListViewItemHover_color;
    QColor m_QListViewItemSelected_background_color;
    QColor m_QListViewItemSelected_color;
    QColor m_QMainWindowSeparator_background_color;
    QColor m_QMenuBar_background_color;
    QColor m_QMenuBarItem_color;
    QColor m_QMenuBarItemPressed_color;
    QColor m_QMenuBarItemSelected_background_color;
    QColor m_QMenuBarItemSelected_color;
    QColor m_QMenu_background_color;
    QColor m_QMenu_color;
    QColor m_QMenuItemSelected_background_color;
    QColor m_QMenuItemSelected_color;
    QColor m_QMenuItemDisabled_background_color;
    QColor m_QPushButton_background_color;
    QColor m_QPushButton_border_color;
    QColor m_QPushButtonHover_background_color;
    QColor m_QPushButtonPressed_background_color;
    QColor m_QPushButtonDefault_background_color;
    QColor m_QPushButtonDefaultPressed_background_color;
    QColor m_QPushButtonDefaultHover_background_color;
    QColor m_QPushButtonDefault_border_color;
    QColor m_QPushButtonDefault_text_color;
    QColor m_QSplitter_handle_start_color;
    QColor m_QSplitter_handle_end_color;
    QColor m_QToolButton_background_color;
    QColor m_QToolButton_color;
    QColor m_QToolButton_border_color;
    QColor m_QToolButtonChecked_background_color;
    QColor m_QToolButtonDisabled_background_color;
    QColor m_QToolButtonDisabled_color;
    QColor m_QStatusBar_border_color;
    QColor m_QTableWidget_color;
    QColor m_QTableWidget_selected_background_color;
    QColor m_QHeaderView_background_color;
    QColor m_QHeaderView_border_color;
    QColor m_QHeaderView_color;
    QColor m_QHeaderViewDisabled_background_color;
    QColor m_QHeaderViewDisabled_border_color;
    QColor m_QHeaderViewDisabled_color;
    QColor m_QTabWidgetPane_border_color;
    QColor m_QTabWidgetPane_background_color;
    QColor m_QTabBarTab_border_color;
    QColor m_QTabBarTabSelected_background_color;
    QColor m_QTabBarTabSelected_color;
    QColor m_QTabBarTabNotSelected_background_color;
    QColor m_QTabBarTabNotSelectedHover_background_color;
    QColor m_QTextEdit_background_color;
    QColor m_QTextEdit_color;
    QColor m_QLineEdit_color;
    QColor m_QLineEdit_background_color;
    QColor m_QLineEdit_border_color;
    QColor m_QLineEditDisabled_background_color;
    QColor m_QLineEditError_background_color;
    QColor m_QSpinBoxArrow_background_color;
    QColor m_QSpinBoxArrow_hover_background_color;
    QColor m_QComboBox_border_color;
    QColor m_QComboBox_background_color;
    QColor m_QComboBoxArrow_background_color;
    QColor m_QComboBoxArrow_hover_background_color;
    QColor m_QComboBoxItem_border_color;
    QColor m_QComboBoxItem_selection_color;
    QColor m_QComboBoxItem_background_color;
    QColor m_QComboBoxDisabled_background_color;
    QColor m_QComboBoxDisabled_color;
    QColor m_QToolBar_background_color;
    QColor m_QToolBar_border_color;
    QColor m_QTreeView_background_color;
    QColor m_QTreeViewItem_background_color;
    QColor m_QTreeViewItem_error_background_color;
    QColor m_QTreeViewItem_color;
    QColor m_QTreeViewItem_error_color;
    int m_QTreeViewItem_font_size;
    QColor m_QTreeViewItemHover_background_color;
    QColor m_QTreeViewItemHover_color;
    QColor m_QTreeViewItemSelectedActive_background_color;
    QColor m_QTreeViewItemSelectedActive_color;
    QColor m_QTreeViewItemSelectedNotActive_background_color;
    QColor m_QTreeViewItemSelectedNotActive_color;
    QColor m_QTreeViewBranch_background_color;
    QColor m_QTreeViewBranchHover_background_color;
    QColor m_QTreeViewBranchHover_color;
    QColor m_QTreeViewBranchSelectedActive_background_color;
    QColor m_QTreeViewBranchSelectedNotActive_background_color;
    QColor m_FilterBackgroundColor;
    QColor m_FilterSelectionColor;
    QColor m_FilterFontColor;
    QColor m_DataArrayPath_DataContainer_color;
    QColor m_DataArrayPath_AttributeMatrix_color;
    QColor m_DataArrayPath_DataArray_color;
    QColor m_DataArrayPath_Invalid_color;
    QColor m_DataArrayPath_DataContainer_background_color;
    QColor m_DataArrayPath_AttributeMatrix_background_color;
    QColor m_DataArrayPath_DataArray_background_color;
    QColor m_DataArrayPath_Invalid_background_color;
    QColor m_DataArrayPath_border_normal;
    QColor m_DataArrayPath_border_not_found;
    QColor m_DataArrayPath_border_drag_enabled;
    QColor m_DataArrayPath_border_drag_disabled;
    QColor m_DataPathLabel_color;
    QColor m_DataPathLabel_Disabled_color;
    QColor m_SIMPLViewPipelineDockWidgetTitle_inactive_background_color;
    QColor m_SIMPLViewPipelineDockWidgetTitle_inactive_text_color;

    /**
     * @brief invalidateColorProperties
     */
    void invalidateColorProperties();

    /**
     * @brief loadStyleSheetFromJson
     * @param filePath
     */
    bool loadStyleSheetFromJson(const QString &filePath);

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

  public:
    SVStyle(const SVStyle&) = delete; // Copy Constructor Not Implemented
    SVStyle(SVStyle&&) = delete;      // Move Constructor Not Implemented
    SVStyle& operator=(const SVStyle&) = delete; // Copy Assignment Not Implemented
    SVStyle& operator=(SVStyle&&) = delete;      // Move Assignment Not Implemented
};
