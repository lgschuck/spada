---
editor_options: 
  markdown: 
    wrap: 72
---

# Spada 0.1.0.9000 (development version)

## TO DO

## 2025.01.02-1

### Bug Fixes

1 - **Analysis page - q1 object not found**: back to calculate q1 and q3. ([#1](https://github.com/lgschuck/spada/issues/1))

2 - **Metadata - Error in zeros count**: now df_info function uses suna(x == 0) instead of length(x[x == 0]) ([#2](https://github.com/lgschuck/spada/issues/2))

### Improvements

1 - utils functions

* **df_info** now uses **suna** instead of length, this change fix errors and provide gain in speed.

* **deletion of format_color_bar and main_value_box** functions given that they are now in use anymore

2 - export_file_module

* now the **nav_panel is outside** the module (inside spada function). This gives the module better "format" being a **bslib::card**

* layout_column_wrap replaced for fluidRow and column (better look)

3 - spada function

* **new page sidebar**: for now showing info about active dataset, in the future will receive
links/shortcuts to other parts of the app

* **new menu Analysis**: the Analysis page became Exploratory and inputs change names from pA_ to pA_E_

* **new menu Options**: Config page and Exit are inside this menu. In the future general optiions and settings will be here.

* new Nav Item **Active Dataset**: now with popover to show rows, cols, NA's and size

* in Data when new name is set a msg is shown.

## 2024.12.30-1

### Improvements

1 - General

* Created **zzz.R** and inserted utils::globalVariables for global variables (check note)

* Value boxes: **resized** to give more space for other elements

2 - utils functions

* CSS, js_exit, operators and date formats passed to utils.R

3 - spada function

* new **export_file module** and migration of Export to Data page

* Use of **IQR** to calcule interquartile distance (faster than q3 - q1 manually)

* Buttons of Edit page to backup now with new ID's (pE_export_ replaced by pE_)

## 2024.12.29-2

### Bug Fixes

1 - 'Edit' Page > Convert

* on changing the active dataset the selected variable may not exist in the new acive dataset and an error is raised from data.table (variable does not exist). **Fix with if clause testing for presence of var in the active dataset**.

### Improvements

1 - utils functions

* **dt_info**: now return number and percentage of zeros for each variable
* **gt_info**: Merge columns with number and percentage values

2 - function spada

* **Delete dataset**: new functionality to delete a selected dataset (as long as it is not the active)
* **Icons**: inclusion of icons in Data tab (data page) and trash icon changed to trash-can
* **Convert**: change renderUi for conditionalPanel to convert to Date variables
* **Analysis - Bins**: bins now inside conditionalPanel and layout_colums substituted by fluidRow for better look
* **Analysis - Boxplot by group**: removed validate to max number of groups
* **stopApp**: now app stops when the browser tab is closed. [Auto kill - Dean Attali](https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app)

## 2024.12.29-1

### Improvements

1 - New functionality: **copy dataset** (Data page)

2 - **Config** page now is a module

3 - New Some reactives now with bindCache

### Bug Fixes

1 - gt cannot show complex in opt_interactive, now convert to char before apply gt function

### Improvements

1 - utils functions

* change color in value boxes (to **gray**) for better looking

* number of rows (value box) now with **decimals**

2 - funciton spada

* **Visual changes**: many visual changes (still in search for an identity)

*	**Analysis page**: change order of plots, because the dot plot took to long to load (histogram way quicker)
*	**Analysis page**: insertion of **validates** in plots and table of stats

* new **Config page** with options of colors (plots)

* value box **Var with biggest size**, now showing formatted number and the 'Bytes' word

## 2024.12.27-1

### Improvements

1 - utils functions

* df_info: improvement in performance (something like half the time in big datasets - 1e6 rows)

* new function: gt_info to generate metadata with gt package

* color palettes: new palettes to df_info function

2 - spada function

* background color change (from # f2f2f2 to #f9f9f9)

* change title to Spada

* change Summary page to Data page

* gt tables instead of DT datatables

## 2024.12.25-1

1 - metadata: new unique info in df_info

2 - new function is_valid_name

3 - possibility of inserting several datasets

4 - visual changes in general

5 - analysis page receives only non-NA variables

6 - new react as conversion trigger

## 2024.12.23-1

1 - Visual improvements (font size, spacing, gradient in card_header)

2 - New function to convert variables and improvements in filter inputs
(especially date inputs)

3 - New Tab 'Table' in Analysis page

4 - Improvements in filter rows and select columns

5 - Improvements in data_info function

6 - Replace of verbatimTextInputs for textInputs

7 - In Filter (Page Edit) only show the existing levels when variable is
factor

## 2024.12.22-1

1 - Inclusion of Order tab in Edit page: order rows and order columns

## 2024.12.19-1

1 - Creation of utils functionss

2 - insert requirement of select variables, operators or formats in edit
page buttons

3 - change actionButtons for bslib input_task_buttons

## 2024.12.18-1

Update in spada function:

1 - Edit page:

-   new between filters

-   inclusion of updateSelectInmput for operators

-   new tab with variables info

-   better filtering(especially for factors)

2 - General:

-   rename some objects

-   several visual improvments

3 - Analysis

-   new boxplot by group

## 2024.12.17-1

1 - Improvements in filters (%in%)

2 - inclusion of Convert tab

3 - visual improvements

4 - improvements in datatable options
