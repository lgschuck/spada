---
editor_options: 
  markdown: 
    wrap: 72
---

# Spada 0.1.0.9000 (development version)

## TO DO

1 - Allow filter multiple conditions

2 - Allow create and calculate variables

3 - Functionalities to detect and treat duplicates

4 - Sample filters for datasets

5 - Functionalities to detect and treat outliers

6 - Inference z and t test

7 - Models: linear model, logistic regression, Kmeans, Trees

## 2025.01.26-1

### Bug Fixes

1 - **Filter rows: operators NA and not NA requires a value to apply the filter**: fixed, filter_rows_module.R refactored, now with much more robust check for filters, operators and values ([#10](https://github.com/lgschuck/spada/issues/10))

### Improvements

1 - **New dependencie**: package [DescTools](https://andrisignorell.github.io/DescTools/)

2 - **Descriptive Stats** module: now with Mode (DescTools package) for numeri, character and factor variables

3 - **Filter Rows** module: refactored to check operators and values. New operators: Outlier (and Not Outlier) and Logical (TRUE and FALSE)

4 - **Stats table** module: now with Mode (DescTools package) for numeric, character and factor variables

5 - **utils.R**: Operators for rows filters now in several objects for better organization

## 2025.01.22-1

### Improvements

1 - **Correlation** module: parameters in card sidebar for better use of space

2 - **df_info** function: new test file and now returns empty data.frame (accepted by gt_info) in case of no columns in the entry data.frame

3 - **Normality test** module: now Ks and Shapiro-wilk tests have gt table, save button and statiCards

4 - **Tests**: new/better test files for df_info, is_date, is_valid_name, mina, percentile and suna functions

## 2025.01.21-1

### Improvements

1 - **Correlation** module: new layout, new table with test results, help button (help documentation), save table button and statiCards (shnywidgets) with Correlation and p value.

2 - **Descriptive Stats** module: now with variable selected by default

3 - **Normality test** module: new Help button in Ks test and Shapiro-Wilk test

4 - **utils.R** new function: **get_help_file** to search help in package documentation

5 - **Startup**: new text in startup page (removed 'Loading...')

## 2025.01.20-1

### Improvements

1 - **Startup page**: now with startup page with shinybusy package (new dependencie)

2 - **radioGroupButtons**: change some radioButtons (shiny) for radioGroupButtons (shinyWidgets) in data_overview_module and exploratory_module for better look

3 - **Page config** module: colorPickr now with 'save' mode for better reset of values and other visual changes fo better look

4 - **New Save gt** module: now the gt table can be saved to hmtl, rtf and docx (gt::gt_save function)

5 - **Stats table** module: check for digits if out of range (0, 9) and new save_gt module in this module

6 - **Descriptive Stats** module: insert req to generate stats and new save_gt_module in this module

7 - **show_toast**: change showNotification (shiny) for show_toast (shinyWidgets) for better look

## 2025.01.17-1

### Improvements

1 - **New module**: Filter Rows

2 - **Convert** module: always align right the preview table

3 - **Descriptive Stats** module: now all options inicially as TRUE 

4 - **Order Cols** and **Order Rows** modules: now require selection of at least one variable

5 - **Page Config** module: now with colorPickr from shinyWidgets to more options

6 - **df_info** function: now with nrows and ncols

7 - **gt_info** function: format number of columns n_valid, n_unique, n_zero, n_nas

8 - **Visual**: many css (margins and padding) for better use of the screen space

9 - Fix in test-mina.R

## 2025.01.14-1

### Improvements

1 - **New module**: Normality test

2 - Module **Correlation** > Scatter Plot now has a button to render plot

3 - Module **Descriptive Stats** now has a button to render table

4 - Other small adjustments in internal code and **new tests**

## 2025.01.13-1

### Bug Fixes

1 - **Edit > Filter: Factor var - no levels after choose operator**: fixed
(inserted req(operator)). Now the levels are shown when a factor var and an operator are selected. ([#9](https://github.com/lgschuck/spada/issues/9))

### Improvements

1 - **testthat**: create structure to run tests (test-fina.R as initial test)

2 - **New modules**: Order Rows, Convert Cols and Exploratory

3 - **Correlation** module: insert req in scatter plot

4 - **Page Config** module: now validate value for file size (requires > 1 MB)

5 - New **ttip** function (basic bslib::tooltip) with placement 'top' as default

6 - Insertion of **info-circle** item in **tooltips** (almost everywhere) for better look

7 - Better imports, **using importFrom** for bslib and gt packages

8 - **Remove all gc()** calls

9 - **df_info** function now test anyNA and if TRUE count NA values (speed improvement)

## 2025.01.12-1

### Improvements

1 - **New modules**: Sidebar and Navbar Df Info

2 - New function **filter_rows** in utils.R

3 - **New reactives**: df_active_ncol, df_active_resume_data and df$df_trigger (to use for updates)

4 - **New Vignette**: Intro

## 2025.01.11-2

1 - **New modules**: Order Cols and Select Cols

2 - **Correlation module** with better visual (using card instead of accordion)

## 2025.01.11-1

1 - **Mainly internal organization**, migrating spada UI to **spada_ui.R** function and spada Server to **spada_server.R** function.

2 - **New modules**: Data Overview and Data Highlights

3 - **Visual changes** in Correlation module and Descriptive Stats module

4 - New exported **function is_date**

## 2025.01.10-1

### Bug Fixes

1 - **Analysis > Exploratory: error ins stats table when percentile out of range 0-100**: now test the range and if the input isTruphy ([#8](https://github.com/lgschuck/spada/issues/8))

### Improvements

1 - **Stats table** now is a module

2 - **new module Correlation**

3 - **new module Descriptive Stats**

4 - **Scatter** (Analysis > Exploratory) now with filled points

5 - **export functions**: df_info, gt_info and is_valid_name now are exported

## 2025.01.05-1

### Bug Fixes

1 - **Metadata - object color_fn not found**: new icon for logical and color format (function data_color) applied only if there is valid (non NA) min and max values ([#4](https://github.com/lgschuck/spada/issues/4))

2 - **Edit > Convert - error in preview complex variable convertion**: fixed converting complex to character in the preview given that gt table in opt_interactive does not show complex properly ([#5](https://github.com/lgschuck/spada/issues/5))

3 - **Edit > Filter: error in filtering complex**: now only show/allow operators '== (Equal)', '!= (Not Equal)', 'Is NA (is.na)', 'Not NA (! is.na)', 'In (%in%)' and 'Not In (! %in%)' (same for character and factors) ([#6](https://github.com/lgschuck/spada/issues/6))

4 - **Edit > Filter: accept blank value**: now the value must have length 1 or bigger ([#7](https://github.com/lgschuck/spada/issues/7))

### Improvements

1 - utils functions

2 - page_config_module: correction of a typo

3 - spada function

* new background color in sidebar

* new value boxes in Data > Highligths (rows, valid, unique, zeros) and better server side checking (returning None if absent)

* new itens in navbar: Options > Documentation link and Github link

## 2025.01.03-1

### Bug Fixes

1 - **Data Overview - after Edit only refresh if updat in rows or sample**: fixed with insertion of buttons inside output$pD_over_gt. ([#3](https://github.com/lgschuck/spada/issues/3))

### Improvements

1 - **export_file_module**: separator order now semicolon as default

2 - new **import_file_module**: allows input csv and RDS files

3 - **page_config_module**: new visual and size of input file as parameter

4 - **spada function**:

* sidebar now with Dataset Info accordion open by default

* small visual changes (icons and capital in some titles)

* **shiny.maxRequestSize** set to 500 MB by default

* **datasets_names_react**: now names of the datasets are a reactive (used several times)

* new **buttons in sidebar** accordion to navigate through pages

* new **buttons in active dataset popover** navigate through pages 

### Bug Fixes

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
