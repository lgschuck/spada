---
editor_options: 
  markdown: 
    wrap: 72
---

# Spada 0.2.0 - Future Version

## 0.2.0 Milestone (dec/25)

1 - Functionalities to detect and treat duplicates

2 - Functionalities to detect and treat outliers

3 - Inference: t test

4 - Models: logistic regression

# Spada 0.1.0.9000 (development version)

## 0.1.0 Milestone (sep/25)

1 - Tests: create tests for the app

## 2025.06.21

Highlights: **New tests** for several modules, **Data Overview** allows to choose which dataset to show and insert it to the Output and **fixed bug #38**.

### Improvements

1 - **Data Overview** module: now is possible to choose which dataset to show and insert it to the Output

2 - **Exploratory** module: now the linear model has a gt table for results and metrics (as in Lm module) and button to insert it to the Output. In the boxplot, fixed bug #38.

3 - **Insert Output** module: now there is a warning when the input_element is a gt table with a data.frame (warns if the data.frame has more than 250 rows or cols). nInserted the warning because large gt_table in the Output causes performance degradation.

4 - **Lm** module: new functions to linear model output and metrics (as gt tables)

5 - **utils.R**: new functions to linear model output and metrics (as gt tables), **as.factor** now is an allowed function, busy_label in **btn_task is 'Running'** instead of 'Processing' and internal function **test_dataset** has two parameters (n_row and n_col)

6 - **New tests** for: calculate_cols, descriptive_stats, filter_rows, gt_info, insert_output and save_gt modules.

### Bug Fixes

1 - **Exploratory > Boxplot: warning when variable 2 is integer**: now numeric variable is converted to factor in ggplot function
([#38](https://github.com/lgschuck/spada/issues/38))


## 2025.06.18

Highlights: **New tests** for df metadata (spada_server) and about spada, export file and rename cols modules. Fixed error when **importing Output** (file not found).

### Improvements

1 - **New tests**: df metadata (spada_server) and about spada, export file and rename cols modules

2 - **check_dir** function: new function to check the existance of a directorie just before save or import files

### Bug Fixes

1 - **Output > Import Output: error to import Output if file does not exist**: fixed, now always check the file before import
([#35](https://github.com/lgschuck/spada/issues/35))

## 2025.06.15

Highlights: **Change behaviour when no datasets are inputed**, new **btn-task-cancel** css class and **bug fix in filter One variable Operator**

### Improvements

1 - **No inputed datasets**: now when Spada function is called with no datasets, iris and mtcars are loaded only when the previous session are not restored.

2 - **btn-task-cancel**: new class for cancel actionButtons.

### Bug Fixes

1 - **Edit > Filter Rows: operator is not being updated**: fixed, updateSelectInput was using wrong id
([#34](https://github.com/lgschuck/spada/issues/34))

## 2025.06.14

Highlights: **Save/Restore data and settings** between user session and **bug fix in copy/rename datasets**

### Improvements

1 - **Save/Restore** data and settings: now **it's possible to save and restore** the data (datasets), output and settings from current/previous session.

2 - **Exiting Spada**: on Exit there is the possibility to **save session data**. Also, some new modals for  better user experience.

3 - **Config** module: now there are options for restoring and saving the previous/current session. Also, the **conf directories** are shown allowing the user to track the session data.

4 - **Data Highlights and LM** modules: **new req** functions for better dependencies management.

5 - **Correlation, Exploratory, Normality Test and Z test** modules: change in name of **color objects** (session$userData)

6 - **Output** module: **new modals** for better user experience and **2 new buttons** to save and restore current/previous output.

7 - **spada.R, spada_server.R, utils.R**: new code to accommodate the **Save/Restore** session data events. Some **new functions** to check **format of data** to be restored. 

8 - **spada_themes.R**: new vector with themes names and objects with default values for plot colors.

### Bug Fixes

1 - **Data > Copy dataset and rename dataset: error with active dataset**: fixed, the session$userData$dt$dt onject was not updated befone renaming and in copy dataset the selection was always in the qactive dataset.
([#32](https://github.com/lgschuck/spada/issues/32))

## 2025.06.10-1

Highlights: Mainly **visual** improvements in Gt tables and **Output** module now asks confirmation to reset

### Improvements

1 - **Output** module: now reset button asks confirmation via modal and the buttons have icons

2 - **Insert Output** module: changed order of button for consistency with other modals and new icons in the buttons.

3 - **Gt Tables**: now Gt tables have a Title for better output (impact in several modules)

4 - **Lm** module: some small visual improvements and now **module is considered completed** for version 0.1.0 (will only receive visuals change and fixes if needed)

5 - **Config** module: now module is called config (before was page_config) and has accordions for better look

6 - **Allowed Functions**: inserted fcase, sum, min and max functions

## 2025.06.08-1

Highlights: **Convert columns of raw** type (from inputed data.frames) to character and better extensions test in **Import file** module

### Improvements

1 - **Import file** module: now accepts extensions in lower and upper case.

### Bug Fixes

1 - **Data.frame with raw column: error and malfunction in several places**: now in the input the columns of raw type are converted to character
([#28](https://github.com/lgschuck/spada/issues/28))

## 2025.05.30-1

Highlights: New tests for **spada_server.R** and **order_cols and select_cols** modules.

### Improvements

1 - New tests for **spada_server.R** and **order_cols and select_cols** modules.

2 - In Data > Data: now after click a button the New Name textInput is cleared

### Bug Fixes

1 - **Error in Data > Data: rename and copy datasets**: fixed, error introduced in 2025.05.28-1 release
([#31](https://github.com/lgschuck/spada/issues/31))

## 2025.05.28-1

Highlights: Many reactives as **session$userData** to avoid passing as module args.

### Improvements

1 - **Session$userData**: now using df_active, output and other reactives as userData to avoid passing reactives as module args. Internal use, increases readbility and maintenance of code.

## 2025.05.26-1

Highlights: fixed selection of variables in **Linear Model** module

### Bug Fixes

1 - **Linear Model: error in variable selection**: fixed to update list of variables intead of renderUI.
([#29](https://github.com/lgschuck/spada/issues/29))

## 2025.05.21-1

Highlights: New tests (testthat) for **Data Overview** module

### Improvements

1 - **Data Overview** module: new testthat file and creation idx and data_gt as reactives for automated tests.

## 2025.05.20-1

Highlights: New operations to use in freehand Calculate var (Edit page)

### Improvements

1 - **Calculate Module** received some new operations: as.integer, as.numeric, as.characater, as.double, ifelse, fifelse and if_else.

2 - **Dangerous functions**: now substitute and substitute2 are in the dangerous functions list (internal use)

## 2025.05.19-1

Highlights: New **Linear Model** module, removed docx format in save table and change in plot colors (internal use)

### Improvements

1 - **Plot colors fill and line** now use session$userData, avoid passing meny times as a module parameter

2 - New **Linear Model** module

### Bug Fixes

1 - **Error when Save Table in docx**: docx demandsd pandoc. Format removed
([#26](https://github.com/lgschuck/spada/issues/26))

## 2025.04.14-1

Highlights: Now **Exploratory Module** allow insert element to output

### Improvements

1 - **Exploratory Page** tables: improved tables with absolute and percent values

2 - **Exploratory Page: insert elements to output**: now elements of Exploratory Page can be added to output
([#25](https://github.com/lgschuck/spada/issues/25))

3 - **spada_server.R**: exploratory module now provide return (output element)

4 - **Stats table** module: now returns table to be added to output

## 2025.04.10-1

Highlights: New dependencie **ggplot2** (all plots now are generated as ggplot objects).

### Improvements

1 - **Plots**: all plots now are genereted with ggplot2 (**Correlation Test, Exploratory, Normality Test, Z Test** modules)

### Bug Fixes

1 - **Add plot to output x Action button**: fixed, now plots are generated with ggplot2
([#19](https://github.com/lgschuck/spada/issues/19))

## 2025.04.01-1

Highlights: speed improvement in **Analysis > Exploratory** page. 

### Improvements

1 - **Stats Table** module: removed bindCache that was causing delay in the return of module and plots

## 2025.03.23-1

Highlights: add output elements in **Correlation Test**, **Normality Test** and **Z Test** modules. 

### Improvements

1 - **Correlation** and **Normality Test** modules: now allow to add elements to output page

2 - **Exploratory** module: removed bindCache because it was being used with objects with data and the check for the bindCache was slow, especially for big datasets

3 - **Insert output** module: now req(input_element())

4 - **spada_serverR**: adjust in modules that return output elements

5 - **utils.R**: new function to add table with 2 columns in output

6 - **Z Test** module: now allow to add plot to output page

7 - In many places the scatter plot now uses **dot in pch param** for big length objects (>1e4), this improves the plot speed

8 - Other **small visual improvements** like icons in buttons and positioning of buttons

### Bug Fixes

1 - Update in NAMESPACE: **htmltools::plotTag** function

## 2025.03.12-1

Highlights: new **Output** page with options to export all output elements to html

### Improvements

1 - New **Output and Insert Output** modules: modules for output generation

2 - **Correlation, Descriptive Stats and Z Test** modules: now allow to add elements to output page

3 - **Correlation** module: deleted output$cor_test_results (not used anymore)

4 - **Descriptive Stats** module: now gt formatting passed to reactive instead to render function, allowing send the exact same gt to Output page

5 - **spada_serverR**: adjust in modules that return output elements

6 - **spada_ui.R**: new Output page

7 - **utils.R**: new functions report_card and gen_element_id to Output modules

## 2025.03.10-1

Highlights: new **About** module and fix usage of ns object inside modules

### Improvements

1 - **About** module: shows (for now) the DESCRIPTION file of Spada package and the session info 

2 - **Calculate** modules: new names for inputs and new button to show allowed operations

3 - **Calculate and Filter** modules: new button to show allowed operations

4 - **Server side** of modules: now ns receives session$ns instead of NS(id). See [Modularizing Shiny app code](https://shiny.posit.co/r/articles/improve/modules/)

5 - **spada_ui.R**: pills now in alphabetical order (Edit page) and new about module

6 - **spada_server.R**: new about module

7 - **utils.R**: some new functions in allowed operations and remove of Mode function (DescTools) because returns len 2 causing error

## 2025.03.04-1

Highlights: new **freehand filter** (Filter Rows module), **freehand code to calculate** variable(Calculate Cols module) and new dependencie (**rlang package**).

### Improvements

1 - **Calculate Cols** module: now allows to calculate with freehand code

2 - **Data highlights** module: inserted Values in Most unique and valid values 

3 - **Descriptive Stats** module: fixe req of gt_stats and align gt columns

4 - **Exploratory, Export file, Filter Rows, Import file, Rename Cols** modules: change in Conditional panels (now with ns instead of sprintf)

5 - **Filter Rows** module: now allows filter with freehand code and

6 - **spada.R**: new dependencie (rlang package)

7 - **spada_ui.R**: change Calculate page to receive freehand Calculate Cols

8 - **utils.R**: new **safe_env function** to create env to evaluate freehand code, new test_data function and new operations to insert in safe_env function

## 2025.02.25-1

Highlights: **Calculate cols now with groupby** and Rename cols allows to **rename multiple variables** together

### Improvements

1 - **Calculate Cols** module: now allows to calculate with groupby 

2 - **Rename Cols** module: now allows to rename multiple variables together

3 - **spada.R**: new dependencie: tools package

4 - **spada_themes.R**: new css for startup screen

5 - **spada_ui.R**: change startup screen css (now in spada_themes) and busyindicator options (now the spinner is 'bars2')

6 - **utils.R**: removed Range function (because it returns 2 values), correct a typo in ceilling function (now ceiling) and update in get_help_file for better visual

## 2025.02.19-1

Highlights: New **Calculate Cols** module, better internal control over changes in datasets and many small visual improvements

### Improvements

1 - New **Calculate Cols** module: allow create new variable by applying a function

2 - **Convert Cols, Data Overview, Filter Rows, Order Cols, Order Rows, Rename Cols and Select Cols** modules and **spada_server.R**: now uses data.table::copy to return changes in dataset properly and maintaining the correct update in reactives (problem probably caused by changes by reference in data.table). Removed all triggers (df_trigger reactiValues) used before.

3 - **Data Overview, Navbar_df_info and Sidebar** modules: removed stylling code (passed to spada_themes).

4 - **spada_ui.R**: some new cards (Data Page: metadata and Overview) for better look and Active dataset navbar item now shows only the name (with out 'Active dataset' prefix)

5 - **spada_themes**: new mini-btn class to format small buttons in Sidebar and Navbar Df Info modules

6 - **utils.R**: new objects with functions to use in Calculate Cols module. 

## 2025.02.16-1

Highlights: New **Darkly** theme and new dependencie: **sass** package.

### Improvements

1 - **Correlation, Descriptive Stats, Normality Test, Sidebar, Spada UI, Z Test** modules: changes in visual parameters and elements (e.g. sidebar now has class) for usage of bs_theme

2 - **Export File** module: now downloadButton has class and correct icon

3 - **Filter Rows** module: new Number of Rows filter 

4 - **spada_themes.R**: new objects with themes and usage of sass package

5 - **Spada Server**: now shiny.maxRequestSize = 1000 MB

## 2025.02.13-1

Highlights: Use of selectizeInput with close and clear button for multiple selections

### Improvements

1 - **Convert Cols, Descriptive Stats, Order Cols, Order Rows and Select Cols** modules: now usage of selectizeInput with close and clear button for multiple selections

2 - **utils.R**: filter_rows function now with env = list(var1) in data.table instead of get(var)

3 - **spada.R**: now usage of importFrom for data.table package

## 2025.02.11-1

Highlights: New filter options in Filter Rows Module

### Improvements

1 - **Filter Rows** module: new options to filter: compare two variables and sample

2 - **Config Page** module: Inicial hex color now in caps

3 - **utils.R**: new function filter_rows_2vars for compare two variables and col_type

## 2025.02.08-1

Highlights: New stats in Descriptive Stats, Shapiro Francia test and bug fix

### Bug Fixes

1 - **Analysis > Scatter: warning if two Factors variables**: fixed, now requires numeric variables 
([#18](https://github.com/lgschuck/spada/issues/18))

### Improvements

1 - **Descriptive Stats** module: added geometric mean, harmonic mean, skewness and kurtosis from DescTools package.

2 - **Normality test** module: new test, Shapiro-Francia from DescTools package.

3 - **Exploratory** module: bug fix in scatter. ([#18](https://github.com/lgschuck/spada/issues/18))

## 2025.02.06-1

Highlights: import sav (SPSS) files and Plot in Z test

### Bug Fixes

1 - **Data > Import: check for already used name not working**: the names were not beeing passed as reactive to File Import module. Fixed. 
([#17](https://github.com/lgschuck/spada/issues/17))

### Improvements

1 - **Import file** module: now csv has a number of lines to read parameter and can read sav (SPSS) files

2 - **Z Test** module: now has a plot with test results

3 - **utils.R**: plot_z_test function

4 - **spada_server.R**: now passes dt_names as reactive to File Import module

## 2025.02.02-1

Highlights: new module Rename Cols and bug fix

### Bug Fixes

1 - **Analysis > Descriptive Stats: error in round for factors**: now f_num function only format numeric values
([#16](https://github.com/lgschuck/spada/issues/16))

### Improvements

1 - **navbar_df_info and sidebar** modules: now number of rows with 1 decimal value (function f_num) for better look (solve side effect after use of nsmall in previous release)

2 - New **Rename Cols** module: new module for rename variables of active dataset

## 2025.02.01-1

Only visual and formatting changes.

### Improvements

1 - **Convert Cols, Data Overview and Sidebar** modules: background color receive object bg_color

2 - **Correlation, Normality Test and Z Test** modules: sidebars color now with bg_color object and stati_card with blue color

3 - **Descriptive Stats** module: gain digits input and f_num for format values

4 - **f_num** function: now with nsmall inside format function for number of decimal digits

5 - **Spada_ui**: now with color objects instead of hex code

6 - **stats Table** module: now values with f_num instead of f_dec function

7 - **utils.R**: new function stati_card (basically shinyWidgets::statiCard with default values)
and colors in objects to use across several places

## 2025.01.29-1

### Bug Fixes

1 - **Analysis > Normality Test > Shapiro: error if all values are equal**: 
Shapiro does not accept all equal values. Now with check and msg_error 
([#15](https://github.com/lgschuck/spada/issues/15))

### Improvements

1 - **Correlation** module: chane name of Alternatives and chance card header title 
to Correlation Test

2 - **Export file** module: now writes Sav (**haven package**) and uses checkbox to compress RDS

3 - **Normality Test** module: better names and better checks for Shapiro-Wilk Test
([#15](https://github.com/lgschuck/spada/issues/15))

4 - **spada.R**: now make.names for the variables inside datasets

5 - **spada_ui.R**: new name in the menu for Correlation (now Correlation Test)

6 - **utils.R**: new internal functions: make_var_names and test_all_equal

7 - **Z Test** module: better visuals and new checks for Mean and Std Deviation inputs

8 - **DESCRIPTION**: insert [haven](https://haven.tidyverse.org)
package as new dependencie

## 2025.01.28-1

### Improvements

1 - New **Z Test** module: new module for Z test (DescTools::ZTest)

## 2025.01.27-1

### Bug Fixes

1 - **Exploratory > Stats Table: digits input has no effect**: the input was
been passed to gt functions (fmt_numeric) but the **value** column was char 
given the paste command for Mode values. Fixed with new function f_dec. ([#13](https://github.com/lgschuck/spada/issues/13))

2 - **Analysis > Correlation: not enough finite observations**: stats::cor.test for 
Pearson demands at least 3 valid values (Spearman and Kendal a least 2 valid values). 
New check throughs a error message if less than 3 valid values for all methods.
([#14](https://github.com/lgschuck/spada/issues/14))

### Improvements

1 - **Correlation** module: now check if **Standard Deviation** of any informed 
variable is zero, avoiding warning. Also fixed 
([#14](https://github.com/lgschuck/spada/issues/14)).

2 - **Exploratory** module: now with req (for main variable and variable 2) 
in render_plot (output$gt_dist)

3 - **Stats table** module: align columns, use '-' for sub_missing becasue the long dash is not an ASCII and could not be replicated in sub_values (devtools::check). Also fixed ([#13](https://github.com/lgschuck/spada/issues/14))

4 - **utils.R**: new intern function f_dec for format number of decimals

5 - **DESCRIPTION**: insert Depends (R >= 4.1.0) given the use of pipe operator

## 2025.01.26-2

### Bug Fixes

1 - **Exploratory Page > Boxplot by groups: error when plot Integer vs Numeric**: The error occurs because there is more unique values in Variable 2 than in colors() function tha is used to sample colors. Changed to replace = T. ([#11](https://github.com/lgschuck/spada/issues/11))

2 - **Exploratory Page > Stats table: Mode NA for numeric, date, logical and complex var**: the gt table was receiving tha NA value as character and the function sub_missing() does not have effect on those values. Now the Mode is passed as character only if it is not NA.
([#12](https://github.com/lgschuck/spada/issues/12))

### Improvements

1 - **Descriptive Stats** module: now Mode returns NA (not as character) and only paste/collapse values if Mode exists. Inserted sub_missing() in gt_stats for better look and consistency with other views

2 - **Stats table** module: now Mode returns NA (not as character) and only paste/collapse values if Mode exists

3 - **Exploratory** module: now the Variable 2 can not be float in Boxplot by Groups, because does not seam reasonable to have an infinite number of groups. Related to ([#11](https://github.com/lgschuck/spada/issues/11))

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
