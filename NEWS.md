---
editor_options: 
  markdown: 
    wrap: 72
---

# Spada 0.2.0 - Future Version

## 0.2.0 Milestone 

[0.2.0 Version Milestone](https://github.com/lgschuck/spada/milestone/2)

# Spada 0.1.3 (in development) - 31/03/2026

## Goals

1 - Implement Extended Tasks in some calculations

2 - Delete elements from Output

3 - Bug fixes

4 - Minor improvements (speed, tests, messages, ...)

### Improvements

1 - New **functions from collapse** for speed purposes

2 - **Filter rows module** copy dataset after validate inputs 

3 - New req in **stats table module** (avoid recalculation when change dataset and var1 is NULL)

4 - Now the insertion of output element is inside **insert_output module** and the module has a **new input element_title**

5 - **Delete elements from Output**: new button to delete elements

6 - **Extended tasks**: mirai now is a dependencie, new extended tasks in several modules

7 - New **table_values module** and new insert_output module call in stats_table module

8 - **Exploratory module**: change renderUI foi updateSelectInput

9 - **Export file module**: change if else for switch in the file name to export

10 - **is_spada_df function**: now test for ncol > 0

11 - **Spada.R**: now uses is_spada_df in test for valid datasets and improved tests

12 - **Filter rows module**: removed unnecessarily msg_erros

13 - **Output module**: new accordions, buttons to order output and extended task

14 - **Spada exported functions**: exported functions are now internall (maybe could put them in a separete package)

### Bug fixes

1 - **Filter rows module**: Filter rows module: zero rows left  ([#67](https://github.com/lgschuck/spada/issues/67))

# Spada 0.1.2

## 2026.02.09

Release of 0.1.2 version.

## Highlights

Update **dependencies (2026-01-30)**, new **Group by and Summarise** modules, **new startup and exit screens** (waiter package) and better **Metadata (re)calculation** (collapse package)

### Improvements

1 - **Filter Rows module**: internal improvements, improved tests and now it's possible to filter values from another dataset variable ([#59](https://github.com/lgschuck/spada/issues/59))

2 - **New startup screen**: new startup and close screens (waiter is now a dependencie, shinybusy removed)

3 - **SelectizeInputs**: new width 80% for better use of screen

4 - **Main sidebar**: now opens by default as its accordions (active dataset and datasets) and better size for Preview datasets popover

5 - **Config nrows for plots**: now the value from the input is multiplied by 1e3

6 - **Metadata calculation**: new functions from collapse package, avoids some recalculation for better performance (new update_meta function) and visual feel ([#62](https://github.com/lgschuck/spada/issues/62))

7 - **IDate in metadata table**: now IDate class receives the calenda icon ([#58](https://github.com/lgschuck/spada/issues/58))

8 - **Data backup**: backup events from Edit page now in data_bkp module

9 - **Metadata tests**: better metadata tests in spada_server

10 - **Data module**: manage dataset events (copy, delete, etc) now in data module

11 - **Calculate module**: check for allowed operations in function apply

12 - **Sidebar module**: new input to change active dataset more dynamically (also included a test in testthat), new button Save Session, new mini button to Metadata and changed from renderUI to textOutput for better visuals

13 - **Output save**: now saves with a CSS style

14 - **Restore Session module**: the code used to restore session is now in a new module

15 - **Navbar df info module**: new mini button to Metadata

16 - **Test write access for session data**: now if there is no write access for session files they are saved in tempdir with message in console

### New Features

1 - **New Summarise module**: distinct and count functions by choosed variable (overwrite or create new dataset) ([#60](https://github.com/lgschuck/spada/issues/60))

2 - **New Group by module**: now it's possible to group by data in a new (or overwriting the active) dataset ([#60](https://github.com/lgschuck/spada/issues/60))

### Bug Fixes

1 - **Import File module**: now apply make_valid_cols and make_var_names when import file ([#65](https://github.com/lgschuck/spada/issues/65))

2 - **Sidebar preview dataset**: now shows 5 or the NROW for smaller than 5 rows datasets ([#61](https://github.com/lgschuck/spada/issues/61))

3 - **Convert cols module**: error to preview when change tha active dataset ([#64](https://github.com/lgschuck/spada/issues/64))

### Visuals

1 - **New restore session modal**

2 - **Change some icons**

3 - **Sharp corners** for buttons, inputs and popovers

4 - **Set font-family**: Segoe UI

5 - **Navbar color**: lighter blue color '#0A5A88'

6 - **Small CSS changes**

# Spada 0.1.1

## 2025.10.10

Release of 0.1.1 version.

**Highlights**: Improvements in **metadata calculation** and new **qs2** format (for session data and as option in **Import and Export** modules)

### Improvements

1 - **utils.R**: improved/new functions (**desc_stats, df_info, is_spada_df, load_conf, spada_plot, test_output_format**) and **make_valid_cols** now converts complex to char

2 - **qs2**: now the session data is saved in qs2 format for better performance/compression

3 - **Import and export** modules: new option for qs2 format 

4 - **Output**: the output structure was rewritten (better management and future options for edit/delete) and new tab **Output meta** showing the elements in a gt table

5 - **Metadata**: now the metadata is calculated only when the data change, big improvement in speed ([#55](https://github.com/lgschuck/spada/issues/55))

6 - **Sidebar and Navbar** modules: now the render only the metadata (removed button) for better visual user experience

7 - **Config** module: new title color and plot limit (for plot samples) options ([#56](https://github.com/lgschuck/spada/issues/56)) and show current conf ([#54](https://github.com/lgschuck/spada/issues/54))

8 - **Visuals**: new default for fill color "#0099F8"

### Bug Fixes

1 - **Imported data.frame with invalid names**: **import file** module uses make-var_names in the inputed data 
([#53](https://github.com/lgschuck/spada/issues/53))
