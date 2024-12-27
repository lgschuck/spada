---
editor_options: 
  markdown: 
    wrap: 72
---

# Spada 0.1.0.9000 (development version)

## 2024.12.27-1

### TO DO

1 - 'Edit' Page > Convert

* on changing the active dataset and a variable is selected an error is rased from data.table (variable does not exist)

### Improvements

1 - function utils

* df_info: improvement in performance (something like half the time in big datasets - 1e6 rows)
* new function: gt_info to generate metadata with gt package
* color palettes: new palettes to df_info function

2 - funciton spada

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

1 - Creation of utils functions

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
