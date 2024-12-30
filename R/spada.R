
#' Spada (Data Analysis)
#'
#' Function that generates a Shiny App for Data Analysis
#'
#' @param ... Objects of data.frame class
#'
#' @examples
#' if(interactive()) spada(datasets::mtcars)
#'
#' @export
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @import data.table
#' @importFrom dplyr arrange filter mutate pull select
#' @import bslib
#' @import bsicons
#' @import DT
#' @import gt
#' @importFrom graphics abline hist
#' @importFrom stats median
#' @importFrom utils object.size head
#' @importFrom graphics boxplot lines barplot mtext text
#' @importFrom stats cor lm sd var rnorm IQR
#' @importFrom grDevices colors

spada <- function(...) {
  datasets <- list(...)
  if(length(datasets) == 0) datasets <- list('iris' = datasets::iris, 'mtcars' = datasets::mtcars)
  stopifnot('Objects must be data.frame and have at least 1 row each' =
    sapply(datasets, is.data.frame) |> all() && all(sapply(datasets, nrow) > 0))

  # set names
  if(is.null(names(datasets))){
    names(datasets) <- lapply(substitute(list(...))[-1], deparse) |>
      unlist() |>
      make.names(unique = T)
  } else {
    new_names <- paste0('dataset_', 1:length(datasets))
    empty_names <- which(names(datasets) == '')
    names(datasets)[empty_names] <- new_names[empty_names]

    names(datasets) <- make.names(names(datasets), unique = T)
  }
  gc()

  ### ===================================================================== ###
  # UI
  ### ===================================================================== ###
  ui <- tagList(
    useBusyIndicators(),

    # close the app
    tag_js_exit,

    # css style
    tag_css,

    page_navbar(
      id = 'navbar',
      theme = bs_theme(
        bg = '#f9f9f9',
        fg = '#000000',
        primary = '#02517d',
        secondary = '#0072B2',
        success = '#009E73',
        font_size_base = "1rem",
      ),
      title = 'Spada',
      bg = '#02517d',

      # page data -------------------------------------------------------------
      nav_panel(
        'Data',
        icon = bs_icon('info-square-fill'),
        layout_column_wrap(
          width = 1,
          min_height = '65px',
          uiOutput('pD_value_box'),
          height = '65px'
        ),

        card(full_screen = T,
             card_body(
               style = 'background-color: #02517d;',
               height = '800px',
               navset_card_pill(
                 nav_panel(
                   'Highlights',
                   card_body(
                     layout_column_wrap(
                       value_box(
                         title = 'Numeric Vars',
                         value = textOutput('pD_var_num_vars'),
                         showcase = bs_icon('123'),
                         theme = 'bg-gradient-yellow-orange'
                       ),
                       value_box(
                         title = 'Character Vars',
                         value = textOutput('pD_var_char_vars'),
                         showcase = bs_icon('alphabet'),
                         theme = 'bg-gradient-blue-indigo'
                       ),
                       value_box(
                         title = 'Factor Vars',
                         value = textOutput('pD_var_factor_vars'),
                         showcase = bs_icon('diagram-3'),
                         theme = 'bg-gradient-green-indigo'
                       ),
                       value_box(
                         title = 'Date Vars',
                         value = textOutput('pD_var_date_vars'),
                         showcase = bs_icon('calendar3'),
                         theme = 'bg-gradient-purple-indigo'
                       )
                     ),
                     layout_column_wrap(
                       value_box(
                         title = "Var with most NA's",
                         value = textOutput('pD_var_most_nas'),
                         showcase = bs_icon('database-x'),
                         theme = 'bg-gradient-red-indigo',
                         p(textOutput('pD_var_most_nas_n', inline = T), ' rows')
                       ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                       value_box(
                         title = "Var with biggest % of NA's",
                         value = textOutput('pD_var_biggest_perc_nas'),
                         showcase = bs_icon('percent'),
                         theme = 'light',
                         p(textOutput('pD_var_biggest_perc_nas_perc', inline = T), ' %')
                       ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                       value_box(
                         title = 'Var with max value',
                         value = textOutput('pD_var_max_value', inline = T),
                         showcase = bs_icon('graph-up-arrow', placement = 'top'),
                         theme = 'bg-gradient-pink-indigo',
                         p('Max value:', textOutput('pD_max_value', inline = T)),
                         hr(),
                         p(bs_icon('graph-down-arrow'), 'Var with min value'),
                         p(textOutput('pD_var_min_value', inline = T)),
                         p('Min value:', textOutput('pD_min_value', inline = T))
                       ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                       value_box(
                         title = "Var with biggest size",
                         value = textOutput('pD_var_biggest_size'),
                         showcase = bs_icon('sd-card'),
                         theme = 'bg-gradient-teal-indigo',
                         p(textOutput('pD_var_biggest_size_size', inline = T), 'Bytes')
                       ) |> tooltip("Showing 1, there may be ties.", placement = 'top')
                     )
                   )
                 ),
                 nav_panel(
                  'Metadata',
                  card_body(
                    gt_output('pD_metadata_gt'),
                  )
                ),
                 nav_panel(
                   'Overview',
                   card_body(
                     gt_output('pD_over_gt')),
                     fluidRow(
                       column(2, numericInput('pD_over_size_sample', 'Number of Rows', 100, 100, 1e4, 100)),
                       column(2, radioButtons(
                         'pD_over_radio_sample', 'Show',
                         c('First rows' = 'first', 'Sample' = 'sample'), inline = T))
                     ),
                 ),
                 nav_panel(
                   'Data',
                   card_body(
                     uiOutput('pD_data_ui_sel_df'),
                     textInput('pD_data_txt_new_name', 'New Name'),
                     layout_column_wrap(
                      btn_task('pD_data_btn_new_name', 'Rename dataset', icon('file-signature')),
                      btn_task('pD_data_btn_active', 'Make dataset Active', icon('check')),
                      btn_task('pD_data_btn_copy_dataset', 'Copy dataset', icon('copy')),
                      btn_task('pD_data_btn_delete_dataset', 'Delete dataset', icon('trash-can')),
                     ),
                   )),
                export_file_ui('pD_export'),

               )
             )
        )
      ),

      # page edit -------------------------------------------------------------
      nav_panel(
        'Edit',
        icon = bs_icon('funnel'),
        layout_column_wrap(
          width = 1,
          min_height = '65px',
          uiOutput('pE_value_box'),
          height = '65px'
        ),
        card(full_screen = T,
             card_body(
               style = 'background-color: #02517d;',
               height = '800px',
               layout_columns(
                navset_card_pill(
                   nav_panel(
                     'Filter',
                     layout_column_wrap(
                       card(
                         card_header('Filter Rows', class = 'mini-header'),
                         card_body(
                           uiOutput('pE_filter_ui_var_filter'),
                           selectInput('pE_filter_operator',
                                       'Operator', c('', filter_operators)),
                           layout_column_wrap(
                             uiOutput('pE_filter_ui_value'),
                             textInput('pE_filter_txt_preview_value', 'Preview value')
                           ),
                         ),
                         card_footer(btn_task('pE_filter_btn_filter', 'Apply filters', icon('check')))
                       ),
                       card(
                         card_header('Select Columns', class = 'mini-header'),
                         card_body(
                           uiOutput('pE_filter_ui_var_sel'),
                           radioButtons('pE_filter_radio_var_sel', NULL,
                                        c('Drop' = 'drop', 'Keep' = 'keep'), inline = T)
                         ),
                         card_footer(btn_task('pE_filter_btn_sel', 'Apply selection', icon('check')))
                       )
                     ),
                   ),
                   nav_panel(
                     'Convert',
                     layout_column_wrap(
                       card(
                         card_header('Conversions', class = 'mini-header'),
                         card_body(
                           layout_column_wrap(
                             height = '200px',
                             uiOutput('pE_convert_ui_var_sel'),
                             textInput('pE_convert_txt_current_format', 'Current Type / Class')
                           ),
                           layout_column_wrap(
                             height = '800px',
                             selectInput('pE_convert_sel_format', 'Select the new format',
                                         c('', 'as.numeric', 'as.integer',
                                           'as.character', 'as.Date', 'as.factor',
                                           'as.double', 'as.complex')),
                             conditionalPanel(
                               condition = "input.pE_convert_sel_format == 'as.Date'",
                               selectInput(
                                 'pE_convert_sel_date_formats', 'Choose the entry format',
                                 date_formats) |> tooltip('For non numeric inputs'),
                               dateInput('pE_convert_sel_date_origin', 'Choose a start date', value = '1970-01-01') |>
                                 tooltip('For numeric inputs')
                              )
                           )
                         ),
                         card_footer(
                           btn_task('pE_convert_btn_apply', 'Apply conversion',
                                    icon('hammer'))
                         )
                       ),
                       card(
                         card_header('Preview', class = 'mini-header'),
                         card_body(
                           gt_output('pE_convert_preview_gt1')),
                         card_footer(btn_task('pE_convert_btn_preview_sample',
                                              'Show new sample', icon('rotate-right')))
                       )
                     )
                   ),
                   nav_panel(
                     'Order',
                     layout_column_wrap(
                       card(
                         card_header('Order Rows', class = 'mini-header'),
                         card_body(
                           uiOutput('pE_order_ui_var_rows'),
                           selectInput('pE_order_vars_descending', 'Vars in descending order', '', multiple = T) |>
                             tooltip('If not informed, the order will be Ascending', placement = 'right'),
                           radioButtons('pE_order_radio_nas', "NA's Placement",
                                        c('Last' = 'last', 'First' = 'first'),
                                        inline = T),
                         ),
                         card_footer(
                           btn_task('pE_order_btn_order_rows', 'Order Rows', icon('shuffle')),
                         )
                       ),
                       card(
                         card_header('Order Columns', class = 'mini-header'),
                         card_body(
                           uiOutput('pE_order_ui_var_cols'),
                           radioButtons('pE_order_radio_cols', NULL,
                                        c('Before' = 'before', 'After' = 'after'),
                                        inline = T),
                           selectInput('pE_order_vars_rest', 'Other Variables', '') |>
                             tooltip('If not informed, the Other Variables will be placed at the end',
                                     placement = 'right'),
                         ),
                         card_footer(
                           btn_task('pE_order_btn_order_cols', 'Order Columns', icon('arrow-right-arrow-left')),
                         )
                       )
                     ),
                   ),
                 )
               )
             ),
             card_footer(
               layout_column_wrap(
                 btn_task('pE_btn_reset', 'Reset Dataset', icon('arrow-rotate-right')) |>
                   tooltip('Restore the original dataset to the Active dataset', placement = 'top'),
                 btn_task('pE_btn_bkp', 'Create Backup', icon('cloud-arrow-up')) |>
                   tooltip('Create a copy of the Active dataset', placement = 'top'),
                 btn_task('pE_btn_restore', 'Restore Backup', icon('cloud-arrow-down')) |>
                   tooltip('Restore a previously created backup', placement = 'top'),
                 btn_task('pE_btn_clear_bkp', 'Clear Backup', icon('trash-can')) |>
                   tooltip('Erase the backup', placement = 'top'),
               ),
               style = 'background-color: #02517d;')
        )
      ),

      # page analysis ---------------------------------------------------------
      nav_panel(
        'Analysis',
        icon = bs_icon('bar-chart-fill'),
        layout_column_wrap(
          width = 1,
          min_height = '65px',
          uiOutput('pA_value_box'),
          height = '65px'
        ),
        card(
          full_screen = T,
          card_body(
            style = 'background-color: #02517d;',
            height = '800px',
            layout_columns(
              col_widths = c(2, 7, 3),
              navset_card_pill(
                full_screen = T,
                nav_panel('Parameters',
                          uiOutput('pA_ui_var_names'),
                          uiOutput('pA_ui_var_names2')
                ),
                nav_panel('Filters', checkboxInput('pA_outliers', 'Remove Outliers', F) |>
                            tooltip('Only for numeric vars', placement = 'top'))
              ),
              navset_card_pill(
                full_screen = T,
                nav_panel(
                  'Distribution',
                  full_screen = T,
                  card_body(plotOutput('pA_g_dist')),
                  card_footer(
                    fluidRow(
                      column(8,
                             radioButtons(
                               'pA_radio_dist_plot',
                               'Plot type:',
                               c('Histogram' = 'hist',
                                 'Boxplot' = 'boxplot',
                                 'Boxplot by Groups' = 'boxplot_group',
                                 'Dots' = 'dots',
                                 'Barplot' = 'barplot'),inline = T)),
                      column(2, numericInput('pA_var_percentile', 'Percentile', 50, 0, 100, 5)),
                      column(2, conditionalPanel(
                        condition = "input.pA_radio_dist_plot == 'hist'",
                        numericInput('pA_bins', 'Bins', 10, 5, step = 10) |>
                          tooltip('Only for Histrograms', placement = 'top')
                        )
                      ),
                    )
                  )
                ),
                nav_panel(
                  'Scatter',
                  full_screen = T,
                  card_body(plotOutput('pA_g_scatter', click = 'plot_brush')),
                  card_footer(
                    layout_column_wrap(
                      checkboxInput('pA_scatter_lm', 'Plot Linear Model', F) |>
                        tooltip('Show the line only if LM model was created'),
                      btn_task('pA_btn_scatter', 'Generate Plot', icon('gear'))
                    )
                  )
                ),
                nav_panel(
                  'Table',
                  full_screen = T,
                  card_body(
                    radioButtons('pA_table_type', 'Table type:',
                                 c('1 Variable' = '1d',
                                   '2 Variables' = '2d'), inline = T),
                    verbatimTextOutput('pA_table', placeholder = T),
                  ),
                  card_footer()
                ),
                nav_panel(
                  'Linear Model',
                  full_screen = T,
                  navset_card_pill(
                    nav_panel(
                      'Parameters',
                      sliderInput('pA_sample_size', 'Sample Size (%)', 0, 100, 100) |>
                        tooltip('Applied only if valid values are greater than 10.000'),
                      layout_column_wrap(
                        btn_task('pA_btn_scatter_lm_run', 'Run Linear Model', icon('gear')),
                        btn_task('pA_btn_scatter_lm_clear', 'Clear Linear Model', icon('trash-can'))
                      )
                    ),
                    nav_panel('Output', verbatimTextOutput('pA_linear_model')),
                    nav_panel(
                      'Residuals',
                      plotOutput('pA_g_lm_resid'),
                      card_footer(
                        layout_column_wrap(
                          radioButtons('pA_radio_lm_resid', 'Plot type:',
                                       c('Histogram' = 'hist', 'Boxplot' = 'boxplot',
                                         'Dots' = 'dots'), inline = T),
                          btn_task('pA_btn_lm_resid', 'Plot residuals', icon('chart-simple')))
                      )
                    ),
                  )),
              ),
              navset_card_pill(
                nav_panel(
                  'Stats',
                  full_screen = T,
                  card_body(
                    gt_output('pA_gt1')),
                  card_footer(numericInput('pA_t1_digits', 'Digits', 2, 0, 9, 1))
                )
              )
            )
          )
        )
      ),

      nav_spacer(),
      # page config -----------------------------------------------------------
      page_config_ui('pC'),

      # page exit -------------------------------------------------------------
      nav_panel(
        value = 'exit',
        title = 'Exit',
        icon = bs_icon('x-square-fill')
      )
    )
  )

  ### ===================================================================== ###
  # Server
  ### ===================================================================== ###
  server <- function(input, output, session) {

    # data --------------------------------------------------------------------
    datasets_react <- reactiveValues(data = lapply(datasets, setDT))

    # inicialize with the first dataset informed
    df <- reactiveValues(
      df_active = copy(datasets[[1]]),
      df_active_name = names(datasets)[1],
      df_backup = NULL
    )

    gc()
    df_active_names <- reactive(df$df_active |> names())

    # main value boxes --------------------------------------------------------
    main_value_box_active <- reactive(main_value_box(df$df_active, df$df_active_name))

    output$pD_value_box <- renderUI({ main_value_box_active() })
    output$pE_value_box <- renderUI({ main_value_box_active() })
    output$pA_value_box <- renderUI({ main_value_box_active() })

    # data page events -----------------------------------------------------
    df_metadata <- reactive({
      pE_convert_trigger()
      df_info(df$df_active)
    })

    output$pD_metadata_gt <- render_gt(
      df_metadata() |> gt_info()
    )

    # overview -----------------------
    output$pD_over_gt <- render_gt(
      {
        req(input$pD_over_size_sample)
        pD_over_n_show <- max(1, input$pD_over_size_sample)
        pD_over_n_show <- min(pD_over_n_show, nrow(df$df_active))

        if(input$pD_over_radio_sample == 'first'){
          pD_over_idx <- 1:pD_over_n_show
        } else if (input$pD_over_radio_sample == 'sample'){
          pD_over_idx <- sample.int(nrow(df$df_active), pD_over_n_show, replace = F)
        }

        df$df_active[pD_over_idx, ] |>
          lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
          as.data.frame() |>
          gt() |>
          opt_interactive(
            page_size_default = 9,
            use_filters = T,
            use_resizers = T,
            use_highlight = T,
            use_compact_mode = T,
            use_text_wrapping = F,
            use_page_size_select = T
          ) |>
          tab_options(table.background.color = '#f9f9f9')
      }
    )

    # values for boxes -----------------------
    output$pD_var_num_vars <- renderText(sapply(df$df_active, is.numeric) |> sum())
    output$pD_var_char_vars <- renderText(sapply(df$df_active, is.character) |> sum())
    output$pD_var_factor_vars <- renderText(sapply(df$df_active, is.factor) |> sum())
    output$pD_var_date_vars <- renderText(sapply(df$df_active, is_date) |> sum())

    output$pD_var_most_nas <- renderText(
      {
        if(df_metadata() |> filter(n_nas > 0) |> nrow() < 1) { 'None'
        } else {
          df_metadata() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pD_var_most_nas_n <- renderText(
      {
        if(df_metadata() |> filter(n_nas > 0) |> nrow() < 1) { '0'
        } else {
          df_metadata() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(n_nas) |> f_num()}
      }
    )

    output$pD_var_biggest_perc_nas <- renderText(
      {
        if(df_metadata() |> filter(perc_nas > 0) |> nrow() < 1) { 'None'
        } else {
          df_metadata() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pD_var_biggest_perc_nas_perc <- renderText(
      {
        if(df_metadata() |> filter(perc_nas > 0) |> nrow() < 1) { '0'
        } else {
          df_metadata() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(perc_nas) * 100 }
      }
    )

    output$pD_var_max_value <- renderText(
      df_metadata() |> arrange(-max) |> head(1) |> pull(var)
    )

    output$pD_max_value <- renderText(
      df_metadata() |> arrange(-max) |> head(1) |> pull(max) |> f_num(dig = 3)
    )

    output$pD_var_min_value <- renderText(
      df_metadata() |> arrange(min) |> head(1) |> pull(var)
    )

    output$pD_min_value <- renderText(
      df_metadata() |> arrange(min) |> head(1) |> pull(min) |> f_num(dig = 3)
    )

    output$pD_var_biggest_size <- renderText(
      df_metadata() |> arrange(-size) |> head(1) |> pull(var)
    )

    output$pD_var_biggest_size_size <- renderText(
      df_metadata() |> arrange(-size) |> head(1) |> pull(size) |> f_num()
    )

    # define active dataset ---------------------------------------------------
    output$pD_data_ui_sel_df <- renderUI(
      selectInput('pD_data_sel_df', 'Select a dataset', names(datasets_react$data))
    )

    observe({
      # save active to original data
      datasets_react$data[[df$df_active_name]] <- df$df_active
      # choose new dataset to be active
      df$df_active <- datasets_react$data[[input$pD_data_sel_df]]
      df$df_backup <- NULL

      df$df_active_name <- input$pD_data_sel_df

      msg(paste('Dataset', df$df_active_name, 'is the active one'))

    }) |> bindEvent(input$pD_data_btn_active)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
          input$pD_data_txt_new_name %in% names(datasets_react$data)){
        msg_error('New name is not valid or already in use')

      } else {
        names(datasets_react$data)[names(datasets_react$data) == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(df$df_active_name == input$pD_data_sel_df){
          df$df_active <- datasets_react$data[[input$pD_data_txt_new_name]]

          df$df_active_name <- input$pD_data_txt_new_name
        }
      }

    }) |> bindEvent(input$pD_data_btn_new_name)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) ||
         (input$pD_data_txt_new_name %in% names(datasets_react$data))){
        msg_error('Name invalid or already in use')
      } else {
        datasets_react$data[[ input$pD_data_txt_new_name ]] <- df$df_active
        msg(paste('Dataset', input$pD_data_txt_new_name, 'created'))
      }
    }) |> bindEvent(input$pD_data_btn_copy_dataset)

    observe({
      if(df$df_active_name == input$pD_data_sel_df){
        msg_error('You can not delete the active dataset')
      } else {
        datasets_react$data[[ input$pD_data_sel_df ]] <- NULL
        msg(paste('Dataset', input$pD_data_sel_df, 'deleted'))
      }
    }) |> bindEvent(input$pD_data_btn_delete_dataset)

    # export ----------------------------------------------------
    export_file_server('pD_export', reactive(df$df_active))

    # edit page events --------------------------------------------------------

    # filter events ---------------------------
    output$pE_filter_ui_var_filter <- renderUI(
      selectInput('pE_filter_vars_filter', 'Variable', c('', df_active_names()))
    )

    output$pE_filter_ui_var_sel <- renderUI(
      selectInput('pE_filter_vars_sel', 'Variable', c('', df_active_names()), multiple = T)
    )

    observe({
      output$pE_filter_ui_value <- renderUI({
        if (df$df_active[[input$pE_filter_vars_filter]] |> is_date() &
            input$pE_filter_operator %in% c('==', '!=', '>', '>=', '<', '<=', 'is_na', 'not_na')){
          dateInput("pE_filter_value", "Date")
        } else if (df$df_active[[input$pE_filter_vars_filter]] |> is_date() &
                   input$pE_filter_operator %in% c('between', 'not_between')){
          dateRangeInput("pE_filter_value", "Date")
        } else {
          selectizeInput(
            'pE_filter_value', 'Value',
            choices = NULL,
            multiple = T,
            options = list(create = T)
          ) |> tooltip('Text should not be in quotes', placement = 'top')
        }
      })
    }) |> bindEvent(input$pE_filter_vars_filter)

    pE_filter_value_temp <- reactive({
      if(df$df_active[[input$pE_filter_vars_filter]] |> is.numeric()){
        unlist(input$pE_filter_value) |> as.numeric()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is_date()){
        unlist(input$pE_filter_value) |> as.Date()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.raw()){
        unlist(input$pE_filter_value) |> as.raw()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.complex()){
        unlist(input$pE_filter_value) |> as.complex()
      } else {
        input$pE_filter_value
      }
    })

    # update selectinput to show pertinent operators
    observe({
      updateSelectInput(
        session, 'pE_filter_operator',
        label = 'Operator',
        choices =
          if(df$df_active[[input$pE_filter_vars_filter]] |> is.factor() |
             df$df_active[[input$pE_filter_vars_filter]] |> is.character()){
            c('',
              '== (Equal)' = '==',
              '!= (Not Equal)' = '!=',
              'Is NA (is.na)' = 'is_na',
              'Not NA (! is.na)' = 'not_na',
              'In (%in%)' = 'in',
              'Not In (! %in%)' = 'not_in')
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$pE_filter_vars_filter)

    pE_filter_preview_value <- reactive({
      req(input$pE_filter_value)
      pE_filter_value_temp()
    })
    # update current format txt
    observe({
      updateTextInput(session, 'pE_filter_txt_preview_value',
                      label = 'Preview value',
                      value = pE_filter_preview_value()
      )
    }) |> bindEvent(pE_filter_preview_value())

    # filter rows
    observe({
      if(input$pE_filter_vars_filter == '' | input$pE_filter_operator == ''){
        msg('Choose a variable and an operator')
      } else if(length(pE_filter_value_temp()) > 1 & input$pE_filter_operator %in%
                c('==', '!=', '>', '>=', '<', '<=')){
        msg_error('Operator requires value of length 1')
      } else if(length(pE_filter_value_temp()) != 2 & input$pE_filter_operator %in%
                c('between', 'not_between')){
        msg_error('Operator requires value of length 2')
      } else {
        if(input$pE_filter_operator == '=='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) == pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '!='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) != pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '>'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) > pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '>='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) >= pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '<'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) < pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '<='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) <= pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'is_na'){
          df$df_active <-
            df$df_active[is.na(get(input$pE_filter_vars_filter)), ]
        } else if(input$pE_filter_operator == 'not_na'){
          df$df_active <-
            df$df_active[!is.na(get(input$pE_filter_vars_filter)), ]
        } else if(input$pE_filter_operator == 'in'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) %in% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'not_in'){
          df$df_active <-
            df$df_active[!get(input$pE_filter_vars_filter) %in% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'between'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) %between% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'not_between'){
          df$df_active <-
            df$df_active[!(get(input$pE_filter_vars_filter) %between% pE_filter_value_temp()), ]
        }
        msg('Filter rows: OK')
      }
    }) |> bindEvent(input$pE_filter_btn_filter)

    # update selectize for factors
    observe({
      req(input$pE_filter_vars_filter)
      if(df$df_active[[input$pE_filter_vars_filter]] |> is.factor()){
        updateSelectizeInput(
          session,
          'pE_filter_value',
          label = 'Value',
          choices = df$df_active[[input$pE_filter_vars_filter]] |> levels(),
          selected = ''
        )
      }
    })

    # clear value after click in button
    observe({
      updateSelectizeInput(
        session,
        'pE_filter_value',
        label = 'Value',
        choices = NULL,
        selected = ''
      )
    }) |> bindEvent(input$pE_filter_btn_filter)

    # select cols ---------------------------
    observe({
      if(input$pE_filter_vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {
        if(input$pE_filter_radio_var_sel == 'keep') {
          df$df_active <- subset(df$df_active, select = input$pE_filter_vars_sel)
          msg('Select columns: OK')
        } else if (input$pE_filter_radio_var_sel == 'drop'){
          if(all(df_active_names() %in% input$pE_filter_vars_sel)){
            msg('Leave at least 1 variable')
          } else {
            df$df_active <- subset(df$df_active,
                                   select = setdiff(names(df$df_active), input$pE_filter_vars_sel))
            msg('Select columns: OK')
          }
        }
      }
    }) |> bindEvent(input$pE_filter_btn_sel)

    # convert events ---------------------------
    pE_convert_trigger <- reactiveVal(0)

    observe({
      pE_convert_trigger(pE_convert_trigger() + 1)
    }) |> bindEvent(input$pE_convert_btn_apply)

    output$pE_convert_ui_var_sel <- renderUI(
      selectInput('pE_convert_vars_sel', 'Variable', c('', df_active_names()))
    )

    current_format <- reactive({
      req(input$pE_convert_vars_sel)
      paste('Type: [', df$df_active[[input$pE_convert_vars_sel]] |> typeof(), '] |',
            'Class: [',
            paste(df$df_active[[input$pE_convert_vars_sel]] |> class(), collapse = '/'),
            ']')
    })
    # update current format txt
    observe({
      updateTextInput(session, 'pE_convert_txt_current_format',
                      label = 'Current Type / Class',
                      value = current_format()
      )
    }) |> bindEvent(current_format())

    # sample to preview conversion
    pE_convert_preview_sample_trigger <- reactiveVal(1)
    pE_convert_preview_sample <- reactive({
      pE_convert_preview_sample_trigger()
      if(nrow(df$df_active) < 8) {
        rep(TRUE, nrow(df$df_active))
      } else {
        sample(nrow(df$df_active), 8, replace = F)
      }
    })

    # update sample in button click
    observe({
      pE_convert_preview_sample_trigger(pE_convert_preview_sample_trigger() + 1)
    }) |> bindEvent(input$pE_convert_btn_preview_sample)

    pE_convert_preview_df <- reactive({
      req(input$pE_convert_vars_sel)
      req(input$pE_convert_sel_format)

      if(input$pE_convert_sel_format == 'as.Date'){
        req(input$pE_convert_sel_date_formats)
      }

      pE_convert_preview_df_temp <- subset(
        df$df_active[pE_convert_preview_sample(), ],
        select = input$pE_convert_vars_sel)

      pE_convert_preview_df_temp[
        , preview := convert(get(input$pE_convert_vars_sel),
                                type = input$pE_convert_sel_format,
                                date_format = input$pE_convert_sel_date_formats,
                                date_origin = input$pE_convert_sel_date_origin)]

    })

    output$pE_convert_preview_gt1 <- render_gt({
      req(input$pE_convert_vars_sel)
      if(input$pE_convert_vars_sel %in% names(df$df_active)){
        pE_convert_preview_df() |>
          gt() |>
          opt_interactive(
            use_sorting = F,
            use_pagination = F,
            use_highlight = T,
            use_compact_mode = T) |>
          tab_options(table.background.color = '#f9f9f9')
      }
    })

    observe({
      if(input$pE_convert_vars_sel == '' | input$pE_convert_sel_format == ''){
        msg('Choose a variable and a new format')
      } else {

        df$df_active[, input$pE_convert_vars_sel :=
                       convert(get(input$pE_convert_vars_sel),
                                  type = input$pE_convert_sel_format,
                                  date_format = input$pE_convert_sel_date_formats,
                                  date_origin = input$pE_convert_sel_date_origin)]
        msg('Conversion applied')
      }
    }) |> bindEvent(input$pE_convert_btn_apply)

    # order rows events ---------------------------
    output$pE_order_ui_var_rows <- renderUI(
      selectInput('pE_order_vars_rows', 'Order by', c('', df_active_names()), multiple = T)
    )

    # vars in descending order
    observe({
      updateSelectInput(
        session,
        'pE_order_vars_descending',
        label = 'Descending Order',
        choices = input$pE_order_vars_rows
      )
    }) |> bindEvent(input$pE_order_vars_rows)

    # btn order rows ---------------------------
    observe({
      pE_order_rows_position <- rep(1, input$pE_order_vars_rows |> length())

      pE_order_rows_position[which(input$pE_order_vars_rows %in% input$pE_order_vars_descending)] <- -1

      setorderv(df$df_active, cols = input$pE_order_vars_rows,
                order = pE_order_rows_position,
                na.last = if(input$pE_order_radio_nas == 'last'){
                  TRUE
                } else if (input$pE_order_radio_nas == 'first'){
                  FALSE
                } else { FALSE }
      )

      msg('Reordering Rows: OK')
    }) |> bindEvent(input$pE_order_btn_order_rows)

    # order cols events ---------------------------
    output$pE_order_ui_var_cols <- renderUI(
      selectInput('pE_order_vars_cols', 'Variables to move', c('', df_active_names()), multiple = T)
    )

    # rest of vars to order
    observe({
      updateSelectInput(
        session,
        'pE_order_vars_rest',
        label = 'Other Variables',
        choices = df_active_names()[df_active_names() %notin% input$pE_order_vars_cols]
      )
    }) |> bindEvent(input$pE_order_vars_cols)

    # btn order cols ---------------------------
    observe({
      if(all(df_active_names() %in% input$pE_order_vars_cols) ||
         input$pE_order_vars_rest == ''){
        setcolorder(df$df_active, input$pE_order_vars_cols)
      } else if(input$pE_order_radio_cols == 'before'){
        setcolorder(df$df_active, input$pE_order_vars_cols,
                    before = input$pE_order_vars_rest)
      } else if (input$pE_order_radio_cols == 'after') {
        setcolorder(df$df_active, input$pE_order_vars_cols,
                    after = input$pE_order_vars_rest)
      }
      msg('Reordering Variables: OK')
    }) |> bindEvent(input$pE_order_btn_order_cols)

    # reset df active ---------------------------
    observe({
      df$df_active <- copy(datasets_react$data[[df$df_active_name]])
      gc()
      msg('Active Dataset Reseted')
    }) |> bindEvent(input$pE_btn_reset)

    # create backup ---------------------------
    observe({
      df$df_backup <- copy(df$df_active)
      gc()
      msg('Backup created')
    }) |> bindEvent(input$pE_btn_bkp)

    # restore backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to restore')
      } else {
        df$df_active <- copy(df$df_backup)
        gc()
        msg('Backup restored')
      }
    }) |> bindEvent(input$pE_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to clear')
      } else {
        df$df_backup <- NULL
        gc()
        msg('Backup cleared')
      }
    }) |> bindEvent(input$pE_btn_clear_bkp)

    # analysis page events ----------------------------------------------------
    var_analysis <- reactive({
      df_metadata() |> filter(perc_nas != 1) |>  pull(var)
    })

    output$pA_ui_var_names <- renderUI(
      selectInput('pA_sel_vars', 'Main Variable', var_analysis()) |>
        tooltip('Dependent Variable', placement = 'top')
    )

    output$pA_ui_var_names2 <- renderUI(
      selectInput('pA_sel_vars2', 'Variable 2', var_analysis(), var_analysis()[2]) |>
        tooltip('Independent Variable', placement = 'top')
    )

    pA_outliers_index <- reactive({
      v <- df$df_active[[input$pA_sel_vars]]
      if(input$pA_outliers & is.numeric(v)) {
        dist_interquatile <- IQR(v, na.rm = T)
        v >= (q1 - dist_interquatile) & v <= (q3 + dist_interquatile)
      } else {
        rep(T, length(v))
      }
    })

    # values to analysis page -------------------------------------------------
    pA_var <- reactive({
      req(input$pA_sel_vars)
      df$df_active[[input$pA_sel_vars]][pA_outliers_index()]
    })

    pA_var2 <- reactive({
      req(input$pA_sel_vars2)
      df$df_active[[input$pA_sel_vars2]][pA_outliers_index()]
    })

    pA_var_percentile <- reactive(
      if(is.numeric(pA_var())){
        pn(pA_var(), input$pA_var_percentile / 100)
      } else { NA }
    ) |> bindCache(pA_var(), input$pA_var_percentile)

    # render plots ------------------------------------------------------------
    output$pA_g_dist <- renderPlot({
      if (input$pA_radio_dist_plot == 'barplot'){
        validate(need(!is.numeric(pA_var()), 'Var can not be numeric'))
        barplot(table(pA_var()), col = color_fill())
      } else {
        validate(need(is.numeric(pA_var()), 'Var must be numeric'))

        if (input$pA_radio_dist_plot == 'boxplot_group'){
          # validate(
          #   need((unique(pA_var2()) |> length()) <= 7, 'Too many groups (max 7)'))

          pA_g_dist_boxg_col <- colors()[sample.int(
            colors() |> length(), unique(pA_var2()) |> length(), replace = F)]

          boxplot(pA_var() ~ pA_var2(), horizontal = T,
                  col = pA_g_dist_boxg_col, xlab = '', ylab = '')
          abline(v = pA_var_percentile(), col = color_line())
        } else {
          validate(
            need(isTruthy(input$pA_var_percentile)
                 && between(input$pA_var_percentile, 0, 100), 'Percentile must be between 0 and 100')
          )

          if(input$pA_radio_dist_plot == 'hist'){
            validate(need(input$pA_bins > 0, 'Bins must be 1 or higher'))
            hist(pA_var(),
                 col = color_fill(),
                 breaks = input$pA_bins,
                 main = '',
                 xlab = '',
                 ylab = 'Count')
            abline(v = pA_var_percentile(), col = color_line(), lwd = 2)
          } else if (input$pA_radio_dist_plot == 'boxplot'){
            boxplot(pA_var(), horizontal = T, col = color_fill())
            abline(v = pA_var_percentile(), col = color_line(), lwd = 2)
          } else if (input$pA_radio_dist_plot == 'dots'){
            plot(pA_var(), col = color_fill(), ylab = 'Values', pch = 19)
            abline(h = pA_var_percentile(), col = color_line(), lwd = 2)
          }
        }
      }
    }) |> bindCache(pA_var(), pA_var2(), input$pA_radio_dist_plot, input$pA_bins,
                    input$pA_var_percentile, color_fill(), color_line())
    # render scatter plot -----------------------------------------------------
    output$pA_g_scatter <- renderPlot({
      if (input$pA_scatter_lm &
          pA_linear_model$y_name == input$pA_sel_vars &
          pA_linear_model$x_name == input$pA_sel_vars2) {
        plot(
          pA_var2(),
          pA_var(),
          type = 'p',
          col = color_fill(),
          xlab = input$pA_sel_vars2,
          ylab = input$pA_sel_vars
        )
        lines(
          pA_linear_model$x,
          pA_linear_model$y,
          col = color_line(),
          lty = 'dotdash',
          lwd = 2
        )
        mtext(paste('Adjusted R Squared:',
                    summary(pA_linear_model$model)$r.squared |> round(4)),
              side = 3)
      } else {
        plot(
          pA_var2(),
          pA_var(),
          type = 'p',
          col = color_fill(),
          xlab = input$pA_sel_vars2,
          ylab = input$pA_sel_vars
        )
        mtext(paste('Pearson Correlation:', pA_stats_correlation() |> round(4)))
      }
    }) |> bindCache(
      input$pA_scatter_lm,
      pA_linear_model$y_name,
      pA_linear_model$x_name,
      input$pA_sel_vars,
      input$pA_sel_vars2,
      pA_var2(),
      pA_var(),
      pA_linear_model$x,
      pA_linear_model$y,
      color_fill(),
      color_line()
    ) |> bindEvent(input$pA_btn_scatter)

    # tables ------------------------------------------------------------------
    output$pA_table <- renderPrint(
      if(input$pA_table_type == '1d') {
        table(pA_var())
      } else if (input$pA_table_type == '2d'){
        table(pA_var(), pA_var2())
      }
    )|> bindCache(
      pA_var(),
      pA_var2(),
      input$pA_table_type)

    # linear model ------------------------------------------------------------
    pA_linear_model <- reactiveValues(
      model = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      if(!is.numeric(pA_var())){
        msg('The Dependent variable must be numeric', 2.5)
      } else if (input$pA_sel_vars == input$pA_sel_vars2) {
        msg('Choose diferent variables for X and Y.', 2.5)
      } else {
        pA_linear_model$y_name <- input$pA_sel_vars
        pA_linear_model$x_name <- input$pA_sel_vars2

        pA_var_size <- length(pA_var())

        if(pA_var_size < 10e3) {
          var_y <- pA_var()
          var_x <- pA_var2()
        } else {
          pA_sample_size <- min(pA_var_size,
                                floor(pA_var_size * min(1, max(0, input$pA_sample_size/100))))
          lm_sample <- sample.int(pA_var_size, pA_sample_size, replace = F) |>
            sort()
          var_y <- pA_var()[lm_sample]
          var_x <- pA_var2()[lm_sample]
        }

        pA_linear_model$model <- lm(var_y ~ var_x, model = F)
        pA_linear_model$x <- var_x
        pA_linear_model$y <- pA_linear_model$model$fitted.values
        msg('Lm model completed.')
      }
    }) |> bindEvent(input$pA_btn_scatter_lm_run)

    observe({
      pA_linear_model$model <- NULL
      pA_linear_model$x <- NULL
      pA_linear_model$y <- NULL
      pA_linear_model$x_name <-
        pA_linear_model$y_name <- ''
      msg('Lm model cleared.')
    }) |> bindEvent(input$pA_btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    output$pA_linear_model <- renderPrint({
      list(
        'Formula' = paste(
          pA_linear_model$y_name,
          '~',
          pA_linear_model$x_name
        ),
        'Model' = summary(pA_linear_model$model)
      )
    }) |> bindCache(pA_linear_model$y_name,
                    pA_linear_model$x_name,
                    pA_linear_model$model)

    # plot linear model residuals ---------------------------------------------
    output$pA_g_lm_resid <- renderPlot({

      validate(need(isTruthy(pA_linear_model$model), 'No residuals to plot'))

        if(input$pA_radio_lm_resid == 'hist'){
          hist(pA_linear_model$model$residuals,
               col = color_fill(),
               main = '',
               xlab = '',
               ylab = 'Count')
        } else if (input$pA_radio_lm_resid == 'boxplot'){
          boxplot(pA_linear_model$model$residuals,
                  horizontal = T, col = color_fill())
        } else if (input$pA_radio_lm_resid == 'dots'){
          plot(pA_linear_model$model$residuals, col = color_fill(),
               ylab = 'Residuals', pch = 19)
          abline(h = 0, col = color_line(), lty = 'dotdash', lwd = 2)
        }
      # }
    }) |> bindEvent(input$pA_btn_lm_resid)

    # metrics -----------------------------------------------------------------
    pA_stats_obs <- reactive(length(pA_var()))
    pA_stats_n_nas <- reactive(length(pA_var()[is.na(pA_var())]))
    pA_stats_min <- reactive(if(is.numeric(pA_var())) mina(pA_var()) else NA)
    pA_stats_q1 <- reactive(if(is.numeric(pA_var())) pn(pA_var(), 0.25) else NA)
    pA_stats_median <- reactive(if(is.numeric(pA_var())) median(pA_var(), na.rm = T) else NA)
    pA_stats_mean <- reactive(if(is.numeric(pA_var())) mean(pA_var(), na.rm = T) else NA)
    pA_stats_q3 <- reactive(if(is.numeric(pA_var())) pn(pA_var(), 0.75) else NA)
    pA_stats_max <- reactive(if(is.numeric(pA_var())) mana(pA_var()) else NA)
    pA_stats_sd <- reactive(if(is.numeric(pA_var())) sd(pA_var(), na.rm = T) else NA)
    pA_stats_correlation <- reactive(
      if(is.numeric(pA_var()) && is.numeric(pA_var2()) && pA_stats_sd() != 0 &&
        !is.na(pA_stats_sd())){
          sd_pA_var2 <- sd(pA_var2(), na.rm = T)
        if(sd_pA_var2 == 0 || sd_pA_var2 |> is.na()) {
          NA
        } else {
        cor(pA_var(), pA_var2(), method = 'p', use = 'na.or.complete')
        }
        } else { NA }
    )
    # stats table -------------------------------------------------------------
    pA_t1 <- reactive(
      data.frame(
        var = c(
          paste("% NA's (", pA_stats_n_nas(), '/', pA_stats_obs(), ')'),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input$pA_var_percentile),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          pA_stats_n_nas() / pA_stats_obs() * 100,
          pA_stats_min(),
          pA_stats_q1(),
          pA_stats_median(),
          pA_stats_mean(),
          pA_stats_q3(),
          pA_stats_max(),
          pA_var_percentile(),
          pA_stats_sd(),
          pA_stats_correlation()
        )
      )
    )

    output$pA_gt1 <- render_gt({
      validate(
        need(
          isTruthy(input$pA_var_percentile) && between(input$pA_var_percentile, 0, 100),
          'Percentile must be between 0 and 100'))

      pA_t1() |>
        gt() |>
        sub_missing() |>
        cols_label(var = 'Measure', value = 'Value') |>
        fmt_number(decimals = input$pA_t1_digits) |>
        opt_interactive(use_pagination = F,
                        use_highlight = T,
                        use_compact_mode = T) |>
        tab_options(table.background.color = '#ffffff')
    }) |> bindCache(
      input$pA_t1_digits,
      pA_t1()
    )

    # config events -----------------------------------------------------------
    mod_pC <- page_config_server('pC')
    color_fill <- reactive(mod_pC$palette()[['fill']])
    color_line <- reactive(mod_pC$palette()[['line']])

    # exit app event ----------------------------------------------------------
    session$onSessionEnded(stopApp)
    observe({
      if (input$navbar == 'exit') {
        gc()
        session$sendCustomMessage(type = 'closeWindow', message = 'message')
        stopApp()
      }
    })
  } # end of server function

  ### Run App -----------------------------------------------------------------
  shinyApp(ui, server, options = list(launch.browser = T))
}
