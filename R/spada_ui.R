
# Function with the ui of spada.R
spada_ui <- function(){
  tagList(
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
        font_size_base = '1rem'
      ),
      title = 'Spada',

      bg = '#02517d',

      # page sidebar ----------------------------------------------------------
      sidebar = sidebar(
        bg = '#e3e3e4',
        open = F,
        accordion(
          open = T,
          accordion_panel(
            style = 'background-color: #02517d; color: white;',
            'Dataset Info',
            icon = bs_icon('file-binary', size = '1.75em'),
            uiOutput('sidebar_df_info')
          ))),

      # page data -------------------------------------------------------------
      nav_panel(
        'Data',
        icon = bs_icon('info-square-fill'),

        card(full_screen = T,
             card_body(
               style = 'background-color: #02517d;',
               height = '800px',
               navset_card_pill(
                 id = 'navset_card_pill_data',
                 nav_panel('Highlights', data_highlights_ui('pD_highlights')),
                 nav_panel('Metadata', card_body(gt_output('pD_metadata_gt'))),
                 nav_panel('Overview', data_overview_ui('pD_overview')),
                 nav_panel(
                   'Data',
                   card_body(
                     uiOutput('pD_data_ui_sel_df'),
                     textInput('pD_data_txt_new_name', 'New name'),
                     layout_column_wrap(
                       btn_task('pD_data_btn_new_name', 'Rename dataset', icon('file-signature')),
                       btn_task('pD_data_btn_active', 'Make dataset Active', icon('check')),
                       btn_task('pD_data_btn_copy_dataset', 'Copy dataset', icon('copy')),
                       btn_task('pD_data_btn_delete_dataset', 'Delete dataset', icon('trash-can')),
                     ),
                   )),
                 nav_panel('Import', import_file_ui('pD_import')),
                 nav_panel('Export', export_file_ui('pD_export'))
               )
             )
        )
      ),

      # page edit -------------------------------------------------------------
      nav_panel(
        'Edit',
        icon = bs_icon('funnel'),

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
                       select_cols_ui('pE_filter_sel_cols'),
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
                                    icon('check'))
                         )
                       ),
                       card(
                         card_header('Preview', class = 'mini-header'),
                         card_body(gt_output('pE_convert_preview_gt1')),
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
                           radioButtons('pE_order_radio_nas', "NA's placement",
                                        c('Last' = 'last', 'First' = 'first'),
                                        inline = T),
                         ),
                         card_footer(
                           btn_task('pE_order_btn_order_rows', 'Order Rows', icon('shuffle')),
                         )
                       ),
                       order_cols_ui('pE_filter_order_cols')
                     )
                   )
                 )
               )
             ),
             card_footer(
               layout_column_wrap(
                 btn_task('pE_btn_reset', 'Reset Dataset', icon('arrow-rotate-right')) |>
                   tooltip('Restore to previous state (before been set as the Active dataset)', placement = 'top'),
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
      nav_menu(
        title = 'Analysis',

        nav_panel(
          'Exploratory',
          icon = bs_icon('bar-chart-line'),
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
                            uiOutput('pA_E_ui_var_names'),
                            uiOutput('pA_E_ui_var_names2')),
                  nav_panel('Filters',
                            checkboxInput('pA_E_outliers', 'Remove Outliers', F) |>
                              tooltip('Only for numeric vars', placement = 'top'))
                ),
                navset_card_pill(
                  full_screen = T,
                  nav_panel(
                    'Distribution',
                    full_screen = T,
                    card_body(plotOutput('pA_E_g_dist')),
                    card_footer(
                      fluidRow(
                        column(8,
                               radioButtons(
                                 'pA_E_radio_dist_plot',
                                 'Plot type:',
                                 c('Histogram' = 'hist',
                                   'Boxplot' = 'boxplot',
                                   'Boxplot by Groups' = 'boxplot_group',
                                   'Dots' = 'dots',
                                   'Barplot' = 'barplot'),inline = T)),
                        column(2, numericInput('pA_E_var_percentile', 'Percentile', 50, 0, 100, 5)),
                        column(2, conditionalPanel(
                          condition = "input.pA_E_radio_dist_plot == 'hist'",
                          numericInput('pA_E_bins', 'Bins', 10, 5, step = 10))
                        ),
                      )
                    )
                  ),
                  nav_panel(
                    'Scatter',
                    full_screen = T,
                    card_body(plotOutput('pA_E_g_scatter', click = 'plot_brush')),
                    card_footer(
                      layout_column_wrap(
                        checkboxInput('pA_E_scatter_lm', 'Plot Linear Model', F) |>
                          tooltip('Show the line only if LM model was created'),
                        btn_task('pA_E_btn_scatter', 'Generate Plot', icon('gear'))
                      )
                    )
                  ),
                  nav_panel(
                    'Table',
                    full_screen = T,
                    card_body(
                      radioButtons('pA_E_table_type', 'Table type:',
                                   c('1 Variable' = '1d',
                                     '2 Variables' = '2d'), inline = T),
                      verbatimTextOutput('pA_E_table', placeholder = T),
                    ),
                    card_footer()
                  ),
                  nav_panel(
                    'Linear Model',
                    full_screen = T,
                    navset_card_pill(
                      nav_panel(
                        'Parameters',
                        sliderInput('pA_E_sample_size', 'Sample Size (%)', 0, 100, 100) |>
                          tooltip('Applied only if valid values are greater than 10.000'),
                        layout_column_wrap(
                          btn_task('pA_E_btn_scatter_lm_run', 'Run Linear Model', icon('gear')),
                          btn_task('pA_E_btn_scatter_lm_clear', 'Clear Linear Model', icon('trash-can'))
                        )
                      ),
                      nav_panel('Output', verbatimTextOutput('pA_E_linear_model')),
                      nav_panel(
                        'Residuals',
                        plotOutput('pA_E_g_lm_resid'),
                        card_footer(
                          layout_column_wrap(
                            radioButtons('pA_E_radio_lm_resid', 'Plot type:',
                                         c('Histogram' = 'hist', 'Boxplot' = 'boxplot',
                                           'Dots' = 'dots'), inline = T),
                            btn_task('pA_E_btn_lm_resid', 'Plot residuals', icon('chart-simple')))
                        )
                      ),
                    )),
                ),
                navset_card_pill(
                  nav_panel(
                    'Stats',
                    full_screen = T,
                    stats_table_ui('pA_stats')
                  )
                )
              )
            )
          )
        ), # end of exploratory

        nav_panel('Descriptive Stats',
                  icon = bs_icon('graph-up'),
                  descriptive_stats_ui('pA_desc_stats'),),

        nav_panel('Correlation',
                  icon = bs_icon('magnet'),
                  correlation_ui('pA_correlation'),)
      ), # end of analysis menu
      # menu options ----------------------------------------------------------
      nav_menu(
        title = 'Options',
        page_config_ui('pC'),
        nav_item(
          tags$a(
            icon('book'),
            'Documentation',
            href = 'https://lgschuck.github.io/spada/',
            target = '_blank'
          )
        ),
        nav_panel(
          value = 'exit',
          title = 'Exit',
          icon = icon('person-walking-arrow-right')
        )),

      nav_item(
        tags$a(
          icon('github'),
          href = 'https://github.com/lgschuck/spada',
          target = '_blank'
        )),

      nav_spacer(),
      # active dataset --------------------------------------------------------
      nav_item('Active dataset:'),
      nav_item(h4(textOutput('df_active_name')) |>
                 popover(uiOutput('navbar_dataset_info'),
                         placement = 'bottom'))
    )
  )
}
