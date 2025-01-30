
# Function with the ui of spada.R
spada_ui <- function(){
  tagList(
    useBusyIndicators(),

    busy_start_up(
      loader = spin_epic('orbit', color = '#FFFFFF'),
      text = tagList(
        h1('Spada',
        style = "font-family: 'Times'; font-size: 120px;"),
        h3('R > Shiny > You',
           style = "font-family: 'Times'; font-size: 20px;")
      ),
      mode = 'auto',
      timeout = 1200,
      color = '#FFFFFF',
      background = '#02517d'
    ),

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
      sidebar = sidebar_ui('sidebar'),

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
                       filter_rows_ui('pE_filter_rows'),
                       select_cols_ui('pE_filter_sel_cols'),
                     ),
                   ),
                   nav_panel('Convert', convert_cols_ui('pE_convert_cols')),
                   nav_panel(
                     'Order',
                     layout_column_wrap(
                       order_rows_ui('pE_filter_order_rows'),
                       order_cols_ui('pE_filter_order_cols')
                     )
                   )
                 )
               )
             ),
             card_footer(
               div(style = "margin-bottom: -12px !important;"),
               layout_column_wrap(
                 btn_task('pE_btn_reset', 'Reset Dataset', icon('arrow-rotate-right')) |>
                   ttip('Restore to previous state (before been set as the Active dataset)'),
                 btn_task('pE_btn_bkp', 'Create Backup', icon('cloud-arrow-up')) |>
                   ttip('Create a copy of the Active dataset'),
                 btn_task('pE_btn_restore', 'Restore Backup', icon('cloud-arrow-down')) |>
                   ttip('Restore a previously created backup'),
                 btn_task('pE_btn_clear_bkp', 'Clear Backup', icon('trash-can')) |>
                   ttip('Erase the backup'),
               ),
               div(style = "margin-bottom: -16px !important;"),
               style = 'background-color: #02517d;')
        )
      ),

      # page analysis ---------------------------------------------------------
      nav_menu(
        title = 'Analysis',
        nav_panel('Exploratory',
                  icon = bs_icon('bar-chart-line'),
                  exploratory_ui('pA_exploratory')),
        nav_panel('Descriptive Stats',
                  icon = bs_icon('graph-up'),
                  descriptive_stats_ui('pA_desc_stats')),
        nav_panel('Correlation Test',
                  icon = bs_icon('magnet'),
                  correlation_ui('pA_correlation')),
        nav_panel('Normality Test',
                  icon = bs_icon('bell'),
                  normality_test_ui('pA_normality_test')),
        nav_panel('Z Test',
                  icon = icon('z'),
                  z_test_ui('pA_z_test')),
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
                 popover(navbar_df_info_ui('navbar_df_info'),
                         placement = 'bottom'))
    )
  )
}
