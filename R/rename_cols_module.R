
# ui --------------------------------------------------------------------------
rename_cols_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Rename Column', class = 'mini-header'),
      card_body(
        uiOutput(ns('ui_var_sel')),
        textInput(ns('txt_new_name'), 'New name'),
      ),
      card_footer(btn_task(ns('btn_rename'), 'Rename Variable', icon('check')))
    ),
    card(
      card_header('Rename Multiple Columns', class = 'mini-header'),
      card_body(
        uiOutput(ns('ui_var_sel_multi')),
        radioButtons(ns('rename_method'), 'Rename Method',
                     choices = c('Add Prefix/Suffix' = 'prefix_suffix',
                                 'Apply Function' = 'function',
                                 'Replace Part' = 'replace',
                                 'Remove Part' = 'remove')),

        conditionalPanel(
          condition = "input.rename_method == 'prefix_suffix'", ns = ns,
          textInput(ns('txt_new_name_multi'), 'New name part'),
          radioButtons(ns('part_position'), 'Part Position',
                       choices = c('Prefix' = 'prefix', 'Suffix' = 'suffix')),
          radioButtons(ns('name_separator'), 'Separator',
                       choices = c('Underscore (_)' = '_', 'Dot (.)' = '.', 'None' = ''))
        ),

        conditionalPanel(
          condition = "input.rename_method == 'function'", ns = ns,
          selectInput(ns('name_function'), 'Function to Apply',
                      choices = c('Upper Case' = 'toupper',
                                  'Lower Case' = 'tolower',
                                  'Title Case' = 'toTitleCase'))
        ),

        conditionalPanel(
          condition = "input.rename_method == 'replace'", ns = ns,
          textInput(ns('txt_replace_part'), 'Part to Replace'),
          textInput(ns('txt_replace_new_part'), 'New Part'),
        ),

        conditionalPanel(
          condition = "input.rename_method == 'remove'", ns = ns,
          textInput(ns('txt_remove_part'), 'Part to Remove')
        ),

      ),
      card_footer(btn_task(ns('btn_rename_multi'), 'Rename Variables', icon('check')))
    )
  )
}

# server ----------------------------------------------------------------------
rename_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    # Reactive to get column names
    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    # rename 1 variable ------------------------------------------------------
    output$ui_var_sel <- renderUI({
      selectInput(ns('vars_sel'), 'Variable', c('', df_names()))
    })

    observe({
      req(input$vars_sel, input$txt_new_name, df$df_active)
      if(input$vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {
        if(is_valid_name(input$txt_new_name) &&
           input$txt_new_name %notin% df_names()){

          temp <- copy(df$df_active)

          setnames(temp, input$vars_sel, input$txt_new_name)

          df$df_active <- copy(temp)

          rm(temp)

          msg('Rename column: OK')
        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_rename)

    # rename multiple veriables -----------------------------------------------
    output$ui_var_sel_multi <- renderUI({
      selectizeInput(ns('vars_sel_multi'), 'Variables', c('', df_names()),
                  multiple = T,
                  options = list(plugins = list('remove_button', 'clear_button')))
    })

    observe({
      req(input$vars_sel_multi, df$df_active)
      if(input$vars_sel_multi |> length() == 0){
        msg('Select at least one variable')
      } else {

        temp <- copy(df$df_active)
        new_names <- input$vars_sel_multi

        if (input$rename_method == 'prefix_suffix') {
          req(input$txt_new_name_multi)
          if (input$part_position == 'prefix') {
            new_names <- paste0(input$txt_new_name_multi,
                                input$name_separator, new_names)
          } else if (input$part_position == 'suffix'){
            new_names <- paste0(new_names, input$name_separator,
                                input$txt_new_name_multi)
          }
        } else if (input$rename_method == 'function') {
          new_names <- sapply(new_names, match.fun(input$name_function))
        } else if (input$rename_method == 'replace') {
          req(input$txt_replace_part, input$txt_replace_new_part)
          new_names <- gsub(input$txt_replace_part, input$txt_replace_new_part,
                            new_names, fixed = TRUE)
        } else if (input$rename_method == 'remove') {
          req(input$txt_remove_part)
          new_names <- gsub(input$txt_remove_part, '', new_names, fixed = TRUE)
        }

        # Validação de nomes únicos e válidos --------------------------------
        if (is_valid_name(new_names) |> all()) {

          if(all(new_names %in% df_names())) {
            msg('No changes to apply')
          } else if (any(new_names %in% df_names())) {
            msg_error('Some new names already in use')
          } else {
            setnames(temp, input$vars_sel_multi, new_names)
            df$df_active <- copy(temp)
            msg('Rename columns: OK')
            rm(temp)
          }
        } else {
          msg_error('New names are not valid')
        }
      }
    }) |> bindEvent(input$btn_rename_multi)

    # return of module --------------------------------------------------------
    return(list(df_rename_cols = reactive(df$df_active)))
  })
}
