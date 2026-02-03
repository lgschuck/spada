
# ui --------------------------------------------------------------------------
rename_cols_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Rename Column', class = 'mini-header'),
      card_body(
        selectInput(ns('vars_sel'), 'Variable', NULL),

        textInput(ns('txt_new_name'), 'New name'),
      ),
      card_footer(btn_task(ns('btn_rename'), 'Rename Variable', icon('check')))
    ),
    card(
      card_header('Rename Multiple Columns', class = 'mini-header'),
      card_body(
        selectizeInput(ns('vars_sel_multi'), 'Variables', NULL,
                       multiple = T,
                       options = list(plugins = list('remove_button', 'clear_button')),
                       width = '80%'),

        radioButtons(ns('rename_method'), 'Rename Method',
                     choices = c('Add Prefix/Suffix' = 'prefix_suffix',
                                 'Apply Function' = 'function',
                                 'Replace Part' = 'replace',
                                 'Remove Part' = 'remove')),

        conditionalPanel(
          condition = "input.rename_method == 'prefix_suffix'", ns = ns,
          textInput(ns('txt_new_name_multi'), 'New part to insert'),
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
rename_cols_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    df_names <- reactive(get_act_dt(session) |> names())

    # rename 1 variable ------------------------------------------------------
    observe({
      req(df_names())
      updateSelectInput(
        session,
        'vars_sel',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())


    observe({
      if(!isTruthy(input$vars_sel)){
        msg('Select at least one variable')
      } else {
        if(is_valid_name(input$txt_new_name) &&
           input$txt_new_name %notin% df_names()){

          temp <- copy(get_act_dt(session))

          setnames(temp, input$vars_sel, input$txt_new_name)

          update_act_dt(session, copy(temp), updated_cols = input$txt_new_name,
                        change_type = 'rename_cols')

          rm(temp)

          msg('Rename column: OK')
        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_rename)

    # rename multiple veriables -----------------------------------------------
    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_sel_multi',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())


    observe({
      if(!isTruthy(input$vars_sel_multi)){
        msg('Select at least one variable')
      } else {

        temp <- copy(get_act_dt(session))
        selected_names <- input$vars_sel_multi

        if (input$rename_method == 'prefix_suffix') {

          if(!isTruthy(input$txt_new_name_multi)){
            msg('Inform a new part to insert')
            return()
          } else {
            if(input$part_position == 'prefix') {
              new_names <- paste0(input$txt_new_name_multi,
                                  input$name_separator, selected_names)
            } else if(input$part_position == 'suffix'){
              new_names <- paste0(selected_names, input$name_separator,
                                  input$txt_new_name_multi)
            }
          }
        } else if(input$rename_method == 'function') {
          new_names <- sapply(selected_names, match.fun(input$name_function))
        } else if(input$rename_method == 'replace') {

          if(!isTruthy(input$txt_replace_part) || !isTruthy(input$txt_replace_new_part)){
            msg('Inform the text to replace')
            return()
          } else {
            new_names <- gsub(input$txt_replace_part, input$txt_replace_new_part,
                              selected_names, fixed = TRUE)
          }

        } else if(input$rename_method == 'remove') {

          if(!isTruthy(input$txt_remove_part)){
            msg('Inform the text to be removed')
            return()
          } else {
            new_names <- gsub(input$txt_remove_part, '', selected_names, fixed = TRUE)
          }
        }

        # Validação de nomes únicos e válidos --------------------------------
        if (is_valid_name(new_names) |> all()) {

          if(all(new_names == selected_names)){
            msg('No changes to apply')
          } else if(anyDuplicated(new_names)){
            msg_error('There is duplication in the new names')
          } else if(any(new_names %in% setdiff(df_names(), selected_names))){
            msg_error('Some new names already in use')
          } else {
            setnames(temp, input$vars_sel_multi, new_names)

            update_act_dt(session, copy(temp), updated_cols = input$new_names,
                          change_type = 'rename_cols')

            msg('Rename columns: OK')
            rm(temp)
          }
        } else {
          msg_error('New names are not valid')
        }
      }
    }) |> bindEvent(input$btn_rename_multi)

  })
}
