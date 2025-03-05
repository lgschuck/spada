
# ui --------------------------------------------------------------------------
filter_rows_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Filter Rows', class = 'mini-header'),
    card_body(
      radioGroupButtons(
        ns('filter_type'),
        'Filter Type',
        c(
          'One Variable' = 'one',
          'Two Variables' = 'two',
          'Sample' = 'sample',
          'Freehand' = 'free'
        ),
        selected = character(0),
        size = 'sm',
        individual = T
      ),

      # panel for one var filter ----------------------------------------------
      conditionalPanel(
        condition = "input.filter_type == 'one'",
        ns = ns,
        uiOutput(ns('ui_one_var_sel')),
        selectInput(ns('one_var_operator'), 'Operator', c('', filter_operators)),
        uiOutput(ns('ui_one_var_value'))
      ),

      # panel for two var filter ----------------------------------------------
      conditionalPanel(
        condition = "input.filter_type == 'two'",
        ns = ns,
        uiOutput(ns('ui_two_var_sel1')),
        selectInput(
          ns('two_var_operator'),
          'Operator',
          c('', equal_operators, compare_operators)
        ),
        uiOutput(ns('ui_two_var_sel2'))
      ),

      # panel for sample filter -----------------------------------------------
      conditionalPanel(
        condition = "input.filter_type == 'sample'",
        ns = ns,
        radioButtons(
          ns('sample_type'),
          NULL,
          c('Number of Rows' = 'rows', '% of Rows' = 'percent'),
          inline = T
        ),
        uiOutput(ns('ui_sample'))
      ),

      # panel for free filter -----------------------------------------------
      conditionalPanel(
        condition = "input.filter_type == 'free'",
        ns = ns,
        textAreaInput(
          ns('txt_code_input'),
          'Input Code',
          width = '800px',
          height = '200px',
          resize = 'both'
        )
      ),
    ),
    card_footer(btn_task(
      ns('btn_filter'), 'Apply filters', icon('check')
    ))
  )
}

# server ----------------------------------------------------------------------
filter_rows_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Store active dataset
    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    # Reactive to get column names
    df_names <- reactive(df$df_active |> names())

    nrow_df_active <- reactive(nrow(df$df_active))

    # variable inputs ---------------------------------------------------------
    output$ui_one_var_sel <- renderUI(
      selectInput(ns('one_var_sel'), 'Variable', c('', df_names()))
    )

    output$ui_two_var_sel1 <- renderUI(
      selectInput(ns('two_var_sel1'), 'Variable 1', c('', df_names()))
    )

    output$ui_two_var_sel2 <- renderUI(
      selectInput(ns('two_var_sel2'), 'Variable 2', c('', df_names()))
    )

    # selected column type ----------------------------------------------------
    col_type_one_var <- reactive({
      req(input$one_var_sel)
      df$df_active[[input$one_var_sel]] |> obj_type()
    })

    col_type_two_var1 <- reactive({
      req(input$two_var_sel1)
      df$df_active[[input$two_var_sel1]] |> obj_type()
    })

    # render UI for sample type -----------------------------------------------
    output$ui_sample <- renderUI({
      req(input$sample_type)
      tagList(

        if (input$sample_type == 'rows') {
          numericInput(
            ns('n_rows'), 'Number of Rows',
            value = (nrow_df_active()/2) |> ceiling(),
            min = 1, max = nrow_df_active()
          )
        } else if (input$sample_type == 'percent') {
          sliderInput(ns('sample_size'), 'Sample Size (%)', 1, 100, 50)
        }
        ,
        checkboxInput(ns('x_sample_replace'), 'Replace') |>
          ttip('Allow sample with replacement')
      )
    })

    # render UI for value input dynamically -----------------------------------
    observe({
      req(input$one_var_sel, input$one_var_operator, df$df_active)
      output$ui_one_var_value <- renderUI({
        if (col_type_one_var() == 'date'){
          if(input$one_var_operator %in% c(equal_operators, compare_operators)){
            dateInput(ns('one_var_value'), 'Date')
          } else if (input$one_var_operator %in% between_operators){
            dateRangeInput(ns('one_var_value'), 'Date', end = Sys.Date() + 30)
          } else if (input$one_var_operator %in% in_operators){
            tagList(
              layout_column_wrap(
                dateInput(ns('one_var_value'), 'Date'),
                textInput(ns('preview_value'), 'Preview Values')
              ),
              layout_column_wrap(
                btn_task(ns('btn_insert_value'), 'Insert Value', icon('plus')),
                btn_task(ns('btn_clear_value'), 'Clear All Values', icon('xmark'))
              )
            )
          }
        } else if (col_type_one_var() == 'numeric'){
          if(input$one_var_operator %in% c(equal_operators, compare_operators)){
            numericInput(ns('one_var_value'), 'Value', value = 0)
          } else if (input$one_var_operator %in% between_operators){
            tagList(
              numericInput(ns('one_var_value'), 'Inicial Value', value = 0),
              numericInput(ns('one_var_value2'), 'Final Value', value = 0)
            )
          } else if (input$one_var_operator %in% in_operators){
            tagList(
              layout_column_wrap(
                numericInput(ns('one_var_value'), 'Value', value = 0),
                textInput(ns('preview_value'), 'Preview Values')
              ),
              layout_column_wrap(
                btn_task(ns('btn_insert_value'), 'Insert Value', icon('plus')),
                btn_task(ns('btn_clear_value'), 'Clear All Values', icon('xmark'))
              )
            )
          }
        } else if (col_type_one_var() == 'factor'){
          if(input$one_var_operator %in% c(equal_operators, in_operators)){
            selectizeInput(
              ns('one_var_value'),
              'Value',
              choices = df$df_active[[input$one_var_sel]] |> levels(),
              multiple = T,
              options = list(create = T)
            )
          }
        } else if (col_type_one_var() %in% c('char', 'complex')){
          if(input$one_var_operator %in% c(equal_operators, in_operators)){
            selectizeInput(
              ns('one_var_value'),
              list('Value', bs_icon("info-circle")) |>
                ttip(PLACE = 'right', 'Text should not be in quotes'),
              choices = NULL,
              multiple = T,
              options = list(create = T)
            )
          }
        } else if (col_type_one_var() == 'logical'){
          div()
        } else if(isTruthy(input$one_var_sel) && isTruthy(input$one_var_operator)){
          selectizeInput(
            ns('one_var_value'),
            list('Value', bs_icon("info-circle")) |>
              ttip(PLACE = 'right', 'Text should not be in quotes'),
            choices = NULL,
            multiple = T,
            options = list(create = T)
          )
        }
      })
    }) |> bindEvent(input$one_var_sel, input$one_var_operator)

    # update selectinput to show pertinent operators --------------------------
    observe({
      req(input$one_var_sel)
      updateSelectInput(
        session, 'operator',
        choices =
          if(col_type_one_var() %in% c('factor', 'char', 'complex')){
            c('',
              equal_operators,
              na_operators,
              in_operators)
          } else if (col_type_one_var() == 'logical'){
            c('', logical_operators)
          } else if (col_type_one_var() == 'date'){
            c('', filter_operators[filter_operators %notin% logical_operators])
          } else if (col_type_one_var() == 'numeric'){
            c('', filter_operators[filter_operators %notin% logical_operators])
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$one_var_sel)

    # update selectinput two vars filter to show pertinent operators ----------
    observe({
      req(input$two_var_sel1)
      updateSelectInput(
        session, 'two_var_operator',
        choices =
          if(col_type_two_var1() %in% c('factor', 'char', 'complex', 'logical')){
            c('', equal_operators)
          } else if (col_type_two_var1() %in% c('date', 'numeric')){
            c('', equal_operators, compare_operators) }
      )
    }) |> bindEvent(input$two_var_sel1)

    # temporary values for multi-inputs ---------------------------------------
    value_temp <- reactiveValues(value_temp_inserted = NULL)

    # insert values -----------------------------------------------------------
    observe({
      req(input$one_var_value)
      if(is.null(value_temp$value_temp_inserted)){
        value_temp$value_temp_inserted <- input$one_var_value
      } else {
        value_temp$value_temp_inserted <- c(value_temp$value_temp_inserted,
                                            input$one_var_value)
      }

      updateTextInput(session, 'preview_value',
                      value = value_temp$value_temp_inserted)
    }) |> bindEvent(input$btn_insert_value)

    # clear inserted values
    observe({
      value_temp$value_temp_inserted <- NULL

      updateTextInput(session, 'preview_value',
                      value = '')
    }) |> bindEvent(input$btn_clear_value)

    # apply btn filter rows ---------------------------------------------------
    observe({
      temp <- copy(df$df_active)
      # filter events for one variable ----------------------------------------
      if (input$filter_type == 'one') {
        # test if var and operator were informed
        if (!isTruthy(input$one_var_sel)) {
          msg_error('Choose a variable')
          return()
        } else if (!isTruthy(input$one_var_operator)) {
          msg_error('Choose an operator')
          return()
        } else if (!isTruthy(input$one_var_value) &
                   input$one_var_operator %notin% c(na_operators, logical_operators, outlier_operators)) {
          msg_error('Insert a value')
          return()
        } else if (input$one_var_operator %in% between_operators) {
          if (col_type_one_var() == 'numeric' &
              !isTruthy(input$one_var_value2)) {
            msg_error('Inform inicial and final values')
            return()
          } else if (col_type_one_var() == 'date' &
                     (!isTruthy(input$one_var_value[1]) |
                      !isTruthy(input$one_var_value[2]))) {
            msg_error('Inform inicial and final dates')
            return()
          }
        } else if (input$one_var_operator %in% in_operators &
                   col_type_one_var() %in% c('date', 'numeric') &
                   is.null(value_temp$value_temp_inserted)) {
          msg_error('Insert values')
          return()
        }

        # use inserted values
        if (input$one_var_operator %in% between_operators) {
          if (col_type_one_var() == 'numeric') {
            value_temp$value_temp <- c(input$one_var_value, input$one_var_value2)
          } else if (col_type_one_var() == 'date') {
            value_temp$value_temp <- input$one_var_value
          }
        } else if (input$one_var_operator %in% in_operators &
                   col_type_one_var() %notin% c('char', 'complex', 'factor')) {
          value_temp$value_temp <- value_temp$value_temp_inserted
        } else {
          value_temp$value_temp <- input$one_var_value
        }
        value_temp$len <- value_temp$value_temp |> length()

        # pass values to filter function
        if (input$one_var_operator %in%
            c(na_operators, logical_operators, outlier_operators)) {
          temp <- filter_rows(temp, input$one_var_sel, input$one_var_operator, NULL)
          msg('Filter rows: OK')
        } else if (value_temp$len > 1 & input$one_var_operator %in%
                   c(equal_operators, compare_operators)) {
          msg('Operator requires value of length 1')
          return()
        } else {
          temp <- filter_rows(temp,
                              input$one_var_sel,
                              input$one_var_operator,
                              value_temp$value_temp)
          msg('Filter rows: OK')
        }

        # reset value_temp
        value_temp$value_temp_inserted <- NULL
        value_temp$value_temp <- NULL
        value_temp$len <- NULL

        # clear value after click in button
        updateSelectInput(session, 'var', selected = '')

        updateSelectInput(session, 'operator', selected = '')

        # filter events for 2 variables -----------------------------------------
      } else if (input$filter_type == 'two') {
        if (!isTruthy(input$two_var_sel1) || !isTruthy(input$two_var_sel2)) {
          msg_error('Choose 2 variables')
          return()
        } else if (!isTruthy(input$two_var_operator)) {
          msg_error('Choose an operator')
          return()
        } else if (temp[[input$two_var_sel1]] |> obj_type() !=
                   temp[[input$two_var_sel2]] |> obj_type()) {
          msg('Variables must be of the same type')
        } else {
          temp <- filter_rows_2vars(temp,
                                    input$two_var_sel1,
                                    input$two_var_sel2,
                                    input$two_var_operator)
          msg('Filter rows: OK')
        }

        # filter events for sample ----------------------------------------------
      } else if (input$filter_type == 'sample') {
        if (input$sample_type == 'rows') {
          if (!isTruthy(input$n_rows) ||
              !between(input$n_rows, 1, nrow_df_active())) {
            msg_error(paste('Number of rows must be between 1 and', nrow_df_active()))
          } else {
            temp <- temp[sample(1:nrow_df_active(),
                                input$n_rows,
                                replace = input$x_sample_replace), ]

            msg('Filter rows: OK')
          }

        } else if (input$sample_type == 'percent') {
          temp <- temp[sample(
            1:nrow_df_active(),
            input$sample_size / 100 * nrow_df_active(),
            replace = input$x_sample_replace
          ), ]

          msg('Filter rows: OK')
        }

      # filter events for freehand --------------------------------------------
      } else if (input$filter_type == 'free') {

        # parse code ----------------------------------------------------------
        parsed_code <- try(parse_expr(input$txt_code_input), silent = T)

        if(inherits(parsed_code, "try-error")){
          return(msg_error('Error to validate expression. Check code'))
        }

        # get operations and variables ----------------------------------------
        code_operations <- all.names(parsed_code, unique = T)
        code_vars <- all.vars(parsed_code)

        code_operations <- code_operations[!code_operations %in% code_vars]

        #  check variables and operations -------------------------------------
        if (!all(code_vars %in% c(df_names(), 'T', 'F'))) {
          msg_error('Some variables are not present in the dataset')
        } else if (!all(code_operations %in% allowed_operations)) {
          msg_error('Some operations are not allowed')
        } else {
          # create safe env for evaluation ------------------------------------
          e1 <- safe_env(allowed_operations)

          # run code ----------------------------------------------------------

          e1$temp <- copy(temp)
          e1$parsed_code <- parsed_code

          e1$temp <- eval(expression(

            try(temp[parsed_code, ,
                             env = list(parsed_code = parsed_code)],
                silent = TRUE)
            ),
            envir = e1
          )

          if(inherits(e1$temp, "try-error")){
            return(msg_error('Error in expression. Check code'))
          } else{
            temp <- copy(e1$temp)
          }

        }
      }

      df$df_active <- copy(temp)
      rm(temp)
      rm(e1)

    }) |> bindEvent(input$btn_filter)

    return(list(df_filter_rows = reactive(df$df_active)))
  })
}
