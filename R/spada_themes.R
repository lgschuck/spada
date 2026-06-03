
# themes names ----------------------------------------------------------------
themes_names <- c('spada_theme', 'spada_dark_theme')

# app colors ------------------------------------------------------------------
main_color <- '#02517d'
navbar_bg <- '#0A5A88'
sidebar_color <- '#e3e3e4'
inputs_border_color <- '#c8c8c8'
bg_color <- '#f9f9f9'
secondary <- '#0072B2'
sucess <- '#009E73'
danger <- '#b60020'
plot_fill_color <- '#0099F8'
plot_line_color <- '#EE7942'
plot_title_color <- '#02517d'

startup_text_color <- '#FFFFFF'
# palettes --------------------------------------------------------------------
gray_palette <- c('#ffffff', '#585858', '#232323')
blue_palette <- c('#ffffff', '#096691', '#134359')
yl_palette <- c('#ffffff', '#ffc107', '#f7a305')
dg_palette <- c('#ffffff','#1c6561', '#284e4c')
lg_palette <- c('#ffffff', '#0cb0a8', '#09918b')
pk_palette <- c('#ffffff', '#bf007f', '#8f0360')
red_palette <- c('#ffffff', '#b60020', '#750217')

# basic rules -----------------------------------------------------------------

theme_basic_rules <- list(
  paste(
    " $main_color:", main_color, ";",
    " $secondary:", secondary, ";",
    " $bg_color:", bg_color, ";",
    " $sidebar_bg:", sidebar_color, ";",
    " $inputs_border_color:", inputs_border_color, ";",

    "
      $grad1: #1f4e72;
      $grad2: #2a6485;
      $grad3: #3a7f9d;
      $grad4: #4b97b6;
      $grad5: #5fa3c2;
      $grad6: #4e96b6;
      $stati_card_text: #ffffff;

      .navbar {
        min-height: 45px !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;
      }

      .main{
        padding-right: 16px !important;
        padding-top: 16px !important;
        padding-bottom: 8px !important;
      }

      .nav-link { font-size: 18px; }

      .big-card{
        background-color: $main_color;
      }

      .mini-header {
        color: white;
        background: linear-gradient(to right, $grad1, $grad2, $grad3, $grad4, $grad5, $grad6);
        /*font-weight: 400 !important;*/
      }

      .btn-task:active {
        background-color: darken($bg_color, 10%) !important;
        transform: scale(0.99);
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.2);
      }

      .btn-task:hover {
        background-color: $secondary !important;
        border-color: $secondary !important;
        color: white !important;
      }

      .btn-task-cancel {
        color: #dc3545 !important;
        background-color: $bg_color !important;
        border-color: #dc3545 !important;
      }

      .btn-task-cancel:hover {
        background-color: $danger !important;
        border-color: $danger !important;
        color: $bg_color !important;
      }

      .mini-btn {
        padding: 5px 10px !important;
        color: $secondary !important;
        background-color: $bg_color !important;
        border-color: $secondary !important;
        border-radius: 0rem;
        font-weight: 400;
      }

      .micro-btn-cancel {
        width: 18px !important;
        height: 18px !important;
        padding: 0 !important;
        font-size: 0.65rem !important;
        border: none !important;
        background: transparent !important;
      }

      .micro-btn-cancel:hover,
      .micro-btn-cancel:focus,
      .micro-btn-cancel:active {
        color: $secondary !important;
      }

      .card, .well {
       --bs-card-inner-border-radius: 0;
      }

      .card-body {border-radius: 0rem;}

      .card-sidebar {
        background-color: $main_color !important;
      }

      .value-box-title {
        font-size: 1rem !important;
      }

      .value-box-value {
        font-size: 1.5rem !important;
      }

      .nav-pills .nav-link {
        font-size: 14px !important;
        padding-top: 6px !important;
        padding-bottom: 6px !important;
      }

      .popover.preview-dt-popup {
        max-width: 120vw;
        width: 600px;
        border-radius: 0 !important;
      }

      .popover.preview-dt-popup-mini {
        max-width: 120vw;
        width: 300px;
        border-radius: 0 !important;
      }

      .popover.preview-dt-popup .popover-body {
        max-height: 100vh;
        overflow: auto;
      }

      .modal-content {
        border-radius: 0 !important;
      }

      .modal-header {
        background-color: #02517d;
        color: white;
        border-radius: 0 !important;
      }

    .navbar-nav .nav-link {
      transition: background-color 0.2s ease;

      border-radius: 10px;
      margin: 1px;

      padding-left: 12px !important;
      padding-right: 12px !important;
    }

    .navbar-nav .nav-link:hover {
      background-color: rgba(255, 255, 255, 0.12);
      color: white !important;
    }

    .navbar-nav .nav-link.active {
      background-color: rgba(255, 255, 255, 0.18);
      color: white !important;
    }

    .selectize-input {
      word-wrap: break-word;
      word-break: break-word;
    }

    .selectize-dropdown {word-wrap: break-word;}

  "
  )
)

# default theme ---------------------------------------------------------------
spada_theme <- bs_theme(
  version = 5,
  bg = bg_color,
  fg = '#000000',
  primary = main_color,
  secondary = secondary,
  success = sucess,
  danger = danger,
  font_size_base = '1rem',
  'nav-pills-border-radius' = '0rem',
  'nav-pills-link-active-color' = main_color,
  'nav-pills-link-active-bg' = sidebar_color,
  'border-radius-sm' = 0,
  'border-radius' = 0,
  'navbar-brand-font-size' = '1.5rem',
  'navbar-brand-padding-y' = '0.250rem',
  'btn-font-weight' = 400,
  'dropdown-bg' = '#f9f9f9',
  'dropdown-color' = main_color,
  'dropdown-link-color' = '#000000',
  'dropdown-link-hover-bg' = sidebar_color,
  base_font = font_collection('Segoe UI', 'Ubuntu', 'system-ui')
) |> bs_add_rules(theme_basic_rules) |>
  bs_add_rules(
    list(
      "
      .accordion-sidebar{
        background-color: $main_color !important;
        color: #ffffff;
      }

      .card {
        border-radius: 0.1rem;
        margin: -8px;
      }

      .big-card-footer{
        background-color: $main_color;
        margin-top: -12px !important;
        padding-bottom: 6px !important;
        height: 60px;
      }

      .btn-task {
        color: $secondary !important;
        background-color: $bg_color !important;
        border-color: $secondary !important;
        padding-top: 6px !important;
        padding-bottom: 6px !important;
        border-radius: 0rem
      }

      .btn-task-cancel {
        color: #dc3545 !important;
        background-color: white !important;
        border-color: #dc3545 !important;
        border-radius: 0rem
      }

      .control-label {
        margin-bottom: 3px !important;
        padding-top: 3px !important;
      }

      .bslib-sidebar-layout>.sidebar {
        background-color: $sidebar_bg !important;
        color: black !important;
      }

      .bslib-sidebar-layout>.collapse-toggle {
        color: black !important;
        background-color: unset;
      }

      .nav-pills .nav-link:hover {
        background-color: #f0f0f0 !important;
      }


      .shiny-input-text,
      .shiny-input-number,
      .selectize-input,
      .shiny-input-textarea textarea {
        border: 1px solid #d1d1d1 !important;
        border-radius: 0 !important;
        box-shadow: none !important;

        transition:
          border-color 0.2s ease,
          box-shadow 0.2s ease;
      }

      .selectize-input:hover,
      .shiny-input-text:hover,
      .shiny-input-number:hover,
      .shiny-input-textarea textarea:hover {
        border-color: #c8c8c8 !important;
        border-bottom: 1px solid $main_color !important;
      }

      .selectize-input:focus,
      .shiny-input-text:focus,
      .shiny-input-number:focus,
      .shiny-input-textarea textarea:focus {
        border-color: #c8c8c8 !important;
        border-bottom: 1px solid $main_color !important;
      }
    ")
  )


# darkly spada theme ----------------------------------------------------------
spada_dark_theme <- bs_theme(
  version = 5,
  bootswatch = 'darkly',
  'border-radius-sm' = 0,
  'border-radius' = 0,
  'navbar-brand-font-size' = '1.5rem',
  'btn-font-weight' = 400,
  base_font = font_collection('Open Sans', 'Ubuntu', 'system-ui')
  ) |>
  bs_add_rules(theme_basic_rules) |>
  bs_add_rules(
    list(
      "
      .card {
        border-radius: 0rem;
        margin: -4px;
      }

      .big-card-footer{
        margin-top: -12px !important;
        padding-bottom: 0px !important;
        height: 60px;
      }

      .btn-task {
        color: $secondary;
        background-color: $bg_color;
        border-color: $secondary;
        border-radius: 0rem
      }

      .nav-pills .nav-link:hover {
        background-color: #5a5a5a !important;
        color: #ffffff !important;
      }

    ")
  )

# themes ----------------------------------------------------------------------
spada_themes <- list(
  spada_theme = spada_theme,
  spada_dark_theme = spada_dark_theme
)
