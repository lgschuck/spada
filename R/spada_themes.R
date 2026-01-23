
# themes names ----------------------------------------------------------------
themes_names <- c('spada_theme', 'spada_dark_theme')

# app colors ------------------------------------------------------------------
main_color <- '#02517d'
sidebar_color <- '#e3e3e4'
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

theme_basic_rules <- as_sass(
  list(
    list(main_color = main_color,
         secondary = secondary,
         bg_color = bg_color,
         sidebar_bg = sidebar_color,
         grad1 = '#1f4e72',
         grad2 = '#2a6485',
         grad3 = '#3a7f9d',
         grad4 = '#4b97b6',
         grad5 = '#5fa3c2',
         grad6 = '#4e96b6',
         navbar_bg = '#007bb5',
         stati_card_text = '#ffffff',
         startup_bg = main_color
    ),
    "
      .navbar {
        background: $navbar_bg !important;
        height: 45px !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;
      }

      .main{
        padding-right: 16px !important;
        padding-top: 16px !important;
        padding-bottom: 8px !important;
      }

      .nav-link { font-size: 18px; }

      body { font-size: 0.9rem; }

      .big-card{
        background-color: $main_color;
      }

      .mini-header {
        color: white;
        background: linear-gradient(to right, $grad1, $grad2, $grad3, $grad4, $grad5, $grad6);
      }

      .btn-task:active {
        background-color: darken($bg_color, 10%) !important;
        transform: scale(0.99); /* efeito de afundar */
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.2);
      }

      .btn-task:hover {
        background-color: $secondary !important;
        border-color: $secondary !important;
        color: white !important;
      }

      .btn-task-cancel {
        color: #dc3545 !important;
        background-color: white !important;
        border-color: #dc3545 !important;
      }

      .btn-task-cancel:hover {
        background-color: $danger !important;
        border-color: $danger !important;
        color: white !important;
      }

      .mini-btn {
        padding: 5px 10px !important;
        color: $secondary !important;
        background-color: $bg_color !important;
        border-color: $secondary !important;
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
      }

      .popover.preview-dt-popup .popover-body {
        max-height: 100vh;
        overflow: auto;
      }
 ")
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
  'nav-pills-link-active-bg' = sidebar_color
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
      }

      .btn-task-cancel {
        color: #dc3545 !important;
        background-color: white !important;
        border-color: #dc3545 !important;
      }

      .btn-task-cancel:hover {
        background-color: $danger !important;
        border-color: $danger !important;
        color: white !important;
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
    ")
  )

# darkly spada theme ----------------------------------------------------------
spada_dark_theme <- bs_theme(version = 5, bootswatch = 'darkly') |>
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
      }

      .nav-pills .nav-link:hover {
        background-color: #5a5a5a !important;
        color: #ffffff !important;
      }

    ")
  )
