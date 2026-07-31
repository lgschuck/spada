
# themes names ----------------------------------------------------------------
themes_names <- c('spada_theme', 'spada_dark_theme')

# app colors ------------------------------------------------------------------
main_color <- '#02517d'
navbar_bg <- '#0A5A88'
sidebar_color <- '#E3E3E4'
inputs_border_color <- '#C8C8C8'
inputs_border_color2 <- '#D1D1D1'
bg_color <- '#F9F9F9'
secondary <- '#0072B2'
sucess <- '#009E73'
danger <- '#B60020'
plot_fill_color <- '#0099F8'
plot_line_color <- '#EE7942'
plot_title_color <- '#02517d'

spada_white <- '#FFFFFF'
spada_black <- '#000000'
spada_navpills_hover <- '#F0F0F0'
spada_navpills_hover_dark <- '#5A5A5A'
spada_blue5 <- '#0A6CA4'
spada_blue6 <- '#0A74B8'
spada_gray5 <- '#424242'
spada_red5 <- '#DC3545'

# palettes --------------------------------------------------------------------
gray_palette <- c('#CFCFCF', '#585858', '#232323')
blue_palette <- c('#229BD4', '#096691', '#134359')
yl_palette   <- c('#F5CB4E', '#FFC107', '#F7A305')
dg_palette   <- c('#378C88', '#1C6561', '#284E4C')
lg_palette   <- c('#0CB0A8', '#228F8A', '#09918B')
pk_palette   <- c('#A35D8C', '#BF007F', '#8F0360')
red_palette  <- c('#8F3646', '#B60020', '#750217')

# themes function -------------------------------------------------------------
spada_bs_theme <- function(theme) {

  basic_rules <- list(
    paste(
      " $main_color:", main_color, ";",
      " $secondary:", secondary, ";",
      " $bg_color:", bg_color, ";",
      " $sidebar_bg:", sidebar_color, ";",
      " $inputs_border_color:", inputs_border_color, ";",
      " $inputs_border_color2:", inputs_border_color2, ";",
      " $navbar_bg:", navbar_bg, ";",
      " $spada_navpills_hover:", spada_navpills_hover, ";",
      " $spada_navpills_hover_dark:", spada_navpills_hover_dark, ";",
      " $spada_white:", spada_white, ";",
      " $spada_blue5:", spada_blue5, ";",
      " $spada_blue6:", spada_blue6, ";",
      " $spada_red5:", spada_red5, ";",
      " $btn_neutral:", spada_gray5, ";",

      "

      $stati_card_text: $spada_white;

      .navbar {
        min-height: 45px !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;

        background: linear-gradient(
          to right,
          $main_color,
          $spada_blue6
        ) !important;

      }

      .main{
        padding-right: 16px !important;
        padding-top: 16px !important;
        padding-bottom: 8px !important;
      }

      .spada-main-sidebar{
        background: $sidebar_bg
      }

      .spada-main-sidebar .accordion-item{
        border: none !important;
        border-color: transparent !important;
      }

      .nav-link { font-size: 18px; }

      .big-card{ background-color: $main_color; }

      .mini-header {
        color: $spada_white;

        background:
          linear-gradient(
            180deg,
            $spada_blue5 0%,
            $navbar_bg 100%
          );

        border-bottom: 1px solid $spada_white;
        border-radius: 0.2rem;
      }

      .btn-task:active,
      .btn-default:active {
        background-color: darken($bg_color, 10%) !important;
        transform: scale(0.99);
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.2);
      }

      .btn-task:hover,
      .btn-default:hover {
        background-color: $secondary !important;
        border-color: $secondary !important;
        color: white !important;
      }

      .btn-task-cancel {
        color: $spada_red5 !important;
        background-color: $bg_color !important;
        border-color: $spada_red5 !important;
      }

      .btn-task-cancel:hover {
        background-color: $danger !important;
        border-color: $danger !important;
        color: $bg_color !important;
      }

      .btn-task.btn-task-neutral {
        color: $btn_neutral !important;
        background-color: white !important;
        border-color: $btn_neutral !important;
      }

      .btn-task.btn-task-neutral:hover {
        background-color: $btn_neutral !important;
        border-color: $btn_neutral !important;
        color: $bg_color !important;
      }

      .mini-btn {
        color: $secondary !important;
        background-color: $bg_color !important;
        border-color: $secondary !important;
        border-radius: 0.2rem;
        font-weight: 400;

        width: 48px;
        height: 32px;
        padding: 0 !important;
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

      .card, .well { --bs-card-inner-border-radius: '0.2rem'; }

      .card-body { border-radius: '0.2rem'; }

      .card-sidebar {
        background:
        linear-gradient(
            180deg,
            $navbar_bg 0%,
            $spada_blue5 100%
        );
      }

      .value-box-title { font-size: 1rem !important; }

      .value-box-value { font-size: 1.5rem !important; }

      .nav-pills .nav-link {
        font-size: 14px !important;
        padding-top: 6px !important;
        padding-bottom: 6px !important;
      }

      .popover.preview-dt-popup {
        max-width: 120vw;
        width: 600px;
        border-radius: 0.2rem !important;
      }

      .popover.preview-dt-popup-mini {
        max-width: 120vw;
        width: 300px;
        border-radius: 0.2rem !important;
      }

      .popover.preview-dt-popup .popover-body {
        max-height: 100vh;
        overflow: auto;
      }

      .modal-content {
        border: none !important;
        border-radius: 0.6rem !important;
        box-shadow:
          0 1.5rem 4rem rgba(0, 0, 0, .20),
          0 .5rem 1rem rgba(0, 0, 0, .08);
      }

      .modal-header {
        background: linear-gradient(
          135deg,
          $main_color 0%,
          $spada_blue6 100%
        );

        color: $spada_white;
        border: none !important;
        padding: 1rem 1.5rem;
      }

      .modal-footer {
        border-top: 1px solid rgba(0, 0, 0, .06);
        padding: 1rem 1.5rem;
      }

      .navbar-nav .nav-link {
        transition: background-color 0.2s ease;
        border-radius: 0.6rem;
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

      .selectize-dropdown {  }

      .navbar .dropdown-menu {
        border: none;
        border-radius: 0.875rem;
        padding: 6px;
        overflow: hidden;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.15);
      }

      .navbar .dropdown-item {
        border-radius: 0.5rem;
        transition: background-color .15s ease, color .15s ease;
      }

      .selectize-dropdown {
        word-wrap: break-word;
        border: none !important;
        border-radius: 0.4rem !important;
        overflow: hidden;
        box-shadow:
          0 0.75rem 2rem rgba(0, 0, 0, .15);
      }

      .selectize-dropdown .option {
        margin: 0.2rem;
        border-radius: 0.2rem;
        transition: background-color .15s ease;
      }

      .selectize-dropdown .active {
        background-color: rgba(10, 108, 164, .08);
      }

      .spada-navbar-df-info-header {
        display: inline-block;
        background: linear-gradient(135deg, $main_color, $spada_blue5);
        color: white;
        padding: 0.6rem 1.2rem;
        border-radius: 0.6rem;
        margin-bottom: 1rem;
      }

      .spada-navbar-df-info-header h2{
        margin: 0;
        font-size: 1.25rem;
        font-weight: 600;
        color: white;
        overflow-wrap: anywhere;
        word-break: break-word;
      }
    "
    )
  )

  if (theme == 'spada_theme' || !(theme %in% themes_names)) {
    bs_theme(
      version = 5,
      bg = bg_color,
      fg = spada_black,
      primary = main_color,
      secondary = secondary,
      success = sucess,
      danger = danger,
      font_size_base = '1rem',
      'nav-pills-border-radius' = '0.2rem',
      'nav-pills-link-active-color' = main_color,
      'nav-pills-link-active-bg' = sidebar_color,
      'border-radius-sm' = '0.2rem',
      'border-radius' = '0.2rem',
      'navbar-brand-font-size' = '1.5rem',
      'navbar-brand-padding-y' = '0.250rem',
      'btn-font-weight' = 400,
      'dropdown-bg' = bg_color,
      'dropdown-color' = main_color,
      'dropdown-link-color' = spada_black,
      'dropdown-link-hover-bg' = sidebar_color,
      base_font = font_collection('Segoe UI', 'Ubuntu', 'system-ui')
    ) |>
      bs_add_rules(basic_rules) |>
      bs_add_rules(
        list(
          "
          .accordion-sidebar{
            background-color: $main_color !important;
            color: $spada_white;
          }

          .card {
            border-radius: 0.2rem;
            margin: -8px;
          }

          .big-card-footer{
            background-color: $main_color;
            margin-top: -12px !important;
            padding-bottom: 6px !important;
            height: 60px;
          }

          .btn-task,
          .btn-default {
            color: $secondary !important;
            background-color: $bg_color !important;
            border-color: $secondary !important;
            padding-top: 6px !important;
            padding-bottom: 6px !important;
            border-radius: 0.2rem
          }

          .btn-task-cancel {
            color: $spada_red5 !important;
            background-color: white !important;
            border-color: $spada_red5 !important;
            border-radius: 0.2rem
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
            background-color: $spada_navpills_hover !important;
          }

          .shiny-input-text,
          .shiny-input-number,
          .selectize-input,
          .shiny-input-textarea textarea {
            border: 1px solid $inputs_border_color2 !important;
            border-radius: 0.2rem !important;
            box-shadow: none !important;

            transition:
              border-color 0.2s ease,
              box-shadow 0.2s ease;
          }

          .selectize-input:hover,
          .shiny-input-text:hover,
          .shiny-input-number:hover,
          .shiny-input-textarea textarea:hover {
            border-color: $inputs_border_color !important;
            border-bottom: 1px solid $main_color !important;
          }

          .selectize-input:focus,
          .shiny-input-text:focus,
          .shiny-input-number:focus,
          .shiny-input-textarea textarea:focus {
            border-color: $inputs_border_color !important;
            border-bottom: 1px solid $main_color !important;
          }

          .navbar .dropdown-item.active,
          .navbar .dropdown-item:active {
            background: linear-gradient(
              135deg,
              $main_color 0%,
              $spada_blue5 100%
            ) !important;
            color: $spada_white !important;
          }

        "
        )
      )
  } else if (theme == 'spada_dark_theme') {
    bs_theme(
      version = 5,
      bootswatch = 'darkly',
      'border-radius-sm' = '0.2rem',
      'border-radius' = '0.2rem',
      'navbar-brand-font-size' = '1.5rem',
      'btn-font-weight' = 400,
      base_font = font_collection('Open Sans', 'Ubuntu', 'system-ui')
    ) |>
      bs_add_rules(basic_rules) |>
      bs_add_rules(
        list(
          "
          .card {
            border-radius: '0.2rem';
            margin: -4px;
          }

          .big-card-footer{
            margin-top: -12px !important;
            padding-bottom: 0px !important;
            height: 60px;
          }

          .btn-task,
          .btn-default {
            color: $secondary;
            background-color: $bg_color;
            border-color: $secondary;
            border-radius: '0.2rem'
          }

          .nav-pills .nav-link:hover {
            background-color: $spada_navpills_hover_dark !important;
            color: $spada_white !important;
          }

        "
        )
      )
  }
}
