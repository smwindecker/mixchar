library(shiny)
library(bslib)
library(DT)

app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Barlow"),
  heading_font = font_google("Space Grotesk"),
  primary = "#0f766e",
  secondary = "#f97316",
  bg = "#f4f6fb",
  fg = "#0f172a"
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(tags$style(
    "
    .sidebarPanel { background: linear-gradient(180deg,#0f172a 0%,#0f766e 100%); color:#f8fafc; }
    .main-panel { background:#f8fafc; border-radius:12px; padding:18px; }
    .shiny-input-container { margin-bottom:12px; }
    "
  )),
  titlePanel("mixchar Shiny workbench"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Workflow steps"),
      radioButtons(
        "step", label = NULL,
        choices = c(
          "1. Load data" = "read",
          "2. Process data" = "process",
          "3. Processed plots" = "plots",
          "4. Pyrolysis phase deconvolution" = "decon",
          "5. Pyrolysis phase deconvolution plots" = "viz",
          "6. Carbon fractions" = "fractions"
        ),
        selected = "read"
      ),
      hr(),
      conditionalPanel(
        "input.step == 'read'",
        fileInput("datafile", "Upload CSV", accept = c(".csv")),
        numericInput("skip_rows", "Rows to skip (metadata)", value = NA, min = 0, step = 1),
        actionButton("use_example", "Use example data", class = "btn-secondary")
      ),
      conditionalPanel(
        "input.step == 'process'",
        helpText("Example data fills defaults; provide values for your own data."),
        selectInput("temp_col", "Temperature column", choices = c("Select column" = "")),
        selectInput("mass_col", "Mass loss column", choices = c("Select column" = "")),
        selectInput("time_col", "Time column", choices = c("Select column" = "")),
        numericInput("init_mass", "Initial mass (mg)", value = NA, min = 0, step = 0.01),
        numericInput("pyro_start", "Pyrolysis start time", value = NA, step = 0.5),
        numericInput("pyro_end", "Pyrolysis end time", value = NA, step = 0.5),
        actionButton("do_process", "Run process()", class = "btn-primary")
      ),
      conditionalPanel(
        "input.step == 'plots'",
        checkboxInput("colour_segments", "Black/white only (hide stage annotations)", value = FALSE),
        NULL
      ),
      conditionalPanel(
        "input.step == 'decon'",
        selectInput("n_peaks", "Number of peaks", choices = c("Auto" = "auto", "3 peaks" = "3", "4 peaks" = "4")),
        numericInput("decon_seed", "Seed", value = 1, min = 1, step = 1),
        actionButton("run_decon", "Run deconvolve()", class = "btn-primary")
      ),
      conditionalPanel(
        "input.step == 'viz'",
        checkboxInput("decon_bw", "Black/white plot", value = TRUE),
        checkboxInput("show_band", "Show quantile bands (3-peak only)", value = FALSE),
        textInput("band_probs", "Quantile probs", value = "0.025, 0.5, 0.975"),
        numericInput("band_draws", "Draws for ribbons", value = 500, min = 50, step = 50),
        numericInput("band_seed", "Ribbon seed", value = 42, min = 1, step = 1)
      ),
      conditionalPanel(
        "input.step == 'fractions'",
        numericInput(
          "fc_hemi",
          "Hemicellulose fixed carbon (% dry basis)",
          value = 13.8, min = 0, step = 0.1
        ),
        numericInput(
          "fc_cell",
          "Cellulose fixed carbon (% dry basis)",
          value = 6.4, min = 0, step = 0.1
        ),
        numericInput(
          "fc_lig",
          "Lignin fixed carbon (% dry basis)",
          value = 34, min = 0, step = 0.1
        )
      )
    ),
    mainPanel(
      width = 9,
      class = "main-panel",
      uiOutput("step_body")
    )
  )
)
