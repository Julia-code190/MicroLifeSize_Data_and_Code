library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Load reference data from spreadsheet --------------------------------
data_path <- "Shinny app data.xlsx"
raw <- readxl::read_excel(data_path, sheet = 1)

ref <- raw %>%
  dplyr::rename(
    taxon    = `Length realistic for..`,
    L_um     = `L (µm)`,
    W_um     = `W (µm)`,
    d_um     = `d (µm)`,
    h_um     = `h (µm)`,
    shape    = `Shape*`,
    density  = `a) density (conversion factor) to estimate wetweight from volume **`,
    ww_to_dw = `b) conversion factor to estimate dry weight from fresh weight ***`,
    dw_to_c  = `c) conversion factor to estiamte carbon content from dry weight ****`,
    combined = `d) one-pass factor from volume in nL to microg C *****`
  ) %>%
  dplyr::filter(!is.na(shape)) %>%
  tidyr::fill(taxon, .direction = "down") %>%
  dplyr::mutate(shape = trimws(shape))

taxon_choices <- unique(ref$taxon)

num_or_na <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0) NA_real_ else v
}

# ---- Volume models (inputs in µm; volume in µm³ = fL) -------------------
calc_volume <- function(shape, L, W = NA, d = NA, h = NA) {
  shape <- tolower(trimws(shape))
  L <- num_or_na(L); W <- num_or_na(W); d <- num_or_na(d); h <- num_or_na(h)
  switch(shape,
    "sphere"             = (4/3) * pi * (L/2)^3,
    "pure carbon sphere" = (4/3) * pi * (L/2)^3,
    "capsule"            = (4/3) * pi * (W/2)^3 + pi * (W/2)^2 * L,
    "cylinder"           = (pi/4) * W^2 * L,
    "prolate spheroid"   = (pi/6) * W^2 * L,
    "ellipsoid"          = (pi/6) * L * W * d,
    "1/2 cone"           = (pi/24) * W^2 * L,
    NA_real_
  )
}

# Required dimension inputs per shape
shape_dims <- function(shape) {
  shape <- tolower(trimws(shape))
  switch(shape,
    "sphere"             = c("L"),
    "pure carbon sphere" = c("L"),
    "capsule"            = c("L", "W"),
    "cylinder"           = c("L", "W"),
    "prolate spheroid"   = c("L", "W"),
    "ellipsoid"          = c("L", "W", "d"),
    "1/2 cone"           = c("L", "W"),
    c("L")
  )
}

# Volume in µm³ -> mass in µg using density g/cm³
# 1 µm³ = 1e-12 cm³, so mass(g) = V_um3 * 1e-12 * density; mass(µg) = V * density * 1e-6
volume_to_masses <- function(V_um3, density, ww_to_dw, dw_to_c) {
  ww_ug <- V_um3 * density * 1e-6
  dw_ug <- ww_ug * ww_to_dw
  c_ug  <- dw_ug * dw_to_c
  list(wet = ww_ug, dry = dw_ug, carbon = c_ug)
}

convert_length <- function(value, unit) {
  switch(unit, "µm" = value, "nm" = value * 1e-3, "mm" = value * 1e3, value)
}

convert_output_unit <- function(value_ug, unit) {
  switch(unit,
    "g"  = value_ug * 1e-6,
    "mg" = value_ug * 1e-3,
    "µg" = value_ug,
    "ng" = value_ug * 1e3,
    "pg" = value_ug * 1e6,
    "fg" = value_ug * 1e9
  )
}

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Microbial Body Mass Estimator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("taxon", "Organism / size class:", choices = taxon_choices),
      selectInput("shape", "Body shape:", choices = NULL),
      selectInput("unit",  "Length unit:", choices = c("µm", "nm", "mm"), selected = "µm"),
      numericInput("L", "Length L:", value = 10, min = 0),
      numericInput("W", "Width W:",  value = 5,  min = 0),
      numericInput("d", "Depth d:",  value = 5,  min = 0),
      hr(),
      numericInput("density",  "Density (g/cm³):", value = 1.1),
      numericInput("ww_to_dw", "Wet→Dry ratio:",   value = 0.25),
      numericInput("dw_to_c",  "Dry→Carbon ratio:", value = 0.4),
      actionButton("apply_defaults", "Apply spreadsheet defaults"),
      actionButton("reset_inputs",   "Reset"),
      selectInput("output_unit", "Output mass unit:",
                  choices = c("g", "mg", "µg", "ng", "pg", "fg"), selected = "µg")
    ),
    mainPanel(
      h4("Results"),
      verbatimTextOutput("results"),
      h4("Mass breakdown"),
      plotOutput("massPlot", height = "260px"),
      h4("Reference rows from spreadsheet for this taxon"),
      tableOutput("refTable")
    )
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {

  current_rows <- reactive({
    ref %>% dplyr::filter(taxon == input$taxon)
  })

  # Update shape choices when taxon changes
  observeEvent(input$taxon, {
    shapes <- unique(current_rows()$shape)
    updateSelectInput(session, "shape", choices = shapes, selected = shapes[1])
  })

  # Apply defaults from the chosen (taxon, shape) row
  observeEvent(input$apply_defaults, {
    row <- current_rows() %>% dplyr::filter(shape == input$shape) %>% dplyr::slice(1)
    if (nrow(row) == 1) {
      updateNumericInput(session, "density",  value = row$density)
      updateNumericInput(session, "ww_to_dw", value = row$ww_to_dw)
      updateNumericInput(session, "dw_to_c",  value = row$dw_to_c)
      updateSelectInput(session,  "unit",     selected = "µm")
      updateNumericInput(session, "L", value = row$L_um)
      Wv <- num_or_na(row$W_um); if (!is.na(Wv)) updateNumericInput(session, "W", value = Wv)
      dv <- num_or_na(row$d_um); if (!is.na(dv)) updateNumericInput(session, "d", value = dv)
    }
  })

  observeEvent(input$reset_inputs, {
    updateSelectInput(session, "taxon", selected = taxon_choices[1])
    updateNumericInput(session, "L", value = 10)
    updateNumericInput(session, "W", value = 5)
    updateNumericInput(session, "d", value = 5)
    updateSelectInput(session, "unit", selected = "µm")
    updateNumericInput(session, "density",  value = 1.1)
    updateNumericInput(session, "ww_to_dw", value = 0.25)
    updateNumericInput(session, "dw_to_c",  value = 0.4)
    updateSelectInput(session, "output_unit", selected = "µg")
  })

  masses <- reactive({
    req(input$shape)
    L <- convert_length(input$L, input$unit)
    W <- convert_length(input$W, input$unit)
    d <- convert_length(input$d, input$unit)
    V <- calc_volume(input$shape, L, W, d)
    list(V = V, m = volume_to_masses(V, input$density, input$ww_to_dw, input$dw_to_c))
  })

  output$results <- renderPrint({
    res <- masses(); u <- input$output_unit
    needed <- shape_dims(input$shape)
    cat(sprintf("Shape: %s  (uses: %s)\n", input$shape, paste(needed, collapse = ", ")))
    cat(sprintf("Estimated volume: %.4g µm³ (= %.4g nL)\n", res$V, res$V * 1e-6))
    cat(sprintf("Wet mass:    %.5g %s\n", convert_output_unit(res$m$wet,    u), u))
    cat(sprintf("Dry mass:    %.5g %s\n", convert_output_unit(res$m$dry,    u), u))
    cat(sprintf("Carbon mass: %.5g %s\n", convert_output_unit(res$m$carbon, u), u))
  })

  output$refTable <- renderTable({
    current_rows() %>%
      dplyr::select(shape, L_um, W_um, d_um, h_um,
                    density, ww_to_dw, dw_to_c, combined)
  }, na = "na")

  output$massPlot <- renderPlot({
    res <- masses(); u <- input$output_unit
    df <- data.frame(
      type = factor(c("Wet mass", "Dry mass", "Carbon mass"),
                    levels = c("Carbon mass", "Dry mass", "Wet mass")),
      value = c(convert_output_unit(res$m$wet,    u),
                convert_output_unit(res$m$dry,    u),
                convert_output_unit(res$m$carbon, u))
    )
    req(all(is.finite(df$value)))
    ggplot(df, aes(x = value, y = type, fill = type)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = signif(value, 3)), hjust = -0.15, size = 4) +
      scale_fill_manual(values = c("Wet mass"    = "#4C8FB0",
                                   "Dry mass"    = "#8FB04C",
                                   "Carbon mass" = "#333333")) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = sprintf("Mass (%s)", u), y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank())
  })
}

shinyApp(ui = ui, server = server)
