library(shiny)
library(fmsb)

# Define basic allometric functions
calculate_volume <- function(length, width, shape) {
  if (shape == "sphere") {
    return((4/3) * pi * (length/2)^3)
  } else if (shape == "cylinder") {
    return(pi * (width/2)^2 * length)
  } else if (shape == "ellipsoid") {
    return((4/3) * pi * (length/2) * (width/2)^2)
  } else {
    return(NA)
  }
}

convert_mass <- function(volume_um3, density = 1.1, ww_to_dw = 0.25, dw_to_c = 0.4) {
  volume_mm3 <- volume_um3 * 1e-9
  wet_mass_mg <- volume_mm3 * density
  dry_mass_mg <- wet_mass_mg * ww_to_dw
  carbon_mg <- dry_mass_mg * dw_to_c
  return(list(
    wet_mass_mg = wet_mass_mg,
    dry_mass_mg = dry_mass_mg,
    carbon_mg = carbon_mg
  ))
}

get_taxon_defaults <- function(taxon) {
  switch(taxon,
         "Bacteria" = list(shape = "cylinder", density = 1.1, ww_to_dw = 0.4, dw_to_c = 0.5),
         "Protist" = list(shape = "ellipsoid", density = 1.05, ww_to_dw = 0.2, dw_to_c = 0.45),
         "Rotifer" = list(shape = "ellipsoid", density = 1.1, ww_to_dw = 0.25, dw_to_c = 0.4),
         "Ciliate" = list(shape = "ellipsoid", density = 1.08, ww_to_dw = 0.25, dw_to_c = 0.45),
         list(shape = "ellipsoid", density = 1.1, ww_to_dw = 0.25, dw_to_c = 0.4))
}

get_mass_range <- function(taxon) {
  switch(taxon,
         "Bacteria" = "Typical range: 5–100 fg C (0.000005–0.0001 µg C)",
         "Protist" = "Typical range: 0.01–10 µg C",
         "Rotifer" = "Typical range: 0.1–100 µg C",
         "Ciliate" = "Typical range: 0.1–50 µg C",
         "General" = "Reference: Bacteria: fg–ng, Protists: pg–µg, Microfauna: µg–mg")
}

convert_units <- function(value, unit) {
  if (unit == "nm") return(value * 0.001)
  if (unit == "mm") return(value * 1000)
  return(value)
}

convert_output_unit <- function(value_mg, unit) {
  switch(unit,
         "mg" = value_mg,
         "µg" = value_mg * 1000,
         "ng" = value_mg * 1e6,
         "pg" = value_mg * 1e9,
         "fg" = value_mg * 1e12)
}

# Define UI
ui <- fluidPage(
  titlePanel("Microbial Body Mass Estimator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("taxon", "Select Taxon:", choices = c("General", "Bacteria", "Protist", "Rotifer", "Ciliate")),
      numericInput("length", "Length:", value = 10, min = 0),
      numericInput("width", "Width:", value = 5, min = 0),
      selectInput("unit", "Unit:", choices = c("µm", "nm", "mm")),
      selectInput("shape", "Body Shape:", choices = c("sphere", "cylinder", "ellipsoid")),
      numericInput("density", "Density (g/cm³):", value = 1.1),
      numericInput("ww_to_dw", "Wet-to-Dry Mass Ratio:", value = 0.25),
      numericInput("dw_to_c", "Dry-to-Carbon Mass Ratio:", value = 0.4),
      actionButton("apply_taxon", "Apply Taxon Defaults"),
      actionButton("reset_inputs", "Reset to Default Values"),
      selectInput("output_unit", "Output Unit:", choices = c("µg", "ng", "pg", "fg", "mg"), selected = "µg")
    ),
    mainPanel(
      h4("Results:"),
      verbatimTextOutput("results"),
      plotOutput("radarPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$apply_taxon, {
    if (input$taxon != "General") {
      defaults <- get_taxon_defaults(input$taxon)
      updateSelectInput(session, "shape", selected = defaults$shape)
      updateNumericInput(session, "density", value = defaults$density)
      updateNumericInput(session, "ww_to_dw", value = defaults$ww_to_dw)
      updateNumericInput(session, "dw_to_c", value = defaults$dw_to_c)
    }
  })
  
  observeEvent(input$reset_inputs, {
    updateSelectInput(session, "taxon", selected = "General")
    updateNumericInput(session, "length", value = 10)
    updateNumericInput(session, "width", value = 5)
    updateSelectInput(session, "unit", selected = "µm")
    updateSelectInput(session, "shape", selected = "ellipsoid")
    updateNumericInput(session, "density", value = 1.1)
    updateNumericInput(session, "ww_to_dw", value = 0.25)
    updateNumericInput(session, "dw_to_c", value = 0.4)
    updateSelectInput(session, "output_unit", selected = "µg")
  })
  
  output$results <- renderPrint({
    length_um <- convert_units(input$length, input$unit)
    width_um <- convert_units(input$width, input$unit)
    volume <- calculate_volume(length_um, width_um, input$shape)
    mass <- convert_mass(volume, input$density, input$ww_to_dw, input$dw_to_c)
    
    unit <- input$output_unit
    cat(sprintf("Estimated Volume: %.2f µm³\n", volume))
    cat(sprintf("Wet Mass: %.5f %s\n", convert_output_unit(mass$wet_mass_mg, unit), unit))
    cat(sprintf("Dry Mass: %.5f %s\n", convert_output_unit(mass$dry_mass_mg, unit), unit))
    cat(sprintf("Carbon Mass: %.5f %s\n", convert_output_unit(mass$carbon_mg, unit), unit))
    cat("\n\nGlobal ballpark range for selected group:\n")
    cat(get_mass_range(input$taxon))
  })
  
  output$radarPlot <- renderPlot({
    length_um <- convert_units(input$length, input$unit)
    width_um <- convert_units(input$width, input$unit)
    volume <- calculate_volume(length_um, width_um, input$shape)
    mass <- convert_mass(volume, input$density, input$ww_to_dw, input$dw_to_c)
    
    unit <- input$output_unit
    wet <- convert_output_unit(mass$wet_mass_mg, unit)
    dry <- convert_output_unit(mass$dry_mass_mg, unit)
    carbon <- convert_output_unit(mass$carbon_mg, unit)
    
    max_val <- max(wet, dry, carbon, 1)
    data <- as.data.frame(rbind(
      rep(max_val, 3),
      rep(0, 3),
      c(wet, dry, carbon)
    ))
    colnames(data) <- c("Wet Mass", "Dry Mass", "Carbon Mass")
    radarchart(data,
               axistype = 1,
               pcol = rgb(0.2, 0.5, 0.5, 0.9),
               pfcol = rgb(0.2, 0.5, 0.5, 0.4),
               plwd = 2,
               cglcol = "grey", cglty = 1, axislabcol = "grey",
               caxislabels = round(seq(0, max_val, length.out = 5), 2))
  })
}

# Run the app
shinyApp(ui = ui, server = server)



#https://www.shinyapps.io/admin/#/dashboard
# Host online 
install.packages('rsconnect')

# rsconnect::setAccountInfo(name='perkinslab',
#                           token='CC790F8EB180F11960D0CE1FBFEA46F9',
#                           secret='YABwuDPRaoHkjURW8VzSpC4Ez1DvihuisG9Z9EQG')
# 


library(rsconnect)
rsconnect::deployApp('/Users/dansnewmac/Library/CloudStorage/OneDrive-BrunelUniversityLondon/Research/Projects/Julia body mass paper/mass_estimator_app')


