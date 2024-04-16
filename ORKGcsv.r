# This script and shiny app will allow for limited data entry
# for ORKG with standardized text responses. It spits out a
# spreadsheet appropriately formatted for inclusion in ORKG
# There should be five fields:
# location - WGS84 coordinates only
# ecosystem type - IUCN typology - biome_iucn.csv
# method - method.csv
# degradation type - degradation.csv
# focal species - GBIF species list? maybe taxize package
# Workaround for multiple values: use a semicolon (;) to separate entries. 
#  - Use Excel to split the column based on that.


library(shiny)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(mapview)
library(raster)
library(rcrossref) #for DOI validation

#This loads all the rasters used for the ecosystem type detection
setwd("C:/Users/Tim Alamenciak/Documents/TReK/IUCN")
#REMOVED M1.10 & T1.3.tif (different extent - grr - throws error)
#Must communicate this to IUCN.
DEM <- stack("F1.1.tif", "F1.2.tif", "F1.3.tif", "F1.4.tif", "F1.5.tif",
             "F1.6.tif", "F1.7.tif", "F2.1.tif", "F2.2.tif", "F2.3.tif", 
             "F2.4.tif", "F2.5.tif", "F2.6.tif", "F2.7.tif", "F2.8.tif", 
             "F2.9.tif", "F2.10.tif", "F3.1.tif", "F3.2.tif", "F3.3.tif",
             "F3.4.tif", "F3.5.tif", "FM1.1.tif", "FM1.2.tif", "FM1.3.tif",
             "M1.1.tif", "M1.2.tif", "M1.3.tif", "M1.4.tif", "M1.5.tif",
             "M1.6.tif", "M1.7.tif", "M1.8.tif", "M1.9.tif",
             "M2.1.tif", "M2.2.tif", "M2.3.tif", "M2.4.tif", "M2.5.tif",
             "M3.1.tif", "M3.2.tif", "M3.3.tif", "M3.4.tif", "M3.5.tif",
             "M3.6.tif", "M3.7.tif", "M4.1.tif", "M4.2.tif", "MFT1.1.tif",
             "MFT1.2.tif","MFT1.3.tif","MT1.1.tif","MT1.2.tif","MT1.3.tif",
             "MT1.4.tif","MT2.1.tif","MT2.2.tif","MT3.1.tif","S1.1.tif",
             "S2.1.tif","SF1.1.tif","SF1.2.tif","SF2.1.tif","SF2.2.tif",
             "SM1.1.tif", "SM1.2.tif","SM1.3.tif","T1.1.tif","T1.2.tif",
             "T1.4.tif","T2.1.tif","T2.2.tif","T2.3.tif",
             "T2.4.tif","T2.5.tif","T2.6.tif","T3.1.tif","T3.2.tif",
             "T3.3.tif","T3.4.tif","T4.1.tif","T4.2.tif","T4.3.tif",
             "T4.4.tif","T4.5.tif","T5.1.tif","T5.2.tif","T5.3.tif",
             "T5.4.tif","T5.5.tif","T6.1.tif","T6.2.tif","T6.3.tif",
             "T6.4.tif","T6.5.tif","T7.1.tif","T7.2.tif","T7.3.tif",
             "T7.4.tif","T7.5.tif","TF1.1.tif","TF1.2.tif","TF1.3.tif",
             "TF1.4.tif","TF1.5.tif","TF1.6.tif","TF1.7.tif")


setwd("C:/Users/Tim Alamenciak/Documents/TReK")

#These load the pre-populated CSVs for limiting the inputs.
method = read.csv("method.csv")
degradation = read.csv("degradation.csv")
iucn = read.csv("biome_iucn.csv")
datf = data.frame()

#This sets up the dataframe that will contain the outputs
outframe = data.frame(DOI=character(),
                      coords=character(),
                      site_city=character(),
                      site_province=character(),
                      site_country=character(),
                      iucn_realm=character(),
                      iucn_biome=character(),
                      iucn_efg=character(),
                      method=character(),
                      degradation=character(),
                      species=character())

#This function adds a row to the outframe.
addline <- function(DOI, coords, site_city, site_province, site_country, iucn_realm, iucn_biome, iucn_efg,
                    method, degradation, species){
  outframe[nrow(outframe) + 1,] <<- c(DOI, coords, site_city, site_province, site_country, iucn_realm, iucn_biome, iucn_efg,
                                     method, degradation, species)
}

#This is the UI for the shiny app
ui <- fluidPage(
  titlePanel("ORKG CSV generator"),
  textInput("doi", "What's the DOI?"),
  fluidRow(
    column(4,
           sidebarLayout(
             sidebarPanel(width = 12, id="location", 
                          h5("Location details"),
                            textInput("site_coords", label = "Coordinates (WGS84)"),
                            actionButton("lookup", "🔍"),
                            textInput("site_city", label = "Research site city"),
                            textInput("site_province", label = "Research site province"),
                            textInput("site_country", label = "Research site country")
             ),
             mainPanel(width = 0)
           )
    ),
    column(4, sidebarLayout(
      sidebarPanel(width = 12, id="ecosystem",
                   h5("Ecosystem type"),
                    selectInput("realm", label = "Realm", choices = unique(iucn$realm), selected = NULL),
                    selectInput("biome", label = "Biome", choices = iucn$biome),
                    selectInput("efg", label = "Ecosystem Functional Group", choices = iucn$efg)),
      mainPanel( width = 0)
    )),
    column(4, sidebarLayout(
      sidebarPanel(width = 12, id="info",
                   h5("Article title:"),
                   textOutput("title"),
                  h5("Suggested ecosystem:"),
                  textOutput("realm")),
      mainPanel( width = 0)
    ))
  ),
  
  textInput("species", label = "Main species (scientific name)"),
  selectInput("method", label = "Method", choices = method$name),
  selectInput("degradation", label = "Degradation type", choices = degradation$name),
  actionButton("save", "Save entry"),
  actionButton("write", "Write to CSV"),
  
    
  tableOutput("table")
)

server <- function(input, output, session) {

  realm <- reactive({
    filter(iucn, realm == input$realm)
  })
  observeEvent(realm(), {
    choices <- unique(realm()$biome)
    updateSelectInput(inputId = "biome", choices = choices)
  })
  
  biome <- reactive({
    req(input$biome)
    filter(realm(), biome == input$biome)
  })
  observeEvent(biome(), { 
    choices <- unique(biome()$efg)
    updateSelectInput(inputId = "efg", choices = choices)
  })
  
  doi <- reactive({
    req(input$doi)
  })
  
  output$title <- renderText({
    article <- cr_works(dois = input$doi)
    paste(article$data$title)
  }) 
  
  
  observeEvent(input$lookup, {
    req(input$site_coords)
    
    #have to separate the coords by the comma before doing geo lookup etc.
    datf <- tidyr::separate(as.data.frame(input$site_coords), 1, into = c("lat", "long"), sep = ",")
    datf$long <- gsub("c\\(", "", datf$long)
    datf$lat <- gsub("\\)", "", datf$lat)
    rev_geo <- reverse_geocode(datf, lat = lat, long = long, method="osm", address = address_found, full_results = TRUE)
    
    # for now this pulls rev_geo$name which I think includes either the town or city.
    # Keep an eye on this though!
    updateTextInput(inputId = "site_city", value = rev_geo$name)
    updateTextInput(inputId = "site_province", value = rev_geo$state)
    updateTextInput(inputId = "site_country", value = rev_geo$country)
    
    #Now we pull ecosystem predictions:
    datf$lat <- as.numeric(datf$lat)
    datf$long <- as.numeric(datf$long)
    coordinates(datf) <- c("long", "lat")

    rasValue = raster::extract(DEM, datf, buffer=1000)
    rf <- as.data.frame(rasValue)
    rf <- rf %>% 
      pivot_longer(1:107, names_to = "biome", values_to = "score", values_drop_na = TRUE) %>%
      group_by(biome)
    
    rf <- aggregate(rf$score, by=list(biome = rf$biome),FUN=sum)
    #ok so this generates some suggested ones - need to make these clickable, and have them change the inputs.
    #Kind of a big deal!

  })
  
  observeEvent(input$save, {
    addline(input$doi, 
            input$site_coords, 
            input$site_city,
            input$site_province,
            input$site_country,
            input$realm, 
            input$biome, 
            input$efg, 
            input$method, 
            input$degradation,
            input$species)
    
    output$table <- renderTable(outframe, width="100%")
    
  })
  
  observeEvent(input$write, {
    write.csv(outframe, "ORKG.csv", col.names = c("paper:doi", "site coordinates", "located in the administrative territorial entity",
                                                  "located in the administrative territorial entity", "located in the administrative territorial entity", "IUCN realm",
                                                  "IUCN biome", "IUCN EFG", "research intervention", 
                                                  "degradation type", "main species"))
  })
}


shinyApp(ui, server)
