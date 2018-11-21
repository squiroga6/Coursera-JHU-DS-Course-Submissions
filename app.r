# load libraries ----
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(plotly)
library(htmltools)

# Get data -----------------

# my.url <- "http://data.ec.gc.ca/data/substances/monitor/greenhouse-gas-reporting-program-ghgrp-facility-greenhouse-gas-ghg-data/PDGES-GHGRP-GHGEmissionsGES-2004-Present.csv"

# ghg <- read.csv(my.url)

# saveRDS(ghg,file="./ghgData.rds")

ghg <- tbl_df(read_rds(path = "./ghgData.rds"))

# Clean Data -----------------------

# get names
names <- colnames(ghg)

# clean column names (remove French version)
my.names <- if_else(is.na(str_extract(names,".*(?=\\.\\.\\.)")),names,str_extract(names,".*(?=\\.\\.\\.)"))
colnames(ghg) <- my.names

# Organize emissions data -----------------
emissions.all <- ghg %>% select(
                         Facility.Name,
                         English.Facility.NAICS.Code.Description, 
                         Facility.Province.or.Territory,
                         Reporting.Company.Legal.Name,
                         Reference.Year,
                         Total.Emissions..tonnes.CO2e.,
                         # Public.Contact.Name,
                         # Public.Contact.Email,
                         Latitude,Longitude) %>%
        filter(complete.cases(Latitude)) %>%
        mutate(Total.Emissions..tonnes.CO2e. = 
                       round(if_else(Total.Emissions..tonnes.CO2e. == 0,1,Total.Emissions..tonnes.CO2e.),1))

# Group by year and sum emissions -----
emissions.all.sumYrs <- emissions.all %>% 
        group_by_at(vars(-Reference.Year,-Total.Emissions..tonnes.CO2e.)) %>% 
        summarize(Sum.of.Total.Emissions = sum(Total.Emissions..tonnes.CO2e.))

# get provinces
provinces <- as.character(unique(emissions.all$Facility.Province.or.Territory))

# Get emissions by year for each province ----
emissions.Prov.Yr <- ghg %>% select(Facility.Province.or.Territory,Reference.Year,Total.Emissions..tonnes.CO2e.) %>% 
        mutate(Reference.Year = as.Date(Reference.Year,format = "%Y")) %>% filter(complete.cases(.))

# set varia les text stile for tabPanel
css.type <- "text/css"
css.position <- ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"

# Define UI ----
ui <- fluidPage(

        navbarPage("Canada's Greenhouse Gas Emissions by Facility",
                   
                   tabPanel("Interactive Map", 
                            div(class="outer",
                            
                            tags$style(type = css.type,
                                       css.position),

                            leafletOutput("map", width="100%", height="100%"),
                            
                            absolutePanel(top = 60, right = 20, width = 300,
                                          draggable = TRUE,height = "auto",style = "opacity: 0.90",
                                          
                                          h3("Canada's Greenhouse Gas Emissions"),
                                          
                                          uiOutput("province"),
                                          
                                          plotOutput("ghgPlot",height = 200),
                                          
                                          plotOutput("ghgPlot2",height = 200)
                                          )
                            
                            )),
                   
                   tabPanel("Data Explorer",
                            DT::dataTableOutput("ghgTbl")
                            ),
                   tabPanel("How It Works",
                            h3("Greenhouse Gas Reporting Program Dashboard"),
                            br(),
                            h4("How to Use the Dashboard"),
                            p("The interative map displays the locations of facilities with reported green-house gas emissions. The circles are colored by total emissions in tons of CO2 Eq. The pop up and labels of each circle indicate the type and name of the facility ."),
                            br(),
                            p("The ineractive map allows the user to choose a province from the dropdown menu. The map will then zoom in to the chosen province. The graphs below indicate the total ghg emissions per year for both the zoomed area and the chosen province"),
                            br(),
                            h4("How the Data Explorer Works"),
                            p("The data explorer is a table of the GHG emissions data that contains more details, and it gets updated as the map is moved or zoomed into a specific circle "),
                            br(),
                            h4("Data Source information"),
                            p("This dashboard uses Facility Greenhouse Gas (GHG) Data from the governmet of Canada's Greenhouse Gas Reporting Program (GHGRP). The Greenhouse Gas Reporting Program (GHGRP) collects information on greenhouse gas (GHG) emissions annually from facilities across Canada. It is a mandatory program for those who meet the requirements."),
                            br(),
                            p("Facilities that emit 50 kilotonnes or more of GHGs, in carbon dioxide (CO2) equivalent (eq.) units, per year must report their emissions to Environment and Climate Change Canada."),
                            br(),
                            p("Emissions data are available by gas (in tonnes and tonnes of CO2 eq.) for each facility and each year of data collected (2004-Present). The GHGs included are: carbon dioxide (CO2), methane (CH4), nitrous oxide (N2O), hydrofluorocarbons (HFC), perfluorocarbons (PFC) and sulphur hexafluoride (SF6)."),
                            br(),
                            p("Note: Data expressed in CO2 eq. units use the most recently revised global warming potential (GWP) values used internationally for GHG reporting."),
                            br(),
                            p("Fore more information visit the GHGRP Webpage", a("here.",href="https://open.canada.ca/data/en/dataset/a8ba14b7-7f23-462a-bdbb-83b0ef629823") )
           
                            )
                   
                   )
        
)

# Define server logic ----
server <- function(input, output) {
        
        # ui object to select Province
        output$province <- renderUI({
                
                provinces <- c('All',provinces)
                
                selectInput("province",label = "Select Province",choices = provinces, selected = 'All')
        })
        
        # filter data within map bounds to make plot 1
        my.data.plot <- reactive({
                
                in_bounding_box <- function(data, Latitude, Longitude, bounds) {
                        data %>% filter(Latitude > bounds$south & 
                                                Latitude < bounds$north & 
                                                Longitude < bounds$east & 
                                                Longitude > bounds$west)
                }
                
                if (is.null(input$map_bounds)){
                        emissions.all
                }else {
                        bounds <- input$map_bounds
                        in_bounding_box(emissions.all, Latitude, Longitude, bounds)
                }

                
        })
        
        
        # make box plot from filtered data based on map bounds
        output$ghgPlot <- renderPlot({
                
                datatoPlot <- my.data.plot()
                
                ggplot(data=datatoPlot,aes(x=Reference.Year,y=log10(Total.Emissions..tonnes.CO2e.))) + 
                        geom_boxplot(color="blue",alpha=0.5) +
                        scale_y_continuous(labels = scales::math_format(10^.x)) +
                        labs(y="Total Emissions (tons of CO2eq)",title="Total Emissions for zoomed area") +
                        theme_bw() +
                        theme(axis.title = element_text(size=12),
                              panel.grid.minor = element_blank(),
                              axis.text = element_text(size=10))
        })  
        
        
        # fitler data based on Province selected
        my.data.plot2 <- reactive({
                if(input$province == 'All'){
                        my.data <- emissions.Prov.Yr %>%
                                group_by(Reference.Year) %>%
                                summarize(Sum.per.Province = sum(sum(Total.Emissions..tonnes.CO2e.)))
                } else{
                        my.data <- emissions.Prov.Yr %>%
                                group_by(Facility.Province.or.Territory,Reference.Year) %>%
                                summarize(Sum.per.Province = sum(Total.Emissions..tonnes.CO2e.)) %>%
                                filter(Facility.Province.or.Territory == input$province)
                }
                my.data
        })
        
        # make scatter plot based on data filtered from Province selected
        output$ghgPlot2 <- renderPlot({
                
                datatoPlot <- my.data.plot2()
                
                ggplot(data=datatoPlot,aes(x=Reference.Year,y=Sum.per.Province)) + 
                        geom_point(size=5,color="blue",alpha=0.5) + 
                        labs(y="Total Emissions (tons of CO2eq)",title="Total Emissions for selected Province") +
                        theme(axis.title = element_text(size=12),
                              axis.text = element_text(size=10))
        })
        
        # Create the map
        output$map <- renderLeaflet({ emissions.all %>% leaflet() %>% addTiles() %>%
                        fitBounds(~min(Longitude),~min(Latitude), ~max(Longitude),~max(Latitude))
        })
        
        # Change the map with input
        observeEvent(input$province,{

                PROVINCE <- input$province
                
                if(PROVINCE != 'All'){
                        my.data <- emissions.all %>% filter(Facility.Province.or.Territory == PROVINCE)
                }else{
                        my.data <- emissions.all
                }
                
                # create color bar function
                pal <- colorNumeric("YlOrRd",domain = log10(my.data$Total.Emissions..tonnes.CO2e.))

                leafletProxy("map",data=my.data) %>%
                        clearControls() %>%
                        clearMarkers() %>%
                        clearPopups() %>%
                        addCircleMarkers(lng = ~Longitude,lat = ~Latitude,
                                         weight = 1,
                                         color = ~pal(log10(my.data$Total.Emissions..tonnes.CO2e.)),
                                         popup = ~Facility.Name,
                                         label = ~English.Facility.NAICS.Code.Description) %>%
                        addLegend("bottomleft",
                                  pal = pal,
                                  values = ~log10(my.data$Total.Emissions..tonnes.CO2e.),
                                  title = "Total Emissions<br>(log10 of Tons CO2 eq)",
                                  opacity = 0.8) %>%
                        setView(lat = median(my.data$Latitude),lng = median(my.data$Longitude),zoom = 4.5) 
                
                
        })
        
        # function to filter data based on map bounds
        in_bounding_box <- function(data, Latitude, Longitude, bounds) {
                data %>% filter(Latitude > bounds$south & 
                                        Latitude < bounds$north & 
                                        Longitude < bounds$east & 
                                        Longitude > bounds$west)
        }
        
        # filtered data that reacts to map bounds
        data_map <- reactive({
                if (is.null(input$map_bounds)){
                        emissions.all
                }else {
                        bounds <- input$map_bounds
                        in_bounding_box(emissions.all, Latitude, Longitude, bounds)
                }
        })
        
        output$ghgTbl <- DT::renderDataTable({
                DT::datatable(data_map(),options = list(scrollX = T,scrollY = T))
        })
}

# Run the app ----
shinyApp(ui = ui, server = server)