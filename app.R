library(shiny)
library(png)
library(shinydashboard)
library(shinydashboardPlus)
library(sf)
library(shinyWidgets)
library(leaflet)
library(magrittr)
library(shinyjs)
library(rgdal)
library(tidyverse)
xmas <- read_csv("xmascards_jit.csv")

# case_when to do multiple conditions (same as SQL `CASE WHEN`)
xmas<- xmas %>% 
    mutate(Region = case_when(
        `State` == "HI" ~ "Hawaii",
        (`State` != "HI" & `Country` == 'US') ~ "Continental",
        `City`== "LONDON" ~ "London"))
xmas$Region <- as.character(xmas$Region)

xmas <- xmas %>% 
    unite("card_comb", card_1:card_8, na.rm = TRUE, sep = ", ", remove = FALSE)

# Very important step. You need to generate it to a shp file so that it can be accurate
xmassf <- st_as_sf(x=xmas, coords= c("lon","lat"))

# creates seperate data set for each region
for(y in unique(xmas$Region)) {
    subsample <- filter(xmassf, Region == y)
    assign(paste("xmassf",y, sep = ""), subsample)
}
# Pop-up labels

# custom title
title <- tags$a(href='https://www.linkedin.com/in/alvin-jacob-soriano-300124133/',
                icon("gem"), "Yu-Gi-Oh")

# custom pop-up icon
ygh_back <- makeIcon(
    iconUrl = "https://i.imgur.com/CsJ1dDB.png",
    iconWidth = 17.1, iconHeight= 24.3,
)
ygh_back2 <- makeIcon(
    iconUrl = "YGH_Back.png",
    iconWidth = 17.1, iconHeight= 24.3)

# m_County <- c("Select All", as.character(sort(unique(iemap$COUNTYNAME))))
ygh_all <- c("Select All", as.character(sort(unique(xmas$Region))))

ygh_regions <-as.character(sort(unique(xmas$Region)))
# 
# h <- leaflet() %>% 
#     addProviderTiles(providers$OpenStreetMap) %>% 
#     setView(lng = -157.80920645819606,lat=20.728373666973095 , zoom = 7) %>%
#     addMarkers(lng = xmas$lon, lat = xmas$lat, icon = ygh_back,
#                label = lapply(cards_rec, htmltools::HTML),
#                group = "myMarkers")

# another <- leaflet() %>%
#     addProviderTiles(providers$OpenStreetMap) %>%
#     setView(lng = -100.4,lat= 40,1 , zoom = 4) %>%
#     addMarkers(data = xmas, lng = xmas$lon, lat = xmas$lat, icon = ygh_back,
#                label = lapply(cards_rec, htmltools::HTML),
#                group = "myMarkers")

# see what happened? rather than sorting it all, just list down all unique regions
ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = title),
                    dashboardSidebar(width = 275,
                                     uiOutput("userpanel"),
                                     
                                     # Side Bar Menu
                                     
                                     sidebarMenu(style = "position: Scroll; overflow: visible;",
                                                 id = "sidebarmenu",
                                                 
                                                 menuItem("Yu-Gi-Where", tabName = "ygw", icon = icon("mail-bulk")),
                                                 
                                                 selectInput(inputId = "i_region",
                                                             label = "Region",
                                                             choices = ygh_regions)
                                                 ## <hr/> horizontal line.
                                                 ## Shiny HTML Glossary https://shiny.rstudio.com/articles/tag-glossary.html
                                     )
                    ),
                    # Body
                    dashboardBody(
                        tabItems( # check if I need to do this since I only have one tab
                            tabItem(tabName = "ygw",
                                    fluidRow(column(10, offset = 0.5, h1("YGO DASHBOARD"))),
                                    fluidRow(style="height:50px;",
                                             valueBoxOutput("count1",width = 3),
                                             valueBoxOutput("count2",width = 3),
                                             valueBoxOutput("count3",width = 3),
                                             valueBoxOutput("count4",width = 3)),
                                    br(),
                                    fluidRow(column(10, offset = 2.5, leafletOutput("map",width = 1100, height = 600))
                                    )
                            )   
                        )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    addClass(selector = "body", class = "sidebar-collapse")
    
    # Value Box 1
    
    output$count1 <- renderValueBox({
        xmas1 <- xmas %>%
            summarise(totalCards = sum(card_count))

        valueBox(paste0(xmas1),
                 "TOTAL CARDS SENT",
                 icon = icon("envelope-open-text"), # find icons at fontawesome.com
                 color = "blue")
    })
    
    # Value Box 2
    
    output$count2 <- renderValueBox({
        xmas2 <- xmas %>% 
            tally()
        valueBox(paste0(xmas2),
                 "PEOPLE RECEIVED",
                 icon = icon("user-friends"),
                 color = "blue")
    })
    
    # Value Box 3
    
    output$count3 <- renderValueBox({
        xmas_cards <- xmas %>% 
            select(starts_with("card")) %>% 
            select(-card_count) %>% 
            pivot_longer(starts_with("card"), names_to = "card_num",
                         values_to = "cards") %>% 
            drop_na()
        xmas3 <- xmas_cards %>%
            select(cards) %>% 
            unique() %>%
            tally()
        valueBox(paste0(xmas3),
                 "CARD TYPES",
                 icon = icon("square"),
                 color = "blue")
    })
    
    # Value Box 4
    
    output$count4 <- renderValueBox({
        xmas4 <- xmas %>%
            select(City) %>% 
            unique() %>%
            tally()
        valueBox(paste0(xmas4),
                 "CITIES SENT",
                 icon = icon("map-pin"),
                 color = "blue")
    })
    
   
    # Map
    output$map <- renderLeaflet({
        
        grabdata <- reactive({get(paste("xmassf", input$i_region, sep = ""))})
        
        mapped <- grabdata()
        # mapped is the reactive data so the map can be linked to the objects on the dropdown
        # we now want to map it so that we have each year$fatality in the ui

        cards_rec <- as.list(paste(
            "<b>", mapped$Name, "</b><br>",
            "<b>Card Count: </b>", mapped$card_count, "<br>",
            "<b>Cards Received: </b>", mapped$card_comb,
            sep = ""))
        
        leaflet(mapped) %>% 
            addProviderTiles("OpenStreetMap") %>% 
            addMarkers(icon = ygh_back2,
                       label = lapply(cards_rec, HTML))
        

        # 
        # leaflet() %>%
        #     addProviderTiles(providers$OpenStreetMap) %>%
        #     setView(lng = -100.4,lat= 40,1 , zoom = 4) %>%
        #     addMarkers(data = xmas_d, lng = xmas_d$lon, lat = xmas_d$lat, icon = ygh_back,
        #                label = lapply(cards_rec, htmltools::HTML),
        #                group = "myMarkers")

    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
