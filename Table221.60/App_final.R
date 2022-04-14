#Version Data: 4/14/2022
# =================================================================================
#load required packages
library(shiny)
library(dplyr)
library(leaflet)
library(highcharter)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(rgdal)
library(maptools)
library(mapproj)
library(rgeos)
library(tigris)
# =================================
#             Data Prep
# =================================
# necessary functions to process the geospatial data
remove.territories = function(.df) {
  subset(.df, 
         .df$id != "AS" &
           .df$id != "MP" &
           .df$id != "GU" & 
           .df$id != "PR" &
           .df$id != "VI" 
  )
}

plain_theme = theme(axis.text=element_blank()) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank())

no_ylab = ylab("") 
no_xlab = xlab("")
# Read in the geospatial data
us<-readOGR(dsn="data/cb_2014_us_state_5m/cb_2014_us_state_5m.shp")
us_aea = spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id = rownames(us_aea@data)
# Change the location of Alaksa
alaska = us_aea[us_aea$STATEFP=="02",]
alaska = elide(alaska, rotate=-50)
alaska = elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska = elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) = proj4string(us_aea)
# Change the location of Hawaii
hawaii = us_aea[us_aea$STATEFP=="15",]
hawaii = elide(hawaii, rotate=-35)
hawaii = elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) = proj4string(us_aea)
# Combine the geospatial data, exclude unwanted regions
us_aea = us_aea[!us_aea$STATEFP %in% c("02", "15"),]
us_aea_inteirm=us_aea[!us_aea$STUSPS %in% c ("AS","MP","GU","PR","VI"),]
us_aea_final = rbind(us_aea_inteirm, alaska, hawaii)
# transform the data into geospatial data
us_aea_final <- spTransform(us_aea_final, proj4string(us))
# Read in datafile
df<-read.csv("data/Score_Transposed_coordinates.csv")
df_data<-df[c("name", "year","Score","SE")] 
# Read the geometric polygon data
# Get US polygon data
US <- readOGR("data/cb_2014_us_state_5m/cb_2014_us_state_5m.shp")
us_aea = spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id = rownames(us_aea@data)
# Change the location of Alaksa
alaska = us_aea[us_aea$STATEFP=="02",]
alaska = elide(alaska, rotate=-50)
alaska = elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska = elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) = proj4string(us_aea)
# Change the location of Hawaii
hawaii = us_aea[us_aea$STATEFP=="15",]
hawaii = elide(hawaii, rotate=-35)
hawaii = elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) = proj4string(us_aea)
# Combine the geospatial data, exclude unwanted regions
us_aea = us_aea[!us_aea$STATEFP %in% c("02", "15"),]
us_aea_inteirm=us_aea[!us_aea$STUSPS %in% c ("AS","MP","GU","PR","VI"),]
us_aea_final = rbind(us_aea_inteirm, alaska, hawaii)
# transform the data into geospatial data
us_aea_final <- spTransform(us_aea_final, proj4string(us))

state_new<-as.data.frame(read.csv("data/data221.csv",head=T))



# =================================
# Define UI 
# =================================
# set up UI
ui <- navbarPage(
  div(
  img(
    src = "NAEP_Large.png"
  )
 
  ),
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            display: flex;
                            align-items: center;
                            height: 60px;
                            }
                           .navbar {
                            align-items: center;
                            min-height:60px !important;}'))
  ),
  tabPanel("NAEP Score Interactive Map", 
           div(class="outer",
               tags$head(
                  #Include our custom CSS
                 includeCSS("style.css"),
               ),
               leafletOutput("mymap", width="100%", height="100%"),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 75, left = "auto", right = 25, bottom = "auto",
                             width = "33%", height = "auto", 
                             hr(),
                             h4("Data Years:"),
                             shinyWidgets::sliderTextInput(inputId ="year", 
                                                           label = "Please Use the Sliderbar to Check the NAEP Score Map from 1998 to 2019: ", 
                                                           choices = c(1998,2002,2003,2004,2007,2009,2011,2013,2015,2017,2019),
                                                           animate=TRUE,
                                                           width= "100%",
                                                           dragRange = TRUE
                             ),
                             hr(),
                             h4("NAEP Score Yearly Trend:"),
                             highchartOutput("quantile_line",height = "1000px"),
                             h4("Dashboard Description:"),
                             p(HTML(
                               "<p style=padding-left:10px; text-align:left;> In this dashoboard, two panels prepared for users to assess NAEP scores throughout the years. 
                           <p style=padding-left:10px; text-align:left;> In the 'NAEP Score Interactive Map' panel, users will be able to assess the NAEP scores of all 50 states in the US via the interactive map. By defaul, the lineplot exhibited in this panel will be showing all locations' NAEP socres throughout the years
                           in the original dataset. If users are interested in one specfiic state's NAEP score trend, users can click the state on the map then the lineplot will be updated to the user-selected state with the reference line of the National NAEP Score. 
                           Users could also click the 'playback' button in the slidebar to review the NAEP score changes over the years from the map directly.
                           <p style=padding-left:10px; text-align:left;> In the 'NAEP Score Comparision' panel, users will able to compare multiple (2 or more) states' NAEP score throughout the years. Users also have the option to decide whether they want to add the National NAEP Score as the reference.
                           <p style=padding-left:10px; text-align:left;> There is a refresh button provided in this panel, users could click it to reload the map page with showing lineplot of all locations in the original dataset."),align="left"),
                             hr(),
                             h4("Data Source:"),
                             p(HTML(
                               "<p style=padding-left:10px; text-align:left;> NOTE: Scale ranges from 0 to 500. State-level data for 1992 and 1994 are not available. 
                           Table does not include private schools, Bureau of Indian Education schools, or (except in the final row) DoDEA schools. Includes public school students who were tested with accommodations; 
                           excludes only those students with disabilities (SD) and English language learners (ELL) who were unable to be tested even with accommodations. 
                           SD and ELL populations, accommodation rates, and exclusion rates vary from state to state. Missing NAEP scores were observed in the early years of the data and the color of 'grey' was assigned to the states when NAEP reading scores were missing of certain years.
                           <p style=padding-left:10px; text-align:left;> The confidence intervals (ribbon and error bars) presented in this dashboard were derived as: μ ± SE.
                           <p style=padding-left:10px; text-align:left;> U.S. Department of Education, National Center for Education Statistics, 
                           National Assessment of Educational Progress (NAEP), 1998, 2002, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, and 2019 Reading Assessments, 
                           retrieved November 3, 2019, from the Main NAEP Data Explorer (https://nces.ed.gov/nationsreportcard/naepdata). 
                           <p style=padding-left:10px; text-align:left;> The source data table was prepared in November 2019."),align="left"),
                             hr(),
                             div(style="align:center;",
                                 actionButton('refresh',"Click this to refresh the page")
                             )
               )
           )
  ),
  tabPanel("NAEP Score Comparision",
           
           sidebarPanel(
           radioButtons(
             "option",
             "Choose whether to plot the National NAEP yearly scores:",
             c("Yes"="Yes","No"="No"),
             selected = "Yes",
             width="100%"
           ),
           hr(),
           awesomeCheckboxGroup(
             inputId = "location",
             label = "Select two or more states to compare NAEP scores:",
             choices =  c("AL"	=	"Alabama"	,
                          "AK"	=	"Alaska"	,
                          "AZ"	=	"Arizona"	,
                          "AR"	=	"Arkansas"	,
                          "CA"	=	"California"	,
                          "CO"	=	"Colorado"	,
                          "CT"	=	"Connecticut"	,
                          "DE"	=	"Delaware"	,
                          "DC"	=	"District of Columbia"	,
                          "FL"	=	"Florida"	,
                          "GA"	=	"Georgia"	,
                          "HI"	=	"Hawaii"	,
                          "ID"	=	"Idaho"	,
                          "IL"	=	"Illinois"	,
                          "IN"	=	"Indiana"	,
                          "IA"	=	"Iowa"	,
                          "KS"	=	"Kansas"	,
                          "KY"	=	"Kentucky"	,
                          "LA"	=	"Louisiana"	,
                          "ME"	=	"Maine"	,
                          "MD"	=	"Maryland"	,
                          "MA"	=	"Massachusetts"	,
                          "MI"	=	"Michigan"	,
                          "MN"	=	"Minnesota"	,
                          "MS"	=	"Mississippi"	,
                          "MO"	=	"Missouri"	,
                          "MT"	=	"Montana"	,
                          "NE"	=	"Nebraska"	,
                          "NV"	=	"Nevada"	,
                          "NH"	=	"New Hampshire"	,
                          "NJ"	=	"New Jersey"	,
                          "NM"	=	"New Mexico"	,
                          "NY"	=	"New York"	,
                          "NC"	=	"North Carolina"	,
                          "ND"	=	"North Dakota"	,
                          "OH"	=	"Ohio"	,
                          "OK"	=	"Oklahoma"	,
                          "OR"	=	"Oregon"	,
                          "PA"	=	"Pennsylvania"	,
                          "RI"	=	"Rhode Island"	,
                          "SC"	=	"South Carolina"	,
                          "SD"	=	"South Dakota"	,
                          "TN"	=	"Tennessee"	,
                          "TX"	=	"Texas"	,
                          "UT"	=	"Utah"	,
                          "VT"	=	"Vermont"	,
                          "VA"	=	"Virginia"	,
                          "WA"	=	"Washington"	,
                          "WV"	=	"West Virginia"	,
                          "WI"	=	"Wisconsin"	,
                          "WY"	=	"Wyoming",
                          "Department of Defense Education Activity"="Department of Defense Education Activity (DoDEA)"),
             selected = c("Pennsylvania", "New York"),
             inline = TRUE, 
             status = "warning"
           ),
           hr(),
           awesomeCheckboxGroup(
             inputId = "region",
             label = "Select one or more Regions to compare NAEP scores:",
             choices =  c( "North east" = "Northeast",
                           "North central" = "Northcentral",
                           "Mid west"="Midwest",
                           "South"="South",
                           "West"="West"),
             inline = TRUE
           )
           )
           ,
           mainPanel(
           h4("NAEP Compairson Line Chart:"),
           highchartOutput("compare",height = "1000px")
           )
  )
)

# =====================================================================================================
# Define server logic required to use leaflet to visualize the reading score of each year in the U.S.
# =====================================================================================================
server <- function(input, output,session) {
  # page refreshe
  observeEvent(input$refresh, {
    session$reload();
  })
  # Use leaflet to generate the map
  output$mymap<-renderLeaflet({
    df_plot<-subset(df_data, year==input$year)
    interim <- geo_join(us_aea_final, df_plot, 'NAME', 'name', how="left")
    # mypal<-colorNumeric(palette = "RdYlBu", domain=as.numeric(interim$Score),na.color = "grey")
    # mypal_nona<-colorNumeric(palette = "RdYlBu", domain=as.numeric(interim$Score),na.color =NA)
    mypal<-colorBin(palette = "RdYlBu", domain=as.numeric(interim$Score), bins=c(230,235,240,245,250,255,260,265,270,275,280),na.color ="grey")
    mypal_nona<-colorBin(palette = "RdYlBu", domain=as.numeric(interim$Score), bins=c(230,235,240,245,250,255,260,265,270,275,280),na.color =NA)
    #create leaflet map visualization
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      # setView(lng = -80, lat = 30, zoom =5) %>%
      # fitBounds(-110,49, -65, 12) %>%
      fitBounds(-110,50, -65, 20) %>%
      addProviderTiles(provider="CartoDB.Positron",options = providerTileOptions(opacity = 0)) %>% 
      addPolygons(data=interim,
                  stroke = T, weight=1, color="white",smoothFactor = 0.5, fillOpacity = 0.8, opacity = 0.3,
                  layerId = interim$name,
                  fillColor = ~mypal(as.numeric(interim$Score)),
                  popup = paste0(
                    "<strong>State: </strong>",interim$name
                    , "</br>"
                    ,"<strong>Year: </strong>",interim$year
                    , "</br>"
                    ,"<strong>NAEP Reading Score (SE): </strong>", interim$Score," (",interim$SE,")"
                  )
      ) %>%
      addLegend(position = "topleft", pal = mypal_nona, values = as.numeric(interim$Score),
                title = "NAEP Reading Score </br> across U.S. Public Schools",
                opacity = 1)
  }
  )
  
  # Create the reactive event for the state selected from the map
  output$quantile_line <- renderHighchart({
    # Create data for linechart showing the yearly trend based on the score of the state selected from the map
    df_selected<-reactive({
      # Use Req to handle the error when using the highcharter
      req(input$mymap_shape_click$id)
      df%>%
        dplyr::filter(name %in% input$mymap_shape_click$id)
    })
    # Render the plot based on the selection
    if (is.null(input$mymap_shape_click$id)==F){
      df_us<-subset(df, location=="United States")
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories=df_selected()$year)%>%
        hc_yAxis(title ="NAEP Reading Scores",
                 tickInterval=10,
                 min=235,
                 max=285) %>%
        hc_yAxis(title = list(text = "NAEP Reading Scores")) %>%
        hc_add_series(name="NAEP Reading Scores",
                      data=as.numeric(df_selected()$Score),
                      colorByPoint = F)%>%
        hc_add_series(data = list_parse(mutate(df_selected(), low = as.numeric(df_selected()$Score)-(as.numeric(df_selected()$SE)), high = as.numeric(df_selected()$Score)+(as.numeric(df_selected()$SE)))),
                      type = "arearange", color = "#9AF1FB", stemWidth = 1,showInLegend = F, opacity=0.5) %>%
        hc_add_series(name="National NAEP Reading Scores",
                      data =as.numeric(df_us$Score),color="orange") %>%
        hc_add_series(data = list_parse(mutate(df_us, low = as.numeric(df_us$Score)-(as.numeric(df_us$SE)), high = as.numeric(df_us$Score)+(as.numeric(df_us$SE)))),
                      type = "arearange", color = "#ECC252", stemWidth = 1,showInLegend = F,opacity=0.5) %>%
        hc_title(
          text = paste0( "The NAEP Reading Scores of ", input$mymap_shape_click$id)
        )
    } else {
      # Default lines of all data in the dataframe if no state is selected
      hchart(df, "line", hcaes(x = year, y = as.numeric(Score), group=location, showInLegend = TRUE))%>%
        hc_xAxis(categories=df$year,title ="Year") %>%
        hc_yAxis(title ="NAEP Reading Scores",
                 tickInterval=10,
                 min=235,
                 max=285) %>%
        hc_title(
          text = "The NAEP Reading Scores of all states (including U.S National and Department of Defense Education Activity)"
        ) %>%
        hc_legend(
          align = "left",
          verticalAlign = "bottom",
          layout = "horizontal"
        ) 
    }
  })
  
  # Create comparison plot, multiple lines based on selection and plot the confidence intervals
  # Create the reactive event for the state selected from the map
  output$compare <- renderHighchart({
    
    index<-na.omit(match(input$region,c("Northeast","Northcentral","Midwest","South","West")))
    list<-list(a=c("CT","MA","ME","NH","RI","VT","NJ","NY","PA"),b=c("IL","IN" ,"MI", "OH", "WI" ,"IA", "KS", "MN", "MO", "NE", "ND", "SD"),
               c=c("IL" ,"IN", "MI" ,"OH", "WI" ,"IA", "KS", "MN" ,"MO" ,"NE" ,"ND", "SD"),
               d=c("DC", "DE", "FL", "GA", "MD" ,"NC" ,"SC", "VA", "WV", "AL" ,"KY" ,"MS", "TN","AR", "LA" ,"OK", "TX"),
               e=c("AZ" ,"CO", "ID" ,"MT" ,"NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
    )
    if(length(index)!=0){
      state<-c()
      for(i in 1:length(index)){
        state<-c(state,list[[index[i]]])
      }
      state<-state_new$full[(match(state,state_new$abbr))]
      input_location<-state
    }else{
      input_location<-input$location
    }
    
    if (input$option=="Yes"){
   
      # Create data for linechart showing the yearly trend based on the score of the state selected from the map
      df_compare<-subset(df, location %in% input_location)
      df_us<-subset(df, location=="United States")
      # Render the plot based on the selection
      highchart() %>% 
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(df_compare, type = "line", hcaes(x = year, y = as.numeric(Score), group=location)) %>%
        hc_add_series(data = df_compare, hcaes(x=year, low = as.numeric(Score)-(as.numeric(SE)), high = as.numeric(Score)+(as.numeric(SE)), group=location),
                      type = "errorbar",color="grey",stemWidth = 1,whiskerLength = 20,showInLegend = F,opacity=0.8)%>%
        hc_add_series(df_us, type="line", hcaes(x = year, y = as.numeric(Score),group=location),color="orange") %>%
        hc_add_series(data = df_us, hcaes(x=year, low = as.numeric(Score)-(as.numeric(SE)), high = as.numeric(Score)+(as.numeric(SE))),
                      type = "errorbar",color="#ECC252",stemWidth = 1,whiskerLength = 20,showInLegend = F,opacity=0.8)
    }
    else {
      # Create data for linechart showing the yearly trend based on the score of the state selected from the map
      df_compare<-subset(df, location %in% input_location)
      # Render the plot based on the selection
      highchart() %>% 
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(df_compare, type = "line", hcaes(x = year, y = as.numeric(Score), group=location)) %>%
        hc_add_series(data = df_compare, hcaes(x=year, low = as.numeric(Score)-(as.numeric(SE)), high = as.numeric(Score)+(as.numeric(SE)), group=location),
                      type = "errorbar",color="grey",stemWidth = 1,whiskerLength = 20,showInLegend = F,opacity=0.8)
    }
  })
}
# ======================
# Run the application 
# =====================
shinyApp(ui = ui, server = server)

