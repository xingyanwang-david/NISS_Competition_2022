#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("1990 - 2019 Labor Statistics in the U.S. by Educational Attainment and Gender"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # reset buttion list
      div(id="form",
          
      # input outcome
      selectInput(
        "outcome",
        label=h4("Please select the outcome of your interest:"),
        choices=list("* Current Dollars"="CD",
                     "* Current 2019 Dollars"="CD_2019",
                     "* Number of persons with earnings who worked full time, year round (in thousands)"="N_full_time",
                     "* Percent of persons with earnings who worked full time, year round"="N_full_time_percentage"
        ),
        selected = c("CD") 
      ),
      # input gender
      checkboxGroupInput("gender", label = h4("Gender:"), 
                         choices = list("Male"="male", "Female"="female"), selected=c("male", "female"), inline = T),
      # checkboxInput("errorbar", label = h5("Show 95 percent confidence intervals"), value = TRUE),
      radioButtons(inputId = "total", label = h4("Display Option:"), 
                   choices = c("Overall in population & in higher education"= TRUE, 
                               "By degree and gender" = FALSE),
                   selected = TRUE, inline = FALSE),
      # checkboxInput("total", label = h5("Show overall trend only"), value = TRUE), 
      # checkboxInput("continous_time", label = h5("Show time in continous"), value = TRUE)
      radioButtons(inputId = "continous_time", label = h4("Time in Continous"), choices = c("Y"= TRUE, "N" = FALSE),
                   selected = TRUE, inline = TRUE)
      ), 
      
      tags$div(
        #"_______________________",tags$br(),
        em(strong("Note:", stype="font-size:15px;")), tags$br(),
        em("* Due to unavailable data between 1990 to 2000, we provide Show Time In Continous
           option to display data in yearly basis.This allows the yearly rate of change to be 
           consistent.",style="font-size:12px;"), tags$br()
        #em("2. Click the submit button to re-generate the visualization.",style="font-size:12px;") # hightlight `submit` 
      ),
      
      radioButtons(inputId = "errorbar", label = h4("95% Confidence Intervals (X-1.96*SE, X+1.96*SE)"), choices = c("Y"= TRUE, "N" = FALSE),
                   selected = FALSE, inline = TRUE),
      # add button to submit input
      actionButton("submit1" ,"Update"),
      #actionButton("reset1" ,"Clear", icon("refresh")),
      #textOutput("text1")
      # add a break
      #br(),

      ########################### Add text1 ###############################
      tags$div(
        "_______________________",tags$br(),
        em(strong("App Usage:", stype="font-si20pt")), tags$br(),
        em("1. Use the dropdown menu to select the outcome of interest.",style="font-size:12px;"), tags$br(),
        em("2. Click the update button to re-generate the visualization.",style="font-size:12px;"), tags$br(),
        em("3. Click/Un-click legend on the top of the figure to include different groups into
           comparison. Detailed information will show up once hover over data points",style="font-size:12px;")
        ), # end div

      ),# end sidebar
      ########################### Add text1 ###############################
      #
    # mainPanel(
    #   h3("TEST INPUT 2"),
    # ),
    
    # Show the plot
    ########################### Add text2 ###############################
    mainPanel(
      highchartOutput("income_plot_overall",height="450pt"),# plot dimension
      # add side note 
      
      tags$div(
        "_______________________",tags$br(),
        em(strong("Graph Description"))),
      # add description 
      htmlOutput("text1"),
      
      tags$div(
        "_______________________",tags$br(),
        em(strong("SideNote (", 
           a("Data Source", href="https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-pinc/pinc-03.html"),
           " Updated in January 2021):"), style = "font-size:12px;"), tags$br(),
        em("1. Constant dollars based on the Consumer Price Index, prepared 
           by the Bureau of Labor Statistics, U.S. Department of Labor.",style = "font-size:12px;"), tags$br(),
        em("2. Caution should be used when comparing 2019 estimates to those
           of prior years due to the impact that the coronavirus pandemic had on 
           interviewing and response rates.", style = "font-size:12px;"), tags$br(), 
        em("3. Data are based on sample surveys of the noninstitutionalized population, 
           which excludes persons living in institutions (e.g., prisons or nursing facilities); 
           data include military personnel who live in households with civilians but exclude 
           those who live in military barracks.",style = "font-size:12px;")
      ), # end div
      ########################### Add text2 ###############################
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ######## Change data location ###
  dir <- paste0(getwd(), "/")
  
  # read in data based on input
  dataInput <- reactive({
    if (is.null(input$outcome)) {return()} 
    
    if (input$outcome == "CD") {
      df <- read.csv(paste0(dir, "income_long_current.csv"))
      title <- "Median Annual Income (in Current Dollar) for \n 
      Full-time Year-round Workers Age 25 and over"
      ylab <- "Income ($)"
    }else if (input$outcome == "CD_2019") {
      df <- read.csv(paste0(dir, "income_long.csv"))
      title <- "Median Annual Income (constant 2019 Dollar) for \n 
      Full-time Year-round Workers Age 25 and over"
      ylab <- " Income ($)"
    }else if (input$outcome == "N_full_time") {
      df <- read.csv(paste0(dir, "ppl_long.csv"))
      title <- "Number of Persons with earnings \n 
      who worked full time, year round"
      ylab <- "Number of Persons (in thousands)"
    } else if (input$outcome == "N_full_time_percentage") {
      df <- read.csv(paste0(dir, "ppl_prop_long.csv"))
      title <- "Percentage of Persons with earnings \n 
      who worked full time, year round"
      ylab <- "%"
    }
    
    # filtered with gender input 
    if (length(input$gender)==1) {
      # case when one sex select 
      df <- df[df$gender==input$gender,]
      
    } else if (is.null(input$gender)) {
      return()
    } else {
      # both sexes selected do nother
    }
    
    # return final data
    df_ls <- list(df=df, title=title,ylab=ylab)
    
    })
  
  
  degree_code <- data.frame(degree = c("Less than 9th Grade", "Some High School", 
                                       "High School", "Associate Degree", "Some College", 
                                       "Bachelor Degree", "Master's Degree", "Professional Degree",
                                       "Doctor Degree","Overall", "Overall Higher Education"), 
                            code = c("Less than 9th grade", "some_high", "High school", "AA", 
                                     "some_college","Bachelor","master","professional","doctor",
                                     "total", "total_high_edu"))
  
  
  # marker list 
  marker_list <- c("square", "triangle",  "cross", "plus", 'circle', 'square','diamond','triangle-down',
                   'star','circle', 'square','diamond','triangle-down','star')
  
  
  output$income_plot_overall <- renderHighchart({
    #### shuang start 
    # select data based on input
      #if(input$submit1 >=0 | input$reset1 >=0){
      if(input$submit1 >=0) {
        isolate({
          if (is.null(input$outcome)) {return()}
          df_ls <- dataInput()
          df <- df_ls$df
          ylab <- df_ls$ylab
          title <- df_ls$title 
          
          # draw plots
          
          # convert numeric values
          df$value <- as.numeric(df$value )
          df$se <- as.numeric(df$se)
          
          
          if(input$continous_time){
            df$year <- as.Date(as.character(df$year), format = "%Y")
            df <- as_tibble(df)
            # try to create an empty canvas
            # hctss <- highchart(type = "stock") %>%
            #   hc_xAxis( title = list(text = "Year"), type = "datetime") %>%
            #   hc_yAxis(title = list(text = ylab)) %>%
            #   hc_legend(align = "left", verticalAlign = "top", layout = "horizontal", width=600, x = 0, y = 0) %>%
            #   hc_title(text= title) %>%
            #   hc_rangeSelector(
            #     verticalAlign = "bottom",
            #     selected = 5
            #   )
            hctss <- highchart() %>%
              hc_xAxis( title = list(text = "Year"), type = "datetime") %>%
              hc_yAxis(title = list(text = ylab)) %>%
              hc_legend(align = "left", verticalAlign = "top", layout = "horizontal", width=600, x = 0, y = 0) %>%
              hc_title(text= title)
          } else {
            df$year <- as.numeric(df$year)
            df <- as_tibble(df)
            # try to create an empty canvas
            hctss <- highchart() %>% 
              hc_xAxis(categories=unique(df$year),title = list(text = "Year")) %>%
              hc_yAxis(title = list(text = ylab)) %>%
              hc_legend(align = "left", verticalAlign = "top", layout = "horizontal", width=600, x = 0, y = 0) %>%
              hc_title(text= title)
          }
          
          
          
          #edu_grp <- unique(df$education)[!grepl("total", unique(df$education))]
          edu_grp <- unique(df$education)
          
          edu_grp <- c("total", "total_high_edu", "Less than 9th grade", "some_high" ,
                       "High school" , "some_college", "AA", "Bachelor","master","professional",
                       "doctor")
          

          for(grp in edu_grp){
            # if (!input$total & grepl("total", grp)) {
            #   # if do not want total, skip this loop
            #   print("IN TOTAL SKIP LOOP")
            #   next 
            # }
            
            # 
            if ((input$total == "TRUE") & (!grepl("total", grp))) {
              
              next
            }
            
            # when display all lines, show total with more thicker line
            
            if (grepl("total", grp)) {
              color_F <- "rgba(242,143,67)"
              color_M <- "rgba(47,126,216)"

            } else {
              color_F <- "rgba(242,143,67,0.5)"
              color_M <- "rgba(47,126,216,0.5)"
            }
            idx <- match(grp, edu_grp)
            
            sub_name_male <- paste0("Male-", degree_code[degree_code$code == grp, 1])
            sub_name_female <- paste0("Female-", degree_code[degree_code$code == grp, 1])

            # based on gender selections 
            if("male" %in% input$gender) {
              if(input$continous_time){
                hctss <- hctss %>%
                  hc_add_series(data = df %>% filter(education == grp & gender == "male") , hcaes(x = year, y = value), 
                                color = color_M,
                                zIndex = -3,
                                name = sub_name_male, marker = list(symbol=marker_list[idx], radius = 3), id = sub_name_male,
                                type = "line") 
                
                if (input$errorbar) {
                  # data = mutate(subset(df, education == grp & gender=="male" ), low = (value-se*1.96), high = (value+se*1.96))
                  hctss <- hctss %>% hc_add_series(data = subset(df, education == grp & gender=="male" ),
                                                   hcaes(x = year, low = value-se*1.96, high =  value+se*1.96), 
                                                   type = "arearange", 
                                                   color = "#9AF1FB", 
                                                   stemWidth = 1,
                                                   zIndex = -10, name = sub_name_male, marker = list( radius = 0),  linkedTo = sub_name_male)
                }
              } else {
                hctss <- hctss %>%
                  hc_add_series(data = df %>% filter(education == grp & gender == "male") %>% .$value,
                                color = color_M,
                                zIndex = -3,
                                name = sub_name_male, marker = list(symbol=marker_list[idx], radius = 3), id = sub_name_male,
                                type = "line") 
                
                if (input$errorbar) {
                  hctss <- hctss %>% hc_add_series(data = list_parse(mutate(subset(df, education == grp & gender=="male" ), low = (value-se*1.96), high = (value+se*1.96))),
                                                   type = "arearange", 
                                                   color = "#9AF1FB", 
                                                   stemWidth = 1,
                                                   zIndex = -10, name = sub_name_male, marker = list( radius = 0),  linkedTo = sub_name_male)
                }
              }
              
                

              
              
            }  
            if ("female" %in% input$gender) {
              if(input$continous_time){
                hctss <- hctss %>%
                  hc_add_series(data = df %>% filter(education == grp & gender == "female") , hcaes(x = year, y = value), 
                                color = color_F, 
                                zIndex = -3,
                                name = sub_name_female, marker = list(symbol=marker_list[idx],  radius = 3), id = sub_name_female, 
                                type = "line")
                
                
                
                
                if (input$errorbar) {
                  hctss <- hctss %>% 
                    hc_add_series(data = subset(df, education == grp & gender=="female" ),
                                  hcaes(x = year, low = value-se*1.96, high =  value+se*1.96),
                                  type = "arearange", color = "#FFE4B5", stemWidth = 1,
                                  zIndex = -10, name = sub_name_female, marker = list( radius = 0), linkedTo = sub_name_female )  
                  
                }
              } else {
                hctss <- hctss %>%
                  hc_add_series(data = df %>% filter(education == grp & gender == "female") %>% .$value,
                                color = color_F, 
                                zIndex = -3,
                                name = sub_name_female, marker = list(symbol=marker_list[idx],  radius = 3), id = sub_name_female)
                
                
                
                
                if (input$errorbar) {
                  hctss <- hctss %>% 
                    hc_add_series(data = list_parse(mutate(subset(df, education == grp & gender=="female" ), low = (value-se*1.96), high = (value+se*1.96))),
                                  type = "arearange", color = "#FFE4B5", stemWidth = 1,
                                  zIndex = -10, name = sub_name_female, marker = list( radius = 0), linkedTo = sub_name_female )  
                  
                }
              }
              
            }
            
          } # end grp loop
          
          
        })# end isolate
      } # end if submit buttion clicks
    

    hctss
  }) # end plot 
    ################# shuang end
    
    # # reset all input to default 
    # observeEvent(input$reset1, {
    #   reset("form")
    # })

  
  # add graph description 
  textInput <- reactive({
    if (is.null(input$outcome)) {return()} 
    
    if (input$outcome == "CD_2019") {
      des <- "The graphic demonstrates the longitudinal change of median annual
      income for full-time year-round workers aged 25 and over from 1990 to 2019
      adjusted for consumer price index.
      The overall median annual income of women showed two episodes in the evolution of earning,
      the first lasting from 1990 to 2000 and the second from 2000 to 2019. The income achieved
      a major increase in 2000, stayed at the similar level and rose again after 2014.
      The overall median annual income of men reached its peak in 2000 and fluctuated since then.
      The income continued to increase after reached its minimal value in 2014.
      At the same education level, the median annual income of men was higher than that of women
      through 1990 to 2019. "
      
      
    }else if (input$outcome == "CD") {
      des <- "The graphic demonstrates the longitudinal change of median 
      annual income in current dollar for full-time year-round workers 
      aged 25 and over from 1990 to 2019. The overall median annual income
      within gender groups trended up since 1990. Except for 2002, the 
      income declined and might be related to the early 2000s recessions.   
      At the same education level, the median annual income of men was higher 
      than that of women through 1990 to 2019. In terms of the overall higher 
      education group, the gender earning gap increased over time."
    }else if (input$outcome == "N_full_time") {
      des <- "The graphic demonstrates the longitudinal change of number of 
      full-time year-round workers aged 25 and over from 1990 to 2019. The overall 
      employment within gender groups trended up since 1990 except a short-term decline from 2007 to 2009. 
      At the same education level, the male full-time workers outnumbered female workers through 
      1990 to 2019. In terms of the overall higher education group, the gender earning gap 
      decreased over time."
    } else if (input$outcome == "N_full_time_percentage") {
      des <- "The graphic demonstrates the longitudinal change of percentage of 
      full-time year-round workers aged 25 and over from 2000 to 2019. 
      The most recent overall full-time employment rate within gender groups remained 
      at the same level as in 2000. The percentage reached its minimal in 2009.  
      At the same education level, the full-time employment rate of men was higher than that 
      of women through 2000 to 2019. "
    }
    return(des)
  }) # end reactive 
  
  ########################### Add text3 ###############################
  output$text1 <- renderUI({
    if(input$submit1 >=0) {
    isolate({ 
    
    if (is.null(input$outcome)) {return()}
    
    des <- textInput()
    
    
    HTML(des)
    
    ########################### Add text3 ###############################    
    
  }) # end isolate 
    } # end submit click
  })# end renderText
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
