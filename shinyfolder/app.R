library(leaflet)
library(tidyverse)
library(readxl)
library(DT)
library(shinydashboard)
library(ggfortify)

wa1 <- read_csv("wa1.csv") %>%select("COUNTRY","EVENT_DATE","number")
countrylist <- wa1$COUNTRY %>% unique()
bar <- read_csv("bar.csv") %>%select("COUNTRY","YEAR","number")
w5 <- read_csv("w5.csv")
nig <- read_csv("nig2.csv")
se <- read_csv("se2.csv")
nigr <- read_csv("nigr.csv")
Lb <- read_csv("lb2.csv")
gui <- read_csv("gui2.csv")
sen <- read_csv("sen2.csv")
ivc <- read_csv("iv2.csv")
mau <- read_csv("mau2.csv")
gunib <- read_csv("gb2.csv")
buifas<- read_csv("bf2.csv")
gamb<- read_csv("gam2.csv")
gha<- read_csv("gha2.csv")
tog<- read_csv("tog2.csv")
ben<- read_csv("ben2.csv")
mal<- read_csv("mal2.csv")
allts <- read_csv("allts.csv")

NIGbar <- read_csv("NIGbar.csv")
SELbar <- read_csv("SELbar.csv")
NIGRbar <- read_csv("NIGRbar.csv")
LBbar <- read_csv("LBbar.csv")
GUIbar <- read_csv("GUIbar.csv")
SENbar <- read_csv("SENbar.csv")
IVbar <- read_csv("IVbar.csv")
MAUbar <- read_csv("MAUbar.csv")
GBbar <- read_csv("GBbar.csv")
BFbar <- read_csv("BFbar.csv")
GAMbar <- read_csv("GAMbar.csv")
GHAbar <- read_csv("GHAbar.csv")
TOGbar <- read_csv("TOGbar.csv")
BENbar <- read_csv("BENbar.csv")
MALbar <- read_csv("MALbar.csv")
col_dat <- read_csv("col_dat.csv")


ui <- navbarPage("West Africa", id="nav",
                 
                 tabPanel("Interactive map of Africa",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto")
                          )),
                 tabPanel("Data explorer",
                          column(3,
                                 selectInput(inputId = "country2",
                                             label="country:",
                                             choices = c("All",unique(as.character(
                                               w5$COUNTRY))),selected = "All"
                                 )),
                          fluidRow(
                            column(3,offset = 0.5,
                                   numericInput("minScore", "Min score", min=0, max=100, value=0)
                            ),
                            column(3,
                                   numericInput("maxScore", "Max score", min=0, max=100, value=100)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   actionButton(inputId = "submit",label = "Submit"))),
                          hr(),
                          DT::dataTableOutput("wa")
                 ),
                 
                 tabPanel("Crisis Analysis",
                          dashboardPage(
                            dashboardHeader(title = "Project"),
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem("Introduction",tabName="hil",icon=icon("globe")),
                                menuSubItem("Time Series",tabName = "Udochi"),
                                menuSubItem("Bar Chart", tabName="hilary"),
                                menuSubItem("Chi-Square Test", tabName="chi-square")
                              )
                            ),
                            
                            dashboardBody(
                              tabItems(
                                tabItem(tabName="hil",class ="background",
                                        fluidRow(
                                          box(HTML('<p>Africa is the second largest continent in the world. Consisting of five regions(Northern, Eastern, Western,Southern and Central).Concerns have been raised on its adverse humanitarian casualities.
                                                       My anaysis was based on crisis level within the 15 countries in the western african region across the span of 21 years,starting from January 1st 1997 to April 16th 2018. My aim was to find out if 
                                                       there was similarity in level of crisis occurrences in the different months for each of the years. 
                                                   </p>','<p><img src="http://i.imgur.com/1KU8ovN.png" width =500 heigth =200/></p>')),
                                          box(HTML('<p><img src="http://www.questconnect.org/images/Nth_Africa_west_phy.jpg" width =500 heigth =100/></p>',
                                                   "West Africa Map"))
                                          )),
                                tabItem(tabName = "Udochi",
                                        fluidRow(
                                          box(plotOutput("plot2",height=300)),
                                          box(selectInput("country","Choose Country",
                                                          choices = countrylist, selected = "Nigeria")),
                                          
                                          box("The Time series above give a detailed trend of the crisis in the 
                                              different west african countries within the span of 21 years,")
                                          )),
                                
                                tabItem(tabName = "hilary",
                                        fluidRow(
                                          box(plotOutput("plot3",height=300,width = 300)),
                                          box(selectInput("country3","Choose Country",
                                                          choices = countrylist, selected = "Nigeria")),
                                          box("The Barplots also represent the crisis trend within these countries in West african in 21 years")
                                        )),
                                tabItem(tabName ="chi-square",
                                        fluidRow(
                                          box(plotOutput("Plot4",height=300,width=300)),
                                          box("As part of the analysis, I wanted to compare the crisis occurrence relationship between each month
                                              across the different years.My intention was to know if there existed a relationship or 
                                              if each month was independent across the years,so i adopted the chi-square test"),
                                          withMathJax(),
                                          box("Chi-Square Test:",br(),"Monthly crisis occurence across each year",br(),
                                              "X-squared = 0.025329",br(),
                                              "df = 11",br(),
                                              "p-value = 1"),
                                          box("Conclusion:",br(),
                                              "The crisis rate in the western african region has existed since 1997, though the occurence in most countries has significantly dropped.In some other areas, there have been recent occurences(Nigeria).This can be clearly seen from the different graphical representations of the crisis trend across 21 years.The Basis of my analysis was to compare the occurences within months across the different years, to know if the rate of occurrence is relational to the month(season). To do this, the chi-square test was used.Based on the resulting p-value i cannot statistically conclude that this relationship exists.",br(), 
                                               "However,pertinent contributory factors to these conflicts include poverty, human rights violations, bad governance and corruption, ethnic marginalization and small arms proliferation.")
                                          )))
                                ))))

server <- function(input, output,session) {
  
  output$map <- renderLeaflet({
    leaflet(w5) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                             attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
      setView(13.5317, 2.4604, zoom = 3.2) %>%
      addCircles(~LONGITUDE, ~LATITUDE, popup=w5$COUNTRY, weight = 3, radius=40,
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8)
  })
  
  output$wa <- DT::renderDataTable(DT::datatable({
    data <- wa1
    if (input$country2 != "All") {
      data <- data%>% filter(COUNTRY == input$country2,
                             number >= input$minScore,
                             number <= input$maxScore)
    }
  },options = list(scrollX = TRUE,dom = 'ft')))
  
  
  output$plot2 <-renderPlot({
    
    if (input$country == "Nigeria")
      data <- ts(nig$numb)
    if (input$country == "Sierra Leone")
      data <- ts(se$numb)
    if (input$country == "Niger")
      data <- ts(nigr$numb)
    if (input$country == "Liberia")
      data <- ts(Lb$numb)
    if (input$country == "Guinea")
      data <- ts(gui$numb)
    if (input$country == "Senegal")
      data <- ts(sen$numb)
    if (input$country == "Ivory Coast")
      data <- ts(ivc$numb)
    if (input$country == "Mauritania")
      data <- ts(mau$numb)
    if (input$country == "Guinea-Bissau")
      data <- ts(gui$numb)
    if (input$country == "Burkina Faso")
      data <- ts(buifas$numb)
    if (input$country == "Gambia")
      data <- ts(gamb$numb)
    if (input$country == "Ghana")
      data <- ts(gha$numb)
    if (input$country == "Togo")
      data <- ts(tog$numb)
    if (input$country == "Benin")
      data <- ts(ben$numb)
    if (input$country == "Mali")
      data <- ts(mal$numb)
    autoplot(ts(data, start =c(1997,1),frequency =12),ts.colour = 'blue',facets = FALSE)
  })
  
  
  output$plot3 <-renderPlot({
    if (input$country3 == "Nigeria")
      data <- NIGbar
    if (input$country3 == "Sierra Leone")
      data <- SELbar
    if (input$country3 == "Niger")
      data <- NIGRbar
    if (input$country3 == "Liberia")
      data <- LBbar
    if (input$country3 == "Guniea")
      data <- GUIbar
    if (input$country3 == "Senegal")
      data <- SENbar
    if (input$country3 == "Ivory Coast")
      data <- IVbar
    if (input$country3 == "Mauritania")
      data <- MAUbar
    if (input$country3 == "Guinea-Bissau")
      data <- GBbar
    if (input$country3 == "Burkina Faso")
      data <- BFbar
    if (input$country3 == "Gambia")
      data <- GAMbar
    if (input$country3 == "Ghana")
      data <- GHAbar
    if (input$country3 == "Togo")
      data <- TOGbar
    if (input$country3 == "Benin")
      data <- BENbar
    if (input$country3 == "Mali")
      data <- MALbar
    ggplot(data=data, aes(y=number, x=EVENT_DATE)) +
      geom_bar(colour="blue", stat="identity")
  })
  
  output$Plot4 <- renderPlot({
    data <- col_dat
    ggplot(col_dat, aes(x = V1, y=V2),colour ="blue") + 
      geom_col(fill="#0000CC") +labs(x="Month",y="Count") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
}

shinyApp(ui = ui, server = server)
