#if not installed, install the following:
#install.packages("shinycssloaders")
#install.packages("directlabels")
#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("cowplot")
#install.packages("sf")
#install.packages("rworldmap")
#install.packages("rgeos")
#install.packages("extrafont")
#install.packages("shinyWidgets")
#install.packages("countrycode")
#install.packages("DT")
library(shinycssloaders)
library(directlabels)
library(tidyverse)
library(shiny)
library(shinythemes)
library(cowplot)   # for theme_minimal_grid()
library(sf)        # for manipulation of simple features objects
library(rworldmap) # for getMap()
library(rgeos)
library(extrafont)
library(shinyWidgets)
library(countrycode)
library(DT)

#load data
wwc_outcomes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")
champions <- read_csv("./data/champions.csv")
country_lats_longs <- read_csv("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")


#---DATA PREPARATION---#
#format squads data to be used in 2019 Roster tab
squads$dob <- as.Date(squads$dob,format = "%m/%d/%y")
squads <- squads %>% rename(Squad_Number=squad_no, Country=country, Position=pos, Player=player,
                            DOB=dob, Age=age, Caps=caps, Goals_Scored_for_Country=goals, Club_Team=club)
squads$Caps[is.na(squads$Caps)] <- 0
squads$Goals_Scored_for_Country[is.na(squads$Goals_Scored_for_Country)] <- 0
squads$Country <- gsub("US", "United States of America", squads$Country)

squads<- squads%>%
  arrange(Country)
 
#create a country "ALL" selection for 2019 Roster data table
names <- c(unique(squads$Country),"All")


#join data with codes to get country column in data frame
critique_wwc_outcomes_wcodes <- left_join(wwc_outcomes, codes, by = "team") %>%
  mutate(year = as.character(year))
critique_wwc_outcomes_wcodes$country <- gsub("China PR", "China", critique_wwc_outcomes_wcodes$country)
critique_wwc_outcomes_wcodes$country <- gsub("Chinese Taipei", "Taiwan", critique_wwc_outcomes_wcodes$country)

#GOALS BY COUNTRY TAB
#Total goals scored by country 
critique_top_countries <- critique_wwc_outcomes_wcodes %>%
  group_by(country) %>%
  summarize(total_score = sum(score)) 

#Change score 'type' to numeric
critique_wwc_outcomes_wcodes$score = as.numeric(critique_wwc_outcomes_wcodes$score)


#join outcome data with total goals scored
critique_joined_wwc <- inner_join(critique_wwc_outcomes_wcodes, critique_top_countries)


#transform data to plot total tournament goals by country and year
critique_plot <- critique_joined_wwc %>%
  group_by(year, country)%>%
  summarize(year_score=sum(score))#%>%

#transform data for total goals over all years
critique_plot1 <- critique_joined_wwc %>%
  group_by(year, country)%>%
  summarize(total_score = median(total_score))

#merge plot and plot1
critique_plot_final <- merge(critique_plot, critique_plot1, by = c("year", "country"))
colnames(critique_plot_final)[4] <- "total_goals_all_years"
colnames(critique_plot_final)[3] <- "year_goals"

#order countries in critique_plot_final to be used in line plot
critique_plot_final1 <- critique_plot_final %>%
  arrange(country) %>%
  na.omit(critique_plot_final1)

#WINS TAB
#join date with champion data frame after summing up total wins per year per country  
champions$country <- gsub("United States of America", "United States", champions$country)

w_l <- critique_wwc_outcomes_wcodes %>%
  mutate(wins = if_else(win_status=="Won", 1, 0))%>%
  group_by(country, year)%>%
  summarize(total_wins = sum(wins))

champions$year <- as.character(champions$year)

w_l<-left_join(w_l, champions, by = c("country", "year"))
w_l<-left_join(w_l, critique_plot_final1, by= c("country", "year"))
w_l$champion[is.na(w_l$champion)] <- "no"


#MAP TAB
more_codes <- countrycode::codelist_panel %>% 
  group_by(country.name.en) %>% 
  top_n(1, year) %>%
  select(country.name.en, ioc, iso2c, iso3c, genc3c, fips)

wwc_outcomes_wcodes <- dplyr::left_join(wwc_outcomes, codes, by = "team") %>%
  select(year, team, score, country) %>%
  left_join(more_codes, by = c("team" = "ioc")) %>%
  mutate(year = as.character(year))

wwc_outcomes_wcodes1 <- wwc_outcomes_wcodes %>%
  mutate(Country = country.name.en)

#creates total goals scored by each team by year
scores_by_team <- wwc_outcomes %>%
  group_by(year, team) %>%
  summarise(total_score = sum(score))

scores_by_year <- right_join(scores_by_team, wwc_outcomes_wcodes1, by=c('team' = 'team')) %>%
  select(team, year.x, total_score, country.name.en, Country)

scores_by_year$Country[scores_by_year$team == 'SCO'] <- 'United Kingdom'
scores_by_year$Country[scores_by_year$team == 'ENG'] <- 'United Kingdom'
scores_by_year$Country[scores_by_year$team == 'NGA'] <- 'Nigeria'

wwc_outcomes_wcodes1 <- wwc_outcomes_wcodes1 %>%
  group_by(year, team, country, country.name.en, iso2c, iso3c, genc3c, fips, Country) %>%
  summarise(n = n()) %>%
  mutate(total_score = sum(wwc_outcomes$score))

wwc_outcomes_wlatsandlongs <- left_join(scores_by_year, country_lats_longs, by =c('Country' = 'Country'))

map.world <- map_data('world')
#View(map.world)
wwc_outcomes_wlatsandlongs <- wwc_outcomes_wlatsandlongs %>%  mutate(Country = recode(Country, `United States` = 'USA'
                                                                                      , `United Kingdom` = 'UK'
                                                                                      , `Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                                                                                      , `Trinidad and Tobago` = 'Trinidad'
                                                                                      , `Congo, Republic of the` = 'Republic of Congo'
                                                                                      )
                                                                    )
# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),

  navbarPage("Women's World Cup",
    tabPanel("Overview",
        sidebarPanel(h6("2011 FIFA Women's World Cup Official Song"), 
                     h6("'Happiness' by Alexis Jordan"),
                     tags$audio(src = "Happiness.wav", type = "audio/wav", autoplay = FALSE, controls = NA),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     img(src="logos.jpg", width= "100%")),
        mainPanel(h1("Welcome!", align= "center"),
                  br(),
                  h4("This app provides information regarding Women's World Cup data from its inauguration in 1991 to 2019.  It is designed to get a glimpse into the tournament's history, and its outcomes."),
                  br(),
                  br(),
                  h4("Within the app, the following data is available:"),
                  h4("-Goals per country per year"), 
                  h4("-Wins per country per year"),
                  h4("-Champion per year"),
                  h4("-Mapping of each country participating in the World Cup per year"),
                  h4("-2019 rosters"),
                  br(),
                  br(),
                  h4("Click on the tabs to navigate throughout the app.  Click on the play arrow, to your left, to get into the spirit of the World Cup as you navigate the data.*"),
                  br(),
                  br(),
                  h6("*The song was chosen since it is the only official song of any Women's World Cup to date."))),
    tabPanel("World Cup Stats",
        tabsetPanel(
            tabPanel("Goals per Country",
                sidebarPanel(actionButton("uncheck", "Uncheck all boxes"),
                             br(),
                             br(),
                             checkboxGroupInput("country1", "Choose countries:", choices = unique(critique_plot_final1$country)),
                             width = 2),
                  mainPanel(column(12, offset = 1, h5(strong(textOutput("instruct"))),
                    plotOutput("plot", brush = brushOpts(id = "plot_brush")),
                    br(),
                    br(),
                    h5(strong(textOutput("note"))),
                    br(),
                    DT::DTOutput("print")))),
            tabPanel("Wins per Country",
                sidebarPanel(
                  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                  sliderInput("year_wins", "Choose a World Cup year:", value=1991,
                              min = 1991, max = 2019, step = 4, animate = TRUE, sep= ""),
                              br(),
                              h5("*Bar in red denotes Women's World Cup champion"),
                              h5("**Numbers on bars are total goals scored during that year")),
                  mainPanel(plotOutput("plot_w", height = "600px")
                            ))
                    )
            ),     
    tabPanel("World Cup Map",
        sidebarPanel(
          selectInput('year', 'Choose a World Cup year:', choice = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019), multiple = FALSE),
            width = 2),
          mainPanel(column(2, offset = 1,
              withSpinner(plotOutput('map', height = '700px', width = '1000px'))
          )
       )
    ),
    tabPanel("2019 Rosters",
        sidebarPanel(selectInput("country", "Choose a country:", choices = names, multiple = FALSE, selected = "All"), 
                     width=3),
          br(),
          mainPanel(DT::DTOutput("roster"))
            ),
    tabPanel("About",
        mainPanel(h1("Project Information"),
                  br(),
                  h4("This was a final data visualization project for the Fall 2019: DSBA 5122 at University of North Carolina at Charlotte.
                     The project was geared to journalists with an interest in the history of the Women's World Cup, gathering
                     information about goals, wins, and champions throughout the years.  In addition, the 2019 rosters provide
                     player information, including international experience."),
                  br(),
                  br(),
                  h1("Want the code, presentation, and report? Click on the links below."),
                  br(),
                  uiOutput("git"),
                  uiOutput("report"))
    )
  )
)


# Define server logic 
server <- function(input, output, session) {
  
url <- a("Final Project at Github", href="https://github.com/joshganz13/Final-Project-Women-s-World-Cup-Shiny-App")
output$git <-renderUI({
  tagList("URL link:", url)
})

url1 <- a("Bookdown Report", href="https://bookdown.org/joshganz13/data_vis_project/")
output$report <- renderUI({
  tagList("URL link:", url1)
})

rsquads <- reactive({
  if(input$country == "All"){
    return(squads)
  }
    
squads %>%
  filter(Country == input$country)
})

#creates a the table to go in the mainPanel of 2019 Roster  
output$roster = DT::renderDT({rsquads()}, server = TRUE)

#create Goals per year line plot and data table with brushing  
cpl <- reactive({
  df<- critique_plot_final1 %>%
    filter(country %in% input$country1) %>%
    tidyr::complete(year, country) #adds in all possible row combinations
  validate(
      need(input$country1 != "", 'Please choose at least one country.')
    )
  df[is.na(df)] <- 0
    return(df)
})
   
cpl1 <- reactive({
  df<-brushedPoints(critique_plot_final1, input$plot_brush) %>%
    filter(country %in% input$country1) %>%
    tidyr::complete(year, country) #adds in all possible row combinations

  df[is.na(df)] <- 0
    return(df)
 
  if(nrow(df)==0)
    df<-critique_plot_final1
    df
})                             

observe({
  if (input$uncheck >0) {
    updateCheckboxGroupInput(session=session, inputId = "country1", choices = unique(critique_plot_final1$country), selected = NULL)
  }
})

  output$instruct <- renderText("To highlight data to view in table, click and drag a rectangle around specific data points.") 
  output$plot <- renderPlot({
    ggplot(cpl(), mapping = aes(year, year_goals, color = country)) +
      geom_point(size=4) +
      geom_line(mapping = aes(group=country), size=1, na.rm = FALSE) +
      labs(y = "", x = "",
           title = "Women's World Cup: Goals per Year (1991 - 2019)",
           caption = "Data from https://data.world/sportsvizsunday/womens-world-cup-data") +
      geom_dl(aes(label = country), method = list(dl.trans(x = x + .2), "last.qp")) +
      coord_fixed(ratio=1/12)+
      theme_bw() +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.line = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            legend.position = "none", plot.title = element_text(size = 20, hjust = 0.5)) +
      scale_x_discrete(limits=c("1991","1995","1999","2003","2007","2011","2015","2019"))+
      scale_y_continuous(breaks = c(0,4,8,12,16,20,24,28,32), limits = c(0,32))
    })
  
  output$note <- renderText("Please note: if a country's year_goals and total_goals_all_years are both zero for a particular year, that country did not participate in that year's World Cup.")
  
  output$print <- DT::renderDT({cpl1()}, server = TRUE)

#create wins by country per year bar graph  
wins<-reactive({
    w_l <- w_l %>%
      filter(year == input$year_wins)
    })

output$plot_w <- renderCachedPlot({
    ggplot(wins(), mapping = aes(x=reorder(country, total_wins), y=total_wins)) +
      geom_col(aes(fill=champion))+
      geom_text(size = 8, aes(label = wins()$year_goals, hjust=1.2)) +  
      scale_fill_manual(values= c("#005AB5", "#DC3220"))+
      coord_flip()+
      labs(y = "", x = "",
         title = "Women's World Cup: Wins per Country")+
      theme_bw() +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none", plot.title = element_text(size = 20, hjust = 0.5)) +
      scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8), expand = c(0,0))
  },
  cacheKeyExpr = {input$year_wins}
)


# Create World Cup Map   
  v <- reactive({
    wwc_outcomes_wlatsandlongs1 <- wwc_outcomes_wlatsandlongs %>%
      filter(year.x == input$year)

    
    #joins the latitude and longitude data with the source data so it can be mapped
    left_join( map.world, wwc_outcomes_wlatsandlongs1, by = c('region' = 'Country'))
    
})
  
output$map <- renderCachedPlot({
    ggplot(data = v(), mapping = aes( x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = total_score)) +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
      labs(fill = 'Goals'
         ,title = 'Goals Scored per Country'
         ,subtitle = input$year
         ,caption = "https://www.sharpsightlabs.com/blog/highlight-countries-on-map/"
         ,x = NULL
         ,y = NULL) +
      theme(text = element_text(color = '#000000')
          ,plot.title = element_text(size = 28)
          ,plot.subtitle = element_text(size = 14)
          ,axis.ticks = element_blank()
          ,axis.text = element_blank()
          ,panel.grid = element_blank()
          ,panel.background = element_rect(fill = '#bbd4dd')
          ,plot.background = element_rect(fill = '#333333')
          ,legend.position = c(.18,.36)
          ,legend.background = element_blank()
          ,legend.key = element_blank()
       ) +
      theme(plot.title = element_text(hjust=0.5, color = 'white'), plot.subtitle = element_text(hjust = 0.5, 
                                      size = 15, face = 'bold', color = 'white'), plot.caption = element_text(size = 8, color = 'white'))
      },
      cacheKeyExpr = {input$year}
  )

}
shinyApp(ui = ui, server = server)

