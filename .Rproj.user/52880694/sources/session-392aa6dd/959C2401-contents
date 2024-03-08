library(tidyverse)
library(readr)
library(countrycode)
library(ggpubr)
library(ggeasy)
library(sp)
library(sf)
library(sfheaders)
library(scales)

#Setting up world map data

#Getting World Happiness Report data ready
happiness <- read.csv("WHR2023.csv")
happiness <- happiness %>% subset(select = c("Country.name", "Ladder.score"))
colnames(happiness) <- c("country", "happiness_level")

#Getting data for world map
world <- map_data("world")

#Figuring out differences in country naming and changing names in WHP data to match those in map data
diff <- setdiff(happiness$country_2, intersect(happiness$country_2, world$region))
#diff

#table(world$region)

world <- world %>%
    within(region[subregion == "Hong Kong"] <- "China (Hong Kong)") %>%
    subset(region != "Antarctica")

happiness <- happiness %>%
    mutate(country_2 = country) %>%
    within(country_2[country == "United States"] <- "USA") %>%
    within(country_2[country == "Taiwan Province of China"] <- "Taiwan") %>%
    within(country_2[country == "Czechia"] <- "Czech Republic") %>%
    within(country_2[country == "United Kingdom"] <- "UK") %>%
    within(country_2[country == "Hong Kong S.A.R. of China"] <- "China (Hong Kong)") %>%
    within(country_2[country == "Congo (Brazzaville)"] <- "Republic of Congo") %>%
    within(country_2[country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo") %>%
    within(country_2[country == "State of Palestine"] <- "Palestine") %>%
    within(country_2[country == "Turkiye"] <- "Turkey") 



#Creating a data set with average values of happiness and trust for each country

#Loading and cleaning World Values Survey data. Q57P = trust, Q46P = happiness
data <- read.csv("WorldValues.csv")
cleaned_data <- data %>%
    subset(Q49 %in% 1:10) %>%
    subset(Q48 %in% 1:10)

#exploring the data... 
table(cleaned_data$Q49)
table(cleaned_data$Q48)

summary(lm(Q49 ~ Q48, data = cleaned_data))

#Checking out the data
table(data$B_COUNTRY_ALPHA)

#Creating data table with mean values, adding a column with country names instead of country codes using countrycode package
values_data <- cleaned_data %>% group_by(B_COUNTRY_ALPHA) %>%
    summarise(satisfaction = mean(Q49), control = mean(Q48)) %>%
    mutate(country_name <- countrycode(sourcevar = B_COUNTRY_ALPHA, origin = "iso3c", destination = "country.name"))

colnames(values_data) <- c("country_code", "satisfaction", "control", "country_name")

#Checking the differences in country names for the 2 datasets
setdiff(values_data$country_name, intersect(values_data$country_name, happiness$country))

#Renaming countries in WVS to match those in WHP
values_data_2 <- values_data %>%
    mutate(country_name_2 = country_name) %>%
    within(country_name_2[country_name == "Hong Kong SAR China"] <- "China (Hong Kong)") %>%
    within(country_name_2[country_name == "Myanmar (Burma)"] <- "Myanmar") %>%
    within(country_name_2[country_name == "United States"] <- "USA") %>%
    within(country_name_2[country_name == "Czechia"] <- "Czech Republic") %>%
    within(country_name_2[country_name == "United Kingdom"] <- "UK") %>%
    within(country_name_2[country_name == "Congo (Brazzaville)"] <- "Republic of Congo") %>%
    within(country_name_2[country_name == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo") %>%
    within(country_name_2[country_name == "State of Palestine"] <- "Palestine")

#Merging data sets
merged_data <- merge(happiness, values_data_2, by.x = "country_2", by.y = "country_name_2")
merged_data <- merged_data %>%
    subset(select = -c(country, country_name))

#Checking out the correlations
summary(lm(satisfaction ~ control, data = merged_data))
summary(lm(happiness_level ~ control, data = merged_data))
summary(lm(satisfaction ~ happiness_level, data = merged_data))


#Creating full data with all happiness countries
mapcountriesdata <- left_join(happiness, merged_data, by = c("country_2"))
mapcountriesdata <- mapcountriesdata %>%
    subset(select = -c(country, happiness_level.y, country_code)) %>%
    rename("happiness_level" = happiness_level.x,
           "country" = country_2)


#Adding region
region_data <- read_csv("Countries by continents.csv")
setdiff(mapcountriesdata$country, intersect(region_data$Country, mapcountriesdata$country))

region_data <- region_data %>%
    within(Country[Country == "United States"] <- "USA") %>%
    within(Country[Country == "Czechia"] <- "Czech Republic") %>%
    within(Country[Country == "United Kingdom"] <- "UK") %>%
    within(Country[Country == "Congo"] <- "Republic of Congo") %>%
    within(Country[Country == "Macedonia"] <- "North Macedonia") %>%
    within(Country[Country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo") %>%
    within(Country[Country == "Burkina"] <- "Burkina Faso") %>%
    within(Country[Country == "Burma (Myanmar)"] <- "Myanmar") %>%
    within(Country[Country == "Hong Kong"] <- "China (Hong Kong)") %>%
    rename(country = "Country")

mapcountriesdata <- left_join(mapcountriesdata, region_data, by = "country")

#adding palestine and kosovo

mapcountriesdata[mapcountriesdata$country == "Palestine", "Continent"] <- "Asia"
mapcountriesdata[mapcountriesdata$country == "Kosovo", "Continent"] <- "Europe"


#Creating map data for both map visualizations
mapdata <- left_join(world, mapcountriesdata, by = c("region" = "country"))


#Creating Visualization 1: Satisfaction Map
scales::seq_gradient_pal("#72A4F9", "#1e2f97", "Lab")(seq(0,1,length.out=5))

satisfaction_map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = satisfaction)) +
    coord_fixed(ratio = 1.2) +
    theme_void() +
    scale_fill_gradientn(name = "Key", 
                         colors = c("grey80", "#FFEC8B", "#FAC90D", "#EAA01E"),
                         na.value = "grey40",
                         breaks = c(min(mapcountriesdata$satisfaction, na.rm = TRUE), max(mapcountriesdata$satisfaction, na.rm = TRUE)), 
                         labels=c(paste(round(min(mapcountriesdata$satisfaction, na.rm = TRUE), 1), "\n(least satisfied\ncountry)"), paste(round(max(mapcountriesdata$satisfaction, na.rm = TRUE), 1), "\n(most satisfied\ncountry)"))) +
    theme(legend.title.align = 0,
          legend.title = element_text(size = 15),
          legend.position = c(.15, .3),
          legend.direction = "vertical",
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    labs(title = NULL,
         subtitle = NULL
    )

satisfaction_map

#Creating Visualization 2: Control Map
control_map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = control)) +
    coord_fixed(ratio = 1.2) +
    theme_void() +
    scale_fill_gradient2(name = "Key", 
                        low = "grey90",
                        mid = "#72A4F9",
                        high = "#1e2f97", 
                        midpoint = 7,
                        breaks = c(min(mapcountriesdata$control, na.rm = TRUE), max(mapcountriesdata$control, na.rm = TRUE)), 
                        labels = c(paste(round(min(mapcountriesdata$control, na.rm = TRUE), 2), "(min)"), paste(round(max(mapcountriesdata$control, na.rm = TRUE), 2), "(max)")), 
                        na.value = "grey40") +
    theme(legend.title.align = 0,
          legend.title = element_text(size = 15),
          legend.position = c(.15, .3),
          legend.direction = "vertical",
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    labs(title = NULL,
         subtitle = NULL)

control_map

#Creating histograms

satisfaction_hist <- ggplot(merged_data) +
    geom_histogram(aes(x = satisfaction),
                   binwidth = .25,
                   fill = "#FFC125") +
    labs(title = "Distribution of Country Satisfaction Scores",
         x = "Average Life Satisfaction Scores (scale of 1-10)",
         y = "Number of Countries") +
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
    theme_minimal()
satisfaction_hist

control_hist <- ggplot(merged_data) +
    geom_histogram(aes(x = control),
                   binwidth = .25,
                   fill = "#72A4F9") +
    labs(title = "Distribution of Country Freedom & Control Scores",
         x = "Average Freedom & Control Scores (scale of 1-10)",
         y = "Number of Countries") +
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
    theme_minimal()
control_hist

#Creating scatterplot between satisfaction and control

scatter_data <- merged_data %>%
    select(c(country_2, satisfaction, control))
colnames(scatter_data) <- c("Country", "Satisfaction", "Control")

satisfaction_scatter <- ggplot(scatter_data, aes(Control, Satisfaction)) +
    geom_point() +
    geom_smooth(method = "lm", color = "#08a045", fullrange = TRUE, se = FALSE) +
    scale_x_continuous(breaks = 6:8) +
    scale_y_continuous(breaks = 4:9) +
    theme_minimal() +
    labs(title = "Relationship Between Satisfaction and Control\nAmong Countries", x = "Average freedom and control score (scale of 1-10)", y = "Average satisfaction score (scale of 1-10)") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "#4D68C7", face = "bold"),
          axis.text.y = element_text(color = "#F5BB15", face = "bold"),
          axis.title.x = element_text(color = "#4D68C7", face = "bold"),
          axis.title.y = element_text(color = "#F5BB15", face = "bold"), ) +
    stat_regline_equation(label.x = 7.7, label.y = 5.5) +
    stat_cor(aes(label=..rr.label..), label.x = 7.7, label.y = 5.25)
satisfaction_scatter


#Creating data for the mean satisfaction value for each control level
means_data <- as.data.frame(cbind(1:10,
                         c(mean(cleaned_data$Q49[cleaned_data$Q48 == 1]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 2]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 3]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 4]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 5]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 6]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 7]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 8]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 9]),
                           mean(cleaned_data$Q49[cleaned_data$Q48 == 10])
)))

colnames(means_data) <- c("control", "mean_satisfaction")


#Creating a heatmap
heatmap_data <- as.data.frame(table(cleaned_data$Q48, cleaned_data$Q49))
colnames(heatmap_data) <- c("control", "satisfaction", "count")
heatmap_data <- heatmap_data %>%
    mutate(means = means_data$mean_satisfaction[means_data$control == control])

individual_heatmap <- ggplot(heatmap_data, aes(x = control, y = satisfaction, fill = count)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("white", "blue", "blue"),
        breaks = c(min(heatmap_data$count), 2500, 5000, 7500, max(heatmap_data$count)),
        labels = c(paste(min(heatmap_data$count), " (min)"), "2500", "5000", "7500", paste(max(heatmap_data$count), " (max)"))) +
    labs(title = "Heatmap of Participant Counts", x = "Freedom & Control", y = "Satisfaction",
         fill = "Number of\nRespondents") +
    theme_minimal() +
    geom_point(aes(x = control, y = means),
               shape = 18,
               size = 3,
               color = "red")
#+ guides(aes(x = control, y = means), color = guide_legend(title = "points"))

individual_heatmap

box_plot <- ggplot(cleaned_data, aes(x = factor(Q48), y = Q49)) +
    geom_boxplot() +
    labs(title = "Distribution of Satisfaction Scores for\nEach Freedom & Control Score", x = "Freedom & Control", y = "Satisfaction") +
    theme_minimal() +
    scale_y_continuous(breaks = 1:10) +
    theme(axis.text.x = element_text(color = "#4D68C7", face = "bold"),
          axis.text.y = element_text(color = "#F5BB15", face = "bold"),
          axis.title.x = element_text(color = "#4D68C7", face = "bold"),
          axis.title.y = element_text(color = "#F5BB15", face = "bold"))


#Creating satisfaction difference map

mapdata <- mapdata %>%
    mutate(difference = satisfaction - happiness_level)

mapcountriesdata <- mapcountriesdata %>%
    mutate(difference = satisfaction - happiness_level)


difference_map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = difference)) +
    coord_fixed(ratio = 1.2) +
    theme_void() +
    scale_fill_gradient2(name = "Key", 
                         low = "red",
                         mid = "grey80",
                         high = "red",
                         midpoint = 0,
                         na.value = "grey40",
                         breaks = c(min(mapcountriesdata$difference, na.rm = TRUE), 0, max(mapcountriesdata$difference, na.rm = TRUE)), 
                         labels=c(round(min(mapcountriesdata$difference, na.rm = TRUE), 1), "0 (smallest difference)", paste(round(max(mapcountriesdata$difference, na.rm = TRUE), 1), "\n(largest difference)"))) +
    theme(legend.title.align = 0,
          legend.title = element_text(size = 15),
          legend.position = c(.15, .3),
          legend.direction = "vertical",
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    labs(title = "Differences in Satisfaction Scores from the\nWorld Values Survey and the World Happiness Report by Country",
         subtitle = NULL
    )

difference_map


#Creating double scatterplot
double_scatter <- ggplot(merged_data) +
    geom_text(aes(control, satisfaction, label = country_code),
              color = "#FFC125",
              position = position_jitter(width = 0.3, height = 0.3)) +
#    geom_smooth(aes(control, satisfaction), method = "lm", color = "grey70", fullrange = TRUE, se = FALSE) +
        scale_x_continuous(breaks = 5:9, limits = c(5, 9)) +
        scale_y_continuous(breaks = 2:9) +
    theme_minimal() +
    labs(title = "Relationship Between Satisfaction and Control\nAmong Countries", x = "Average freedom and control score (scale of 1-10)'", y = "Average satisfaction score (scale of 1-10)", caption = "Data source: World Values Survey (2017-2022), World Happiness Report (2023)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(control, happiness_level, label = country_code),
              color = "red",
              position = position_jitter(width = 0.3, height = 0.3)) +
    coord_equal()
#    geom_smooth(aes(control, happiness_level), method = "lm", color = "black", fullrange = TRUE, se = FALSE) +
#    stat_regline_equation(aes(control, happiness_level), label.x = 7.7, label.y = 5.5) +
#    stat_cor(aes(x = control, y = happiness_level, label=..rr.label..), label.x = 7.7, label.y = 5.25)
    
double_scatter

###################################################
###################################################
###################################################
###################################################
###################################################

library(shiny)
library(shinydashboard)


ui <- dashboardPage(
    dashboardHeader(#tags$li(a(
#        tags$style(HTML(".main-header { background-color: #72A4F9; }")),
#        "Header Text"
#    ))
        ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction"),
            menuItem("Satisfaction Overview", tabName = "satisfaction"),
            menuItem("Freedom & Control Overview", tabName = "control"),
            menuItem("Analysis", tabName = "analysis"),
            menuItem("Beware of Your Data", tabName = "beware")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "introduction",
                    fluidRow(
                        box(width = 8,
                            tags$h2("Welcome, Explorer...", style = "color: #08a045;"),
                            sliderInput("user_satisfaction", 
                                        label = "All things considered, how satisfied are you with your life as a whole these days?",
                                        min = 1, 
                                        max = 10,
                                        value = 5,
                                        step = 1,
                                        width = "100%"),
                            sliderInput("user_control", 
                                        label = "How much freedom of choice and control do you feel that you have over the way your life turns out?",
                                        min = 1, 
                                        max = 10,
                                        value = 5,
                                        step = 1,
                                        width = "100%"),
                            tags$p("The World Values Survey asked respondents these exact same questions as part of their mission to uncover how shifting values affect the social, political and economic development of societies over time. We now hope to use their data to answer a question of our own: How does having an internal or external locus of control impact life satisfaction, and what implications does this have for policy?"),
                            tags$p("Locus of control refers to the degree of control people feel that they have over their lives, in comparison to external forces. We believe that the second question above is measuring each participant's locus of control: those with answers closer to 1 have a more external locus, while those who answer closer to 10 have an internal locus."),
                            img(src = "locusofcontrol.png", width = "100%"),
                            tags$p("\nIn this series of visualizations, we will explore the relationship between locus of control and life satisfaction, both at the national level and at the individual level. We hope that our findings will motivate further research on how people can shift their locus of control inward, and how this can be brought about in society through policy.")
                        ))),
            
            # Second tab content
            tabItem(tabName = "satisfaction",
                    fluidRow(
                        box(width = 5,
                            tags$h2("Average Self-reported Life Satisfaction Levels by Country", style = "color: #FFC125;"),
                            tags$p(tags$em("Data source: World Values Survey (2017-2022)")),
                            tags$p("This map allows you to explore the trends in life satisfaction worldwide. As you can see, some countries, and even broader regions, have significantly higher average scores than others. Select a country from the drop-down menu to see its specific score.")),
                            plotOutput("satisfactionhist", height = "300px", width = "100%")),
                    fluidRow(
                        box(width = 4,
                            title = "Country",
                            selectInput("satisfaction_country", label = NULL, choices = sort(merged_data$country_2))),
                        box(width = 8,
                            tableOutput("satisfaction_table"))),
                    fluidRow(plotOutput("satisfactionmap", click = "plot_click"))
            ),
            
            # Third tab content
            tabItem(tabName = "control",
                    fluidRow(
                        box(width = 5,
                            tags$h2("Average Self-reported Freedom & Control Levels by Country", style = "color: #72A4F9;"),
                            tags$p(tags$em("Data source: World Values Survey (2017-2022)")),
                            tags$p("And now, on to our second variable, locus of control! Select a country from the drop-down menu to see its average locus of control score.")),
                        box(plotOutput("controlhist", height = "300px", width = "100%"))),
                    fluidRow(
                        box(width = 4,
                            title = "Country",
                            selectInput("control_country", label = NULL, choices = sort(merged_data$country_2))),
                        box(width = 8,
                            tableOutput("control_table"))
                    ),
                    fluidRow(plotOutput("controlmap", click = "plot_click"))
            ),
            
            #Fourth tab content
            tabItem(tabName = "analysis",
                    fluidRow(
                        box(width = 12,
                            tags$h2("Relationship Between Satisfaction and Locus of Control", style = "color: #08a045;"),
                            tags$p(tags$em("Data source: World Values Survey (2017-2022)")),
                            tags$p("Now that we’ve examined the distribution of both satisfaction and locus of control individually, it’s time to look at their relationship. As we can see below, there is a positive correlation between the variables both at a national and individual level.")),
                    ),
                    fluidRow(
                        box(plotOutput("satisfactionscatter", click = "plot_click"),
                             ),
                        tags$h3("By Country", style = "color: #08a045;"),
                        tags$p("At a country level, there is a clear positive correlation between average life satisfaction and average freedom & control scores."),
                        tags$h4("Click on a point on the graph to see which country it is."),
                        box(tableOutput("clicktable"))
                        ),
                    fluidRow(
                        box(plotOutput("boxplot"),
                        ),
                        tags$h3("By Individual", style = "color: #08a045;"),
                        tags$p("This boxplot demonstrates that as level of control increases, the average satisfaction score of the people within that control level also increases. In other words, locus of control and satisfaction are positively correlated at the individual level as well. The green tringle shows how your scores, which you input on the first page, compare with the rest of the distribution.")
                    )),
                    
            
            #Fifth tab content
            tabItem(tabName = "beware",
                    fluidRow(
                        box(width = 12,
                            tags$h1("Beware of Your Data", style = "color: red;"),
                            tags$p("The trends we have found have important implications, but we must remember that happiness and life satisfaction are nuanced and difficult to measure. We compared the measure of life satisfaction from the World Values Survey with a similar measure from the World Happiness Report (2023), and found very different results. This could be due to a difference in question wording:"),
                            tags$p("The World Values Survey asks, 'All things considered, how satisfied are you with your life as a whole these days?'"),
                            tags$p("The World Happiness Report uses the Cantril Ladder to rate participant well-being, which asks respondents to rate their own lives as though on a ladder, with the best possible life for them being a 10 and the worst possible life being a 0."),
                            tags$p("This shows that there are multiple ways of asking people to self-report their well-being, happiness, and life satisfaction, and different phrasing may produce different results. Another possible reason for the discrepancy may have been a difference in sampling methods or survey techniques."),
                            tags$p("The following map shows the differences in satisfaction scores between the two surveys by country. The scatterplot demonstrates that World Happiness Report Scores are on the whole lower than World Values Survey scores, and that the correlation between freedom & control and satisfaction is weaker for the World Happiness Report.")
                            )
                    ),
                    fluidRow(plotOutput("differencemap"),
                             plotOutput("doublescatter", height = "800px", width = "100%")
                    )
            )
        )
    )
)



server <- function(input, output) {
    output$satisfactionhist <- renderPlot({
        satisfaction_hist
    },
    width = function() 400, height = function() 300
    )
    
    output$controlhist <- renderPlot(
        control_hist
    )
    
    output$satisfactionmap <- renderPlot(
        satisfaction_map +
            geom_polygon(data = subset(mapdata, region == input$satisfaction_country),
                         aes(x = long, y = lat, group = group),
                         color = "red", 
                         fill = NA,
                         size = .7)
    )
    
    output$controlmap <- renderPlot(
        control_map +
            geom_polygon(data = subset(mapdata, region == input$control_country),
                         aes(x = long, y = lat, group = group),
                         color = "red", 
                         fill = NA,
                         size = .7)
    )
    
    output$satisfaction_table <- renderTable({
        country_table <- cbind(input$satisfaction_country,
                               round(mapcountriesdata$satisfaction[mapcountriesdata$country == input$satisfaction_country], 2), 
                               paste(round(mean(mapcountriesdata$satisfaction[mapcountriesdata$Continent == mapcountriesdata$Continent[mapcountriesdata$country == input$satisfaction_country]], na.rm = TRUE), 2), " (", mapcountriesdata$Continent[mapcountriesdata$country == input$satisfaction_country], ")", sep = ""),
                               round(mean(mapcountriesdata$satisfaction, na.rm = TRUE), 2))
        
        colnames(country_table) <- c("Country", "Average Satisfaction", "Average by Continent", "Average Worldwide")
        
        country_table
    }
    )
    
    output$control_table <- renderTable({
        country_table <- cbind(input$control_country,
                               round(mapcountriesdata$control[mapcountriesdata$country == input$control_country], 2), 
                               paste(round(mean(mapcountriesdata$control[mapcountriesdata$Continent == mapcountriesdata$Continent[mapcountriesdata$country == input$control_country]], na.rm = TRUE), 2), " (", mapcountriesdata$Continent[mapcountriesdata$country == input$control_country], ")", sep = ""),
                               round(mean(mapcountriesdata$control, na.rm = TRUE), 2))
        
        colnames(country_table) <- c("Country", "Average Freedom & Control", "Average by Continent", "Average Worldwide")
        
        country_table
    }
    )
    
    output$satisfactionscatter <- renderPlot({
        satisfaction_scatter
    })
    
    output$clicktable <- renderTable({
        nearPoints(scatter_data, input$plot_click, xvar = "Control", yvar = "Satisfaction")
    })
    
    output$boxplot <- renderPlot({
        box_plot +
            geom_point(aes(x = input$user_control, y = input$user_satisfaction),
                       shape = 17,
                       color = "#08a045",
                       size = 3,
                       ) +
            geom_text(aes(x = input$user_control, y = input$user_satisfaction),
                      label = "You!",
                      color = "#08a045",
                      vjust = -0.5,
                      hjust = 0.5, 
                      size = 5,
                      fontface = "bold")
    })
    
    output$differencemap <- renderPlot({
        difference_map
    })
    
    output$doublescatter <- renderPlot({
        double_scatter
    })
    
}


shinyApp(ui, server)