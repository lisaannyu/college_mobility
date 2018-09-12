library(tidyverse)
library(shiny)
library(shinydashboard)
library(shiny)
# Parameters
# Table 1: Baaseline Cross-Sectional Estimates by College
table_2_file_path <- 
  "http://www.equality-of-opportunity.org/data/college/mrc_table2.dta"

table_2 <- haven::read_dta(table_2_file_path) %>% 
  arrange(name)

COLOR_TIER <- "#00BFC4"
COLOR_OTHER_TIERS <- "#F8766D"
COLOR_STATE <- "#F03B20"
COLOR_OTHER_STATES <- "#FFFF00"

header <- dashboardHeader(title = "")
side <- dashboardSidebar(
  selectInput('school', label = 'School Name', choices = table_2$name,
              selected = "Stanford University"),
  sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItem("Access", tabName = "access"),
    menuItem("Success", tabName = "success"),
    menuItem("Mobility", tabName = "mobility")
    ))
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "intro",
      h4(strong("What effect does college have on mobility?")),
      # Link: (http://www.equality-of-opportunity.org/data/index.html#college)
      HTML(paste0(
        p('Dr. Raj Chetty and his "Equality of Opportunity" research team examined 
        this question by collecting
          millions of anonymized tax filings and financial-aid records from 
          10.8 million people, the most comprehensive study of college graduates
          to date.  All students in this dataset attended school
          between 1980 and 1991.  Other variables include: parental income 
          quantiles and personal income quantiles at age 30+.')
      )),
      HTML(paste("This study was described for the layperson by David Leonhardt in the", 
        tags$em("New York Times ("), 
        a('"America\'s Great Working Class Colleges"',
          href = "https://www.nytimes.com/2017/01/18/opinion/sunday/americas-great-working-class-colleges.html",
          target = "_blank"), 
        ") and Dylan Matthew in", em("Vox"), "(",
        a('"These colleges are better than Harvard at making poor kids rich"',
          href = "http://www.vox.com/policy-and-politics/2017/2/28/14359140/chetty-friedman-college-mobility",
          target = "_blank")),
        ")."),
      br(),
      br(),
      "There are 3 ways of evaluating a college:",
      tags$ul(
        tags$li("Access: Who attends this college? Is this school accessible to those of lower income?"),
        tags$li("Success: Do graduates from this college have high incomes after graduation?"),
        tags$li("Mobility: How many students from lower parental income backgrounds end up in higher income brackets after graduation?")
      )
    ),
    tabItem(
      tabName = "access",
      h4(strong("Access: what kinds of students attend this school?")),
      h4("Parental Income"),
      tabBox(
        title = "",
        id = "par_med",
        tabPanel("Overall", plotOutput("median")),
        tabPanel("Compared to others in its tier", plotOutput("median_tier")),
        tabPanel("Compared to others in its state", plotOutput("median_state"))
      ),
      tabBox(
        title = "",
        id = "quintile",
        tabPanel("Quintile Distribution", plotOutput("quintiles"))
      )
    ),
    tabItem(
      tabName = "success",
      h4(strong("Success: how do students fare after college?")),
      h4("Individual Income at Age 34"),
      tabBox(
        title = "",
        id = "kid_med",
        tabPanel("Overall", plotOutput("k_median")),
        tabPanel("Compared to others in its tier", plotOutput("k_median_tier")),
        tabPanel("Compared to others in its state", plotOutput("k_median_state"))
      ),
      tabBox(
        title = "",
        id = "quintile",
        tabPanel("Quintile Distribution", plotOutput("k_quintiles"))
      ),
      h4("Married in 2014"),
      tabBox(
        title = "",
        id = "married",
        tabPanel("Overall", plotOutput("married")),
        tabPanel("Compared to others in its tier", plotOutput("k_married_tier")),
        tabPanel("Compared to others in its state", plotOutput("k_married_state"))
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    tabItem(
      tabName = "mobility",
      h4(strong("Mobility: Does this school help move students up financially?")),
      tabBox(
        title = "",
        id = "mr",
        tabPanel("Q1", plotOutput("mr_pq1")),
        tabPanel("Top 1%", plotOutput("mr_top1"))
      )
    )
  )
)
ui <- dashboardPage(header, side, body)

server <- function(input, output, session) {
  school <- reactive({table_2 %>% filter(name == input$school)})
  
  plot_basic <- function(variable) {
    variable_value <- school() [variable][[1]][1]
    table_2 %>% 
      ggplot(mapping = aes_string(x = variable)) +
      geom_vline(xintercept = variable_value,
                 color = "red") +
      geom_histogram(bins = 20)
  }
  
  output$median <- renderPlot({
    plot_basic("par_median") +
      labs(x = "Median Parental Income for all Schools",
           y = "Count",
           title = paste0("Median Parental Income for a student from ",
                          input$school, " is $", scales::comma(school() %>% .$par_median)),
           subtitle = paste0("Average Income Percentile: ",
                             round(100 * school() %>% .$par_rank, 1))) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$quintiles <- renderPlot({
    table_2 %>% 
      filter(name == input$school) %>% 
      select(starts_with("par_q")) %>% 
      gather(key = "quintile", value = "percent") %>% 
      ggplot(mapping = aes(x = quintile, y = percent)) +
      geom_hline(yintercept = 0.2, color = "red") +
      geom_col() +
      labs(x = "Parental Income Quintile (1: lowest, 5: highest)",
           y = "Percent",
           title = paste0("Percent of students from each income quintile at ",
                          input$school),
           subtitle = "If parental income were unrelated to admission, each school would have 20% of its students from each quintile"
      ) +
      scale_x_discrete(labels = paste0("Q", 1:5))
  })
  
  output$median_tier <- renderPlot({
    tier_name_school <- school() %>% .$tier_name
    par_med_school <- school() %>% .$par_median
    table_2 %>% 
      ggplot(mapping = aes(x = par_median, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = par_med_school, color = "red") +
      labs(x = "Parental Median Income", 
           y = "Density",
           title = paste0("Parental Income Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$median_state <- renderPlot({
    state_school <- school() %>% .$state
    par_med_school <- school() %>% .$par_median
    table_2 %>% 
      ggplot(mapping = aes(x = par_median, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = par_med_school, color = "red") +
      labs(x = "Parental Median Income", 
           y = "Density",
           title = paste0("Parental Income Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_median <- renderPlot({
    k_med_school <- school() %>% .$k_median
    table_2 %>% 
      ggplot(mapping = aes(x = k_median)) +
      geom_vline(xintercept = k_med_school,
                 color = "red") +
      geom_histogram(bins = 20) +
      labs(x = "Median Indiv Income at age 34 for all Schools",
           y = "Count",
           title = paste0("Median Individual Income at age 34 for a student from ", 
                          input$school, " is $", scales::comma(k_med_school)),
           subtitle = paste0("Average Income Percentile: ", 
                             round(100 * school() %>% .$k_rank, 1))) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_median_tier <- renderPlot({
    tier_name_school <- school() %>% .$tier_name
    k_med_school <- school() %>% .$k_median
    table_2 %>% 
      ggplot(mapping = aes(x = k_median, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = k_med_school, color = "red") +
      labs(x = "Median Individual Income at age 34 for all Schools", 
           y = "Density",
           title = paste0("Individual Income at age 34 Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_median_state <- renderPlot({
    state_school <- school() %>% .$state
    k_med_school <- school() %>% .$k_median
    table_2 %>% 
      ggplot(mapping = aes(x = k_median, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = k_med_school, color = "red") +
      labs(x = "Median Individual Income at age 34 for all Schools", 
           y = "Density",
           title = paste0("Individual Income at age 34 Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_quintiles <- renderPlot({
    table_2 %>% 
      filter(name == input$school) %>% 
      select(starts_with("k_q")) %>% 
      gather(key = "quintile", value = "percent") %>% 
      ggplot(mapping = aes(x = quintile, y = percent)) +
      geom_hline(yintercept = 0.2, color = "red") +
      geom_col() +
      labs(x = "Student Income Quintile (1: lowest, 5: highest)",
           y = "Percent",
           title = paste0("Percent of students from each income quintile at ",
                          input$school),
           subtitle = "If income were unrelated to school, each school would have 20% of its students from each quintile"
      ) +
      scale_x_discrete(labels = paste0("Q", 1:5))
  })
  
  output$married <- renderPlot({
    table_2 %>% 
      ggplot(mapping = aes(x = k_married)) +
      geom_vline(xintercept = school() %>% .$k_married,
                 color = "red") +
      geom_histogram(bins = 20) +
      labs(x = "Fraction married in 2014 for all Schools",
           y = "Count",
           title = paste0(round(100 * school() %>% .$k_married, 1),
                          "% students from ", 
                          input$school, " were married in 2014")) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_married_tier <- renderPlot({
    tier_name_school <- school() %>% .$tier_name
    table_2 %>% 
      ggplot(mapping = aes(x = k_married, 
                           y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 30, alpha = 0.5) +
      geom_vline(xintercept = school() %>% .$k_married, color = "red") +
      labs(x = "Fraction married in 2014 for all Schools", 
           y = "Density",
           title = paste0("Married in 2014 Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$k_married_state <- renderPlot({
    state_school <- school() %>% .$state
    table_2 %>%
      ggplot(mapping = aes(x = k_married,
                           y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = school() %>% .$k_married, color = "red") +
      labs(x = "Fraction married in 2014 for all Schools",
           y = "Density",
           title = paste0("Fraction married in 2014 Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school),
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$mr_pq1 <- renderPlot({
    plot_basic("mr_kq5_pq1") +
      labs(x = "Percent of kids in top 20% that came from bottom 20%")
  })
  
  output$mr_top1 <- renderPlot({
    plot_basic("mr_ktop1_pq1") +
      labs(x = "Percent of kids in top 1% that came from bottom 20%")
  })
  
}
shinyApp(ui, server)