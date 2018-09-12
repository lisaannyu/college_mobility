source('helper.R')

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
      tabName = "college_level",
      h4(strong("College-level Characteristics"))
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