source('helper.R')

header <- dashboardHeader(title = "College Mobility")
side <- dashboardSidebar(
  selectInput('school', label = 'School Name', choices = table_2$name,
              selected = "Stanford University"),
  sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItem("College-level Characteristics", tabName = "college_level"),
    menuItem("Access", tabName = "access"),
    menuItem("Success", tabName = "success"),
    menuItem("Mobility", tabName = "mobility")
  ))
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "intro",
      h4(strong("What effect does college have on income mobility?")),
      # Link: (http://www.equality-of-opportunity.org/data/index.html#college)
      HTML(paste0(
        p('A subset of the "Equality of Opportunity" research team, a 
          collaboration directed by Raj Chetty of Stanford, John N. Friedman of 
          Brown, and Nathaniel Hendren of Harvard, examined this question by collecting
          millions of anonymized tax filings and financial-aid records from 
          10.8 million people, the most comprehensive study of college graduates
          to date.  All students in this dataset attended school
          between 1980 and 1991.  Other variables include: parental income 
          quantiles and personal income quantiles at age 30+.')
        )),
      HTML(paste("A manuscript of the paper can be found here:",
                 a('"Mobility Report Cards: The Role of Colleges in Intergenerational Mobility',
                   href = "http://www.equality-of-opportunity.org/papers/coll_mrc_paper.pdf",
                   targe = "_blank")),
           '."'),
      br(),
      br(),
      HTML(paste("Alternatively, this study was described for the layperson by David Leonhardt in the", 
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
      "I wanted to use this valuable public data source to see if I could create
      a tool allowing users to evaluate a particular college.  I used statistical
      analyses to help determine which variables were most important.",
      br(),
      br(),
      "There are 3 ways of evaluating a college:",
      tags$ul(
        tags$li("Access: Who attends this college? Is this school accessible to those of lower income?"),
        tags$li("Success: Do graduates from this college have high incomes after graduation?"),
        tags$li("Mobility: How many students from lower parental income backgrounds end up in higher income brackets after graduation?")
      ),
      br(),
      "Mobility is essentially access x success."
        ),
    tabItem(
      tabName = "college_level",
      h4(strong("College-level Characteristics")),
      DT::dataTableOutput("college_level_table")
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
      ),
      "Note: If parental income were unrelated to admission, each school would have 20% of its students from each quintile"
    ),
    tabItem(
      tabName = "success",
      h4(strong("Success: how do students fare (in terms of income) after college?")),
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
      "Note: If individual income were unrelated to admission, each school would have 20% of its students from each quintile"
    ),
    tabItem(
      tabName = "mobility",
      h4(strong("Mobility: Does this school help move students up financially?")),
      HTML(paste0("A ", em("mobility rate"), 
                  " at the ", em("x"),
                  "% level is defined as the percent of all students whose parental income is in the bottom 20% AND whose personal income at age 34 is in the top ",
                  em("x"), "%.")),
      br(),
      br(),
      "There are two levels of mobility rates (MR) in this dataset:",
      tags$ul(
        tags$li("mobility rate at the 20% level (denoted MR 20%): the percent of all students for a given college whose parental income is in the bottom 20% and whose personal income is in the top 20%"),
        tags$li("mobility rate at the 1% level (denoted MR 1%): the percent of all students for a given college whose parental income is in the bottom 20% and whose personal income is in the top 1%")
      ),
      tabBox(
        title = "MR 20%",
        id = "mr_20%",
        tabPanel("Overall", plotOutput("mr_kq5")),
        tabPanel("Compared to others in its tier", plotOutput("mr_kq5_tier")),
        tabPanel("Compared to others in its state", plotOutput("mr_kq5_state"))
      ),
      tabBox(
        title = "MR 1%",
        id = "mr_1%",
        tabPanel("Overall", plotOutput("mr_ktop1")),
        tabPanel("Compared to others in its tier", plotOutput("mr_ktop1_tier")),
        tabPanel("Compared to others in its state", plotOutput("mr_ktop1_state"))
      )
    )
    )
  )
ui <- dashboardPage(header, side, body)