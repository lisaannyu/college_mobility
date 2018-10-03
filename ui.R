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
    menuItem("Mobility", tabName = "mobility"),
    menuItem("Methodology", tabName = "methodology")
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
          anonymized tax filings and financial-aid records from 
          10.8 million people, the most comprehensive study of college graduates
          to date.  All students in this dataset attended school
          between 1980 and 1991.  Other individual-level variables include: parental income 
          quantiles and personal income quantiles at age 30+ at the individual level.
          College-level variables include: Tier, Sticker Price in 2000 and 2013,
          % STEM in 2000, and % Asian, Hispanic, and Black.')
        )),
      HTML(paste("If interested in the details, a manuscript of the paper can be found here:",
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
      "There are 3 ways of evaluating a college:",
      tags$ul(
        tags$li("Access: Is this school accessible to those of lower parental incomes?"),
        tags$li("Success: Do students from this college have high incomes at age 34?"),
        tags$li("Mobility: How many students both have parental incomes in the bottom 20% and end up in the top income brackets at age 34?")
      ),
      br(),
      "Most schools have high access (i.e. have a large share of students from the bottom 20%) or
      high success (i.e. students earn incomes in the top income brackets at age 34), 
      but only a few have high mobility, relative to what is expected.  
      Mobility is essentially access x success.",
      br(),
      br(),
      "I wanted to use this valuable public data source to see if I could create
      a tool allowing users to evaluate a particular college.  I used statistical
      analyses to help determine which variables were most important."
        ),
    tabItem(
      tabName = "college_level",
      h4(strong("College-level Characteristics")),
      DT::dataTableOutput("college_level_table"),
      plotOutput("major_share")
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
    ),
    tabItem(
      tabName = "methodology",
      h4(strong("Methodology: What predicts high mobility rates?")),
      "Both journalists in the articles mentioned in the Introduction page
      mentioned certain schools with high mobility rates.  However, neither
      had ideas about what those schools were doing correctly that other schools
      could hope to emulate.  There are probably other reasons besides these not
      measured in the dataset, but this is my best attempt at identifying
      important predictors of mobility.",
      br(),
      br(),
      "Mobility rate at the 20% level is a different animal from mobility rate
      at the 1% level, so I ran an elastic net regression for each separately.",
      br(),
      br(),
      "MR 20% important factors:",
      tags$ul(
        tags$li("Asian or Pacific share (2000)"),
        tags$li("Hispanic share (2000)"),
        tags$li("Black share (2000)"),
        tags$li("Female share"),
        tags$li("Art and Humanities share (2000)"),
        tags$li("STEM share (2000)"),
        tags$li("Midwest (relative to West)")
      ),
      br(),
      "MR 1% important factors:",
      tags$ul(
        tags$li("Tier: Ivy Plus"),
        tags$li("Asian or Pacific share (2000)"),
        tags$li("Other elite schools (public and private)"),
        tags$li("Barron's selectivity index"),
        tags$li("Instructional Expenditures per Student (2013)"),
        tags$li("STEM share (2000)"),
        tags$li("Tier: Highly selective public"),
        tags$li("SAT Avg (2013)")
      ),
      br(),
      "I will let you guess whether each of those factors has a positive or 
      negative impact on MR%",
      br(),
      br(),
      "Since Tier Name is an important factor, I created these two plots to 
      show visually what MR x% represents.",
      br(),
      br(),
      img(src = "mr_20.png", width = 1000, align = "center"),
      br(),
      img(src = "mr_1.png", width = 1000, align = "center"),
      br(),
      br(),
      "For those interested, this is my workflow:",
      tags$ol(
        tags$li("Exclude variables definitionally related to the outcome, 
                including all variables related to parental income or student's 
                income at age 34"),
        tags$li("Remove observations missing MR 20% or MR 1%, which amounts to 
                about 11% of the colleges"),
        tags$li("Deal with missingness: Determine if variables are missing completely
                at random.  If so, add columns to denote missingness for columns with missingness.
                e.g.  If `female` has missing values, replace those missing
                values with -1 and add `female_NA` which is 1 when
                `female` is missing, and 0 otherwise."),
        tags$li("Transform all variables so the minimum is 0 and the maximum is 1:
                binary-code categorical variables and take the percentile of
                continuous variables"),
        tags$li("Elastic net: Select the optimal level for alpha, which denotes
                whether the result is closer to ridge or lasso, and examine the
                coefficients in the top decile by absolute value")
      )
    )
    )
  )
ui <- dashboardPage(header, side, body)