source('helper.R')

server <- function(input, output, session) {
  school <- reactive({table_2 %>% filter(name == input$school)})
  
  output$college_level_table <- DT::renderDataTable({
    # school() %>% 
    table_2 %>% 
      select(name, tier_name, state, par_median, k_median, female, 
             sat_avg_2013, sticker_price_2013, pct_stem_2000) %>% 
      rename_all(funs(stringr::str_to_title)) %>% 
      rename(Tier = Tier_name,
             `Parental Median Income` = Par_median,
             `Kids' Median Income` = K_median,
             `% Female` = Female,
             `Avg SAT (2013) / 1600` = Sat_avg_2013,
             `Sticker Price (2013)` = Sticker_price_2013,
             `% STEM (2000)` = Pct_stem_2000) %>% 
      mutate(`Parental Median Income` = scales::dollar(`Parental Median Income`),
             `Kids' Median Income` = scales::dollar(`Kids' Median Income`),
             `% Female` = scales::percent(round(`% Female`, 3)),
             `Avg SAT (2013) / 1600` = round(`Avg SAT (2013) / 1600`, 0),
             `Sticker Price (2013)` = scales::dollar(`Sticker Price (2013)`),
             `% STEM (2000)` = scales::percent(round(`% STEM (2000)` / 100, 3))) %>% 
      DT::datatable(options = list(pageLength = 1)) %>% 
      DT::formatStyle('Parental Median Income',
                      backgroundColor = DT::styleInterval(c(64200, 85200),
                        # quantile(table_2$par_median, c(1 / 3, 2 / 3)),
                                                          c("red", "yellow", "green")))
    # Not sure why this isn't working - ideally want 5 levels, not 3
  })
  
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