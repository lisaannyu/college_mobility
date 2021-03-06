source('helper.R')

server <- function(input, output, session) {
  school <- reactive({table_2 %>% filter(name == input$school)})
  
  output$college_level_table <- DT::renderDataTable({
    school() %>% 
      select(name, tier_name, state, par_median, k_median, female, 
             sat_avg_2013, sticker_price_2013, pct_stem_2000, 
             ends_with("_share_fall_2000")) %>% 
      rename_all(funs(stringr::str_to_title)) %>% 
      rename(Tier = Tier_name,
             `Median Parental Income` = Par_median,
             `Kids' Median Income` = K_median,
             `% Female` = Female,
             `Avg SAT (2013) / 1600` = Sat_avg_2013,
             `Sticker Price (2013)` = Sticker_price_2013,
             `% STEM (2000)` = Pct_stem_2000,
             `% Asian or Pacific (2000)` = Asian_or_pacific_share_fall_2000,
             `% Black (2000)` = Black_share_fall_2000,
             `% Hisp (2000)` = Hisp_share_fall_2000,
             `% Alien (2000)` = Alien_share_fall_2000
      ) %>% 
      mutate(`Median Parental Income` = scales::dollar(`Median Parental Income`),
             `Kids' Median Income` = scales::dollar(`Kids' Median Income`),
             # `% Female` = scales::percent(round(`% Female`, 3)),
             `Avg SAT (2013) / 1600` = round(`Avg SAT (2013) / 1600`, 0),
             `Sticker Price (2013)` = scales::dollar(`Sticker Price (2013)`),
             `% STEM (2000)` = scales::percent(round(`% STEM (2000)` / 100, 3))) %>% 
      mutate_at(vars(`% Female`, `% Asian or Pacific (2000)`, `% Black (2000)`, 
                     `% Hisp (2000)`, `% Alien (2000)`),
                funs(scales::percent(round(., 3))))
    
    # # school() %>% 
    # table_2 %>% 
    #   select(name, tier_name, state, par_median, k_median, female, 
    #          sat_avg_2013, sticker_price_2013, pct_stem_2000) %>% 
    #   rename_all(funs(stringr::str_to_title)) %>% 
    #   rename(Tier = Tier_name,
    #          `Median Parental Income` = Par_median,
    #          `Kids' Median Income` = K_median,
    #          `% Female` = Female,
    #          `Avg SAT (2013) / 1600` = Sat_avg_2013,
    #          `Sticker Price (2013)` = Sticker_price_2013,
    #          `% STEM (2000)` = Pct_stem_2000) %>% 
    #   mutate(`Median Parental Income` = scales::dollar(`Median Parental Income`),
    #          `Kids' Median Income` = scales::dollar(`Kids' Median Income`),
    #          `% Female` = scales::percent(round(`% Female`, 3)),
    #          `Avg SAT (2013) / 1600` = round(`Avg SAT (2013) / 1600`, 0),
    #          `Sticker Price (2013)` = scales::dollar(`Sticker Price (2013)`),
    #          `% STEM (2000)` = scales::percent(round(`% STEM (2000)` / 100, 3))) %>% 
    #   DT::datatable(options = list(pageLength = 1)) %>% 
    #   DT::formatStyle('Median Parental Income',
    #                   backgroundColor = DT::styleInterval(c(64200, 85200),
    #                     # quantile(table_2$par_median, c(1 / 3, 2 / 3)),
    #                                                       c("red", "yellow", "green")))
    # Not sure why this isn't working - ideally want 5 levels, not 3
  })
  
  output$major_share <- renderPlot({
    school() %>% 
      select(matches("pct_[A-Za-z]+_2000")) %>% 
      gather(key = "major", value = "percent") %>% 
      mutate(major = factor(major,
                            levels = c("pct_arthuman_2000",
                                       "pct_business_2000",
                                       "pct_health_2000",
                                       "pct_multidisci_2000",
                                       "pct_publicsocial_2000",
                                       "pct_stem_2000",
                                       "pct_socialscience_2000",
                                       "pct_tradepersonal_2000"
                            ), 
                            labels = c("Arts & Humanities",
                                       "Business",
                                       "Health & Medicine",
                                       "Multi/Interdisciplinary Studies",
                                       "Public and Social Services",
                                       "STEM",
                                       "Social Science",
                                       "Trades & Personal Services"))) %>%
      mutate(percent = percent / 100# ,
             # major = reorder(major, desc(percent))
      ) %>% 
      ggplot(aes(major, percent)) +
      geom_col() +
      geom_text(aes(label = scales::percent(round(percent, 2))),
                vjust = -0.5) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size = 15)) +
      labs(x = "Major (2000)",
           y = "Percent",
           title = "Share in Each Major in 2000")
  })
  
  plot_basic <- function(variable) {
    variable_value <- school() [variable][[1]][1]
    table_2 %>% 
      ggplot(mapping = aes_string(x = variable)) +
      geom_vline(xintercept = variable_value,
                 color = "red") +
      geom_histogram(bins = 20) +
      labs(y = "Count")
  }
  
  output$median <- renderPlot({
    plot_basic("par_median") +
      labs(x = "Median Parental Income for All Schools",
           title = paste0("Median Parental Income for ",
                          input$school, " is ", scales::dollar(school() %>% .$par_median)),
           subtitle = paste0("Average Income Percentile: ",
                             round(100 * school() %>% .$par_rank, 1))) +
      scale_x_continuous(labels = scales::dollar)
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
                          input$school)
           ) +
      scale_x_discrete(labels = paste0("Q", 1:5)) +
      scale_y_continuous(labels = scales::percent)
  })
  
  output$median_tier <- renderPlot({
    tier_name_school <- school() %>% .$tier_name
    par_med_school <- school() %>% .$par_median
    table_2 %>% 
      ggplot(mapping = aes(x = par_median, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = par_med_school, color = "red") +
      labs(x = "Median Parental Income", 
           y = "Density",
           title = paste0("Parental Income Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::dollar)
  })
  
  output$median_state <- renderPlot({
    state_school <- school() %>% .$state
    par_med_school <- school() %>% .$par_median
    table_2 %>% 
      ggplot(mapping = aes(x = par_median, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = par_med_school, color = "red") +
      labs(x = "Median Parental Income", 
           y = "Density",
           title = paste0("Parental Income Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::dollar)
  })
  
  output$k_median <- renderPlot({
    k_med_school <- school() %>% .$k_median
    plot_basic("k_median") +
      labs(x = "Median Individual Income at Age 34 for All Schools",
           title = paste0("Median Indiv Income at Age 34 for a student from ", 
                          input$school, " is ", scales::dollar(k_med_school)),
           subtitle = paste0("Average Income Percentile: ", 
                             round(100 * school() %>% .$k_rank, 1))) +
      scale_x_continuous(labels = scales::dollar)
  })
  
  output$k_median_tier <- renderPlot({
    tier_name_school <- school() %>% .$tier_name
    k_med_school <- school() %>% .$k_median
    table_2 %>% 
      ggplot(mapping = aes(x = k_median, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = k_med_school, color = "red") +
      labs(x = "Median Individual Income at Age 34 for All Schools", 
           y = "Density",
           title = paste0("Individual Income at Age 34 Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::dollar)
  })
  
  output$k_median_state <- renderPlot({
    state_school <- school() %>% .$state
    k_med_school <- school() %>% .$k_median
    table_2 %>% 
      ggplot(mapping = aes(x = k_median, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = k_med_school, color = "red") +
      labs(x = "Median Individual Income at Age 34 for All Schools", 
           y = "Density",
           title = paste0("Individual Income at Age 34 Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::dollar)
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
                          input$school)
      ) +
      scale_x_discrete(labels = paste0("Q", 1:5)) +
      scale_y_continuous(labels = scales::percent)
  })
  
  output$mr_kq5 <- renderPlot({
    mr_kq5 <- school()$mr_kq5_pq1
    
    plot_basic("mr_kq5_pq1") +
      labs(x = "Mobility Rate at the 20% Level for All Schools",
           title = paste0("MR 20% for ", input$school, ": ",
                          scales::percent(mr_kq5))) +
      scale_x_continuous(labels = scales::percent)
  })
  
  output$mr_ktop1 <- renderPlot({
    mr_ktop1 <- school()$mr_ktop1_pq1
    
    plot_basic("mr_ktop1_pq1") +
      labs(x = "Mobility Rate at the 1% Level for All Schools",
           title = paste0("MR 1% for ", input$school, ": ",
                             scales::percent(mr_ktop1))) +
      scale_x_continuous(labels = scales::percent)
  })
  
  output$mr_kq5_tier <- renderPlot({
    tier_name_school <- school()$tier_name
    kq5_school <- school()$mr_kq5_pq1
    
    table_2 %>% 
      ggplot(mapping = aes(x = mr_kq5_pq1, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = kq5_school, color = "red") +
      labs(x = "Mobility Rate at the 20% Level for All Schools", 
           y = "Density",
           title = paste0("MR 20% Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::percent)
  })
  
  output$mr_ktop1_tier <- renderPlot({
    tier_name_school <- school()$tier_name
    ktop1_school <- school()$mr_ktop1_pq1
    
    table_2 %>% 
      ggplot(mapping = aes(x = mr_ktop1_pq1, y = ..density.., fill = tier_name == tier_name_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = ktop1_school, color = "red") +
      labs(x = "Mobility Rate at the 1% Level for All Schools", 
           y = "Density",
           title = paste0("MR 1% Distribution: ", tier_name_school, " Schools vs. All Other Tiers")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other Tiers", tier_name_school), 
                        values = c(COLOR_OTHER_TIERS, COLOR_TIER)) +
      scale_x_continuous(labels = scales::percent)
  })  
  
  output$mr_kq5_state <- renderPlot({
    state_school <- school()$state
    kq5_school <- school()$mr_kq5_pq1
    
    table_2 %>% 
      ggplot(mapping = aes(x = mr_kq5_pq1, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = kq5_school, color = "red") +
      labs(x = "Mobility Rate at the 20% Level for All Schools", 
           y = "Density",
           title = paste0("MR 20% Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::percent)
  })
  
  output$mr_ktop1_state <- renderPlot({
    state_school <- school()$state
    ktop1_school <- school()$mr_ktop1_pq1
    
    table_2 %>% 
      ggplot(mapping = aes(x = mr_ktop1_pq1, y = ..density.., fill = state == state_school)) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.5) +
      geom_vline(xintercept = ktop1_school, color = "red") +
      labs(x = "Mobility Rate at the 1% Level for All Schools", 
           y = "Density",
           title = paste0("MR 1% Distribution: ", state_school, " Schools vs. All Other States")) +
      guides(fill = guide_legend("", reverse = TRUE)) +
      scale_fill_manual(labels = c("All Other States", state_school), 
                        values = c(COLOR_OTHER_STATES, COLOR_STATE)) +
      scale_x_continuous(labels = scales::percent)
  })
  
}