source('helper.R')

# Predict MR
summary(lm(mr_ktop1_pq1 ~ . - mr_kq5_pq1,
           data = table_2 %>% select(-super_opeid, -name, -cz, -czname, -cfips, -county,
                                     -shareimputed, -imputed)))

summary(lm(mr_ktop1_pq1 ~ .,
           data = table_2 %>% 
             select(tier, state, count, female, 
                    par_median, par_rank,
                    sat_avg_2013, sticker_price_2013, pct_stem_2000,
                    mr_ktop1_pq1)))

# Convert some vars
table_2$super_opeid <- as.integer(table_2$super_opeid)
table_10$region <- factor(table_10$region, 
                          labels = c("Northeast", "Midwest", "South", "West"))

# Regression (Top 1%)
summary(lm(mr_ktop1_pq1 ~ .,
  data = table_10 %>% 
    left_join(table_2 %>% select(super_opeid, mr_ktop1_pq1), 
                                by = "super_opeid") %>% 
    select(-super_opeid, -name, -fips, -cz, -czname, -cfips, -county, -zip, -tier,
           -scorecard_median_earnings_2011) ))

# Tier, Flagship, avg_facsal_2013, sat_avg_2013
# Coefficients tiny

# Regresion (Top 20%)
summary(lm(mr_kq5_pq1 ~ .,
           data = table_10 %>% 
             left_join(table_2 %>% select(super_opeid, mr_kq5_pq1), 
                       by = "super_opeid") %>% 
             select(-super_opeid, -name, -fips, -cz, -czname, -cfips, -county, -zip, -tier,
                    -scorecard_median_earnings_2011) ))
# State: GA, HI, multi, hbcu, scorecard_netprice_2013, endowment_pc_2000, 
#   asian_or_pacific_share_fall_2000, black_share_fall_2000, hispanic_share_fall_2000

# Lasso: probably not necessary since coefficients not huge

# Add cz-level information
file_path_cz <- "http://www.equality-of-opportunity.org/data/health/health_ineq_online_table_10.dta"
cz <- haven::read_dta(file_path_cz)
cz$cz <- as.integer(cz$cz)

table_preds <- table_10 %>% 
  left_join(table_2 %>% select(super_opeid, mr_kq5_pq1), 
            by = "super_opeid") %>% 
  left_join(cz %>% select(-fips, -czname, -statename, -stateabbrv, 
                          -matches("smoke"), -matches("obese"), -matches("exercise"),
                          -matches("diab")), by = "cz") %>% 
  select(-super_opeid, -name, 
         -fips, -cz, -czname, -cfips, -county, -state, -zip, -tier,
         -scorecard_median_earnings_2011)
summary(lm(mr_kq5_pq1 ~ .,
           data = table_preds))
# exp_instr_pc_2013, hbcu, ipeds_enrollment_2013, ipeds_enrollment_2000, endowment_pc_2000,
#   asian_or_pacific_share_fall_2000, black_share_fall_2000, hisp_share_fall_2000
#   pct_arthuman_2000, pct_business_2000, pct_health_2000, pct_publicsocial_2000,
#   pct_stem_2000, pct_socialscience_2000, pct_tradepersonal_2000, 
#   adjmortmeas_chfall30day, cs00_seg_inc_pov25, poor_share, frac_middleclass,
#   scap_ski90pcm, cs_frac_black, cs_frac_hisp, cs_labforce, pop_density, 
#   frac_traveltime_lt15, hhinc00, taxrate

# Check missingness
vars_w_missingness <- table_preds %>% 
  purrr::map_int(~ sum(is.na(.))) %>% 
  .[. != 0] %>% 
  names()

## Add column to denote missingness
add_missing_col <- function(df, var) {
  # Add 1/0 col, change NA to -1 in original column
  df[paste0(var, "_NA")] <- if_else(is.na(df[var]), 1, 0)
  
  df[var] <- if_else(is.na(df[var]), -1, df[var]) # don't think this will work
}

add_missing_col(table_preds, "female")

table_preds %>% 
  mutate(exp_instr_pc_2000_NA = if_else(is.na(exp_instr_pc_2000), 1, 0))

# Elastic net: probably correlated factors
library(glmnet)
cv.glmnet(y = table_preds$mr_kq5_pq1,
          x = table_preds %>% select(-mr_kq5_pq1),
          alpha = 0.5)
# lots of NAs - have to do something about that
table_preds %>% select(-mr_kq5_pq1) %>% map_int(~sum(is.na(.))) %>% .[. != 0]
