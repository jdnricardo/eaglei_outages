# Brelsford, C., Tennille, S., Myers, A., Chinthavali, S., Tansakul, V., Denman, M., Coletti, M., Grant, J., Lee, S., Allen, K., Johnson, E., Huihui, J., Hamaker, A., Newby, S., Medlen, K., Maguire, D., Dunivan Stahl, C., Moehl, J., Redmond, D. P., … Bhaduri, B. (2023). The Environment for Analysis of Geo-Located Energy Information’s Recorded Electricity Outages 2014-2023 (Version 2). figshare. https://doi.org/10.6084/m9.figshare.24237376.v2

load_eaglei_year <- function(year = "2021", folder = "data") {
  here("data", paste0("eaglei_outages_", year, ".csv")) %>%
    fread()
}

add_outage_id <- function(eaglei_df, outage_gap_min = 15) {
  outage_stamp <- stamp("20240101_000000", "%Y%Om%d_%H%M%S", quiet = TRUE)
  
  outage_gap_min <- as.numeric(outage_gap_min, units = "mins")
  
  eaglei_df %>%
    arrange(state, county, run_start_time) %>%
    filter(!is.na(customers_out)) %>%
    mutate(outage_id = if_else(
      lead(run_start_time) - run_start_time > outage_gap_min,
      paste(state, county, outage_stamp(run_start_time), sep = "_"),
      NA_character_
    )) %>%
    fill(outage_id,
         .direction = "up",
         .by = c(state, county)) %>%
    mutate(
      outage_id = case_when(
        !is.na(outage_id) ~ outage_id,
        run_start_time - lag(run_start_time) > outage_gap_min ~
          paste(state, county, outage_stamp(run_start_time), sep = "_"),
        .default = NA_character_
      )
    ) %>%
    fill(outage_id,
         .direction = "down",
         .by = c(state, county))
}

calc_saidi <- function(eaglei_df,
                       census_df,
                       # Time intervals are 15min per documentation, or 0.25h
                       data_interval = as.numeric(0.25, units = "hours"),
                       summ_by) {
  
  eaglei_df %>%
    summarise(
      # Within each outage interval, how many customers were affected
      tot_cust_hrs = sum(customers_out),
      # whether grouped by state, state+county, etc.
      .by = all_of(summ_by)
    ) %>% 
    # Join the relevant population data (may need to add year to possible vars)
    join_eaglei_census(census_df,
                       join_spec = intersect(summ_by,
                                             c("state", "county"))) %>% 
    # Turn number of intervals into units of time (hours by default)
    mutate(
      across(c(tot_cust_hrs), \(x) { x * data_interval}),
      # Then calculate SAIDI
      saidi = tot_cust_hrs / pop
    )
}

join_eaglei_census <- function(eaglei_df,
                               census_df,
                               join_spec) {
  
  # For now, preserving any outage data without population data
  left_join(
    eaglei_df,
    select(census_df, -variable) %>% 
      summarise(
        value = sum(value), 
        .by = all_of(join_spec)
      ) %>% 
      rename(pop = value),
    by = join_spec
  )
}

state_ecdf <- function(summ_eaglei, state) {
  stopifnot(all(state %in% summ_eaglei$state))
  
  summ_eaglei %>%
    filter(state %in% state) %>%
    ggplot(aes(group = state)) +
    stat_ecdf(aes(x = customers_out))
}

state_county_ecdf <- function(summ_eaglei, state, county) {
  stopifnot(all(state %in% summ_eaglei$state))
  stopifnot(all(county %in% summ_eaglei$county))
  
  summ_eaglei %>%
    filter(state %in% state, county %in% county) #%>%
  # ggplot(aes(group = county)) +
  # stat_ecdf(aes(x = customers_out))
}
