# Brelsford, C., Tennille, S., Myers, A., Chinthavali, S., Tansakul, V., Denman, M., Coletti, M., Grant, J., Lee, S., Allen, K., Johnson, E., Huihui, J., Hamaker, A., Newby, S., Medlen, K., Maguire, D., Dunivan Stahl, C., Moehl, J., Redmond, D. P., … Bhaduri, B. (2023). The Environment for Analysis of Geo-Located Energy Information’s Recorded Electricity Outages 2014-2023 (Version 2). figshare. https://doi.org/10.6084/m9.figshare.24237376.v2

load_eaglei_year <- function(year = "2021", folder = "data") {
  here("data", paste0("eaglei_outages_", year, ".csv")) %>%
    fread() %>%
    # Data needs to be chronologically sorted
    arrange(state, county, run_start_time)
}

stamp_outage <- function(x, y = NULL, ts) {
  
  if(!missing(y)) x <- paste(x, y, sep = "_")
  
  outage_stamp <- stamp("20240101_000000", 
                        "%Y%Om%d_%H%M%S",
                        quiet = TRUE)
  
  paste(
    x,
    outage_stamp(ts),
    sep = "_"
  )
}

calc_saifi <- function(eaglei_df,
                       census_df,
                       summ_by,
                       outage_gap = "days") {
  
  stopifnot("Need to aggregate at county level" = "county" %in% summ_by)
  
  eaglei_df %>%
    # Rolling up 15min interval to hours or days to determine how many customers
    # are affected by each separate outage, 
    mutate(
      period = lubridate::floor_date(run_start_time, outage_gap)
    ) %>% 
    # i.e. outages are separated by the period specified in fn arg 'outage_gap'
    summarise(
      any_outages = any(!is.na(customers_out)),
      max_customers = max(customers_out, na.rm = TRUE) %>% as.integer(),
      .by = c("state", "county", "period")
    ) %>%
    # Take the max number of customers affected in each outage
    summarise(interrupted_customers = sum(max_customers, na.rm = TRUE),
              .by = all_of(summ_by)) %>% 
    # Join the relevant population data (may need to add year to possible vars)
    join_eaglei_census(census_df,
                       join_spec = intersect(summ_by,
                                             c("state", "county"))) %>% 
    # Divide number of customers interrupted by customers served, again we are
    # still assuming utilities serve entire counties
    mutate(
      saifi = interrupted_customers / pop
    )
}

calc_saidi <- function(eaglei_df,
                       census_df,
                       summ_by,
                       # Time intervals are 15min per documentation, or 0.25h
                       data_interval = as.numeric(0.25, units = "hours")) {
  
  eaglei_df %>%
    filter(!is.na(customers_out)) %>%
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
