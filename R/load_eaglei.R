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

summarise_mo_hr <- function(eaglei_df,
                            summ_by,
                            # Time intervals are 15min per documentation, or 0.25h
                            data_interval = as.numeric(0.25, units = "hours")) {
  eaglei_df %>%
    mutate(month = month(run_start_time),
           hr = hour(run_start_time)) %>%
    mutate(out_hours = data_interval,
           customer_hours = customers_out * out_hours) %>%
    summarise(
      across(c(out_hours,customer_hours),
             \(x) sum(x, na.rm = TRUE)),
      .by = all_of(summ_by)
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
