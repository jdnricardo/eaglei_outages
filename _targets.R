# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(here)
library(tidycensus)
library(tidytable)
library(lubridate)

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c("here", "tidytable", "lubridate", "tidycensus", 
               "ggplot2"),
  format = "qs" # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = chosen_states,
    command = c(
      # Started with
      "Louisiana", "Maine",
      # Expanded to
      "Texas", "West Virginia", "Mississippi",
      "Michigan", "Kentucky", "Oregon"
    )
  ),
  tar_target(
    name = load_one_year,
    command = load_eaglei_year(2021)
  ),
  tar_target(
    name = states_eaglei,
    command = filter(load_one_year,
                     state %in% chosen_states)
  ),
  tar_target(
    name = county_pop,
    command = get_decennial(
      geography = "county",
      variable = "P1_001N",
      year = 2020)
  ),
  tar_target(
    name = clean_census,
    command = county_pop %>% 
      separate(
        NAME, c("county", "state"), sep = ", ", remove = TRUE
      ) %>% 
      # Some states, like Louisiana, use something other than "County"
      # e.g. Parish, or Census Area
      mutate(county = sub("\\s(County|Parish|Census.Area)", "", county))
  ),
  tar_target(
    name = states_census,
    command = filter(clean_census, 
                     state %in% chosen_states)
  ),
  tar_target(
    name = add_features,
    command = add_outage_id(states_eaglei)
  ),
  tar_target(
    name = county_saidi,
    command = calc_saidi(add_features,
                         states_census,
                         summ_by = c("state", "county"))
  ),
  tar_target(
    name = state_saidi,
    command = calc_saidi(add_features,
                         states_census,
                         summ_by = c("state"))
  )#,
  # tar_target(
  #   name = ny_ecdf,
  #   command = state_county_ecdf(county_month_hour,
  #                               c("New York"),
  #                               c("Kings", "Erie"))
  # ),
  # tar_target(
  #   name = eia_ecdf,
  #   command = state_ecdf(state_month_hour,
  #                        c("Maine",
  #                          "Texas",
  #                          "West Virginia",
  #                          "Mississippi",
  #                          "Louisiana",
  #                          "Michigan",
  #                          "Kentucky",
  #                          "Oregon",
  #                          # Least
  #                          "District of Columbia",
  #                          "Delaware",
  #                          "Florida",
  #                          "North Dakota",
  #                          "Nevada"))
  # )
)
