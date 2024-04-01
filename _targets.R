# Load pipeline infrastructure libraries
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# Load library into the pipeline
tar_option_set(packages = "austemp")

# Define function to load package datasets
read_data <- function(data_name, package_name) {
  temp <- new.env(parent = emptyenv())
  
  data(list = data_name,
       package = package_name,
       envir = temp)
  
  get(data_name, envir = temp)
}

# Create pipeline
list(
  tar_target(Adelaide,
             read_data("Adelaide", "austemp")),
  tar_target(Brisbane,
             read_data("Brisbane", "austemp")),
  tar_target(Canberra,
             read_data("Canberra", "austemp")),
  tar_target(Darwin,
             read_data("Darwin", "austemp")),
  tar_target(Hobart,
             read_data("Hobart", "austemp")),
  tar_target(Melbourne,
             read_data("Melbourne", "austemp")),
  tar_target(Perth,
             read_data("Perth", "austemp")),
  tar_target(Sydney,
             read_data("Sydney", "austemp")),
  tar_render(analyse,
             "weather_graphs.Rmd",
             output_dir = "/home/austemp/pipeline_output")
)