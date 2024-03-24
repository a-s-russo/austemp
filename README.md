# Australian temperatures
A pipeline to produce graphs of maximum and minimum temperatures in summer and winter for Australian capital cities.

## How to run

- Clone the repository: `git clone https://github.com/a-s-russo/austemp.git`.
- Switch to the `pipeline` branch: `git switch pipeline`.
- Start an R session in the folder and use `renv::restore()` (and `renv::status()` as necessary) to install the project’s dependencies (or just open the R project file `austemp.Rproj`).
- Run the pipeline sequentially with `targets::tar_make()` or in parallel with `targets::tar_make_future(workers = 2)` (the latter should be quicker).
- Inspect the file `weather_graphs.html` for the output.