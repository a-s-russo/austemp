# Adelaide temperatures
A pipeline to produce graphs of Adelaide airport maximum and minimum temperatures in summer and winter, respectively.

## How to run

- Clone the repository: `git clone https://github.com/a-s-russo/Adelaide-temperatures.git`.
- Switch to the `pipeline` branch: `git switch pipeline`.
- Start an R session in the folder and use `renv::restore()` (and `renv::status()` as necessary) to install the project’s dependencies (or just open the R project file `Adelaide-temperatures.Rproj`).
- Run the pipeline sequentially with `targets::tar_make()` or in parallel with `targets::tar_make_future(workers = 2)` (the latter should be quicker).
- Inspect the images `graph-airport-summer.png` and `graph-airport-winter.png` for the output.