# Australian temperatures
A dockerised pipeline to produce graphs of maximum and minimum temperatures in summer and winter for Australian capital cities.

## How to run

- Make sure [Docker](https://www.docker.com/products/docker-desktop/) is installed and working on your system.
- Clone the repository: `git clone https://github.com/a-s-russo/austemp.git`.
- Switch to the `docker` branch: `git switch docker`.
- Build the Docker image using `docker build -t austemp_image .` (do not forget the full stop on the end).
- Run the Docker container (and mount a volume) using `docker run --rm --name austemp_container -v /path/to/shared_folder:/home/austemp/shared_folder:rw austemp_image` (changing `/path/to/shared_folder` to a location on your computer).
- Inspect the contents of `shared_folder` for the output (see the file `weather_graphs.html`).