# Run an Antares Simulation

![Antares API OK](figures/badge_api_ok.svg)

Run an ANTARES study

## Usage

``` r
runSimulation(
  name,
  mode = "economy",
  path_solver = getOption("antares.solver"),
  wait = TRUE,
  show_output_on_console = FALSE,
  parallel = TRUE,
  ...,
  opts = antaresRead::simOptions()
)
```

## Arguments

- name:

  Name of the simulation. In API mode, `name` will be used as
  `output_suffix` argument.

- mode:

  Simulation mode, can take value "economy", "adequacy" or "draft".

- path_solver:

  Character containing the Antares Solver path

- wait:

  Logical, indicating whether the R interpreter should wait for the
  simulation to finish, or run it asynchronously.

- show_output_on_console:

  Logical, indicating whether to capture the ANTARES log and show it on
  the R console.

- parallel:

  Logical. If `TRUE` the ANTARES simulation will be run in parallel mode
  (Work only with ANTARES v6.0.0 or more). In that case, the number of
  cores used by the simulation is the one set in
  advanced_settings/simulation_cores (see ANTARES interface).

- ...:

  Additional arguments (API only), such as `nb_cpu`, `time_limit`, ...
  See API documentation for all available options.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

In API mode it return a `list` with either the job id in case of success
of the command or details about the error produce. In non-API mode the
function does not return anything, it is used to launch an ANTARES
simulation.
