# API methods

API methods

## Usage

``` r
api_patch(opts, endpoint, ..., default_endpoint = "v1/studies")
```

## Arguments

- opts:

  Antares simulation options or a `list` with an `host = ` slot.

- endpoint:

  API endpoint to interrogate, it will be added after
  `default_endpoint`. Can be a full URL (by wrapping ìn
  [`I()`](https://rdrr.io/r/base/AsIs.html)), in that case
  `default_endpoint` is ignored.

- ...:

  Additional arguments passed to API method
  ([`httr::PATCH()`](https://httr.r-lib.org/reference/PATCH.html)).

- default_endpoint:

  Default endpoint to use.

## Value

Response from the API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple example to update st-storages properties 

# read existing study 
opts <- setSimulationPath("path_to_the_study", "input")

# make list of properties
prop <- list(efficiency = 0.5,
  reservoircapacity = 350, 
  initialleveloptim = TRUE)
  
# convert to JSON
body <- jsonlite::toJSON(prop,
  auto_unbox = TRUE)   
  
# send to server (see /apidoc)
api_patch(opts = opts, 
  endpoint = file.path(opts$study_id, 
                     "areas", 
                      area,
                     "storages",
                     cluster_name), 
 body = body, 
 encode = "raw")   

} # }
```
