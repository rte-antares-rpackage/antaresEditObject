# Read adequacy patch config.yml into Antares (v8.5+)

Use this function to load config.yml used in older Antares versions for
adequacy patch. Areas in config will be updated to be included in
adequacy patch perimeter.

## Usage

``` r
convertConfigToAdq(opts = simOptions(), path = "default")
```

## Arguments

- opts:

  List. study options.

- path:

  Character. path to config.yml. Default points to
  "/user/adequacypatch/" in study

## See also

[`updateAdequacySettings`](updateAdequacySettings.md)
