# Descargar y procesar variables climáticas históricas de WorldClim v2.1

Descarga datos climáticos históricos de WorldClim v2.1 y los procesa
según los parámetros especificados. Soporta múltiples variables
climáticas y resoluciones espaciales. Opcionalmente recorta los datos a
un área de interés (AOI).

## Usage

``` r
get_worldclim_historic(
  var = "bio",
  res = 10,
  aoi = NULL,
  retries = 3,
  timeout = 300,
  destination_dir = NULL
)
```

## Arguments

- var:

  Character. Variable climática a descargar. Opciones:

  - "bio" — Variables bioclimáticas.

  - "tavg" — Temperatura media.

  - "tmin" — Temperatura mínima.

  - "tmax" — Temperatura máxima.

  - "prec" — Precipitación.

  - "srad" — Radiación solar.

  - "wind" — Velocidad del viento.

  - "vapr" — Presión de vapor.

  Por defecto: \`"bio"\`.

- res:

  Numeric. Resolución espacial en minutos de arco. Valores válidos:
  \`0.5\`, \`2.5\`, \`5\`, \`10\`. Estos valores se mapean internamente
  a cadenas aceptadas por WorldClim:

  - 0.5 → "30s"

  - 2.5 → "2.5m"

  - 5 → "5m"

  - 10 → "10m"

  Por defecto: \`10\`.

- aoi:

  sf o SpatRaster opcional. Área de interés para recortar los datos.

- retries:

  Integer. Número de intentos de descarga en caso de fallo. Por defecto:
  \`3\`.

- timeout:

  Numeric. Tiempo máximo de descarga en segundos. Por defecto: \`300\`.

- destination_dir:

  Character. Carpeta donde guardar los datos descargados. Si NULL, se
  usa un directorio temporal.

## Value

Un objeto \`SpatRaster\` con las variables climáticas históricas. Si se
especifica \`aoi\`, los datos se recortan a esa área.

## References

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology, 37(12), 4302–4315.
[doi:10.1002/joc.5086](https://doi.org/10.1002/joc.5086)

## Examples

``` r
# \donttest{
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
nc <- sf::st_transform(nc, crs = 4326)

climate_historic <- get_worldclim_historic(
  var = "tmin",
  res = 5,
  aoi = nc
)
#> No destination_dir provided. Using temporary directory: /tmp/RtmpPIbJHF
#> Download URL: https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_5m_tmin.zip
#> Raster saved at: /tmp/RtmpPIbJHF/wc2.1_5m_tmin.tif
# }
```
