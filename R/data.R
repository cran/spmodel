#' Heavy metals in mosses near a mining road in Alaska, USA
#'
#' @description Heavy metals in mosses near a mining road in Alaska, USA.
#'
#' @format An \code{sf} object with 365 rows and 10 columns:
#'
#' \itemize{
#'   \item sample: A factor with a sample identifier. Some samples were
#'     replicated in the field or laboratory. As a result, there are 318 unique
#'     sample identifiers.
#'   \item field_dup: A factor representing field duplicate. Takes values \code{1}
#'     and \code{2}.
#'   \item lab_rep: A factor representing laboratory replicate. Takes values \code{1}
#'     and \code{2}.
#'   \item year: A factor representing year. Takes values \code{2001} and \code{2006}.
#'   \item sideroad: A factor representing direction relative to the haul road.
#'     Takes values \code{N} (north of the haul road) and \code{S} (south
#'     of the haul road).
#'   \item log_dist2road: The log of distance (in meters) to the haul road.
#'   \item log_Zn: The log of zinc concentration in moss tissue (mg/kg).
#'   \item geometry: \code{POINT} geometry representing coordinates in an Alaska
#'     Albers projection (EPSG: 3338). Distances between points are in meters.
#' }
#' @source Data were obtained from Peter Neitlich and Linda Hasselbach of the National
#'   Park Service.  Data were used in the publications listed in References.
#' @references
#' Neitlich, P.N., Ver Hoef, J.M., Berryman, S. D., Mines, A., Geiser, L.H.,
#'   Hasselbach, L.M., and Shiel, A. E. 2017. Trends in Spatial Patterns of Heavy
#'   Metal Deposition on National Park Service Lands Along the Red Dog Mine Haul
#'   Road, Alaska, 2001-2006. PLOS ONE 12(5):e0177936 DOI:10.1371/journal.pone.0177936
#'
#' Hasselbach, L., Ver Hoef, J.M., Ford, J., Neitlich, P., Berryman, S., Wolk B.
#'   and Bohle, T. 2005. Spatial Patterns of Cadmium, Lead and Zinc Deposition
#'   on National Park Service Lands in the Vicinity of Red Dog Mine, Alaska.
#'   Science of the Total Environment 348: 211-230.
"moss"



#' A caribou forage experiment
#'
#' @description A caribou forage experiment.
#'
#' @format A \code{tibble} with 30 rows and 5 columns:
#' \itemize{
#'   \item water: A factor representing whether water was added. Takes values
#'     \code{N} (no water added) and \code{Y} (water added).
#'   \item tarp: A factor representing tarp cover. Takes values \code{clear}
#'     (a clear tarp), \code{shade} (a shade tarp), and \code{none} (no tarp).
#'   \item z: The percentage of nitrogen.
#'   \item x: The x-coordinate.
#'   \item y: The y-coordinate.
#' }
#' @source These data were provided by Elizabeth Lenart of the Alaska Department
#'   of Fish and Game.  The data were used in the publication listed in References.
#' @references Lenart, E.A., Bowyer, R.T., Ver Hoef, J.M. and Ruess, R.W. 2002.
#'   Climate Change and Caribou: Effects of Summer Weather on Forage. Canadian
#'   Journal of Zoology 80: 664-678.
"caribou"

#' Estimated harbor-seal trends from abundance data in southeast Alaska, USA
#'
#' @description Estimated harbor-seal trends from abundance data in southeast Alaska, USA.
#'
#' @format A \code{sf} object with 149 rows and 2 columns:
#' \itemize{
#'   \item log_trend: The log of the estimated harbor-seal trends from abundance data.
#'   \item stock: A seal stock factor with two levels: 8 and 10. The factor levels indicate the
#'     type of seal stock (i.e., type of seal). Stocks 8 and 10 are two distinct stocks
#'     (out of 13 total stocks) in southeast Alaska.
#'   \item geometry: \code{POLYGON} geometry representing polygons in an Alaska
#'     Albers projection (EPSG: 3338).
#' }
#' @source These data were collected by the Polar Ecosystem Program of the Marine
#'   Mammal Laboratory of the Alaska Fisheries Science Center of NOAA Fisheries.
#'   The data were used in the publication listed in References.
#' @references
#' Ver Hoef, J.M., Peterson, E. E., Hooten, M. B., Hanks, E. M., and Fortin, M.-J. 2018.
#'   Spatial Autoregressive Models for Statistical Inference from Ecological Data.
#'   Ecological Monographs, 88: 36-59. DOI: 10.1002/ecm.1283.
"seal"

#' Sulfate atmospheric deposition in the conterminous USA
#'
#' @description Sulfate atmospheric deposition in the conterminous USA.
#'
#' @format An \code{sf} object with 197 rows and 2 columns.
#' \itemize{
#'   \item sulfate: Total wet deposition sulfate in kilograms per hectare.
#'   \item geometry: \code{POINT} geometry representing coordinates in a
#'     Conus Albers projection (EPSG: 5070). Distances between points are in meters.
#' }
#' @source
#' These data were used in the publication listed in References. Data were downloaded from the
#' National Atmospheric Deposition Program National Trends Network.
#' @references
#' Zimmerman, D.L. (1994). Statistical analysis of spatial data. Pages 375-402 in
#'   \emph{ Statistical Methods for Physical Science}, J. Stanford and
#'   S. Vardeman (eds.), Academic Press: New York.
"sulfate"

#' Locations at which to predict sulfate atmospheric deposition in the conterminous USA
#'
#' @description Locations at which to predict sulfate atmospheric deposition in the conterminous USA.
#'
#' @format An \code{sf} object with 197 rows and 1 column.
#' \itemize{
#'   \item geometry: \code{POINT} geometry representing coordinates in a
#'     Conus Albers projection (EPSG: 5070).
#' }
#' @source
#' These data were used in the publication listed in References. Data were downloaded from
#' the National Atmospheric Deposition Program National Trends Network.
#' @references
#' Zimmerman, D.L. (1994). Statistical analysis of spatial data. Pages 375-402 in
#'   \emph{ Statistical Methods for Physical Science}, J. Stanford and
#'   S. Vardeman (eds.), Academic Press: New York.
"sulfate_preds"

#' Moose counts and presence in Alaska, USA
#'
#' @description Moose counts and presence in Alaska, USA.
#'
#' @format An \code{sf} object with 218 rows and 5 columns.
#' \itemize{
#'   \item elev: The elevation.
#'   \item strat: A factor representing strata (used for sampling). Can take values \code{L} and \code{M}.
#'   \item count: The count (number) of moose observed.
#'   \item presence: A binary factor representing whether no moose were observed (value \code{0}) or at least one moose was observed
#'     (value \code{1}).
#'   \item geometry: \code{POINT} geometry representing coordinates in an Alaska
#'     Albers projection (EPSG: 3338). Distances between points are in meters.
#' }
#' @source
#' Alaska Department of Fish and Game, Division of Wildlife Conservation has released
#'   this data set under the CC0 license.
"moose"

#' Locations at which to predict moose counts and presence in Alaska, USA
#'
#' @description Locations at which to predict moose counts and presence in Alaska, USA.
#'
#' @format An \code{sf} object with 100 rows and 3 columns.
#' \itemize{
#'   \item elev: The elevation.
#'   \item strat: A factor representing strata (used for sampling). Can take values \code{L} and \code{M}.
#'   \item geometry: \code{POINT} geometry representing coordinates in an Alaska
#'     Albers projection (EPSG: 3338). Distances between points are in meters.
#' }
#' @source
#' Alaska Department of Fish and Game, Division of Wildlife Conservation has released
#'   this data set under the CC0 license.
"moose_preds"

#' National Lakes Assessment Data
#'
#' @description Lake data collected as part of the United States Environmental Protection
#'   Agency's 2012 and 2017 National Lakes Assessment and LakeCat.
#'
#' @format An \code{sf} object with 102 rows and 9 columns:
#'
#' \itemize{
#'   \item comid: A common identifier from NHDPlusV2.
#'   \item log_cond: The natural logarithm of lake conductivity.
#'   \item state: The US state: One of Arizona (AZ), Colorado (CO), Nevada (NV),
#'     Utah (UT).
#'   \item temp: Lake catchment 30-year average temperature (in degrees Celsius).
#'   \item precip: Lake watershed 30-year average precipitation (in centimeters).
#'   \item elev: Lake elevation (in meters).
#'   \item origin: Lake origin (human-made or natural).
#'   \item year: A factor representing year (2012 or 2017).
#'   \item geometry: \code{POINT} geometry representing coordinates in a NAD83
#'     projection (EPSG: 5070). Distances between points are in meters.
#' }
"lake"

#' Lakes Prediction Data
#'
#' @description Lake prediction data collected as part of the United States Environmental Protection
#'   Agency's 2012 and 2017 National Lakes Assessment and LakeCat.
#'
#' @format An \code{sf} object with 10 rows and 8 columns:
#'
#' \itemize{
#'   \item comid: A common identifier from NHDPlusV2.
#'   \item state: The US state: One of Arizona (AZ), Nevada (NV), Utah (UT).
#'   \item temp: Lake catchment 30-year average temperature (in degrees Celsius).
#'   \item precip: Lake watershed 30-year average precipitation (in centimeters).
#'   \item elev: Lake elevation (in meters).
#'   \item origin: Lake origin (human-made or natural).
#'   \item year: A factor representing year (2012 or 2017).
#'   \item geometry: \code{POINT} geometry representing coordinates in a NAD83
#'     projection (EPSG: 5070). Distances between points are in meters.
#' }
"lake_preds"

#' Texas Turnout Data
#'
#' @description Texas voter turnout data collected during the United States 1980 Presidential
#'   election.
#'
#' @format An \code{sf} object with 254 rows and 4 columns:
#'
#' \itemize{
#'   \item FIPS: Federal Information Processing System (FIPS) county codes.
#'   \item turnout: Proportion of eligible voters who voted.
#'   \item log_income: The natural logarithm of average per capita (in dollars) income.
#'   \item geometry: \code{POINT} geometry representing coordinates in a NAD83
#'     projection (EPSG: 5070). Distances between points are in meters.
#' }
#' @source
#' The data source is the \code{elect80} data set in the \code{spData} R package.
"texas"


#' Four Corners State Borders
#'
#' @description State borders for the four corners states in the United States:
#'   Arizona, Colorado, New Mexico, Utah.
#'
#' @format An \code{sf} object with 4 rows and 4 columns:
#'
#' \itemize{
#'   \item NAME: State name.
#'   \item STUSPS: State postal code.
#'   \item GEO.ID: State GEO.ID from the United States Census Bureau.
#'   \item geometry: \code{POINT} geometry representing coordinates in a NAD83
#'     projection (EPSG: 5070). Distances between points are in meters.
#' }
#' @source
#' The data source is the United States Census Bureau TIGER/Line Shapefiles.
"fc_borders"
# start with us_borders <- tigris::states(cb = TRUE, year = 2024)
