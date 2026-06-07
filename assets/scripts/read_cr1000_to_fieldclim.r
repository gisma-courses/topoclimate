# -----------------------------------------------------------------------------
# read_cr1000_to_fieldclim.R
# -----------------------------------------------------------------------------
# Purpose:
#   Read a Campbell Scientific CR1000 / TOA5 .dat file and convert the relevant
#   measurement columns into a fieldClim `weather_station` object.
#
# Why this is needed:
#   Campbell logger files are not ordinary CSV files. A TOA5 file usually starts
#   with four header lines:
#
#   1. logger metadata
#   2. column names
#   3. units
#   4. aggregation / processing information
#
#   The real measurements start only after these four lines. This function reads
#   the file correctly, parses the timestamp, converts numeric columns, maps the
#   logger column names to the names expected by fieldClim, and returns a
#   `weather_station` object.
#
# Important:
#   This function does not repair, fill, interpolate, smooth, or quality-control
#   the measurements. Missing values remain NA. Suspicious values remain visible.
#   That is intentional because fieldClim's inspection workflow expects raw,
#   traceable input fields.
# -----------------------------------------------------------------------------

read_cr1000_to_fieldclim <- function(
    file,

    # -------------------------------------------------------------------------
    # Required site metadata.
    # fieldClim needs these for several radiation and heat-flux methods.
    # Use decimal degrees for lon/lat and metres above sea level for elev.
    # -------------------------------------------------------------------------
    lon,
    lat,
    elev,

    # -------------------------------------------------------------------------
    # Time handling.
    # The uploaded CR1000 file uses timestamps like "14.10.2022 14:55".
    # The default format therefore is day.month.year hour:minute.
    # Set tz explicitly to the station time zone.
    # -------------------------------------------------------------------------
    tz = "UTC",
    timestamp_col = "TIMESTAMP",
    timestamp_format = "%d.%m.%Y %H:%M",

    # -------------------------------------------------------------------------
    # Site / surface assumptions used by fieldClim methods.
    # surface_type should use a value known by fieldClim, e.g. "field".
    # z1 and z2 are the two measurement heights of the lower and upper sensors.
    # obs_height is used by methods that need a reference/observation height.
    # -------------------------------------------------------------------------
    surface_type = "field",
    z1 = 2,
    z2 = 10,
    obs_height = z1,

    # -------------------------------------------------------------------------
    # Column mapping from this CR1000 file to fieldClim names.
    # These defaults match the uploaded file CR1000_Table1_26Mar.dat.
    # Change them only if the logger program or column names differ.
    # -------------------------------------------------------------------------
    temp_col_1 = "AirTC_Avg",
    temp_col_2 = "AirTC_2_Avg",
    rh_col_1 = "RH_Avg",
    rh_col_2 = "RH_2_Avg",
    wind_col_1 = "WSData_S_WVT",
    wind_col_2 = "WSData_S2_WVT",
    wind_dir_col = "WSData_D1_WVT",
    pressure_col = "BP_hPa",
    rain_col = "rain_mm_Tot",
    rad_bal_col = "NetTot_Avg",
    soil_flux_col = "hfp01sc_1_Avg",

    # -------------------------------------------------------------------------
    # Optional radiation component columns.
    # These are stored only for inspection/export. The main fieldClim heat-flux
    # paths primarily need rad_bal and soil_flux.
    # Set include_radiation_components = FALSE if the component sign convention
    # in the logger file is unclear.
    # -------------------------------------------------------------------------
    include_radiation_components = TRUE,
    sw_in_col = "CM3Up_Avg",
    sw_out_col = "CM3Dn_Avg",
    lw_in_col = "CG3Up_Avg",
    lw_out_col = "CG3Dn_Avg",
    sw_bal_col = "NetRs_Avg",
    lw_bal_col = "NetRl_Avg",
    albedo_col = "Albedo_Avg",

    # -------------------------------------------------------------------------
    # Sign factors.
    # fieldClim uses the convention:
    #   rad_bal  > 0  means radiative energy input at the surface.
    #   soil_flux > 0 means heat flux into the soil.
    # If your logger uses the opposite sign, set the corresponding sign to -1.
    # The defaults do not change the logger signs.
    # -------------------------------------------------------------------------
    rad_bal_sign = 1,
    soil_flux_sign = 1,
    sw_in_sign = 1,
    sw_out_sign = 1,
    lw_in_sign = 1,
    lw_out_sign = 1,
    sw_bal_sign = 1,
    lw_bal_sign = 1,

    # -------------------------------------------------------------------------
    # Store original raw table and Campbell header as attributes on the returned
    # weather_station object. This keeps provenance without disturbing fieldClim.
    # -------------------------------------------------------------------------
    keep_raw = TRUE
) {

  # ---------------------------------------------------------------------------
  # Basic input checks.
  # These checks stop early when essential information is missing.
  # ---------------------------------------------------------------------------
  if (missing(file) || length(file) != 1 || !nzchar(file)) {
    stop("`file` must be a single path to a CR1000 / TOA5 .dat file.")
  }

  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  if (missing(lon) || missing(lat) || missing(elev)) {
    stop("Please provide `lon`, `lat`, and `elev` explicitly.")
  }

  if (!requireNamespace("fieldClim", quietly = TRUE)) {
    stop("Package `fieldClim` is not installed or not available in this R session.")
  }

  # ---------------------------------------------------------------------------
  # Read the first four Campbell header lines.
  # The second line contains the variable names. The third line contains units.
  # The fourth line contains logger processing information such as Avg or Smp.
  # ---------------------------------------------------------------------------
  header_lines <- readLines(file, n = 4, warn = FALSE)

  if (length(header_lines) < 4) {
    stop("The file has fewer than four header lines and does not look like TOA5.")
  }

  column_names <- strsplit(header_lines[2], "\t", fixed = TRUE)[[1]]
  column_units <- strsplit(header_lines[3], "\t", fixed = TRUE)[[1]]
  column_proc  <- strsplit(header_lines[4], "\t", fixed = TRUE)[[1]]

  # ---------------------------------------------------------------------------
  # Read the measurement block.
  # skip = 4 means: ignore metadata, names, units, and processing rows.
  # check.names = FALSE keeps the original Campbell names unchanged.
  # na.strings converts Campbell NAN and common missing-value markers to NA.
  # ---------------------------------------------------------------------------
  raw <- read.table(
    file = file,
    sep = "\t",
    skip = 4,
    header = FALSE,
    col.names = column_names,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("NAN", "NaN", "nan", "NA", "NULL", ""),
    quote = "",
    comment.char = "",
    fill = TRUE
  )

  # ---------------------------------------------------------------------------
  # Helper: read a named column from the raw table.
  # If required = TRUE, missing columns stop the function.
  # If required = FALSE, missing columns return NULL and are simply not passed
  # to fieldClim.
  # ---------------------------------------------------------------------------
  get_col <- function(data, col, required = FALSE, sign = 1) {
    if (is.null(col) || length(col) == 0 || is.na(col) || !nzchar(col)) {
      if (required) {
        stop("A required column name is empty.")
      }
      return(NULL)
    }

    if (!col %in% names(data)) {
      if (required) {
        stop("Required column not found in CR1000 file: ", col)
      }
      return(NULL)
    }

    sign * data[[col]]
  }

  # ---------------------------------------------------------------------------
  # Convert all non-time columns to numeric.
  # Campbell files are text files; after import, columns can be character.
  # Numeric conversion is necessary before fieldClim can calculate anything.
  # The timestamp column is excluded because it is parsed separately below.
  # ---------------------------------------------------------------------------
  for (nm in names(raw)) {
    if (!identical(nm, timestamp_col)) {
      raw[[nm]] <- suppressWarnings(as.numeric(raw[[nm]]))
    }
  }

  # ---------------------------------------------------------------------------
  # Parse timestamps.
  # The main format is given by timestamp_format. Several common fallback formats
  # are tried as well, so that seconds or ISO-style timestamps do not immediately
  # break the import.
  # ---------------------------------------------------------------------------
  if (!timestamp_col %in% names(raw)) {
    stop("Timestamp column not found in CR1000 file: ", timestamp_col)
  }

  parse_datetime <- function(x, tz, formats) {
    out <- as.POSIXct(rep(NA_character_, length(x)), tz = tz)

    for (fmt in formats) {
      idx <- is.na(out) & !is.na(x) & nzchar(x)
      if (any(idx)) {
        out[idx] <- as.POSIXct(x[idx], format = fmt, tz = tz)
      }
    }

    out
  }

  datetime_formats <- unique(c(
    timestamp_format,
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M"
  ))

  datetime <- parse_datetime(raw[[timestamp_col]], tz = tz, formats = datetime_formats)

  if (all(is.na(datetime))) {
    stop(
      "No timestamps could be parsed. Check `timestamp_format` and `tz`. ",
      "Example for this file: timestamp_format = '%d.%m.%Y %H:%M'."
    )
  }

  if (any(is.na(datetime))) {
    warning(sum(is.na(datetime)), " timestamps could not be parsed and are NA.")
  }

  # ---------------------------------------------------------------------------
  # Build the fieldClim argument list.
  # fieldClim expects specific names such as temp, rh, t1, t2, hum1, hum2,
  # v1, v2, rad_bal and soil_flux. The logger names are mapped here.
  # ---------------------------------------------------------------------------
  ws_args <- list(
    # Time axis and station metadata.
    datetime = datetime,
    lon = lon,
    lat = lat,
    elev = elev,

    # Standard near-surface meteorological variables.
    temp = get_col(raw, temp_col_1, required = TRUE),
    rh = get_col(raw, rh_col_1, required = TRUE),

    # Two-height profile variables.
    t1 = get_col(raw, temp_col_1, required = TRUE),
    t2 = get_col(raw, temp_col_2, required = TRUE),
    hum1 = get_col(raw, rh_col_1, required = TRUE),
    hum2 = get_col(raw, rh_col_2, required = TRUE),
    v1 = get_col(raw, wind_col_1, required = TRUE),
    v2 = get_col(raw, wind_col_2, required = FALSE),
    wind_dir = get_col(raw, wind_dir_col, required = FALSE),

    # Measurement heights.
    z1 = z1,
    z2 = z2,

    # Radiation balance and soil heat flux.
    rad_bal = get_col(raw, rad_bal_col, required = TRUE, sign = rad_bal_sign),
    soil_flux = get_col(raw, soil_flux_col, required = TRUE, sign = soil_flux_sign),

    # Additional useful variables.
    pressure = get_col(raw, pressure_col, required = FALSE),
    rain = get_col(raw, rain_col, required = FALSE),

    # Surface assumptions.
    surface_type = surface_type,
    obs_height = obs_height
  )

  # ---------------------------------------------------------------------------
  # Optional radiation components.
  # These are useful for checking radiation structure but should only be included
  # when the logger sign convention is understood.
  # ---------------------------------------------------------------------------
  if (isTRUE(include_radiation_components)) {
    ws_args$sw_in <- get_col(raw, sw_in_col, required = FALSE, sign = sw_in_sign)
    ws_args$sw_out <- get_col(raw, sw_out_col, required = FALSE, sign = sw_out_sign)
    ws_args$lw_in <- get_col(raw, lw_in_col, required = FALSE, sign = lw_in_sign)
    ws_args$lw_out <- get_col(raw, lw_out_col, required = FALSE, sign = lw_out_sign)
    ws_args$sw_bal <- get_col(raw, sw_bal_col, required = FALSE, sign = sw_bal_sign)
    ws_args$lw_bal <- get_col(raw, lw_bal_col, required = FALSE, sign = lw_bal_sign)
    ws_args$albedo <- get_col(raw, albedo_col, required = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Remove NULL elements.
  # Optional columns that do not exist in the logger file should not be passed as
  # empty fields to fieldClim.
  # ---------------------------------------------------------------------------
  ws_args <- ws_args[!vapply(ws_args, is.null, logical(1))]

  # ---------------------------------------------------------------------------
  # Build the actual fieldClim weather_station object.
  # do.call() passes the named list above as named arguments.
  # ---------------------------------------------------------------------------
  ws <- do.call(fieldClim::build_weather_station, ws_args)

  # ---------------------------------------------------------------------------
  # Add provenance as attributes.
  # Attributes do not change the fieldClim field names and do not affect normal
  # fieldClim calculations. They are only useful when checking where the object
  # came from.
  # ---------------------------------------------------------------------------
  attr(ws, "source_file") <- normalizePath(file, mustWork = FALSE)
  attr(ws, "campbell_header") <- list(
    metadata = header_lines[1],
    column_names = column_names,
    column_units = column_units,
    column_processing = column_proc
  )
  attr(ws, "fieldclim_column_map") <- list(
    datetime = timestamp_col,
    temp = temp_col_1,
    rh = rh_col_1,
    t1 = temp_col_1,
    t2 = temp_col_2,
    hum1 = rh_col_1,
    hum2 = rh_col_2,
    v1 = wind_col_1,
    v2 = wind_col_2,
    wind_dir = wind_dir_col,
    pressure = pressure_col,
    rain = rain_col,
    rad_bal = rad_bal_col,
    soil_flux = soil_flux_col,
    sw_in = sw_in_col,
    sw_out = sw_out_col,
    lw_in = lw_in_col,
    lw_out = lw_out_col,
    sw_bal = sw_bal_col,
    lw_bal = lw_bal_col,
    albedo = albedo_col
  )

  if (isTRUE(keep_raw)) {
    attr(ws, "raw_cr1000_table") <- raw
  }

  ws
}

# -----------------------------------------------------------------------------
# Minimal usage example
# -----------------------------------------------------------------------------
# library(fieldClim)
# source("assets/scripts/read_cr1000_to_fieldclim.R")
#
# ws <- read_cr1000_to_fieldclim(
#   file = "data/CR1000_Table1_26Mar.dat",
#   lon = -79.17,          # replace with exact station longitude
#   lat = -4.11,           # replace with exact station latitude
#   elev = 2700,           # replace with exact station elevation in metres
#   tz = "America/Guayaquil",
#   z1 = 2,
#   z2 = 10,
#   surface_type = "field",
#   rad_bal_sign = 1,
#   soil_flux_sign = 1
# )
#
# class(ws)
# names(ws)
# head(as.data.frame(ws, reduced = TRUE))
# fieldClim::inspect_weather_station_inputs(ws)
# -----------------------------------------------------------------------------
