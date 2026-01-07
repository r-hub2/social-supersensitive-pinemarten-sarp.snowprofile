#' Write snowprofileSet to a PRO file
#'
#' Write SNOWPACK PRO files from a [snowprofileSet].
#'
#' @param profiles [snowprofileSet] object to write.
#' @param filename output file path ending in `.pro`.
#' @param meta optional profile metadata (defaults to `summary(profiles, fast = TRUE)` when omitted).
#' @param header_comment character string added to the file header.
#' @return Writes a PRO file to disk; returns `NULL` invisibly.
#' @seealso [snowprofilePro]
#'
#' @examples
#'
#' ## Path to example pro file
#' Filename <- system.file('extdata', 'example.pro', package = 'sarp.snowprofile')
#'
#' ## Read entire time series and plot
#' Profiles <- snowprofilePro(Filename)
#' plot(Profiles, main = 'Timeseries read from example.pro')
#'
#' ## Write to file
#' tmppath = tempfile(pattern = "written_", fileext = ".pro")
#' writePro(Profiles, tmppath)
#'
#' ## Re-read from tempfile
#' Profiles2 <- snowprofilePro(tmppath)
#' (testbool <- all(Profiles[[1]]$layers == Profiles2[[1]]$layers))
#' if (!testbool) stop("error in writePro re-reading!")
#' @export
writePro <- function(profiles, filename, meta = NaN,
                     header_comment = "time of creation and SNOWPACK version unknown") {

  if (!inherits(profiles, 'snowprofileSet'))
    stop("writePro requires an object of class snowprofileSet")
  if (all(is.na(meta))) meta = summary(profiles, fast=TRUE)
  if (length(unique(na.omit(meta$station))) > 1 | length(unique(na.omit(meta$station_id))) > 1) {
    stop("writePro can only write information from one single vstation to file.")
  }
  file_conn <- file(filename, open = "w")

  # STATION PARAMETERS
  writeLines("[STATION_PARAMETERS]", file_conn)
  SP = profiles[[1]]
  for (name in names(SP)) {
    code_value <- station_parameters[[name]]
    if (!is.null(code_value)) {

      # Write coordinates
      if (name == "latlon") {
        latitude <- SP[[name]][1]
        longitude <- SP[[name]][2]
        writeLines(paste("Latitude=", latitude), file_conn)
        writeLines(paste("Longitude=", longitude), file_conn)
        next
      }

      # Check date format
      if (inherits(SP[[name]], "POSIXct")) {
        value <- format(SP[[name]], "%Y-%m-%d %H:%M:%S")
      } else if (inherits(SP[[name]], "Date")) {
        value <- format(SP[[name]], "%Y-%m-%d")
      } else {
        value <- SP[[name]]
      }

      # Write the mapped code and value to the file
      writeLines(paste(code_value, value), file_conn)
    }
  }

  # HEADER
  writeLines(paste0('\n[HEADER]\n# ', header_comment), file_conn)
  writeLines(header_codes, file_conn)

  # DATA
  writeLines("\n[DATA]", file_conn)
  for (profile in profiles) {
    layers <- profile$layers
    # Write Datetime
    formatted_datetime <- format(profile$datetime, "%d.%m.%Y %H:%M:%S")
    writeLines(paste0("0500", ",", formatted_datetime), file_conn)

    for (name in names(codes_pro)) {
      if (name %in% names(layers)) {
        code <- codes_pro[[name]]
        values <- paste(layers[[name]], collapse=",")
        # Modify values for grain type
        if (code == "0513") {
          array_values <- unlist(strsplit(values, ","))
          values <- sapply(array_values, function(swiss_value) {
            if (swiss_value == "MFcr") {
              return(772)
            } else {
              return(match(swiss_value, swisscode))
            }
          })
          # Add a "0" at the end of the values for grain type
          values <- paste(c(values, 0), collapse = ",")
        } else if (name == "hardness") {
          values <- paste(-1*layers[[name]], collapse=",")  # make hardness negative (SNOWPACK/PRO convention?!)
        }
        len <- length(unlist(strsplit(values, ",")))
        writeLines(paste0(code, ",", len, ",", values), file_conn)
      }
    }
  }
  close(file_conn)
}

station_parameters <- list(
  station = "StationName=",
  latlon = "Latitude Longitude=",
  elev = "Altitude=",
  angle = "SlopeAngle=",
  aspect = "SlopeAzi="
)

header_codes <- '0500,Date
0501,nElems,height [> 0: top, < 0: bottom of elem.] (cm)
0502,nElems,element density (kg m-3)
0503,nElems,element temperature (degC)
0504,nElems,element ID (1)
0505,nElems,element deposition date (ISO)
0506,nElems,liquid water content by volume (%)
0508,nElems,dendricity (1)
0509,nElems,sphericity (1)
0510,nElems,coordination number (1)
0511,nElems,bond size (mm)
0512,nElems,grain size (mm)
0513,nElems,grain type (Swiss Code F1F2F3)
0514,3,grain type, grain size (mm), and density (kg m-3) of SH at surface
0515,nElems,ice volume fraction (%)
0516,nElems,air volume fraction (%)
0517,nElems,stress in (kPa)
0518,nElems,viscosity (GPa s)
0519,nElems,soil volume fraction (%)
0520,nElems,temperature gradient (K m-1)
0521,nElems,thermal conductivity (W K-1 m-1)
0522,nElems,absorbed shortwave radiation (W m-2)
0523,nElems,viscous deformation rate (1.e-6 s-1)
0530,8,position (cm) and minimum stability indices:
       profile type, stability class, z_Sdef, Sdef, z_Sn38, Sn38, z_Sk38, Sk38
0531,nElems,deformation rate stability index Sdef
0532,nElems,natural stability index Sn38
0533,nElems,stability index Sk38
0534,nElems,hand hardness in index steps (1)
0535,nElems,optical equivalent grain size (mm)
0601,nElems,snow shear strength (kPa)
0602,nElems,grain size difference (mm)
0603,nElems,hardness difference (1)
0604,nElems,ssi
0605,nElems,inverse texture index ITI (Mg m-4)
0606,nElems,critical cut length (m)
0901,nElems,the degree of undersaturation, (rhov-rohv_sat)/rhov_sat (-)
0902,nElems,the water vapor diffusion flux (kg m-2 s-1)
0903,nElems,the cumulative density change due to water vapor transport (kg m-3)
0904,nElems,the snow density change rate due to water vapor transport (1.0e-6 kg m-3)
0905,nElems,the element tracking for comparison, (-)
1130,nElems,p_unstable from Mayer et al., 2022 (-)
1131,nElems,the proportion of unstable gridpoints, i.e. p_unstable >= 0.77 (-)'
