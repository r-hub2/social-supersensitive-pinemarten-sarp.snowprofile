#' Construct snowprofile object from PRO file
#'
#' Read .pro files from SNOWPACK model output
#'
#' @param Filename path to pro file
#' @param ProfileDate read specific profiles from file either by individual date or a vector of dates (default = NA will read all profiles)
#' @param tz time zone (default = 'UTC')
#' @param remove_soil if soil layers are present in PRO file, remove them from snowprofile objects?
#' @param consider_SH_surface boolean switch to read the special PRO field 0514--SH at surface (this will produce NA for many unknown layer properties except gsize, density, hardness, gtype, height)
#' @param suppressWarnings boolean switch
#'
#' @return a single snowprofile object or a snowprofileSet (list of multiple snowprofile objects) depending on whether multiple or single dates are being read. If several ProfileDates are being given,
#' but only one of the dates contains snow, a snowprofileSet of length 1 is returned.
#'
#' @details
#'
#' Several SNOWPACK model output formats exist see \href{https://snowpack.slf.ch/doc-release/html/snowpackio.html}{SNOWPACK documentation}
#'
#' Definitions of PRO files are provided at \href{https://snowpack.slf.ch/doc-release/html/pro_format.html}{https://snowpack.slf.ch/doc-release/html/pro_format.html} and an example file is available at \href{https://run.niviz.org/?file=resources%2Fexample.pro}{niViz}
#'
#' PRO files typically contain profiles from the same station at multiple time steps. If a specific `ProfileDate` is provided a single snowprofile object is returned (search available dates with `scanProfileDates`), otherwise all profiles are read and a list of snowprofile objects is returned.
#'
#' @seealso [snowprofilePrf], [scanProfileDates], [snowprofileSno]
#'
#' @author shorton, dmauracher
#'
#' @examples
#'
#' ## Path to example pro file
#' Filename <- system.file('extdata', 'example.pro', package = 'sarp.snowprofile')
#'
#' ## Download example pro file from niViz
#' #Filename <- tempfile(fileext = '.pro')
#' #download.file('https://niviz.org/resources/example.pro', Filename)
#'
#' ## Scan dates in file
#' Dates <- scanProfileDates(Filename)
#' print(Dates)
#'
#' ## Read a single profile by date and plot
#' ProfileDate <- Dates[3]
#' Profile <- snowprofilePro(Filename, ProfileDate = ProfileDate)
#' plot(Profile)
#'
#' ## Read entire time series and plot
#' Profiles <- snowprofilePro(Filename)
#' plot(Profiles, main = 'Timeseries read from example.pro')
#'
#' ## Read several specific dates and plot
#' specificDates <- Dates[2:3]
#' Profiles <- snowprofilePro(Filename, ProfileDate = specificDates)
#' plot(Profiles)
#' @export
#'
#' @import data.table
#'
snowprofilePro <- function (Filename,
                            ProfileDate = NA,
                            tz = "UTC",
                            remove_soil = TRUE,
                            consider_SH_surface = TRUE,
                            suppressWarnings = FALSE) {

  if (suppressWarnings) {
    old.warn <- options(warn = -1)
    on.exit(options(old.warn))
  }

  if (!endsWith(Filename, ".pro"))
    stop(paste(Filename, "needs .pro extension"))

  Lines <- data.table::fread(Filename, sep = "~", data.table = FALSE)
  HeaderStart <- which(Lines == "[HEADER]")
  DataStart <- which(Lines == "[DATA]")

  StationData <- list()
  Header <- Lines[1:(HeaderStart - 1), 1]

  for (Row in Header[Header != ""]) {
    KeyValue <- unlist(strsplit(gsub(" ", "", Row), "="))
    StationData[[KeyValue[1]]] <- KeyValue[2]
  }

  Lines <- data.frame(Lines[(DataStart + 1):nrow(Lines), ], stringsAsFactors = FALSE)

  if (length(ProfileDate) == 1 && is.na(ProfileDate)) {
    DateLines <- which(startsWith(Lines[, 1], "0500"))
    Nrows <- c(diff(DateLines), nrow(Lines) - DateLines[length(DateLines)] + 1)
    ProfileIndex <- rep(1:length(DateLines), Nrows)
    TabularProfiles <- split(Lines, ProfileIndex)
    SP <- lapply(TabularProfiles, readTabularProfile,
                 StationData = StationData, tz = tz,
                 remove_soil = remove_soil, consider_SH_surface = consider_SH_surface)
    SP <- SP[!sapply(SP, is.null)]
    SP <- snowprofileSet(SP)
  } else {
    DateLines <- which(startsWith(Lines[, 1], "0500"))
    Dates <- na.omit(gsub("0500,", "", Lines[DateLines, ]))
    Dates <- as.POSIXct(Dates, format = "%d.%m.%Y %H:%M", tz = tz)

    if (!all(ProfileDate %in% Dates))
      stop(paste("No profile with date", ProfileDate, "found in", Filename))

    SP <- list()
    for (pdate in ProfileDate) {
      StartLine <- DateLines[which(Dates == pdate)]
      EndLine <- StartLine
      nLines <- nrow(Lines)

      while (!startsWith(Lines[(EndLine + 1), ], "0500")) {
        EndLine <- EndLine + 1
        if (EndLine == nLines) break
      }
      TabularProfile <- data.frame(Lines[StartLine:EndLine, ], stringsAsFactors = FALSE)
      sp <- readTabularProfile(TabularProfile, StationData, tz, remove_soil, consider_SH_surface)
      SP[[length(SP)+1]] <- sp
    }
    if (length(SP) == 1 && length(ProfileDate) == 1) {
      SP <- SP[[1]]
    } else {
      SP <- snowprofileSet(SP)
    }
  }

  if (suppressWarnings) options(old.warn)
  return(SP)
}


#' PRO field code lookup
#'
#' Named list of SNOWPACK PRO field codes used by [snowprofilePro] and [writePro].
#'
#' @return A named list of character codes.
#' @export
codes_pro <- list (
  height = "0501",
  density = "0502",
  temperature = "0503",
  lwc = "0506",
  dendricity = "0508",
  sphericity = "0509",
  bond_size = "0511",
  gsize = "0512",
  gtype = "0513",
  sh_sfc = "0514",
  v_strain_rate = "0523",
  sk38 = "0533",
  hardness = "0534",
  ogs = "0535",
  ddate = "0540",
  shear_strength = "0601",
  ssi = "0604",
  ski_pen = "0607",
  p_unstable = "1130",
  ppu = "1131"
)


readTabularProfile <- function(TabularProfile, StationData, tz, remove_soil, consider_SH_surface) {
  TabularProfile <- TabularProfile[, 1]
  Codes <- sapply(strsplit(TabularProfile, ","), "[", 1)

  if (length(codeLookup(TabularProfile, codes_pro$height)) == 1) {
    return(NULL)
  }
  else {
    datetime <- codeLookup(TabularProfile, "0500")[2]
    datetime <- as.POSIXct(datetime, format = "%d.%m.%Y %H:%M", tz = tz)

    height <- codeLookup(TabularProfile, codes_pro$height)
    nHeight <- length(height)
    Layers <- data.frame(height = height)

    lookup_and_append <- function(field, factor=1) {
      if (codes_pro[[field]] %in% Codes) {
        values <- factor*codeLookup(TabularProfile, codes_pro[[field]])
        Layers[[field]] <<- append(values, rep(NA, times = nHeight - length(values)), after = 0)
      }
    }

    # Iterate over all fields except special cases
    for (field in setdiff(names(codes_pro), c("gtype", "ddate", "ski_pen", "hardness", "sh_sfc"))) {
      lookup_and_append(field)
    }
    if (codes_pro$hardness %in% Codes) lookup_and_append("hardness", factor = -1)
    if (codes_pro$gtype %in% Codes) {
      gclass <- codeLookup(TabularProfile, codes_pro$gtype)
      gclass <- gclass[1:(length(gclass) - 1)]
      gtype <- sapply(gclass, function(x) ifelse(substr(x, 3, 3) == "2", "MFcr", swisscode[as.integer(substr(x, 1, 1))]))
      Layers$gtype <- as.factor(append(gtype, rep(NA, times = nHeight - length(gtype)), after = 0))
    }
    if (codes_pro$ddate %in% Codes) {
      ddate <- codeLookup(TabularProfile, codes_pro$ddate)
      Layers$ddate <- append(ddate, rep(NA, times = nHeight - length(ddate)), after = 0)
      Layers$ddate <- as.POSIXct(Layers$ddate, format = "%Y-%m-%dT%H:%M:%S", tz = tz)
    }
    if (codes_pro$ski_pen %in% Codes) {
      ski_pen <- as.double(codeLookup(TabularProfile, codes_pro$ski_pen))
    } else {
      ski_pen <- NA
    }
    if (consider_SH_surface & codes_pro$sh_sfc %in% Codes) {
      sh_props <- codeLookup(TabularProfile, codes_pro$sh_sfc)
      if (!all(sh_props == c(-999, -999, -999))) {
        nSH <- nrow(Layers)+1
        gsSH <- sh_props[2]
        densitySH <- sh_props[3]
        Layers[nSH, "height"] <- Layers[nSH-1, "height"] + gsSH
        Layers[nSH, "hardness"] <- 1
        Layers[nSH, "gsize"] <- gsSH
        Layers["gtype"] <- as.factor(append(as.character(Layers$gtype[1:(nSH-1)]), "SH"))
        Layers[nSH, "density"] <- densitySH
      }
    }

    if (remove_soil) {
      Layers <- Layers[Layers$height > 0, ]
    }

    Layers[Layers == -999] <- NA
    Layers <- snowprofileLayers(layerFrame = Layers)

    SP <- snowprofile(
      station = StationData$StationName,
      station_id = StationData$StationName,
      datetime = datetime,
      latlon = as.numeric(c(StationData$Latitude, StationData$Longitude)),
      elev = as.numeric(StationData$Altitude),
      angle = as.numeric(StationData$SlopeAngle),
      aspect = as.numeric(StationData$SlopeAzi),
      type = "modeled",
      ski_pen = ski_pen,
      layers = Layers
    )

    return(SP)
  }
}


codeLookup <- function(Lines, Code) {
  Row <- Lines[which(startsWith(Lines, Code))]
  Row <- unlist(strsplit(Row, ","))
  Row <- Row[3:length(Row)]
  Row <- type.convert(Row, as.is = TRUE)
  return(Row)
}
