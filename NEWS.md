# sarp.snowprofile 1.4.0
  
  * Add `writePro` function that writes a `snowprofileSet` to a PRO file.
  * Fixed few bugs in the `snowprofile` constructor that would not populate scalar parameters provided (e.g., `ski_pen`, `hn24`, etc)
  * `snowprofilePro` can now read several specific dates (before only all dates or one specific date)
  * `snowprofilePro` now (optionally, but per default) considers SH at surface (PRO code 0514) even if it's not a dedicated layer yet.
  * Add functionality to plot hourly timeseries in `plot.snowprofileSet`
    - This is the default now, switch to `SortMethod="time_daily"` for daily simulations and legacy behavior
    - Full flexibility remains, in particular with regard to time series labeling, scaling plots to visualize a continuous timeseries even if data sampling has a different resolution, etc.
    - Add new profile timeseries to demo new plotting features, `SPtimeline_3hourly`
  * Add method `assignDatetags`
    - Documentation updated and warnings are aggregated for snowprofileSet calls
  * Parse deposition date information from `.pro` files (as code 0505).
  * Add feature in `snowprofilePro` to read in a subset of profiles by their dates (instead of all or one only).
  * Update compatibility with SnowPilot (v7) caaml (v6) read routine.
  * Deprecate `deriveDatetag`

# sarp.snowprofile 1.3.2

  * Include weibull scaling and crust correction in `computeRTA`
  * Enfore dependency on R 4.2 (older versions break functionality)

# sarp.snowprofile 1.3.0

  * Introduce a switch to compute profile summaries *faster* (see `summary.snowprofile` and `summary.snowprofileSet`)
  * Fix bug in `snowprofilePRO` when soil layers where present; and: read SK38 if present
  * Introduce function `computeSLABrhogs` to characterize cohesion of slabs (and also `computeSLABrho` for mean slab density)
  * Adjust method to compute burial dates of layers to align more closely with human interpretation. see `deriveDatetag`
  * Fix minor bug in `findPWL`: time window search ranges are now applied more transparent and meaningful
  * Properly define popular generics with methods for the different classes (see e.g., `?computeRTA`, `?deriveDatetag`)
  * `plot.snowprofileSet`
      - Introduce coloring of snowprofileSet plots according to stability indices with a compound color palette that allows identifying of index-specific classification thresholds (see `?plot.snowprofileSet`,  `?getColoursStability`, `?getColoursPercentage`)
      - add many new features for customizing snowprofileSet plots (e.g., emphasize specific layers, overplot timeseries with stability index, etc..)

# sarp.snowprofile 1.2.0

 * New functions 
   - to compute structural stability threshold sums/ lemons (i.e., `computeTSA`, `computeRTA`)
   - to flexibly search for specific or generic weak layers and other layers of interest in the profiles (see, `findPWL`)
   - new and improved functionality in `plot.snowprofile` and `plot.snowprofileSet`

 * Restructured snowprofile object classes to better cope with top-down measured manual profiles
 that potentially have unknown total snow heights.
   - Main change: Insert mandatory snowprofile field `maxObservedDepth` and precisely distinguish that from `hs`.
   - The existing constructors from snowprofile and snowprofileLayers are set up to auto-fill the fields if possible.
   - Highlight profiles with unknown hs in `plot.snowprofile`, add `TopDown = "auto"`
   - include strategy to handle unobserved basal layers
   
  * Speed up several bottlenecks that slowed down long computations


# sarp.snowprofile 1.1.0

 * v1.1.0 contains a variety of minor improvements listed below. The version should be free of major bugs, 
 but several improvements are to be done in future versions.

 * Improvements of `snowprofileCaaml`:
   - Make routine compatible with caaml files from snowpilot.org 
   (fix bugs related to bottom up profiles and workaround namespace issue)
   - Originally compatible with caaml files from niviz.org
   - Convert undefined grain types
   
 * Handle grain types that are undefined in package with `validate_snowprofile` and `reformat_snowprofile`
   
 * Enable plotting of a temperature profile that is independent of the snow layer grid
 
 * `writeSmet` routine for exporting smet files
 
 * Add object classes for `snowprofileTests` and `snowprofileInstabilitySigns`
 
 * Add new read routine for advanced csv files containing a comprehensive set of profile information, see `?snowprofileCsv_advanced`.
 
 * Add few new field names to layer structure.
 
 * A `deriveDatetag` function calculates `bdate`s and `datetag`s for snowprofile layers.
 
 * A `simplifyGtypes` function trims unsupported IACS gtype subclasses into their supported main classes (e.g., PPsd --> PP)
 

# sarp.snowprofile 1.0.0

 * initial release of `sarp.snowprofile`
