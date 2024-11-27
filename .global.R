# This repository is ours and it has the latest versions of our packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
# Need the latest version
if (tryCatch(packageVersion("SpaDES.project") < "0.1.1", error = function(x) TRUE))
  install.packages(c("SpaDES.project", "Require"), repos = repos)
#

out <- SpaDES.project::setupProject(
  Restart = TRUE,
  updateRprofile = TRUE,
  useGit = TRUE,
  paths = list(projectPath = getwd()),
  modules = c("PredictiveEcology/Biomass_borealDataPrep@main",
              "PredictiveEcology/Biomass_core@main",
              "PredictiveEcology/Biomass_regeneration@main",
              "PredictiveEcology/scfm@development"),
              #note scfm is a series of modules on a single git repository
  params = list(
    .globals = list(
      dataYear = 2011, #will get kNN 2011 data, and NTEMS 2011 landcover
      sppEquivCol = "LandR",
      .plots = c("png"),
      .useCache = c(".inputObjects", "init")
    ),
    scfmDriver = list(targetN = 1000, #default is 4000 - higher targetN adds time + precision
                      # targetN would ideally be minimum 2000 - mean fire size estimates will be bad with 1000
                      .useParallelFireRegimePolys = TRUE) #assumes parallelization is an otpion

  ),
  options = list(#spades.allowInitDuringSimInit = TRUE,
    spades.allowSequentialCaching = TRUE,
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = 1
  ),
  packages = c('RCurl', 'XML', 'snow', 'googledrive'),
  times = list(start = 2011, end = 2511),
  #70 years of fire should be enough to evaluate MAAB
  studyArea = {
    targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                       "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sa <- terra::vect(cbind(-1209980, 7586865), crs = targetCRS)
    sa <- LandR::randomStudyArea(center = sa, size = 10000 * 250 * 30000, seed = 1002)
    sa <- sf::st_as_sf(sa)
  },
  studyAreaLarge = {
    sf::st_buffer(studyArea, 5000)
  },
  rasterToMatchLarge = {
    rtml<- terra::rast(terra::ext(studyAreaLarge), res = c(250, 250))
    terra::crs(rtml) <- terra::crs(studyAreaLarge)
    rtml[] <- 1
    rtml <- terra::mask(rtml, studyAreaLarge)
  },
  rasterToMatch = {
    rtm <- terra::crop(rasterToMatchLarge, studyArea)
    rtm <- terra::mask(rtm, studyArea)
  },
  sppEquiv = {
    speciesInStudy <- LandR::speciesInStudyArea(studyAreaLarge)
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  }
)

outSim <- do.call(SpaDES.core::simInitAndSpades, out)