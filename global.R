# This repository is ours and it has the latest versions of our packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
# Need the latest version
if (tryCatch(packageVersion("SpaDES.project") < "0.1.1", error = function(x) TRUE)) {
  install.packages(c("SpaDES.project", "Require", "SpaDES.core"), repos = repos)
}

out <- SpaDES.project::setupProject(
  Restart = TRUE,
  updateRprofile = TRUE,
  paths = list(projectPath = getwd()),
  modules = c("PredictiveEcology/Biomass_borealDataPrep@main",
              "PredictiveEcology/Biomass_core@main",
              "PredictiveEcology/Biomass_regeneration@development",
              "PredictiveEcology/scfm@development"
              ),
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
                      .useParallelFireRegimePolys = TRUE), #assumes parallelization is an otpion
    scfmSpread = list(.plotInterval = 40),
    Biomass_core = list(.plotInterval = 10)
  ),
  options = list(#spades.allowInitDuringSimInit = TRUE,
    spades.allowSequentialCaching = TRUE,
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = 1
  ),
  packages = c('RCurl', 'XML', 'snow', 'googledrive'),
  times = list(start = 2011, end = 2511),
  useGit = TRUE,
  #70 years of fire should be enough to evaluate MAAB
  studyArea = {
    ecod <- reproducible::prepInputs(url = "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/ECODISTR.zip")
    ecod <- ecod[ecod$DIST_NAME == "Hornepayne",]
    ecod
  },
  studyAreaLarge = {
    sf::st_buffer(studyArea, 2000)
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
    rtm
  },
  sppEquiv = {
    speciesInStudy <- LandR::speciesInStudyArea(studyAreaLarge)
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  }
)

out$modules <- c("Biomass_core", "Biomass_borealDataPrep", "Biomass_speciesData",
                file.path("modules/scfm", c("scfmLandcoverInit", "scfmRegime", "scfmDriver",
                 "scfmIgnition", "scfmEscape", "scfmSpread",
                 "scfmDiagnostics")))
out$paths$modulePath <- c("modules", "modules/scfm/modules")

outSim <- simInitAndSpades2(out)
