# This repository is ours and it has the latest versions of our packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
# Need the latest version
if (tryCatch(packageVersion("SpaDES.project") < "0.1.1", error = function(x) TRUE)) {
  install.packages(c("SpaDES.project", "Require", "SpaDES.core"), repos = repos)
  Require::Require("stringr")
}

ecoregionName <- "Lac Seul Upland"
studyTime <- 750
studyAreaName <- stringr::str_replace_all(ecoregionName, " ", "")
# uniqueFts <- ""
runName <- paste0(studyAreaName, studyTime
                  # ,"_", uniqueFts
                  )


out <- SpaDES.project::setupProject(
  Restart = TRUE,
  functions = "ianmseddy/Regen_testing@main/R/customFunctions.R",
  updateRprofile = TRUE,
  paths = list(projectPath = getwd(),
               inputPath = "inputs",
               outputPath = file.path("outputs", runName),
               cachePath = "cache"),
  modules = c("PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_core@development",
              "PredictiveEcology/Biomass_regeneration@development",
              "PredictiveEcology/Biomass_speciesParameters@manual",
              "PredictiveEcology/scfm@development"
              ),
              #note scfm is a series of modules on a single git repository
  params = list(
    .globals = list(
      dataYear = 2011, #will get kNN 2011 data, and NTEMS 2011 landcover
      sppEquivCol = "LandR",
      .plots = c("png"),
      .studyAreaName = studyAreaName,
      .useCache = c(".inputObjects", "init")
    ),
    #modify scfm separately
    Biomass_borealDataPrep = list(

    )
    Biomass_core = list(.plotInterval = 25)
  ),
  options = list(#spades.allowInitDuringSimInit = TRUE,
    spades.allowSequentialCaching = TRUE,
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = 1,
    LandR.verbose = TRUE #for regen messages
  ),
  packages = c('RCurl', 'XML', 'snow', 'googledrive'),
  times = list(start = 2011, end = 2011 + studyTime),
  useGit = TRUE,
  #70 years of fire should be enough to evaluate MAAB
  studyArea = {
    targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                       "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    if (!file.exists("inputs/ecoregion_shp.zip")) {
      download.file(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                    destfile = "inputs/ecoregion_shp.zip")

    }
    ecor <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                                     destinationPath = 'inputs')
    ecor <- ecor[ecor$REGION_NAM == ecoregionName,]
    ecor <- sf::st_transform(ecor, targetCRS)
    ecor <- sf::st_buffer(ecor, 10000)
  },
  studyAreaLarge = studyArea,
  studyAreaCalibration = {
    sf::st_buffer(studyArea, 20000)
  },
  sppEquiv = {
    speciesInStudy <- LandR::speciesInStudyArea(studyAreaLarge,
                                                dPath = "inputs")
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  },
  rasterToMatchLarge = {
    rtml<- terra::rast(studyAreaLarge, res = c(250, 250))
    rtml[] <- 1
    rtml <- terra::mask(rtml, studyAreaLarge)
  },
  rasterToMatch = { #FIX THIS TO USE THE NTEMS CRS
    rtm <- terra::crop(rasterToMatchLarge, studyArea)
    rtm <- terra::mask(rtm, studyArea)
    rtm
  },
  rasterToMatchCalibration = {
    rtmc<- terra::rast(studyAreaCalibration, res = c(250, 250))
    rtmc[] <- 1
    rtmc <- terra::mask(rtmc, studyAreaCalibration)
  }
)

out$paths$modulePath <- c(file.path("modules"),
                          file.path("modules/scfm/modules"))
out$modules <- c("Biomass_core", "Biomass_borealDataPrep", "Biomass_regeneration",
                 "Biomass_speciesParameters",
                 "scfmLandcoverInit", "scfmRegime", "scfmDriver",
                 "scfmIgnition", "scfmEscape", "scfmSpread",
                 "scfmDiagnostics"
                 )
out$params$scfmDriver = list(targetN = 3000, #default is 4000 - higher targetN adds time + precision
                  # targetN would ideally be minimum 2000 - mean fire size estimates will be bad with 1000
                  .useParallelFireRegimePolys = TRUE) #assumes parallelization is an option
out$params$scfmSpread = list(.plotInterval = 25)

outSim <- SpaDES.core::simInitAndSpades2(out)
