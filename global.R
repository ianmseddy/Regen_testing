# This repository is ours and it has the latest versions of our packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
# Need the latest version
if (tryCatch(packageVersion("SpaDES.project") < "0.1.1", error = function(x) TRUE)) {
  install.packages(c("SpaDES.project", "Require", "SpaDES.core"), repos = repos)
  Require::Require("stringr")
}

ecoregionName <- "Horn Plateau"
studyTime <- 100
studyAreaName <- stringr::str_replace_all(ecoregionName, " ", "")
runName <- paste0(studyAreaName, studyTime)


out <- SpaDES.project::setupProject(
  Restart = TRUE,
  functions = "ianmseddy/Regen_testing@main/R/customFunctions.R",
  updateRprofile = TRUE,
  packages = c('RCurl', 'XML', 'snow', 'googledrive', "PredictiveEcology/LandR@development"),
  useGit = TRUE,
  paths = list(projectPath = getwd(),
               inputPath = "inputs",
               outputPath = file.path("outputs", runName),
               cachePath = "cache"),
  options = list(#spades.allowInitDuringSimInit = TRUE,
    "~/googledriveAuthentication.R",
    spades.allowSequentialCaching = TRUE,
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = 1
  ),
  modules = c(
    "PredictiveEcology/Biomass_speciesData@development",
    "PredictiveEcology/Biomass_borealDataPrep@development",
    "PredictiveEcology/Biomass_core@development",
    "PredictiveEcology/Biomass_regeneration@development",
    "PredictiveEcology/Biomass_speciesParameters@development",
    "PredictiveEcology/scfm@development"
  ),
              #note scfm is a series of modules on a single git repository
  times = list(start = 2020, end = 2020 + studyTime),
  params = list(
    .globals = list(
      dataYear = 2020, #will get kNN 2011 data, and NTEMS 2011 landcover
      sppEquivCol = "LandR",
      .plots = c("png"),
      .studyAreaName = studyAreaName,
      .useCache = c(".inputObjects")
    ),
    Biomass_core = list(.plotInterval = 25)
  ),
  #70 years of fire should be enough to evaluate MAAB
  studyArea = {
    targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                       "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    ecor <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                                     destinationPath = 'inputs', fun = "terra::vect")
    ecor <- ecor[ecor$REGION_NAM == ecoregionName,]
    ecor <- terra::project(ecor, targetCRS)
    ecor <- terra::buffer(ecor, 10000)
  },
  studyArea_biomassParam = terra::buffer(studyArea, 5000),
  sppEquiv = {
    speciesInStudy  <- LandR::speciesInStudyArea(studyArea = studyArea, dPath = "inputs")
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  },
  rasterToMatch_biomassParam = {
    rtml = terra::rast(studyArea_biomassParam, res = c(250, 250), vals = 1)
    rtml = terra::mask(rtml, studyArea_biomassParam)
  },
  rasterToMatch = { #FIX THIS TO USE THE NTEMS CRS
    rtm = reproducible::postProcess(rasterToMatch_biomassParam, to = studyArea)
  }
)

pineUpdate = function(species){
  species[species %in% c("Pinu_con", "Popu_tre"), shadetolerance := 1.5]
}

out$paths$modulePath <- c(file.path("modules"),
                          file.path("modules/scfm/modules"))
out$modules <- c("Biomass_core", "Biomass_borealDataPrep", "Biomass_regeneration",
                 "Biomass_speciesParameters",
                 "scfmDataPrep", "scfmDiagnostics",
                 "scfmIgnition", "scfmEscape", "scfmSpread"
)
out$params$scfmDataPrep = list(targetN = 2000,
                               fireRegimePolysType = "FRU",
                               .useParallelFireRegimePolys = TRUE) #assumes parallelization is an option
out$params$scfmSpread = list(.plotInterval = 25)
out$params$Biomass_borealDataPrep$speciesUpdateFunction = list(
  quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable,
                                  sim$sppEquiv, P(sim)$sppEquivCol)),
  quote(pineUpdate(sim$species)))

outSim <- SpaDES.core::simInitAndSpades2(out)

