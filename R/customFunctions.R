speciesTableUpdate_custom <- function (species, speciesTable, sppEquiv = NULL, sppEquivCol = NULL)
{
  if (!"Area" %in% names(species)) {
    stop("Can't find 'Area' column in 'sim$species'")
    test <- !any(unique(species$Area) %in% c("BSW", "BP",
                                             "MC"))
    if (test) {
      message(red("Areas in 'species$Area' do not match any of 'BSW', 'BP' or 'MC',",
                  "\nno changes made to 'sim$species'."))
      return(species)
    }
  }
  if (is.null(sppEquiv)) {
    sppEquiv <- data.table(utils::data("sppEquivalencies_CA",
                                       package = "LandR", envir = environment()))
  }
  if (is.null(sppEquivCol)) {
    stop("Please provide sppEquivCol")
  }
  browser()
  names(speciesTable) <- .speciesTableColNames
  speciesTableShort <- speciesTable[Area %in% c("BSW", "BP",
                                                "MC"), .(species, longevity, shadetolerance)]
  speciesTableShort[species == "ABIE.BAL", `:=`(longevity,
                                                200)]
  speciesTableShort[species == "ABIE.LAS", `:=`(longevity,
                                                240)]
  speciesTableShort[species == "BETU.PAP", `:=`(longevity,
                                                150)]
  speciesTableShort[species == "LARI.LAR", `:=`(longevity,
                                                350)]
  speciesTableShort[species == "LARI.OCC", `:=`(longevity,
                                                450)]
  speciesTableShort[species == "PICE.ENG", `:=`(longevity,
                                                460)]
  speciesTableShort[species == "PICE.GLA", `:=`(longevity,
                                                400)]
  speciesTableShort[species == "PICE.MAR", `:=`(longevity,
                                                250)]
  speciesTableShort[species == "PINU.BAN", `:=`(longevity,
                                                150)]
  speciesTableShort[species == "PINU.CON.LAT", `:=`(longevity,
                                                    335)]
  speciesTableShort[species == "PINU.PON", `:=`(longevity,
                                                575)]
  speciesTableShort[species == "POPU.BAL", `:=`(longevity,
                                                200)]
  speciesTableShort[species == "POPU.TRE", `:=`(longevity,
                                                200)]
  speciesTableShort[species == "PSEU.MEN", `:=`(longevity,
                                                525)]
  speciesTableShort[species == "THUJ.PLI", `:=`(longevity,
                                                1500)]
  speciesTableShort[species == "TSUG.HET", `:=`(longevity,
                                                500)]
  speciesTableShort[species == "TSUG.MER", `:=`(longevity,
                                                800)]
  speciesTableShort[species == "ABIE.BAL", `:=`(shadetolerance,
                                                3)]
  speciesTableShort[species == "ABIE.LAS", `:=`(shadetolerance,
                                                3)]
  speciesTableShort[species == "PICE.ENG", `:=`(shadetolerance,
                                                3)]
  speciesTableShort[species == "PICE.GLA", `:=`(shadetolerance,
                                                2)]
  speciesTableShort[species == "PICE.MAR", `:=`(shadetolerance,
                                                3)]
  speciesTableShort[species == "TSUG.HET", `:=`(shadetolerance,
                                                4)]
  speciesTableShort[species == "TSUG.MER", `:=`(shadetolerance,
                                                3)]
  speciesTableShort[species %in% c("PINU.BAN", "LARI.LAR",
                                   "POPU_TRE", "POPU_BAL"), `:=`(shadetolerance,1.5)]


  sppEquiv <- sppEquiv[!is.na(sppEquiv[[sppEquivCol]]), ]
  sppNameVector <- species$species
  speciesTableShort <- speciesTableShort[species %in% equivalentName(sppNameVector,
                                                                     sppEquiv, "LANDIS_traits", multi = TRUE)]
  speciesTableShort[, `:=`(species, equivalentName(speciesTableShort$species,
                                                   sppEquiv, sppEquivCol))]
  speciesTableShort <- speciesTableShort[, .(longevity = min(longevity),
                                             shadetolerance = min(shadetolerance)), by = "species"]
  cols <- setdiff(names(species), c("longevity", "shadetolerance"))
  speciesTemp <- species[, ..cols]
  speciesTemp <- speciesTableShort[speciesTemp, on = "species",
                                   nomatch = 0]
  species <- rbind(species[!species %in% speciesTemp$species],
                   speciesTemp)[order(species)]
  species[, `:=`(Area = as.factor(Area), firetolerance = asInteger(firetolerance),
                 growthcurve = as.numeric(growthcurve), longevity = asInteger(longevity),
                 leaflongevity = asInteger(leaflongevity), leafLignin = as.numeric(leafLignin),
                 mortalityshape = asInteger(mortalityshape), postfireregen = as.factor(postfireregen),
                 resproutage_max = asInteger(resproutage_max), resproutage_min = asInteger(resproutage_min),
                 resproutprob = as.numeric(resproutprob), seeddistance_eff = asInteger(seeddistance_eff),
                 seeddistance_max = asInteger(seeddistance_max), sexualmature = asInteger(sexualmature),
                 shadetolerance = as.numeric(shadetolerance), species = as.character(species),
                 wooddecayrate = as.numeric(wooddecayrate))]
  return(species)
}