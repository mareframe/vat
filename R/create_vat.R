#' Create a visualising Atlantis toolbox object
#' 
#' This function creates a visualising Atlantis toolbox object which should be fed to the \code{vat} function. This object can be quite and this function may take a little while depending on how long you have run Atlantis for and how big your model is.
#' 
#'  @param outdir Path to Atlantis output directory
#'  @param fgfile Functional group 
#'  @param ncout Name of output ncdf4 file excluding nc suffix (i.e. name given after -o flag)
#'  @param starttime Year that the model starts
#'  @export
#'  @seealso \code{\link{vat}}, \code{\link{animate_vat}}
#'  @examples
#'  \dontrun{
#' obj <- create_vat(outdir = "/atlantis/output_dir/", fgfile = "/atlantis/functionalgroup.csv", ncout = "output_atlantis", starttime = 1948)
#'  }
create_vat <- function(outdir, fgfile, ncout){
  cat("### ------------ Reading in data                                         ------------ ###\n")
  nc_out <- ncdf4::nc_open(paste(outdir, ncout, ".nc", sep = ""))
  bio_agg <- read.table(paste(outdir, ncout, "BoxBiomass.txt", sep = ""), header = T)
  ssb <- read.table(paste(outdir, ncout, "SSB.txt", sep = ""), header = TRUE)
  yoy <- read.table(paste(outdir, ncout, "YOY.txt", sep = ""), header = TRUE)
  bgm <- readLines(paste(outdir, grep(".bgm",dir(outdir), value = T), sep = ""))
  biomass <- read.table(paste(outdir, ncout, "BiomIndx.txt", sep = ""), header = T)
  rel_bio <- biomass[,c(1, grep("Rel",colnames(biomass)))]
  diet <- read.table(paste(outdir, ncout, "DietCheck.txt", sep = ""), header = TRUE, stringsAsFactors = TRUE)
  fun_group <- read.csv(fgfile, header = T, stringsAsFactors = FALSE)[, c(1,4,5,6, 9,16, 12)]
  fun_group$isFished <- ifelse(fun_group$isFished == 1, "Yes", "No")
  fun_group$isAssessed <- ifelse(fun_group$isAssessed == 1, "Yes", "No")
  
  
  cat("### ------------ Creating dynamic labels for vat                         ------------ ###\n")
  bioagg_names <- colnames(bio_agg)[c(-1,-2)]
  ssb_names <- colnames(ssb)[-1]
  yoy_names <- colnames(yoy)[-1]
  rel_names <- colnames(rel_bio)[-1]
  max_tracer <- nc_out$nvars
  max_layers <- length(nc_out$dim$z$vals)
  max_time <- length(nc_out$dim$t$vals)
  var_names <- names(nc_out$var)
  
  cat("### ------------ Creating map from BGM file                              ------------ ###\n")
 
  # Find number of boxes
  numboxes <- length(grep("# Box number", bgm))
  
  # Extract the box vertices
  vertices <- data.frame()
  for(i in 1:numboxes){
    vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), bgm)
    vertices <- rbind(vertices, cbind(i - 1, bgm[vert_tmp]))
  }
  
  # Extract latitude and longitude
  coords_tmp <- strsplit(as.character(vertices$V2), " ")
  x <- sapply(coords_tmp,`[`,2)
  y <- sapply(coords_tmp,`[`,3)
  
  # Create the map for ggplot2
  map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
  map_base$x <- as.double(as.character(map_base$x))
  map_base$y <- as.double(as.character(map_base$y))
  
  # Check for islands
  
  islands <- grep("botz\t0", bgm, value = T)
  if(length(islands > 0)){
    islands <- strsplit(islands, "[.]")
    islands <- sapply(islands,`[`, 1)
    islands <- strsplit(islands, "box")
    islands <- sapply(islands,`[`, 2)
  }
  cat("### ------------ Setting up diet matrix plot                             ------------ ###\n")
  diet$Predator <- factor(diet$Predator, levels = unique(diet$Predator))
  diet <- subset(diet, Time == unique(diet$Time)[2])
  diet$Habitat <- ifelse(diet$Habitat == "WC", "Water Column", ifelse(diet$Habitat == "SED", "Sediment", "Epibenthic"))
  diet$Time <- NULL
  diet_m <- reshape::melt(diet, id.vars = c("Predator", "Habitat"))
  
  cat("### ------------ Setting up disaggregated spatial plots                  ------------ ###\n")
  nums <- grep("Nums", var_names, value = TRUE)
  N <- grep("_N", var_names, value = TRUE)
  N <- N[-grep("_Nums", N, value = FALSE)]
  tot_num <- c(nums)
  
  # extract tracers for the ncd4 object
  vars <- list()
  for (i in 1:length(tot_num)){
    vars[[i]] <- ncdf4::ncvar_get(nc = nc_out, varid = tot_num[i])
  }
  names(vars) <- tot_num
  
  
  cat("### ------------ Setting up aggregated diagnostic plots                  ------------ ###\n")
  cat("### ------------ This part takes a while. Better grab a Snickers.        ------------ ###\n")
  # ------------------------------------ #
  # - Reserve/Structual Nitrogen Plots - #
  # ------------------------------------ #
  str_N <- grep("StructN", var_names, value = TRUE)
  res_N <- grep("ResN", var_names, value = TRUE)
  
  # Subset vertebrates
  rs_names <- fun_group[fun_group$InvertType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]
  
  # Subset invertebrates
  invert_names <-fun_group[!(fun_group$InvertType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")),]
  
  colnames(fun_group) <- c("Code", "Name", "Long Name", "Number of Age Groups", "Is it Fished?", "Is it Assessed?", "Type of Group")
  
  
  sn_list <- list()
  rn_list <- list()
  for (i in 1:length(str_N)){
    sn_list[[i]] <- ncvar_get(nc = nc_out, varid = str_N[i])
    rn_list[[i]] <- ncvar_get(nc = nc_out, varid = res_N[i])  
  }
  names(sn_list) <- str_N
  names(rn_list) <- res_N
  
  # Aggregate arrays
  agg <- function(x){
    plyr::adply(x, 3, sum)
  }
  
  structN <- plyr::ldply(sn_list, agg)
  reserveN <- plyr::ldply(rn_list, agg)
  totalnums <- plyr::ldply(vars, agg)
  
  structN$.id <- factor(structN$.id, levels = unique(structN$.id))
  structN$Time <- as.numeric(as.character(structN$X1))/12 + starttime
  
  reserveN$.id <- factor(reserveN$.id, levels = unique(reserveN$.id))
  reserveN$Time <- as.numeric(as.character(reserveN$X1))/12 + starttime
  
  totalnums$.id <- factor(totalnums$.id, levels = unique(totalnums$.id))
  totalnums$Time <- as.numeric(as.character(totalnums$X1))/12 + starttime
  
  output <- list(disagg = vars,var_names = tot_num, max_layers = max_layers, max_time = max_time, bioagg_names = bioagg_names, rs_names = rs_names, diet_m = diet_m, rel_names = rel_names, ssb_names = ssb_names, yoy_names = yoy_names, islands = islands, rel_bio = rel_bio, ssb = ssb, yoy = yoy, structN = structN, reserveN = reserveN, totalnums = totalnums, map_base = map_base, numboxes = numboxes, fun_group = fun_group, invert_names = invert_names)
  cat("### ------------ vat object created, you can now run the vat application ------------ ###\n") 
  return(output)
  class(output) <- "vat"
}