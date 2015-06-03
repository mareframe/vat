#' Create a visualising Atlantis toolbox object
#' 
#' This function creates a visualising Atlantis toolbox object which should be fed to the \code{vat} function. This object can be quite and this function may take a little while depending on how long you have run Atlantis for and how big your model is.
#' 
#'  @param outdir Path to Atlantis output directory
#'  @param fgfile Functional group file
#'  @param biolprm Biology parameter file 
#'  @param ncout Name of output ncdf4 file excluding nc suffix (i.e. name given after -o flag)
#'  @param startyear Year that the model starts
#'  @param toutinc Periodicity of writing output (in days)
#'  @export
#'  @seealso \code{\link{vat}}, \code{\link{animate_vat}}
#'  @examples
#'  \dontrun{
#' obj <- create_vat(outdir = "/atlantis/output_dir/", fgfile = "/atlantis/functionalgroup.csv", biolprm = "/atlantis/biol.prm", ncout = "output_atlantis", startyear = 1948, toutinc = 30)
#'  }
create_vat <- function(outdir, fgfile, biolprm, ncout, startyear, toutinc){
  require("ncdf4")
  require("plyr")
  require("dplyr")
  require("tidyr")
  require("stringr")
  require("DT")
  cat("### ------------ Reading in data                                         ------------ ###\n")
  nc_out <- ncdf4::nc_open(paste(outdir, ncout, ".nc", sep = ""))
  prod_out <- ncdf4::nc_open(paste(outdir, ncout, "PROD.nc", sep = ""))
  bio_agg <- read.table(paste(outdir, ncout, "BoxBiomass.txt", sep = ""), header = T)
  ssb <- read.table(paste(outdir, ncout, "SSB.txt", sep = ""), header = TRUE)
  yoy <- read.table(paste(outdir, ncout, "YOY.txt", sep = ""), header = TRUE)
  bgm <- readLines(paste(outdir, grep(".bgm",dir(outdir), value = T), sep = ""))
  biomass <- read.table(paste(outdir, ncout, "BiomIndx.txt", sep = ""), header = T)
  rel_bio <- biomass[,c(1, grep("Rel",colnames(biomass)))]
  tot_bio <- biomass[,c(1:(grep("Rel",colnames(biomass))[1]-1))]
  diet <- read.table(paste(outdir, ncout, "DietCheck.txt", sep = ""), header = TRUE, stringsAsFactors = TRUE)
  
  # Extract a and b parameters from biology parameter
  biolprm <- readLines(biolprm)
  biol_a <- grep("li_a", biolprm, value = TRUE)
  biol_b <- grep("li_b", biolprm, value = TRUE)
  a_split <- unlist(str_split_fixed(biol_a, pattern = ' ', n = 2))
  a_split <- apply(a_split,2, str_trim, side = "both")
  a_group <- a_split[,1]
  a_param <- str_split_fixed(a_split[,2], pattern = ' ', n = 2)[,1]
  
  # trim trailing white space
  b_split <- unlist(str_split_fixed(biol_b, pattern = ' ', n = 2))
  b_split <- apply(b_split,2, str_trim, side = "both")
  b_group <- b_split[,1]
  b_param <- str_split_fixed(b_split[,2], pattern = ' ', n = 2)[,1]
  
  ab_params <- data.frame(a_name = a_group, a = as.numeric(as.character(a_param)),
                          b_name = b_group, b = as.numeric(as.character(b_param)))
  
  
  
  fun_group <- read.csv(fgfile, header = T, stringsAsFactors = FALSE)#[, c(1,3, 4,5,6, 9,16, 12)]
  
  
  ## Drop those functional groups that are not turned on
  fun_group <- fun_group[fun_group$IsTurnedOn == 1,]# c(1,3:8)]
  
  #fun_group$isFished <- ifelse(fun_group$isFished == 1, "Yes", "No")
  #fun_group$isAssessed <- ifelse(fun_group$isAssessed == 1, "Yes", "No")
  
  if(sum(names(fun_group) == "InvertType") > 0)
    names(fun_group)[names(fun_group) == "InvertType"] <- "GroupType"
  
  # Subset vertebrates
  rs_names <- fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]
  
  # Subset invertebrates
  invert_names <-fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")),]
  
  colnames(ssb) <- c("Time", rs_names)
  colnames(yoy) <- c("Time", rs_names)
  
  colnames(rel_bio) <- c("Time", fun_group$Name, "DIN")
  colnames(tot_bio) <- c("Time", fun_group$Name, "DIN")
  rel_bio$Time <- startyear + rel_bio$Time/365
  tot_bio$Time <- startyear + tot_bio$Time/365
  ssb$Time <- startyear + ssb$Time/365
  yoy$Time <- startyear + yoy$Time/365
  
  cat("### ------------ Creating dynamic labels for vat                         ------------ ###\n")
  bioagg_names <- colnames(bio_agg)[c(-1,-2)]
  ssb_names <- colnames(ssb)[-1]
  yoy_names <- colnames(yoy)[-1]
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
  diet_l <- diet %>%
    gather("Prey", "eaten", 4:ncol(diet))
  colnames(diet_l) <- c("Time", "Predator", "Habitat", "Prey", "eaten")
  tot_pred <- diet_l %>%
    group_by(Predator,Prey) %>%
    summarize(Eaten = mean(eaten))
  
  cat("### ------------ Setting up disaggregated spatial plots                  ------------ ###\n")
  nums <- grep("Nums", var_names, value = TRUE)
  N <- grep("_N", var_names, value = TRUE)
  N <- N[-grep("_Nums", N, value = FALSE)]
  tot_num <- c(nums)
  
  # extract tracers from the ncdf4 object
  vars <- list()
  for (i in 1:length(tot_num)){
    vars[[i]] <- ncdf4::ncvar_get(nc = nc_out, varid = tot_num[i])
  }
  names(vars) <- tot_num
  
  # extract physical tracers from the ncdf4 object
  phy_names <- names(nc_out$var)[!(names(nc_out$var) %in% tot_num)]
  phy_names <- phy_names[-grep("_ResN", phy_names)] 
  phy_names <- phy_names[-grep("_StructN", phy_names)]
  phy_names <-  phy_names[!(phy_names %in% N[1:last(which(fun_group$NumCohorts == 10))])]
  phy_names <- phy_names[-which(phy_names == "nominal_dz")]
  invert_nums <- grep("_N", phy_names, value = F)
  invert_mnames <- phy_names[invert_nums]
  trace_names <- phy_names[-(invert_nums)]
  
  invert_vars <- list()
  for (i in 1:length(invert_mnames)){
    invert_vars[[i]] <- ncdf4::ncvar_get(nc = nc_out, varid = invert_mnames[i])
  }
  names(invert_vars) <- invert_mnames
  
  trace_vars <- list()
  for (i in 1:length(trace_names)){
    trace_vars[[i]] <- ncdf4::ncvar_get(nc = nc_out, varid = trace_names[i])
  }
  names(trace_vars) <- trace_names
  
  
  cat("### ------------ Setting up data for production output                   ------------ ###\n")
  # Create the production output
  prod_names <- names(prod_out$var)
  t <- prod_out$dim$t$vals
  time <- t/60/60/24/365
  time <- startyear + time
  b <- prod_out$dim$b$vals
  z <- prod_out$dim$z$vals
  
  # Seperate the vertebrate and invertebrate groups
  vert_group <- unique(grep(paste(rs_names, collapse ="|"), prod_names, value = TRUE))
  invert_group <- unique(grep(paste(invert_names$Name, collapse ="|"), prod_names, value = TRUE))
  
  # Read in the data and put it into long format
  invert_all <- list()
  for(i in invert_group){
    invert_all[[i]] <- ncvar_get(prod_out, i)
  }
  
  invert_all <- ldply(invert_all, colSums)
  colnames(invert_all) <- c("id", time)
  invert_l <- invert_all %>%
    gather("variable", "value", -id)
  
  vert_all <- list()
  for(i in vert_group){
    vert_all[[i]] <- ncvar_get(prod_out, i)
  }
  vert_all <- ldply(vert_all, colSums)
  colnames(vert_all) <- c("id", time)
  vert_l <- vert_all %>%
    gather("variable", "value", -id)
  
  cat("### ------------ Setting up aggregated diagnostic plots                  ------------ ###\n")
  cat("### ------------ This part takes a while. Better grab a kleina.          ------------ ###\n")
  # ------------------------------------ #
  # - Reserve/Structural Nitrogen Plots - #
  # ------------------------------------ #
  str_N <- grep("StructN", var_names, value = TRUE)
  res_N <- grep("ResN", var_names, value = TRUE)
  
  # Subset vertebrates
  rs_names <- fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]
  
  # Subset invertebrates
  invert_names <-fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")),]
  
  #colnames(fun_group) <- c("Code", "Name", "Long Name", "Number of Age Groups", "Is it Fished?", "Is it Assessed?", "Type of Group")
  
  
  sn_list <- list()
  rn_list <- list()
  for (i in 1:length(str_N)){
    sn_list[[i]] <- ncvar_get(nc = nc_out, varid = str_N[i])
    rn_list[[i]] <- ncvar_get(nc = nc_out, varid = res_N[i])  
  }
  names(sn_list) <- str_N
  names(rn_list) <- res_N
  
  # Aggregate arrays
  mean_agg <- function(x){
    plyr::adply(x, 3, mean)
  }
  
  sum_agg <- function(x){
    plyr::adply(x, 3, sum)
  }
  
  structN <- plyr::ldply(sn_list, mean_agg)
  reserveN <- plyr::ldply(rn_list, mean_agg)
  totalnums <- plyr::ldply(vars, sum_agg)
  
  structN$.id <- factor(structN$.id, levels = unique(structN$.id))
  structN$Time <- as.numeric(as.character(structN$X1)) * toutinc / 365 + startyear
  
  #structN$Time <- as.numeric(as.character(structN$X1))/12 + startyear
  
  reserveN$.id <- factor(reserveN$.id, levels = unique(reserveN$.id))
  reserveN$Time <- as.numeric(as.character(reserveN$X1)) * toutinc / 365 + startyear
  #reserveN$Time <- as.numeric(as.character(reserveN$X1))/12 + startyear
  
  totalnums$.id <- factor(totalnums$.id, levels = unique(totalnums$.id))
  totalnums$Time <- as.numeric(as.character(totalnums$X1)) * toutinc / 365 + startyear
  #totalnums$Time <- as.numeric(as.character(totalnums$X1))/12 + startyear
  
  output <- list(disagg = vars,invert_vars = invert_vars, invert_mnames = invert_mnames, trace_vars = trace_vars, trace_names = trace_names, var_names = tot_num, max_layers = max_layers, max_time = max_time, bioagg_names = bioagg_names, rs_names = rs_names, tot_pred = tot_pred, ssb_names = ssb_names, yoy_names = yoy_names, islands = islands, rel_bio = rel_bio, tot_bio = tot_bio, ssb = ssb, yoy = yoy, structN = structN, reserveN = reserveN, totalnums = totalnums, map_base = map_base, numboxes = numboxes, fun_group = fun_group, invert_names = invert_names, invert_l = invert_l, vert_l = vert_l, ab_params = ab_params)
  cat("### ------------ vat object created, you can now run the vat application ------------ ###\n") 
  return(output)
  class(output) <- "vat"
}