#' Create a visualizing Atlantis diagnostic tool object
#' 
#' This function creates a visualizing Atlantis diagnostic tool object which should be fed to the \code{vadt} function. This object can be quite large and this function may take a little while depending on how long you have run Atlantis for and the complexity of your model.
#' 
#'@param outdir Path to Atlantis output directory
#'@param fgfile Functional group file
#'@param biolprm Biology parameter file 
#'@param ncout Name of output ncdf4 file excluding nc suffix (i.e. name given after -o flag)
#'@param startyear Year that the model starts
#'@param toutinc Periodicity of writing output (in days)
#'@param diet Include diagnostic diet plots? default is TRUE
#'@import dplyr
#'@importFrom data.table data.table 
#'@importFrom ncdf4 nc_open
#'@importFrom ncdf4 ncvar_get
#'@importFrom plyr ldply
#'@importFrom plyr adply
#'@importFrom tidyr gather
#'@importFrom stringr str_split_fixed
#'@importFrom stringr str_trim
#'@export
#'@seealso \code{\link{vadt}}, \code{\link{animate_vadt}}
#'@examples
#'\dontrun{
#' outdir = "out/"
#' fgfile = "fg.csv"
#' biolprm = "bio.prm"
#' ncout = "atlantis_output"
#' startyear = 1948
#' toutinc = 30
#' diet = TRUE
#' obj <- create_vadt(outdir, fgfile, biolprm, ncout, startyear, toutinc, diet)
#' }
#' 

create_vadt <- function(outdir, fgfile, biolprm, ncout, startyear, toutinc, diet = TRUE){
  # contants
  nsecs <- 86400
  ndays <- 365
  g_per_ton <- 1e6
  xCN <- 5.7
  species <- c("BIRD", "FISH", "MAMMAL", "SHARK")
  tons <- function(mgN) return(mgN * 5.7 * 20 / 1e9)
  
  cat("### ------------ Reading in data                                         ------------ ###\n")
  nc_out <- nc_open(paste(outdir, ncout, ".nc", sep = ""))
  prod_out <- nc_open(paste(outdir, ncout, "PROD.nc", sep = ""))
  bio_agg <- read.table(paste(outdir, ncout, "BoxBiomass.txt", sep = ""), header = T)
  ssb <- read.table(paste(outdir, ncout, "SSB.txt", sep = ""), header = TRUE)
  yoy <- read.table(paste(outdir, ncout, "YOY.txt", sep = ""), header = TRUE)
  bgm <- readLines(paste(outdir, grep(".bgm",dir(outdir), value = T), sep = ""))
  fish_total <- read.table(paste(outdir, ncout, "Catch.txt", sep = ""), header = T)
  fish_fishery <- read.table(paste(outdir, ncout, "CatchPerFishery.txt", sep = ""), header = T)
  biomass <- read.table(paste(outdir, ncout, "BiomIndx.txt", sep = ""), header = T)
  rel_bio <- biomass[,c(1, grep("Rel",colnames(biomass)))]
  tot_bio <- biomass[,c(1:(grep("Rel",colnames(biomass))[1]-1))]
  fun_group <- read.csv(fgfile, header = T, stringsAsFactors = FALSE)#[, c(1,3, 4,5,6, 9,16, 12)]
  fun_group <- subset(fun_group, fun_group$IsTurnedOn == 1)
  
  # Set up fisheries files
  ts_act <- grep("TsAct", colnames(fish_total))
  fish_biomass_year <- fish_total[,1:(ts_act[1] - 1)]
  fish_tsact_year <- fish_total[, c(1, ts_act)]
  fish_biomass_year$Time <- startyear + fish_biomass_year$Time/365
  fish_tsact_year$Time <- startyear + fish_tsact_year$Time/365
  fishedFish <- colnames(fish_biomass_year)[-1]
  fishedFish <- fun_group[match(fishedFish, fun_group$Code), "Name"]
  colnames(fish_biomass_year) <- c("Time", fishedFish)
  colnames(fish_tsact_year) <- c("Time", fishedFish)
  colnames(fish_fishery) <- c("Time", "Fishery", fishedFish)
  fish_fishery_l <- fish_fishery %>%
   gather("Species", "biomass", 3:ncol(fish_fishery))
  fish_fishery_l$Time <- startyear + fish_fishery_l$Time/365
  
  
  ## Reads in and prepares the diet data if it's available
  if(diet){
    cat("### ------------ Setting up diet matrix plot                             ------------ ###\n")    
    diet <- read.table(paste(outdir, ncout, "DietCheck.txt", sep = ""), header = TRUE, stringsAsFactors = TRUE)
    if(any(names(diet) == "Predator")) {
      colnames(diet) <- c("Time", "Code", "Habitat", fun_group[,4])
    } else {
    colnames(diet) <- c("Time", "Code", "Cohort", "Stock", fun_group[,4])
    }
    diet <- merge(diet, fun_group[,c(1,2,4)])
    diet <- arrange(diet, Index)
    diet$Code <- diet$Name
    diet$Name <- NULL; diet$Index <- NULL
    
    if(any(names(diet) == "Habitat")){
    diet_l <- diet %>%
      gather("Prey", "eaten", 4:ncol(diet))
    colnames(diet_l) <- c("Predator","Time","Habitat", "Prey", "eaten")
    } else {
      diet_l <- diet %>%
        gather("Prey", "eaten", 5:ncol(diet))
      colnames(diet_l) <- c("Predator","Time","Cohort", "Stock", "Prey", "eaten")
    }
    diet_l$Time <- startyear + diet_l$Time/365
    tot_pred <- diet_l %>%
      group_by(Predator,Prey) %>%
      summarize(Eaten = mean(eaten))
    diet_l <- data.table(diet_l)
    tot_pred <- as.data.frame(tot_pred)
  } else {
    diet_l <- NULL
    tot_pred <- NULL
  }
  
  # Calculate by biomass by box - this is for the within-box plot
  biomass_by_box <- bio_agg %>%
    gather("Code", "value", -c(Time,Box))
  biomass_by_box <- merge(biomass_by_box, fun_group[,c(1, 2, 4)])
  biomass_by_box <- arrange(biomass_by_box, Index)
  biomass_by_box$Name <- factor(biomass_by_box$Name, levels = unique(biomass_by_box$Name))
  biomass_by_box$Time <- startyear + biomass_by_box$Time/365

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
  
  # todo: yoy as number of individuals
  colnames(yoy)[2:length(names(yoy))] <- str_split_fixed(names(yoy)[2:length(names(yoy))], pattern = '.0', n = 2)[,1]
  yoy <- yoy[-1,]  # exclude first cohort
  kwrr <- grep("KWRR", biolprm, value = T)
  kwrr_split <- unlist(str_split_fixed(kwrr, pattern = ' ', n = 2))
  kwrr_split <- apply(kwrr_split,2, str_trim, side = "both")
  kwrr_group <- str_split_fixed(kwrr_split[,1], pattern = '_', n = 2)[,2]
  kwrr_param <- str_split_fixed(kwrr_split[,2], pattern = ' ', n = 2)[,1]

  kwsr <- grep("KWSR", biolprm, value = T)
  kwsr_split <- unlist(str_split_fixed(kwsr, pattern = ' ', n = 2))
  kwsr_split <- apply(kwsr_split,2, str_trim, side = "both")
  kwsr_group <- str_split_fixed(kwsr_split[,1], pattern = '_', n = 2)[,2]
  kwsr_param <- str_split_fixed(kwsr_split[,2], pattern = ' ', n = 2)[,1]
  kwrr <- data.frame(group = kwrr_group, kwrr_param)
  kwsr <- data.frame(group = kwsr_group, kwsr_param)
  kwrr_kwsr_params <- merge(kwrr, kwsr)
  kwrr_kwsr_params$total <- as.double(as.character(kwrr_kwsr_params$kwrr_param)) + as.double(as.character(kwrr_kwsr_params$kwsr_param))
  kwrr_kwsr_params$total <- kwrr_kwsr_params$total * 5.720 / 1000000000
  for(i in 2:ncol(yoy)){
    findColumn <- kwrr_kwsr_params$group == names(yoy)[i]
    yoy[, i] <- yoy[,i] / kwrr_kwsr_params$total[findColumn]
  }
  
  # 
  ## Drop those functional groups that are not turned on
  fun_group <- fun_group[fun_group$IsTurnedOn == 1,]# c(1,3:8)]

  if(sum(names(fun_group) == "InvertType") > 0)
    names(fun_group)[names(fun_group) == "InvertType"] <- "GroupType"
  
  # Subset invertebrates and vertebrates
  rs_names <- str_trim(fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]) # trim white space
  rs_codes <- str_trim(fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Code"])
  invert_names <- fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")),]
  
  # Rename columns
  colnames(ssb) <- c("Time", rs_names)
  colnames(yoy) <- c("Time", rs_names)
  
  # Biomass Long
  tot_bio_l <- tot_bio %>%
    gather("Code", "value", -Time)
  
  # Subset vertebrates
  tot_bio_v <- filter(tot_bio_l, Code %in% rs_codes)
  # Subset invertebrates
  tot_bio_i <- filter(tot_bio_l, !(Code %in% rs_codes))
  tot_bio_v <- merge(tot_bio_v, fun_group[c(1,2,4)], by = "Code")
  tot_bio_v <- arrange(tot_bio_v, Index)
  tot_bio_v$Name <- factor(tot_bio_v$Name, levels = unique(tot_bio_v$Name))
  tot_bio_v$Time <- startyear + tot_bio_v$Time/365
  tot_bio_i <- merge(tot_bio_i, fun_group[c(1,2,4)], by = "Code")
  tot_bio_i <- arrange(tot_bio_i, Index)
  tot_bio_i$Name <- factor(tot_bio_i$Name, levels = unique(tot_bio_i$Name))
  tot_bio_i$Time <- startyear + tot_bio_i$Time/365
  
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
  
  # Find area of boxes
  areaboxes <- grep("area", bgm, value = TRUE)
  areaboxes <- strsplit(areaboxes, "area\t")
  boxarea <- as.numeric(sapply(areaboxes,`[`, 2))/1e6 # to km2
  boxnr <-sapply(areaboxes,`[`, 1)
  boxnr <- strsplit(boxnr, "box")
  boxnr <- sapply(boxnr,`[`, 2)
  boxnr <- as.numeric(gsub('.$', '', boxnr))
  areaboxes <- data.frame('box' = boxnr, 'area' = boxarea)
  areaboxes <-areaboxes[order(areaboxes$box),]
  
  
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
 
  cat("### ------------ Setting up disaggregated spatial plots                  ------------ ###\n")
  # Include all layer-box-time combos
  # nums <- grep("Nums", var_names, value = TRUE)
  # N <- grep("_N", var_names, value = TRUE)
  # N <- N[-grep("_Nums", N, value = FALSE)]
  # tot_num <- c(nums)
  # 
  # # extract tracers from the ncdf4 object
  # vars <- list()
  # for (i in 1:length(tot_num)){
  #   vars[[i]] <- ncvar_get(nc = nc_out, varid = tot_num[i])
  # }
  # names(vars) <- tot_num
  
  # Just layer-time combos
  nums <- grep("Nums", var_names, value = TRUE)
  N <- grep("_N", var_names, value = TRUE)
  N <- N[-grep("_Nums", N, value = FALSE)]
  tot_num <- c(nums)
  str_N <- grep("StructN", var_names, value = TRUE)
  res_N <- grep("ResN", var_names, value = TRUE)
  
  
  # extract tracers from the ncdf4 object (vertebrates)
  vars <- list()
  dens <- list()
  sn_list <- list()
  rn_list <- list()
  for (i in 1:length(tot_num)){
    tempNums <- ncvar_get(nc = nc_out, varid = tot_num[i])
    dens[[i]] <- apply(tempNums,c(2,3),sum)/areaboxes$area
    vars[[i]] <- tempNums
    tempRN <- ncvar_get(nc = nc_out, varid = res_N[i])
    tempRN[tempNums<=1e-16] <- NA  
    tempSN <- ncvar_get(nc = nc_out, varid = str_N[i])
    tempSN[tempNums<=1e-16] <- NA
    sn_list[[i]] <- tempSN
    rn_list[[i]] <- tempRN
  }
  names(vars) <- tot_num
  names(dens) <- tot_num
  names(sn_list) <- str_N
  names(rn_list) <- res_N
  
  # Create Erla's plots
  nominal_dz <- ncvar_get(nc = nc_out, varid = "nominal_dz")
  depth_layers <- nominal_dz[,which.max(colSums(nominal_dz))]
  depth_layers <- depth_layers[-c(1, length(depth_layers))]
  depth_layers <- cumsum(rev(depth_layers))
  
  depth_labels <- rep(NA, (length(depth_layers) + 1))
  for(i in 1:(length(depth_layers) + 1)){
    if(i == 1){
      depth_labels[i] <- paste("0 - ", depth_layers[i], sep = "")
    } else if(i == (length(depth_layers) + 1))
    {
      depth_labels[i] <- paste(depth_layers[i - 1], " + ", sep ="")
    } else depth_labels[i] <- paste(depth_layers[i - 1], " - ",  depth_layers[i], sep ="")
  }
  depth_labels <- c(depth_labels, "Sediment")
  
  vert_names <- fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Code"]
  mat_age <- grep("_age_mat", biolprm, value = T)
  mat_names <- str_split_fixed(mat_age, "_", n = 2)[,1]
  species_ids <- str_split_fixed(mat_age, "_age_mat", n = 2)[,1]
  #juvenile_age <- as.numeric(gsub("[^\\d]+", "", mat_age, perl=TRUE))[which(species_ids %in% vert_names)]
  get_first_number<-function(x){
    yy<-gsub("([^\\d])","#",x,perl=TRUE)
    yyy<-unlist(str_split(yy,"#"))
    xPos<-grep("[^\\d]",yyy)[1]
    thisNum<-as.numeric(yyy[xPos])
  }
  temp <- lapply(mat_age,FUN=get_first_number)
  juvenile_age <- as.numeric(unlist(temp))
  juvenile_age <- data.frame(mat_names, juvenile_age)
  species_ids <- species_ids[which(species_ids %in% vert_names)]
  
  erla_plots <- list()
  for(i in 1:length(species_ids)){
    spp <- fun_group[fun_group$Code == species_ids[i],c("Name", "NumCohorts")]
    spp <- str_trim(spp)
    if(juvenile_age[i] != 1){
      juv <- paste(spp[[1]], 1:(juvenile_age[i]+1), "_Nums", sep = "")   
      ad <- paste(spp[[1]], (juvenile_age[i]+2):spp[[2]], "_Nums", sep = "") 
      # Create the juveniles data
      juv_tmp <- NULL
      for(j in juv){
        x <- adply(vars[[j]], c(1, 3))
        juv_tmp <- rbind(juv_tmp, x)
      }
      juv_tmp <- juv_tmp %>%
        group_by(X1, X2) %>%
        summarize_each(funs(sum))
      colnames(juv_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(juv_tmp)-3), sep =" "))
      juv_tmp$Layer <- factor(juv_tmp$Layer,levels(juv_tmp$Layer)[c(((length(unique(juv_tmp$Layer)))-1):1, length(unique(juv_tmp$Layer)))])
      levels(juv_tmp$Layer) <- depth_labels
      juv_tmp <- as.data.frame(juv_tmp)
      juv_tmp <- gather(juv_tmp, Box, number, 3:ncol(juv_tmp))
      
      erla_plots[[paste(spp[[1]], "Juvenile")]] <- juv_tmp
      
      # Create the adults data
      ad_tmp <- NULL
      for(j in ad){
        x <- adply(vars[[j]], c(1, 3))
        ad_tmp <- rbind(ad_tmp, x)
      }
      
      ad_tmp <- ad_tmp %>%
        group_by(X1, X2) %>%
        summarize_each(funs(sum))
      colnames(ad_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(ad_tmp)-3), sep =" "))
      ad_tmp$Layer <- factor(ad_tmp$Layer,levels(ad_tmp$Layer)[c(((length(unique(ad_tmp$Layer)))-1):1, length(unique(ad_tmp$Layer)))])
      levels(ad_tmp$Layer) <- depth_labels
      ad_tmp <- as.data.frame(ad_tmp)
      ad_tmp <- gather(ad_tmp, Box, number, 3:ncol(ad_tmp))
      
      erla_plots[[paste(spp[[1]], "Adult")]] <- ad_tmp
    } 
    else {
      ad <- paste(spp[[1]], juv_age:spp[[2]], "_Nums", sep = "")
      ad_tmp <- NULL
      for(j in ad){
        x <- adply(vars[[j]], c(1, 3))
        ad_tmp <- rbind(ad_tmp, x)
      }
      ad_tmp <- ad_tmp %>%
        group_by(X1, X2) %>%
        summarize_each(funs(sum))
      colnames(ad_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(ad_tmp)-3), sep =" "))
      ad_tmp$Layer <- factor(ad_tmp$Layer,levels(ad_tmp$Layer)[c(((length(unique(ad_tmp$Layer)))-1):1, length(unique(ad_tmp$Layer)))])
      levels(ad_tmp$Layer) <- depth_labels
      ad_tmp <- as.data.frame(ad_tmp)
      ad_tmp <- gather(ad_tmp, Box, number, 3:ncol(ad_tmp))
      erla_plots[[paste(spp[[1]], "Adult")]] <- ad_tmp
    }
  }
  
  # --- End Erla Plots -- #
  
  # --- pPrey code ---- #
  pPREY <- data.frame(matrix(NA,ncol=(2*length(species_ids)),nrow=dim(fun_group)[1]))
  rownames(pPREY)<-fun_group$Code
  colnames(pPREY)<-paste(sort(rep(species_ids,2)),seq(1,2),sep="")
  
  for(i in 1:length(species_ids)){
    thisCode<-species_ids[i]
    spp <- fun_group[fun_group$Code == thisCode,c("Name", "NumCohorts")]
    spp <- str_trim(spp)
    for(age in 1:2){
      thisPreyVarNames<-paste("pPREY",seq(1,2),thisCode,age,sep="")
      xx<-str_trim(biolprm[grep(thisPreyVarNames[1],biolprm)+1])
      if(length(grep("\t", xx))>0) ## check for tab seperation and replace with " "
        xx <- gsub("\t", " ", xx)
      xx<-as.double(unlist(str_split(xx," ")))
      yy<-str_trim(biolprm[grep(thisPreyVarNames[2],biolprm)+1])
      if(length(grep("\t", yy))>0)  ## check for tab seperation and replace with " "
        yy <- gsub("\t", " ", yy)
      yy<-as.double(unlist(str_split(yy," ")))
      thisPreyAvail<-xx+yy
      thisColName<-paste(thisCode,age,sep="")
      pPREY[,match(thisColName,colnames(pPREY))]<-thisPreyAvail[1:dim(fun_group)[1]]
    }
  }
  # ------------------- #
  
  # extract physical tracers from the ncdf4 object
  phy_names <- names(nc_out$var)[!(names(nc_out$var) %in% tot_num)]
  phy_names <- phy_names[-grep("_ResN", phy_names)] 
  phy_names <- phy_names[-grep("_StructN", phy_names)]
  vert_N <- paste(str_trim(rs_names), "_N", sep = "")
  phy_names <-  phy_names[!(phy_names %in% vert_N)]
  phy_names <- phy_names[-which(phy_names == "nominal_dz")]
  invert_nums <- grep("_N", phy_names, value = F)
  invert_mnames <- phy_names[invert_nums]
  trace_names <- phy_names[-(invert_nums)]
  
  invert_vars <- list()
  for (i in 1:length(invert_mnames)){
    tmp <- ncvar_get(nc = nc_out, varid = invert_mnames[i])
  
    if(length(dim(tmp)) == 2){
      if(dim(tmp)[1] == numboxes){
        tmp_invert <- tmp
        tmp_invert <- as.data.frame(tmp_invert)
        tmp_invert$Box <- paste("Box", 0:(numboxes - 1))
        tmp_invert <- gather(tmp_invert, Time, value = number, 1:(ncol(tmp_invert)-1))
        tmp_invert$Time <- substring(tmp_invert$Time, 2)
        tmp_invert$Time <- as.numeric(as.character(tmp_invert$Time))
        erla_plots[[invert_mnames[i]]] <- tmp_invert
        invert_vars[[i]] <- tmp
      }
    } else{
       tmp_invert <- adply(tmp, c(1,3))
        # tmp_invert <- tmp_array %>%
        #   group_by(X1, X2) %>%
        #   summarize_each(funs(sum))
        colnames(tmp_invert) <- c("Layer", "Time", paste("Box", 0:(ncol(tmp_invert)-3), sep =" "))
        tmp_invert$Layer <- factor(tmp_invert$Layer,levels(tmp_invert$Layer)[c(((length(unique(tmp_invert$Layer)))-1):1, length(unique(tmp_invert$Layer)))])
        levels(tmp_invert$Layer) <- depth_labels
        tmp_invert <- as.data.frame(tmp_invert)
        tmp_invert <- gather(tmp_invert, Box, number, 3:ncol(tmp_invert))
        invert_vars[[i]] <- tmp
        erla_plots[[invert_mnames[i]]] <- tmp_invert
    }
  }
  names(invert_vars) <- invert_mnames
    
  trace_vars <- list()
  for (i in 1:length(trace_names)){
    tmp <- ncvar_get(nc = nc_out, varid = trace_names[i])
    if(length(dim(tmp)) == 2){
      if(dim(tmp)[1] == numboxes){
        tmp_trace <- tmp
        tmp_trace <- as.data.frame(tmp_trace)
        tmp_trace$Box <- paste("Box", 0:(numboxes - 1))
        tmp_trace <- gather(tmp_trace, Time, value = number, 1:(ncol(tmp_trace)-1))
        tmp_invert$Time <- substring(tmp_invert$Time, 2)
        tmp_invert$Time <- as.numeric(as.character(tmp_invert$Time))
        erla_plots[[trace_names[i]]] <- tmp_trace
        trace_vars[[i]] <- tmp
      }
    } else{
      tmp_array <- adply(tmp, c(1,3))
      tmp_trace <- tmp_array %>%
        group_by(X1, X2) %>%
        summarize_each(funs(sum))
      colnames(tmp_trace) <- c("Layer", "Time", paste("Box", 0:(ncol(tmp_trace)-3), sep =" "))
      tmp_trace$Layer <- factor(tmp_trace$Layer,levels(tmp_trace$Layer)[c(((length(unique(tmp_trace$Layer)))-1):1, length(unique(tmp_trace$Layer)))])
      levels(tmp_trace$Layer) <- depth_labels
      tmp_trace <- as.data.frame(tmp_trace)
      tmp_trace <- gather(tmp_trace, Box, number, 3:ncol(tmp_trace))
      trace_vars[[i]] <- tmp
      erla_plots[[trace_names[i]]] <- tmp_trace
    }
  }
  names(trace_vars) <- trace_names
  
  
  cat("### ------------ Setting up data for production output                   ------------ ###\n")
  cat("### ------------ This part takes a while.                                ------------ ###\n")
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
  # ------------------------------------- #
  # - Reserve/Structural Nitrogen Plots - #
  # ------------------------------------- #
  str_N <- grep("StructN", var_names, value = TRUE)
  res_N <- grep("ResN", var_names, value = TRUE)
  
  # Subset invertebrates
  invert_names <-fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")),]
  
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
    adply(x, 3, mean)
  }
  
  sum_agg <- function(x){
    adply(x, 3, sum)
  }
  
  structN <- ldply(sn_list, mean_agg)
  reserveN <- ldply(rn_list, mean_agg)
  totalnums <- ldply(vars, sum_agg)
  
  structN$.id <- factor(structN$.id, levels = unique(structN$.id))
  structN$Time <- as.numeric(as.character(structN$X1)) * toutinc / 365 + startyear
  
  reserveN$.id <- factor(reserveN$.id, levels = unique(reserveN$.id))
  reserveN$Time <- as.numeric(as.character(reserveN$X1)) * toutinc / 365 + startyear
  
  totalnums$.id <- factor(totalnums$.id, levels = unique(totalnums$.id))
  totalnums$Time <- as.numeric(as.character(totalnums$X1)) * toutinc / 365 + startyear
  
  output <- list(disagg = vars,invert_vars = invert_vars, invert_mnames = invert_mnames, trace_vars = trace_vars, trace_names = trace_names, var_names = tot_num, max_layers = max_layers, max_time = max_time, bioagg_names = bioagg_names, rs_names = rs_names, tot_pred = tot_pred, ssb_names = ssb_names, yoy_names = yoy_names, islands = islands, rel_bio = rel_bio, tot_bio = tot_bio, ssb = ssb, yoy = yoy, structN = structN, reserveN = reserveN, totalnums = totalnums, map_base = map_base, numboxes = numboxes, fun_group = fun_group, invert_names = invert_names, invert_l = invert_l, vert_l = vert_l, ab_params = ab_params, diet_l = diet_l, erla_plots = erla_plots, toutinc = toutinc, startyear = startyear, tot_bio_v = tot_bio_v, tot_bio_i = tot_bio_i, biomass_by_box = biomass_by_box, fgnames = fun_group[,4], fish_fishery_l = fish_fishery_l, fish_tsact_year = fish_tsact_year, fish_biomass_year = fish_biomass_year, fishedFish = fishedFish, pPREY = pPREY, dens = dens)
  cat("### ------------ vat object created, you can now run the vat application ------------ ###\n") 
  return(output)
  class(output) <- "vadt"
}