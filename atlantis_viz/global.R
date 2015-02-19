# Load requirement packages
library("ncdf4")
library("reshape")
library("plyr")

# -------------------- #
# - DATA PREPARATION - #
# -------------------- #

# Functional CSV
fg <- read.csv("~/Dropbox/atlantis_shared/steady_state_run/GroupsIceland.csv", header = T, stringsAsFactors = FALSE)[,c(1,4,12)]

# Output
output <- nc_open("/home/chris/Dropbox/atlantis_shared/steady_state_run/steady_state_output/output_iceland.nc")
bio_agg <- read.table("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/output_icelandBoxBiomass.txt", header = T)
biomass <- read.table("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/output_icelandBiomIndx.txt", header = T)
ssb <- read.table("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/output_icelandSSB.txt", header = TRUE)
yoy <- read.table("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/output_icelandYOY.txt", header = TRUE)
bgm <- readLines("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/atlantis_L93.bgm")
match <- grep("Rel",colnames(biomass))  # identifies relative biomass
relative <- biomass[,c(1,match)]

# For diet plots
diet <- read.table("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/steady_state_run/steady_state_output/output_icelandDietCheck.txt", header = TRUE, stringsAsFactors = TRUE)
diet$Predator <- factor(diet$Predator, levels = unique(diet$Predator))
diet <- subset(diet, Time == unique(diet$Time)[2])
diet$Time <- NULL

epibenthic <- subset(diet, diet$Habitat == "EPIBENTHIC")
wc <- subset(diet, diet$Habitat == "WC") 
sed <- subset(diet, diet$Habitat == "SED") 
epi.m <- melt(epibenthic[,-2], id.vars = "Predator")
wc.m <- melt(wc[,-2], id.vars = "Predator")
sed.m <- melt(sed[,-2], id.vars = "Predator")

# extract tracer numbers, levels for the sliders,
# and relative biomass
bio_agg_names <- colnames(bio_agg)[c(-1,-2)]
ssb_names <- colnames(ssb)[-1]
yoy_names <- colnames(yoy)[-1]
rel_names <- colnames(relative)[-1]
max_tracer <- output$nvars
max_layers <- length(output$dim$z$vals)
max_time <- length(output$dim$t$vals)
var_names <- names(output$var)

nums <- grep("Nums", var_names, value = TRUE)
#n <- grep("_N", var_names, value = TRUE)
#tot_num <- c(nums,n)

# extract tracers for the ncd4 object
vars <- list()
for (i in 1:length(nums)){
  vars[[i]] <- ncvar_get(nc = output, varid = nums[i])
}
names(vars) <- nums

# ------------------------------------ #
# - Reserve/Structual Nitrogen Plots - #
# ------------------------------------ #
str_N <- grep("StructN", var_names, value = TRUE)
res_N <- grep("ResN", var_names, value = TRUE)
rs_names <- fg[fg$InvertType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]


sn_list <- list()
rn_list <- list()
for (i in 1:length(str_N)){
  sn_list[[i]] <- ncvar_get(nc = output, varid = str_N[i])
  rn_list[[i]] <- ncvar_get(nc = output, varid = res_N[i])  
}
names(sn_list) <- str_N
names(rn_list) <- res_N

# Aggregate arrays
agg <- function(x){
  adply(x, 3, sum)
}

structN <- ldply(sn_list, agg, .progress = "text")
reserveN <- ldply(rn_list, agg, .progress = "text")
totalnums <- ldply(vars, agg, .progress = "text")

structN$.id <- factor(structN$.id, levels = unique(structN$.id))
structN$Time <- as.numeric(as.character(structN$X1))/12 + 1948

reserveN$.id <- factor(reserveN$.id, levels = unique(reserveN$.id))
reserveN$Time <- as.numeric(as.character(reserveN$X1))/12 + 1948

totalnums$.id <- factor(totalnums$.id, levels = unique(totalnums$.id))
totalnums$Time <- as.numeric(as.character(totalnums$X1))/12 + 1948


