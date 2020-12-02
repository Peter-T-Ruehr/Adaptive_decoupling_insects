remove.parasites = F
mirrored = F
halve.curves = T

# source("//blanke-nas-1.zoologie.uni-koeln.de/PROTOCOLS/_SCRIPTS/R/Peter/functions_PTR.R")
source("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/functions_PTR.R")
# load("N:/PAPERS/PTR_Influence of the nymphal life history/R/workspace_good_unmirrored.RData")

# # load array.2D from LM analysis script - if this does not exist, the code to create it is at the end of this script
# if(remove.specimens){
#   array.2D <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1BjS1ejG5ikzlTCz1sOc-Vcjz5ofDPBsyC_6Am8pisZA#gid=482941805')})) # 482941805 = pooled & filtered
# } else {
#   if(mirrored == T){
#     array.2D <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1BjS1ejG5ikzlTCz1sOc-Vcjz5ofDPBsyC_6Am8pisZA#gid=979750323')})) # 0 = un-pooled; 557348376 = pooled
#   } else {
#     array.2D <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1BjS1ejG5ikzlTCz1sOc-Vcjz5ofDPBsyC_6Am8pisZA#gid=557348376')})) # 0 = un-pooled; 557348376 = pooled
#   }
# }


array.2D <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1BjS1ejG5ikzlTCz1sOc-Vcjz5ofDPBsyC_6Am8pisZA#gid=0')}))
# array.2D <- read.xlsx2(paste0("O:/_SCRIPTS/R/Peter/checkpoint_LMs/all_Checkpoint_files/", today(), "_array.2D.everything.xlsx"), sheetIndex = 1)

Hemimeridae <- c("1604", "1744", "0015", "0017", "1602", "1603")
Arixeniidae <- c("1601", "0657", "1715", "1600", "1743")
parasites <- c(Hemimeridae, Arixeniidae)
list.positions.parasites <- which(array.2D$ERC %in% parasites)
if(remove.parasites){
  array.2D <- array.2D[-list.positions.parasites,]
  ERC_numbers_all <- ERC_numbers_all[-list.positions.parasites]
  PBT_Derma_Pleco <- PBT_Derma_Pleco %>% 
    filter(ERC %!in% parasites)
  message(paste0("removed ", paste(parasites, collapse = ", "), " from PBT and array.2D"))
}

# G.marginalis.ERC <- "1736"
# PBT_Derma_Pleco <- PBT_Derma_Pleco %>% 
#   filter(ERC %!in% G.marginalis.ERC)

# Scopura has no superfamily <- "Scopuroidea"
array.2D$superfamily[array.2D$genus == "Scopura"] <- "Scopuroidea"

# delete some taxa
# array.2D <- array.2D %>% 
#   filter(family != "Hemimeridae" & family != "Arixeniidae")


# load PBT
PBT <- load.PBT()
PBT$ID <- paste0(PBT$genus_GBIF, "_", PBT$species_GBIF)

# set LM.sets dir temprarily
# setwd("//blanke-nas-1.zoologie.uni-koeln.de/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/")
setwd("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/")
# get all landmark names that should be analyzed from file
# LMs.hc <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_head.txt", header = F)$V1)
# LMs.hc.halve.curves <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_head_halve_curves.txt", header = F)$V1)
# LMs.hc.halve.curves.no.occ.ant <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_head_halve_curves_no_occ_ant.txt", header = F)$V1)
# LMs.hc.no.curves <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_head_no_curves.txt", header = F)$V1)

if(mirrored == T & halve.curves == T){
  # mirrored +; halve curves +
  head.capsule <- as.character(read.csv("./_2020-06-25 - 1st_paper_head_halve_curves_no_ant_no_occ_mirr.txt", header = F)$V1)
} else if(mirrored == F & halve.curves == T){
  # mirrored -; halve curves +
  head.capsule <- as.character(read.csv("./_2020-06-25 - 1st_paper_head_halve_curves_no_ant_no_occ.txt", header = F)$V1)
} else if(mirrored == F & halve.curves == F){
  # mirrored -; halve curves -
  head.capsule <- as.character(read.csv("./_2020-06-25 - 1st_paper_head_no_ant_no_occ.txt", header = F)$V1)
} else if(mirrored == T & halve.curves == F){
  # mirrored +; halve curves -
  head.capsule <- as.character(read.csv("./_2020-06-25 - 1st_paper_head_no_ant_no_occ_mirr.txt", header = F)$V1)
}
# delete landmarks to heck their influence
# head.capsule <- head.capsule[-which(head.capsule == "atp_med" | head.capsule == "atp_lat")]

LMs.Mds <- as.character(read.csv("./2020-06-25 - 1st_paper_Md.txt", header = F)$V1)
# LMs.Lbr <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_Lbr.txt", header = F)$V1)
LMs.Car <- as.character(read.csv("./2020-06-25 - 1st_paper_Car.txt", header = F)$V1)
LMs.Lab <- as.character(read.csv("./2020-06-25 - 1st_paper_Lab.txt", header = F)$V1)
# load block LM lists with non-overlap paired LMs
PLS.LMs.hc_Md <- as.character(read.csv("./2020-06-25 - 1st_paper_PLS_head_no_curves_against_Md.txt", header = F)$V1)
PLS.LMs.hc_Car <- as.character(read.csv("./2020-06-25 - 1st_paper_PLS_head_no_curves_against_Car.txt", header = F)$V1)
PLS.LMs.hc_Lab <- as.character(read.csv("./2020-06-25 - 1st_paper_PLS_head_no_curves_against_Lab.txt", header = F)$V1)

# curve.LMs <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_curve_landmarks.txt", header = F)$V1)
# occ.ant.LMs  <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_eye_ant.txt", header = F)$V1)
# ant.LMs  <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_ant.txt", header = F)$V1)
# occ.LMs  <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/2020-06-25 - 1st_paper_occ.txt", header = F)$V1)

# set actual working dir
# setwd("//blanke-nas-1.zoologie.uni-koeln.de/DATA/PAPERS/PTR_Influence of the nymphal life history/R")
setwd("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/R")

# add LM subsets to list of subsets
LM.subsets.all <- list(head.capsule, LMs.Mds, LMs.Car, LMs.Lab, PLS.LMs.hc_Md, PLS.LMs.hc_Car, PLS.LMs.hc_Lab) # head.capsule, head.capsule
LM.subset.names.all <- c("head.capsule", "LMs.Mds", "LMs.Car", "LMs.Lab", "PLS.LMs.hc_Md", "PLS.LMs.hc_Car", "PLS.LMs.hc_Lab") # "head.capsule", "head.capsule", 


# load global tree
tree <- read.nexus(file = "./trees/3rd_combined/2_original_correlated/combined_corr_correlated_ultra_corr2.tre")

# # correct tree file
# tree <- read.nexus(file = "./trees/3rd_combined/2_original_correlated/combined_corr_correlated_ultra_corr.tre")
# plot(ladderize(tree, right = T), cex=.8, edge.lty = 1, use.edge.length = T, label.offset = .002)


# switch taxon positions
## tip.C.amer <- which(tree$tip.label == "Carcinophora_americana")
## tip.G.marginalis <- which(tree$tip.label == "Gonolabis_marginalis")
# 
# tree$tip.label[tip.C.amer] <- "Gonolabis_marginalis"
# tree$tip.label[tip.G.marginalis] <- "Carcinophora_americana"

# tip.XXXXX <- which(tree$tip.label == "XXXXXX")
# tree <- drop.tip(tree, tip.XXXXX)
# write.nexus(tree, file = "./trees/3rd_combined/2_original_correlated/combined_corr_correlated_ultra_corr2.tre")
# plot(ladderize(tree, right = T), cex=.8, edge.lty = 1, use.edge.length = T, label.offset = .002)



# create loop to go through each LM.subset and do all the GMM stuff
# cluster <- makeCluster(15)
# registerDoParallel(cluster)
start.time <- Sys.time()
# foreach(s = 1:length(LM.subsets.all), .packages = c("geomorph", "magrittr", "dplyr", "RColorBrewer", "ape")) %dopar% {

# define taxon groups to test
taxon.names <- c("all", "Dermaptera", "Plecoptera")
for(taxon.name in 1:length(taxon.names)){ # length(taxon.names)
  for(s in 1:length(LM.subsets.all)){ # length(LM.subsets.all)
    # get taxon name
    curr.taxon.name <- taxon.names[taxon.name]
    message("******************")
    message(curr.taxon.name)
    
    # get subset name
    curr.subset.name <- LM.subset.names.all[s]
    message(curr.subset.name)
    message("******************")
    
    # get LM subset
    curr.subset <- LM.subsets.all[[s]]
    curr.subset.orig <- curr.subset
    
    # add _X, _Y, _Z to landmark names so we can find the columns
    LMs.to.analyze <- c(sapply(curr.subset,paste,c("X", "Y", "Z"), sep="_"))
    
    # select the columns with landmark data
    LM.col.numbers <-  which(colnames(array.2D) %in% LMs.to.analyze)
    
    # delete rows with species that do not belong to curr.taxon.name
    if(curr.taxon.name == "all"){
      array.2D.taxon <- array.2D %>% 
        filter(order == "Dermaptera" | order == "Plecoptera")
    } else {
      array.2D.taxon <- array.2D %>% 
        filter(order == curr.taxon.name)
    }
    
    # delete columns with LMs that are not to be analyzed
    array.2D.LMs <- array.2D.taxon[,c(1,LM.col.numbers)]
    
    # clean array from NAs (each SPECIMEN with a single NA will be deleted)
    array.2D.max.LMs <- as.data.frame(na.omit(array.2D.LMs))
    
    # turn ERC column into rownames and delete ERC column for 3D array creation
    rownames(array.2D.max.LMs) <- as.character(array.2D.max.LMs$ERC)
    array.2D.max.LMs$ERC <- NULL
    # colnames(array.2D.max.LMs)
    # sort(rownames(array.2D.max.LMs))
    
    # print dimensions of array_2D
    dim(array.2D.max.LMs)
    
    # save as excel for supplement
    LM.df.save <- array.2D.max.LMs
    colnames(LM.df.save) <- gsub("frontS_X_coronS", "1", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("lat_clyplbrS", "2", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("atp_med", "3", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("atp_lat", "4", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("dta_at_head", "5", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("dta_at_ata_ant", "6", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("mdcond_ant", "7", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("mdcond_post", "8", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("cardo_artic", "9", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("lat_of_postmentS", "10", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("C_of_clyplbrS", "11", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("C_of_epistS", "12", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("ant_corpotent", "13", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("foramendors", "14", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("foramenvent", "15", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("C_of_postmentS", "16", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("clypeus_shape", "17", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("frons_shape", "18", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("vertex_shape", "19", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("md1_insterion_ant", "20", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("md1_insterion_post", "21", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("md3_insertion", "22", colnames(LM.df.save))
    colnames(LM.df.save) <- gsub("dist_incisiv", "23", colnames(LM.df.save))
    
    LM.species.df <- as.data.frame(convert.ERCs.to.IDs(rownames(LM.df.save), classifier))
    colnames(LM.species.df) <- "species"
    LM.df.save <- cbind(LM.species.df, LM.df.save)
    write.xlsx2(LM.df.save, paste0("N://PAPERS//PTR_Influence of the nymphal life history//FIGURES//SUPPLEMENT//array_2D//", today(), "_", curr.subset.name, "_array_2D.xlsx"),
                row.names = F)
    
    
    
    # turn 2D array into 3D array
    array.3D <- arrayspecs(array.2D.max.LMs,(ncol(array.2D.max.LMs)/3),3, sep = ".") # p = # landmarks; k = # dimensions; n = # specimens
    dim(array.3D)
    
    # create classifier that contains everything but LM coordinates
    classifier <- array.2D.taxon[, (1:which(colnames(array.2D.taxon) == LMs.to.analyze[1])-1)]
    
    # add order color column to classifier
    classifier <- classifier %>%
      mutate(col.order = case_when(order == "Dermaptera" ~ "orange",
                                   order == "Plecoptera" ~ "blue"))
    
    
    # add superfamily colors to classifier
    superfamilies <- unique(classifier$superfamily)
    superfamily_cols <- get.Palette.Set2(length(superfamilies))
    for (f in 1:length(superfamilies)) {
      classifier$col.superfamily[classifier$superfamily==superfamilies[f]] <- superfamily_cols[f]
      classifier$point.shape.superfamily[classifier$superfamily==superfamilies[f]] <- f
    }
    
    # add family colors to classifier
    families <- unique(classifier$family)
    family_cols <- get.Palette.Set2(length(families))
    for (f in 1:length(families)) {
      classifier$col.family[classifier$family==families[f]] <- family_cols[f]
    }
    
    # add larval food colors to classifier
    for (v in 1:nrow(classifier)) {
      curr.food.l <- classifier$food.larva[v]
      if(is.na(curr.food.l)){
        classifier$col.food.l[v] <-"grey70"
      } else if(curr.food.l == "NA") {
        classifier$col.food.l[v] <-"grey70"
      } else if(curr.food.l == "herbivory") {
        classifier$col.food.l[v] <-"springgreen4"
      } else if(curr.food.l == "predatory") {
        classifier$col.food.l[v] <-"tomato2"
      } else if(curr.food.l == "detritivoric") {
        classifier$col.food.l[v] <-"tan4"
      } else if(curr.food.l == "ectoparasitic") {
        classifier$col.food.l[v] <-"palevioletred1"
      } else if(curr.food.l == "scavenger") {
        classifier$col.food.l[v] <-"hotpink4"
      }
    }
    food.ls <- unique(classifier$food.larva)
    food.l.point.shapes <- c(20, 2, 4, 5, 8, 9)
    for (f in 1:length(food.ls)) {
      classifier$point.shape.food.l[classifier$food.larva==food.ls[f]] <- food.l.point.shapes[f]
      classifier$point.shape.food.l[is.na(classifier$food.larva)] <- 1
    }
    
    # add adult food colors to classifier
    for (v in 1:nrow(classifier)) {
      curr.food.a <- classifier$food.adult[v]
      if(is.na(curr.food.a)){
        classifier$col.food.a[v] <-"grey70"
      } else if(curr.food.a == "herbivory") {
        classifier$col.food.a[v] <-"springgreen4"
      } else if(curr.food.a == "predatory") {
        classifier$col.food.a[v] <-"tomato2"
      } else if(curr.food.a == "detritivoric") {
        classifier$col.food.a[v] <-"tan4"
      } else if(curr.food.a == "ectoparasitic") {
        classifier$col.food.a[v] <-"palevioletred1"
      } else if(curr.food.a == "scavenger") {
        classifier$col.food.a[v] <-"hotpink4"
      } else if(curr.food.a == "non_feeding") {
        classifier$col.food.a[v] <-"deepskyblue1"
      }
    }
    
    # since some rows have been deleted from array.2D.max.LMs:
    classifier <- as.data.frame(classifier[which(classifier$ERC %in% rownames(array.2D.max.LMs)), ])
    rownames(classifier) <- classifier$ERC
    
    # add _1 and _2 to IDs
    for(i in 2:nrow(classifier)){
      doubles <- which(classifier$ID %in% classifier$ID[i])
      if(length(doubles) == 2){
        classifier$ID[doubles[1]] <- paste0(classifier$ID[doubles[1]], "_1")
        classifier$ID[doubles[2]] <- paste0(classifier$ID[doubles[2]], "_2")
      }
    }
    
    # check if taxa are missing in tree
    if(length(setdiff(classifier$ID, tree$tip.label) == 0)){
      message("!!!!!!!!!!!!!!!!!!!!!")
      message("Taxa missing in tree:")
      message(setdiff(classifier$ID, tree$tip.label))
      message("!!!!!!!!!!!!!!!!!!!!!")
    }
    
    # drop tree tips that are not part of the classifier
    tips.to.drop <- which(tree$tip.label %!in% classifier$ID)
    for (t in 1:length(tips.to.drop)){
      curr.tip.label <- tree$tip.label[tips.to.drop[t]]
      if(sub("_1", "", curr.tip.label) %in% classifier$ID){
        tree$tip.label[tips.to.drop[t]] <- sub("_1+", "", curr.tip.label)
      }
    }
    tips.to.drop <- which(tree$tip.label %!in% classifier$ID)
    tree.dropped <- drop.tip(tree, tips.to.drop)
    
    # # for supplementary tree figure:
    # tree.plot <- ladderize(tree.dropped)
    # plot(tree.plot, right = T, cex=.5, edge.lty = 1, use.edge.length = T, label.offset = .002)
    # tree.plot.rot <-rotate.multi(tree = tree.plot, node = 221)
    # plot(tree.plot.rot, right = T, cex=.5, edge.lty = 1, use.edge.length = T, label.offset = .002)
    # is_tip <- tree.plot.rot$edge[,2] <= length(tree.plot.rot$tip.label)
    # ordered_tips <- tree.plot.rot$edge[is_tip, 2]
    # plot.labels <- rev(tree.plot.rot$tip.label[ordered_tips])
    # writeClipboard(plot.labels)
    # 
    # Protodermaptera
    # 
    # 
    # plot.suborders <- c()
    # for(plot.label in plot.labels){
    #   plot.suborders <- c(plot.suborders, get.suborders.from.PBT(str_split(plot.label, pattern = "_")[[1]][1]))
    # }
    # writeClipboard(plot.suborders)
    # 
    # plot.infraorders <- c()
    # for(plot.label in plot.labels){
    #   plot.infraorders <- c(plot.infraorders, get.infraorders.from.PBT(str_split(plot.label, pattern = "_")[[1]][1]))
    # }
    # writeClipboard(plot.infraorders)
    # 
    # 
    # plot.superfamilies <- c()
    # for(plot.label in plot.labels){
    #   plot.superfamilies <- c(plot.superfamilies, classifier$superfamily[classifier$ID == plot.label])
    # }
    # writeClipboard(plot.superfamilies)
    # 
    # plot.families <- c()
    # for(plot.label in plot.labels){
    #   plot.families <- c(plot.families, classifier$family[classifier$ID == plot.label])
    # }
    # writeClipboard(plot.families)
    # plot(tree.plot.rot, right = T, cex=.5, edge.lty = 1, use.edge.length = T, label.offset = .002)
    # axisPhylo()
    
    
    
    # some checks <- should not be necessary to run if everything is good
    # comp.df <- data.frame(cbind(sort(classifier$ID), sort(tree.dropped$tip.label)))
    
    # setdiff(classifier$ID, tree.dropped$tip.label)
    
    # for some reason Taeniopteryx_nebulosa is still twice in tree
    # tree.dropped <- drop.tip(tree.dropped, which(tree.dropped$tip.label == "Taeniopteryx_nebulosa")[2])
    
    # turn tip label IDs into ERC numbers
    tip.label.IDs <- tibble::enframe(tree.dropped$tip.label)
    for(i in 1:nrow(tip.label.IDs)){
      tip.label.IDs$ERC[i] <- classifier$ERC[classifier$ID == tip.label.IDs$value[i]]
    }
    tree.dropped_ERC <- tree.dropped
    tree.dropped_ERC$tip.label <- unlist(tip.label.IDs$ERC)
    
    
    plot(ladderize(tree.dropped_ERC, right = T), cex=.4, edge.lty = 1, use.edge.length = T, label.offset = .002)
    
    #### define sliding LM matrix
    if(length(grep("shape", curr.subset))>0){
      message("sliding LMs:")
      
      # define empty sliding matrices to bind later
      sliding.matrix.hc.curves <- create_empty_df(nrow = 0, 
                                                  names = c("before", "slide", "after"))
      sliding.matrix.occ <- create_empty_df(nrow = 0, 
                                            names = c("before", "slide", "after"))
      sliding.matrix.ant <- create_empty_df(nrow = 0, 
                                            names = c("before", "slide", "after"))
      
      # only if curr.subset contains clypeus (& frons & vertex) curve LMs
      if(length(grep("clypeus_shape", curr.subset))>0){
        print("clypeus, frons & vertex LMs...")
        
        # get type I LM rownumbers to define points between which sliding will happen
        type.I.LMs <- c("C_of_clyplbrS", "C_of_epistS", "frontS_X_coronS", "foramendors")
        for(t in type.I.LMs){
          # find rownumber within array.3D that contains current LM coordinates
          assign(paste0("row.number.", t, ".LM"), which(curr.subset == t))
        }
        
        # get type III LMs
        type.III.LMs <- c("clypeus_shape", "frons_shape", "vertex_shape")
        for(t in type.III.LMs){
          # find rownumbers within array.3D that contain current LM coordinates
          assign(paste0("row.numbers.", t, ".LMs"), grep(t, curr.subset))
        }
        
        # now create matrix (this could also be manually done for each specimen via 'define.sliders(array.3D[, 1], nsliders = 10)')
        sliding.matrix.hc.curves <- create_empty_df(nrow = length(c(row.numbers.clypeus_shape.LMs,
                                                                    row.numbers.frons_shape.LMs,
                                                                    row.numbers.vertex_shape.LMs)), 
                                                    names = c("before", "slide", "after"))
        sliding.matrix.hc.curves$before <- c(row.number.C_of_clyplbrS.LM, row.numbers.clypeus_shape.LMs[1:(length(row.numbers.clypeus_shape.LMs)-1)],
                                             row.number.C_of_epistS.LM, row.numbers.frons_shape.LMs[1:(length(row.numbers.frons_shape.LMs)-1)],
                                             row.number.frontS_X_coronS.LM, row.numbers.vertex_shape.LMs[1:(length(row.numbers.vertex_shape.LMs)-1)])
        sliding.matrix.hc.curves$slide <- c(row.numbers.clypeus_shape.LMs,
                                            row.numbers.frons_shape.LMs,
                                            row.numbers.vertex_shape.LMs)
        sliding.matrix.hc.curves$after <- c(row.numbers.clypeus_shape.LMs[2:length(row.numbers.clypeus_shape.LMs)], row.number.C_of_epistS.LM,
                                            row.numbers.frons_shape.LMs[2:length(row.numbers.frons_shape.LMs)], row.number.frontS_X_coronS.LM,
                                            row.numbers.vertex_shape.LMs[2:length(row.numbers.vertex_shape.LMs)], row.number.foramendors.LM)
      }
      
      # only if curr.subset contains eye curve LMs
      if(length(grep("occS_shape", curr.subset))>0){
        for(m in 1){
          if(m==1){
            print("occipital LMs...")
            
            # get type I LM rownumbers to define points between which sliding will happen
            type.I.LMs <- c("occS_shape_eq_1")
            for(t in type.I.LMs){
              # find rownumber within array.3D that contains current LM coordinates
              assign(paste0("row.number.", t, ".LM"), which(curr.subset.orig == t))
            }
            
            # get type III LMs
            type.III.LMs <- c("occS_shape_eq")
            for(t in type.III.LMs){
              # find rownumbers within array.3D that contain current LM coordinates
              assign(paste0("row.numbers.", t, ".LMs"), grep(t, curr.subset.orig))
            }
            
            # now create matrix (this could also be manually done for each specimen via 'define.sliders(array.3D[, 1], nsliders = 10)')
            sliding.matrix.occ <- create_empty_df(nrow = length(c(row.numbers.occS_shape_eq.LMs))-1, 
                                                  names = c("before", "slide", "after"))
            sliding.matrix.occ$before <- c(row.number.occS_shape_eq_1.LM, row.numbers.occS_shape_eq.LMs[2:(length(row.numbers.occS_shape_eq.LMs)-1)])
            sliding.matrix.occ$slide <- c(row.numbers.occS_shape_eq.LMs[2:length(row.numbers.occS_shape_eq.LMs)])
            sliding.matrix.occ$after <- c(row.numbers.occS_shape_eq.LMs[3:length(row.numbers.occS_shape_eq.LMs)], row.number.occS_shape_eq_1.LM)
          }
        }
        if(m==2){
          print("mirrored occipital LMs...")
          
          # get type I LM rownumbers to define points between which sliding will happen
          type.I.LMs <- c("occS_shape_eq_mirr_1")
          for(t in type.I.LMs){
            # find rownumber within array.3D that contains current LM coordinates
            assign(paste0("row.number.", t, ".LM"), which(curr.subset == t))
          }
          
          # get type III LMs
          type.III.LMs <- c("occS_shape_eq_mirr")
          for(t in type.III.LMs){
            # find rownumbers within array.3D that contain current LM coordinates
            assign(paste0("row.numbers.", t, ".LMs"), grep(t, curr.subset))
          }
          
          # now add mirrored LM matrix
          sliding.matrix.occ.mirr <- create_empty_df(nrow = length(c(row.numbers.occS_shape_eq_mirr.LMs))-1, 
                                                     names = c("before", "slide", "after"))
          sliding.matrix.occ.mirr$before <- c(row.number.occS_shape_eq_mirr_1.LM, row.numbers.occS_shape_eq_mirr.LMs[2:(length(row.numbers.occS_shape_eq_mirr.LMs)-1)])
          sliding.matrix.occ.mirr$slide <- c(row.numbers.occS_shape_eq_mirr.LMs[2:length(row.numbers.occS_shape_eq_mirr.LMs)])
          sliding.matrix.occ.mirr$after <- c(row.numbers.occS_shape_eq_mirr.LMs[3:length(row.numbers.occS_shape_eq_mirr.LMs)], row.number.occS_shape_eq_mirr_1.LM)
          
          sliding.matrix.occ <- rbind(sliding.matrix.occ, sliding.matrix.occ.mirr)
        }
      }
      
      # only if curr.subset contains antennal curve LMs
      if(length(grep("antS_shape", curr.subset))>0){
        for(m in 1){
          if(m==1){
            print("antennal LMs...")
            
            # get type I LM rownumbers to define points between which sliding will happen
            type.I.LMs <- c("antS_shape_eq_1")
            for(t in type.I.LMs){
              # find rownumber within array.3D that contains current LM coordinates
              assign(paste0("row.number.", t, ".LM"), which(curr.subset.orig == t))
            }
            
            # get type III LMs
            type.III.LMs <- c("antS_shape_eq")
            for(t in type.III.LMs){
              # find rownumbers within array.3D that contain current LM coordinates
              assign(paste0("row.numbers.", t, ".LMs"), grep(t, curr.subset.orig))
            }
            
            # now create matrix (this could also be manually done for each specimen via 'define.sliders(array.3D[, 1], nsliders = 10)')
            sliding.matrix.ant <- create_empty_df(nrow = length(c(row.numbers.antS_shape_eq.LMs))-1, 
                                                  names = c("before", "slide", "after"))
            sliding.matrix.ant$before <- c(row.number.antS_shape_eq_1.LM, row.numbers.antS_shape_eq.LMs[2:(length(row.numbers.antS_shape_eq.LMs)-1)])
            sliding.matrix.ant$slide <- c(row.numbers.antS_shape_eq.LMs[2:length(row.numbers.antS_shape_eq.LMs)])
            sliding.matrix.ant$after <- c(row.numbers.antS_shape_eq.LMs[3:length(row.numbers.antS_shape_eq.LMs)], row.number.antS_shape_eq_1.LM)
          }
          if(m==2){
            print("mirrored antennal LMs...")
            
            # get type I LM rownumbers to define points between which sliding will happen
            type.I.LMs <- c("antS_shape_eq_mirr_1")
            for(t in type.I.LMs){
              # find rownumber within array.3D that contains current LM coordinates
              assign(paste0("row.number.", t, ".LM"), which(curr.subset == t))
            }
            
            # get type III LMs
            type.III.LMs <- c("antS_shape_eq_mirr")
            for(t in type.III.LMs){
              # find rownumbers within array.3D that contain current LM coordinates
              assign(paste0("row.numbers.", t, ".LMs"), grep(t, curr.subset))
            }
            
            # now add mirrored LM matrix
            sliding.matrix.ant.mirr <- create_empty_df(nrow = length(c(row.numbers.antS_shape_eq_mirr.LMs))-1, 
                                                       names = c("before", "slide", "after"))
            sliding.matrix.ant.mirr$before <- c(row.number.antS_shape_eq_mirr_1.LM, row.numbers.antS_shape_eq_mirr.LMs[2:(length(row.numbers.antS_shape_eq_mirr.LMs)-1)])
            sliding.matrix.ant.mirr$slide <- c(row.numbers.antS_shape_eq_mirr.LMs[2:length(row.numbers.antS_shape_eq_mirr.LMs)])
            sliding.matrix.ant.mirr$after <- c(row.numbers.antS_shape_eq_mirr.LMs[3:length(row.numbers.antS_shape_eq_mirr.LMs)], row.number.antS_shape_eq_mirr_1.LM)
            
            sliding.matrix.ant <- rbind(sliding.matrix.ant, sliding.matrix.ant.mirr)
          }
        }
        
      }
      # bind all sliding matrices
      sliding.matrix <- do.call(rbind, list(sliding.matrix.hc.curves, sliding.matrix.occ, sliding.matrix.ant))
      
      # Procrustes alignment with sliding
      gpa <- gpagen(array.3D, curves = sliding.matrix)
      summary(gpa$coords)
      
    } else{
      # Procrustes alignment without sliding
      message("no sliding...")
      sliding.matrix <- NULL # create_empty_df(nrow = 1, names = c("before", "slide", "after"))
      gpa <- gpagen(array.3D)
      summary(gpa$coords)
    }
    
    
    
    # replace current.moderate and current_montane_fast in converged eco type data with current_fast
    classifier$current[which(classifier$current == "current_moderate")] <- "current_fast"
    classifier$current[which(classifier$current == "current_montane_fast")] <- "current_fast"
    
    # ECOLOGY REDUCTION
    # replace collector_predator in converged eco type data with predator
    classifier$feeding.type[which(classifier$feeding.type == "collector_predator")] <- "predator"
    
    # replace collector_browser in converged eco type data with shredder
    classifier$feeding.type[which(classifier$feeding.type == "collector_browser")] <- "shredders"
    
    # create geomorph data frmae
    gdf <- geomorph.data.frame(gpa,
                               phy = tree.dropped_ERC,
                               ERC = as.factor(classifier$ERC),
                               ID = as.factor(classifier$ID),
                               order = as.factor(classifier$order), 
                               superfamily = as.factor(classifier$superfamily), 
                               family = as.factor(classifier$family),
                               feeding.type = as.factor(classifier$feeding.type),
                               food.larva = as.factor(classifier$food.larva),
                               food.adult = as.factor(classifier$food.adult),
                               microhabitat.larva = as.factor(classifier$microhabitat.larva),
                               microhabitat.adult = as.factor(classifier$microhabitat.adult),
                               current = as.factor(classifier$current),
                               wings = as.factor(classifier$wings),
                               flight = as.factor(classifier$flight))
    # ,
    # PCoA.1.current.l = as.factor(classifier$PCoA.1.current.l),
    # PCoA.2.current.l = as.factor(classifier$PCoA.2.current.l)
    
    # store uncorrected shape data
    shape.uncorr <- gpa$coords
    
    # run uncorrected PCA (phy is only added so it can be referenced)
    pca.data.uncorr <- gm.prcomp(A = shape.uncorr, phy = tree.dropped_ERC)
    
    ############
    # simple allometry analysis
    allometry.uncorr <- procD.lm(coords~log(Csize), data=gdf, iter=999)
    summary(allometry.uncorr)
    
    # Obtain allometry-adjusted residuals
    shape.resid.allo.corr <- arrayspecs(allometry.uncorr$residuals, p=dim(gpa$coords)[1], k=dim(gpa$coords)[2])
    
    # store allomtry-corrected shape
    shape.allofree <- shape.resid.allo.corr + array(gpa$consensus, dim(shape.resid.allo.corr)) # allometry-free shapes
    
    # run allometry-corrected PCA (phy is only added so it can be referenced)
    pca.data.allofree <- gm.prcomp(A = shape.allofree, phy = tree.dropped_ERC)
    
    ############
    # phylogenetically corrected allomtery
    allometry.phylcorr <- procD.pgls(data=gdf, f1 = coords~log(Csize), phy = phy)
    summary(allometry.phylcorr)
    
    # Obtain phylogeny- and allometry-adjusted residuals
    shape.resid.allo.phylo.corr <- arrayspecs(allometry.phylcorr$pgls.residuals, p=dim(gpa$coords)[1], k=dim(gpa$coords)[2])
    
    # store allomtry-corrected shape
    shape.allofree.phylcorr <- shape.resid.allo.phylo.corr + array(gpa$consensus, dim(shape.resid.allo.phylo.corr)) # allometry-free shapes
    
    # run allometry-free and phylocorr PCA (phy is only added so it can be referenced)
    pca.data.allofree.phylcorr <- gm.prcomp(A = shape.allofree.phylcorr, phy = tree.dropped_ERC)
    
    ############
    
    # assign landmark, GPA, PCA and GPA allo free, size.adjusted shapes, disparity data to work with later
    assign(paste0("slid.matrix.", curr.taxon.name, ".", curr.subset.name), sliding.matrix)
    assign(paste0("GPA.", curr.taxon.name, ".", curr.subset.name), gpa)
    assign(paste0("PCA.uncorr.", curr.taxon.name, ".", curr.subset.name), pca.data.uncorr)
    assign(paste0("PCA.allofree.uncorr.", curr.taxon.name, ".", curr.subset.name), pca.data.allofree)
    assign(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name), pca.data.allofree.phylcorr)
    # assign(paste0("PCA.allofree.phylocorr.", curr.subset.name), pca.data.allofree.phylocorr)
    assign(paste0("shape.uncorr.", curr.taxon.name, ".", curr.subset.name), shape.uncorr)
    assign(paste0("shape.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name), shape.allofree.phylcorr)
    assign(paste0("allometry.", curr.taxon.name, ".", curr.subset.name), allometry.uncorr)
    assign(paste0("allometry.phylcorr.", curr.taxon.name, ".", curr.subset.name), allometry.phylcorr)
    
    # assign(paste0("morph.disp.orders.", curr.subset.name), morphol.disparity.orders)
    # assign(paste0("morph.disp.superfamilies.", curr.subset.name), morphol.disparity.superfamilies)
    
    # assign(paste0("array.2D.max.LMs", curr.subset.name), array.2D.max.LMs)
    assign(paste0("array.3D.", curr.taxon.name, ".", curr.subset.name), array.3D)
    assign(paste0("gdf.", curr.taxon.name, ".", curr.subset.name), gdf)
    assign(paste0("classifier.", curr.taxon.name, ".", curr.subset.name), classifier)
    
    # assign trees
    assign(paste0("tree.IDs", curr.taxon.name, ".", curr.subset.name), tree.dropped)
    assign(paste0("tree.", curr.taxon.name, ".", curr.subset.name), tree.dropped_ERC)
  }
}

LM.subsets <- list(head.capsule, LMs.Mds) # head.capsule, head.capsule
LM.subset.names <- c("head.capsule", "LMs.Mds") #  "head.capsule", "head.capsule"

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# parallel::stopCluster(cluster)












# calculate PCoAs. We use ecology.cats here, which need to be in the created by workspace of script 1
# create list of tibbles with ecological data for stack plotting and PCoA calculation
ecology.cats <- list(current.l, food.l, microhabitat.l,
                     food.a, microhabitat.a, flight.a, wing.a,
                     Csize.ant, Csize.occ)
ecology.cat.names <- list("current.l",  "food.l", "microhabitat.l",
                          "food.a", "microhabitat.a", "flight.a", "wing.a",
                          "Csize.ant", "Csize.occ")

phylo.int.tibble <- create.empty.tibble(nrow = 1, names = c("taxon", "ecol.cat", "LM.set", "n", 
                                                            "r.pls", "p")) %>% 
  mutate_at("taxon", as.character) %>% 
  mutate_at("ecol.cat", as.character) %>% 
  mutate_at("LM.set", as.character)

PLS.tibble <- create.empty.tibble(nrow = 1, names = c("taxon", "ecol.cat", "LM.set", "n", 
                                                      "r.pls", "p")) %>% 
  mutate_at("taxon", as.character) %>% 
  mutate_at("ecol.cat", as.character) %>% 
  mutate_at("LM.set", as.character)

row.counter = 1
iterations <- 9999
for(t in 1:length(taxon.names)){ # length(taxon.names)
  # run Procrustes PGLS / ANOVAs for all ecol cats and LM sets with corrected and uncorrected data
  for(s in 1:length(LM.subset.names)){ # length(LM.subset.names)
    
    for(e in 1:length(ecology.cats)){
      curr.taxon.name <- taxon.names[t]
      # get current LM.subset name
      curr.LM.subset.name <- LM.subset.names[s]
      # get current eco cat
      curr.eco.cat <- unlist(ecology.cat.names[e])
      message(paste0(curr.taxon.name, ": ", curr.eco.cat, ": ", curr.LM.subset.name))
      
      if(curr.taxon.name == "Dermaptera" & curr.eco.cat == "current.l"){
      } else{
        curr.df.all <- ecology.cats[[e]]
        if(curr.taxon.name != "all"){
          curr.df <- curr.df.all %>%
            filter(order == curr.taxon.name)
        } else {
          curr.df <- curr.df.all
        }
        
        # get classifier according to subset name
        curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.LM.subset.name))
        
        # add ERC
        curr.df <- left_join(curr.df, select(curr.classifier, ERC, ID), by = "ID") %>% 
          drop_na(ERC)
        ERCs.with.eco.data <- curr.df$ERC
        
        # get eco data and ID columns only
        curr.df <- as.data.frame(curr.df[, 10:ncol(curr.df)])
        
        # delete duplicate rows resulting from multiple scans per species (don't think this is necessary)
        curr.df <- distinct(curr.df)
        curr.df <- delete.rows.with.na.in.column(curr.df)
        
        # set rownames as ID and delete ID & ERC columns
        rownames(curr.df) <- curr.df$ERC
        curr.df$ID <- NULL
        curr.df$ERC <- NULL
        
        if(curr.eco.cat != "wing.a" & curr.eco.cat != "flight.a" & curr.eco.cat != "Csize.ant" & curr.eco.cat != "Csize.occ"){
          # calculate dissimilarity index of community
          diss.curr.df <- vegdist(curr.df, method = "bray")
          # sim.curr.df <- 1-diss.curr.df
          
          # calculate & plot PCoA
          pcoa.curr.df <- pcoa(diss.curr.df)
          assign(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", curr.eco.cat), pcoa.curr.df)
          assign(paste0("PCoA.df.", curr.taxon.name, ".", curr.LM.subset.name, ".", curr.eco.cat), curr.df)
          # biplot(pcoa.curr.df, curr.df, cex = c(0.5, 0.1), main = paste0("PCoA: ", curr.taxon.name, ": ", curr.eco.cat))
        } else {
          # safe non-PCoA values for univariate groups
          assign(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", curr.eco.cat), curr.df)
        }
        
        ############################
        if(curr.taxon.name == "Dermaptera" & curr.eco.cat == "current.l"){
          # message("Dermaptera have only one value in current.larva PCoA1 and this cannot be tested.")
          # ecol.interactions.tibble[nrow(ecol.interactions.tibble)+1, 1] = curr.taxon.name
          # ecol.interactions.tibble$ecol.cat[nrow(ecol.interactions.tibble)] = curr.eco.cat
          # ecol.interactions.tibble$LM.set[nrow(ecol.interactions.tibble)] = curr.LM.subset.name
          # ecol.interactions.tibble$n[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$Rsq[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$"F"[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$Z[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$p[nrow(ecol.interactions.tibble)] = 1
          # ecol.interactions.tibble$Rsq.corr[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$"F.corr"[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$Z.corr[nrow(ecol.interactions.tibble)] = 0
          # ecol.interactions.tibble$p.corr[nrow(ecol.interactions.tibble)] = 1
        } else {
          # get tree
          curr.tree <- get(paste0("tree.", curr.taxon.name, ".", curr.LM.subset.name))
          
          
          # find ERC numbers that possess PCoA vectors
          if(curr.eco.cat != "wing.a" & curr.eco.cat != "flight.a" & curr.eco.cat != "Csize.ant" & curr.eco.cat != "Csize.occ"){
            ERCs.w.PCoAs <- rownames(pcoa.curr.df$vectors)
          } else {
            ERCs.w.PCoAs <- curr.classifier$ERC[curr.classifier$ERC %in% rownames(curr.df)]
          }
          
          # prune tree
          tips.to.drop <- setdiff(curr.tree$tip.label, ERCs.w.PCoAs)
          curr.tree.dropped <- drop.tip(curr.tree, tips.to.drop)
          
          # find rownumbers in classifier that do not correspond to any PCoA results
          if(length(setdiff(curr.classifier$ERC, ERCs.w.PCoAs)) != 0){
            curr.classifier <- curr.classifier[-which(curr.classifier$ERC %!in% ERCs.w.PCoAs),]
          }
          
          # get array.3D according to subset name
          curr.array.3D <- get(paste0("array.3D.", curr.taxon.name, ".", curr.LM.subset.name))
          
          # delete list elements within array.3D that contain no data for PCoA1
          curr.array.3D <- prune.array.3D(ERCs = curr.classifier$ERC, array.3D = curr.array.3D) 
          
          # run GPA (with sliding in case of "LMs.hc")
          if(curr.LM.subset.name == "head.capsule"){
            message("sliding...")
            curr.gpa <- gpagen(curr.array.3D, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.LM.subset.name)), print.progress = F)
          } else {
            message("no sliding...")
            curr.gpa <- gpagen(curr.array.3D, print.progress = F)
          }
          # names(curr.gpa$Csize)
          # plot(curr.gpa, label = T, plot.param = list(pt.cex = 0.1, txt.cex = 1))
          
          message("PLS analyses...")
          if(curr.eco.cat != "wing.a" & curr.eco.cat != "flight.a" & curr.eco.cat != "Csize.ant" & curr.eco.cat != "Csize.occ"){
            # sensitivity test for importance of PCoA vectors to check how important their number is
            # for(max.PCoA in 2:ncol(pcoa.curr.df$vectors)){
            #   if(max.PCoA == 2) {
            #     r.pls.scores <- c()
            #     p.values <- c()
            #     }
            #   message(max.PCoA)
            #   curr.phylo.integr <- phylo.integration(A = curr.gpa$coords, A2 = pcoa.curr.df$vectors[,1:max.PCoA], phy = curr.tree.dropped,
            #                                          iter = iterations, print.progress = F)
            #   r.pls.scores <- c(r.pls.scores, curr.phylo.integr$r.pls)
            #   p.values <- c(p.values, curr.phylo.integr$P.value)
            #   print(curr.phylo.integr)
            # }
            curr.phylo.integr <- phylo.integration(A = curr.gpa$coords, A2 = pcoa.curr.df$vectors, phy = curr.tree.dropped,
                                                   iter = iterations, print.progress = F)
            curr.PLS <- two.b.pls(A = curr.gpa$coords, A2 = pcoa.curr.df$vectors,
                                  iter = iterations, print.progress = T)
          } else {
            # curr.df[,1][curr.df[,1]==100] <- "macropterous"
            non.pcoa.vals <- as.data.frame(curr.df[,1]) # setNames(curr.df[,1], rownames(curr.df))
            rownames(non.pcoa.vals) <- rownames(curr.df)
            if(curr.eco.cat == "Csize.ant" | curr.eco.cat == "Csize.occ"){
              for(d in 1:nrow(non.pcoa.vals)){
                curr.ERC <- rownames(non.pcoa.vals)[d]
                curr.Csize.head <- curr.gpa$Csize[names(curr.gpa$Csize) %in% curr.ERC]
                non.pcoa.vals[d,1] <- non.pcoa.vals[d,1]/curr.Csize.head
              }
            }
            curr.phylo.integr <- phylo.integration(A = curr.gpa$coords, A2 = non.pcoa.vals, phy = curr.tree.dropped,
                                                   iter = iterations, print.progress = T)
            # x <- plot(curr.phylo.integr, label = dimnames(curr.gpa$coords)[[3]])
            # picknplot.shape(x)
            
            curr.PLS <- two.b.pls(A = curr.gpa$coords, A2 = non.pcoa.vals,
                                  iter = iterations, print.progress = T)
          }
          
          phylo.int.tibble[row.counter,1] <- curr.taxon.name
          phylo.int.tibble$ecol.cat[row.counter] <- curr.eco.cat
          phylo.int.tibble$LM.set[row.counter] <- curr.LM.subset.name
          phylo.int.tibble$n[row.counter] <- nrow(curr.phylo.integr$XScores)
          phylo.int.tibble$r.pls[row.counter] <- curr.phylo.integr$r.pls
          phylo.int.tibble$p[row.counter] <- curr.phylo.integr$P.value
          
          print(phylo.int.tibble[row.counter,])
          
          PLS.tibble[row.counter,1] <- curr.taxon.name
          PLS.tibble$ecol.cat[row.counter] <- curr.eco.cat
          PLS.tibble$LM.set[row.counter] <- curr.LM.subset.name
          PLS.tibble$n[row.counter] <- nrow(curr.PLS$XScores)
          PLS.tibble$r.pls[row.counter] <- curr.PLS$r.pls
          PLS.tibble$p[row.counter] <- curr.PLS$P.value
          
          print(PLS.tibble[row.counter,])
          
          assign(paste0("phylo.int__", curr.taxon.name, "__", curr.eco.cat, "__", curr.LM.subset.name), curr.phylo.integr)
          
          row.counter <- row.counter+1
          
          
          message("**************")
          
          # add proc.D.lm data into tibble
          # ecol.interactions.tibble$ecol.cat[nrow(ecol.interactions.tibble)+1] <- curr.LM.subset.name
          # ecol.interactions.tibble <- rbind(ecol.interactions.tibble, 
          #                                   add.procD.lm.data.to.tibble(taxon = curr.taxon.name,
          #                                                               eco.cat = curr.eco.cat, 
          #                                                               procD.lm.results = curr.procD.lm, 
          #                                                               procD.pgls.results = curr.procD.pgls, 
          #                                                               LM.subset.name = curr.LM.subset.name))
          ####################
        }
      }
    }
  }
}

# sort phylo tibble
phylo.int.tibble <- phylo.int.tibble %>%
  # group_by(taxon) %>% 
  arrange(taxon, LM.set, factor(ecol.cat, levels = c("current.l", "food.l", "food.a", "microhabtitat.l", "microhabtitat.a", "wings", "flight")))

# sort phylo tibble
PLS.tibble <- PLS.tibble %>%
  # group_by(taxon) %>% 
  arrange(taxon, LM.set, factor(ecol.cat, levels = c("current.l", "food.l", "food.a", "microhabtitat.l", "microhabtitat.a", "wings", "flight")))


if(remove.specimens == T){
  write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis_filtered.xlsx"), row.names = F)
  write.xlsx2(as.data.frame(PLS.tibble), paste0("./results/", today(), " - non-phylo.int.bray-curtis_filtered.xlsx"), row.names = F)
} else if(remove.specimens == F){
  write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis.xlsx"), row.names = F)
  write.xlsx2(as.data.frame(PLS.tibble), paste0("./results/", today(), " - non-phylo.int.bray-curtis.xlsx"), row.names = F)
}
# compare PLS
# all
# HC
curr.taxon = "all"
all.phylo.integr <- compare.pls(phylo.int__all__current.l__head.capsule, phylo.int__all__food.l__head.capsule,
                                phylo.int__all__food.a__head.capsule, phylo.int__all__microhabitat.l__head.capsule,
                                phylo.int__all__microhabitat.a__head.capsule, 
                                phylo.int__all__flight.a__head.capsule, phylo.int__all__wing.a__head.capsule, 
                                phylo.int__all__Csize.ant__head.capsule, phylo.int__all__Csize.occ__head.capsule)

PLS.compare.tibble.all.hc <- compare.PLS.to.data.frame(pls.list = all.phylo.integr, write = T, taxon = curr.taxon, suffix = "hc")
PLS.compare.tibble.all.hc$taxon <- curr.taxon
PLS.compare.tibble.all.hc$LM.set <- "head.capsule"

compare.PLS.to.pairwise.data.frame(pls.list = all.phylo.integr, m = "p", write = T, taxon = "all", suffix = "hc")
compare.PLS.to.pairwise.data.frame(pls.list = all.phylo.integr, m = "z", write = T, taxon = "all", suffix = "hc")

# all
# Mds
all.phylo.integr.Md <- compare.pls(phylo.int__all__current.l__LMs.Mds, phylo.int__all__food.l__LMs.Mds,
                                   phylo.int__all__food.a__LMs.Mds, phylo.int__all__microhabitat.l__LMs.Mds,
                                   phylo.int__all__microhabitat.a__LMs.Mds, 
                                   phylo.int__all__flight.a__LMs.Mds, phylo.int__all__wing.a__LMs.Mds, 
                                   phylo.int__all__Csize.ant__LMs.Mds, phylo.int__all__Csize.occ__LMs.Mds)


PLS.compare.tibble.all.Md <- compare.PLS.to.data.frame(pls.list = all.phylo.integr.Md, write = T, taxon = "all", suffix = "Md")
PLS.compare.tibble.all.Md$taxon <- curr.taxon
PLS.compare.tibble.all.Md$LM.set <- "LMs.Mds"

compare.PLS.to.pairwise.data.frame(pls.list = all.phylo.integr.Md, m = "p", write = T, taxon = "all", suffix = "Md")
compare.PLS.to.pairwise.data.frame(pls.list = all.phylo.integr.Md, m = "z", write = T, taxon = "all", suffix = "Md")


# Dermaptera
curr.taxon = "Dermaptera"
# head capsule
Dermaptera.phylo.integr <- compare.pls(phylo.int__Dermaptera__food.l__head.capsule,
                                       phylo.int__Dermaptera__food.a__head.capsule, phylo.int__Dermaptera__microhabitat.l__head.capsule,
                                       phylo.int__Dermaptera__microhabitat.a__head.capsule, 
                                       phylo.int__Dermaptera__flight.a__head.capsule, phylo.int__Dermaptera__wing.a__head.capsule,
                                       phylo.int__Dermaptera__Csize.ant__head.capsule, phylo.int__Dermaptera__Csize.occ__head.capsule)

PLS.compare.tibble.Derma.hc <- compare.PLS.to.data.frame(pls.list = Dermaptera.phylo.integr, write = T, taxon = "Dermaptera", suffix = "hc")
PLS.compare.tibble.Derma.hc$taxon <- curr.taxon
PLS.compare.tibble.Derma.hc$LM.set <- "head.capsule"

compare.PLS.to.pairwise.data.frame(pls.list = Dermaptera.phylo.integr, m = "p", write = T, taxon = "Dermaptera", suffix = "hc")
compare.PLS.to.pairwise.data.frame(pls.list = Dermaptera.phylo.integr, m = "z", write = T, taxon = "Dermaptera", suffix = "hc")

# Dermaptera
# Mds
Dermaptera.phylo.integr.Mds <- compare.pls(phylo.int__Dermaptera__food.l__LMs.Mds,
                                           phylo.int__Dermaptera__food.a__LMs.Mds, phylo.int__Dermaptera__microhabitat.l__LMs.Mds,
                                           phylo.int__Dermaptera__microhabitat.a__LMs.Mds, 
                                           phylo.int__Dermaptera__flight.a__LMs.Mds, phylo.int__Dermaptera__wing.a__LMs.Mds,
                                           phylo.int__Dermaptera__Csize.ant__LMs.Mds, phylo.int__Dermaptera__Csize.occ__LMs.Mds)

PLS.compare.tibble.Derma.Md <- compare.PLS.to.data.frame(pls.list = Dermaptera.phylo.integr.Mds, write = T, taxon = "Dermaptera", suffix = "Md")
PLS.compare.tibble.Derma.Md$taxon <- curr.taxon
PLS.compare.tibble.Derma.Md$LM.set <- "LMs.Mds"

compare.PLS.to.pairwise.data.frame(pls.list = Dermaptera.phylo.integr.Mds, m = "p", write = T, taxon = "Dermaptera", suffix = "Md")
compare.PLS.to.pairwise.data.frame(pls.list = Dermaptera.phylo.integr.Mds, m = "z", write = T, taxon = "Dermaptera", suffix = "Md")


# Plecoptera
curr.taxon = "Plecoptera"
# head capsule
Plecoptera.phylo.integr <- compare.pls(phylo.int__Plecoptera__current.l__head.capsule, phylo.int__Plecoptera__food.l__head.capsule,
                                       phylo.int__Plecoptera__food.a__head.capsule, phylo.int__Plecoptera__microhabitat.l__head.capsule,
                                       phylo.int__Plecoptera__microhabitat.a__head.capsule, 
                                       phylo.int__Plecoptera__flight.a__head.capsule, phylo.int__Plecoptera__wing.a__head.capsule, 
                                       phylo.int__Plecoptera__Csize.ant__head.capsule, phylo.int__Plecoptera__Csize.occ__head.capsule)

PLS.compare.tibble.Pleco.hc <- compare.PLS.to.data.frame(pls.list = Plecoptera.phylo.integr, write = T, taxon = "Plecoptera", suffix = "hc")
PLS.compare.tibble.Pleco.hc$taxon <- curr.taxon
PLS.compare.tibble.Pleco.hc$LM.set <- "head.capsule"

compare.PLS.to.pairwise.data.frame(pls.list = Plecoptera.phylo.integr, m = "p", write = T, taxon = "Plecoptera", suffix = "hc")
compare.PLS.to.pairwise.data.frame(pls.list = Plecoptera.phylo.integr, m = "z", write = T, taxon = "Plecoptera", suffix = "hc")

# Plecoptera
# Mds
Plecoptera.phylo.integr.Mds <- compare.pls(phylo.int__Plecoptera__current.l__LMs.Mds, phylo.int__Plecoptera__food.l__LMs.Mds,
                                           phylo.int__Plecoptera__food.a__LMs.Mds, phylo.int__Plecoptera__microhabitat.l__LMs.Mds,
                                           phylo.int__Plecoptera__microhabitat.a__LMs.Mds, 
                                           phylo.int__Plecoptera__flight.a__LMs.Mds, phylo.int__Plecoptera__wing.a__LMs.Mds, 
                                           phylo.int__Plecoptera__Csize.ant__LMs.Mds, phylo.int__Plecoptera__Csize.occ__LMs.Mds)

PLS.compare.tibble.Pleco.Md <- compare.PLS.to.data.frame(pls.list = Plecoptera.phylo.integr.Mds, write = T, taxon = "Plecoptera", suffix = "Md")
PLS.compare.tibble.Pleco.Md$taxon <- curr.taxon
PLS.compare.tibble.Pleco.Md$LM.set <- "LMs.Mds"

compare.PLS.to.pairwise.data.frame(pls.list = Plecoptera.phylo.integr.Mds, m = "p", write = T, taxon = "Plecoptera", suffix = "Md")
compare.PLS.to.pairwise.data.frame(pls.list = Plecoptera.phylo.integr.Mds, m = "z", write = T, taxon = "Plecoptera", suffix = "Md")


# bind all PLS compare tibbles
PLS.compare.tibble <- bind_rows(PLS.compare.tibble.all.hc, PLS.compare.tibble.all.Md,
                                PLS.compare.tibble.Derma.hc, PLS.compare.tibble.Derma.Md,
                                PLS.compare.tibble.Pleco.hc, PLS.compare.tibble.Pleco.Md)

# join phylo.int.tibble and PLS.compare.tibble
phylo.int.tibble <- left_join(phylo.int.tibble, PLS.compare.tibble, by = c("ecol.cat", "taxon", "LM.set"))


if(remove.specimens == T){
  if(mirrored == T){
    write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis.all_mirr_filtered.xlsx"), row.names = F)
  } else if(mirrored == F){
    write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis.all_filtered.xlsx"), row.names = F)
  }
} else if(remove.specimens == F){
  if(mirrored == T){
    write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis.all_mirr.xlsx"), row.names = F)
  }
  else if(mirrored == F){
    write.xlsx2(as.data.frame(phylo.int.tibble), paste0("./results/", today(), " - phylo.int.bray-curtis.all.xlsx"), row.names = F)
  }
}





# # create heat map for phylogenetic integration
# phylo.int.tibble.heat <- phylo.int.tibble %>% 
#   select(taxon, ecol.cat, LM.set, p, z) %>% 
#   filter(ecol.cat != "Csize.ant" & ecol.cat != "Csize.occ") %>% 
#   add_row(taxon = "Dermaptera", ecol.cat = "current.l", LM.set = "head.capsule", p = NA, z = NA) %>% 
#   add_row(taxon = "Dermaptera", ecol.cat = "current.l", LM.set = "LMs.Mds", p = NA, z = NA) %>% 
#   mutate(z=replace(z, p > 0.05, NA)) %>% 
#   # filter(ecol.cat != "wing.a" & ecol.cat != "flight.a") %>% 
#   mutate(ecol.cat = factor(ecol.cat, 
#                            levels = c("current.l", 
#                                           "food.l", "food.a",
#                                           "microhabitat.l", "microhabitat.a",
#                                           "wing.a", "flight.a")), 
#          LM.set = factor(LM.set)) %>% 
#   # create a new variable from count
#   mutate(countfactor=cut(z, breaks=c(1.49999, 1.99999, 2.499999, 2.99999,  3.499999,       3.99999,     4.499999,      4.99999,      5.5),
#                          labels = c("1.5-2.0", "2.5-3.0", "3.0-3.5", "3.0-3.5", "3.5-4.0", "4.0-4.5", "4.5-5.0", "5.0-5.5"))) %>% 
#   mutate(taxon = gsub("all", "both taxa", taxon), 
#          LM.set = gsub("head.capsule", "head shape", LM.set),
#          LM.set = gsub("LMs.Mds", "mandible shape", LM.set)) %>% 
#   filter(taxon != "both taxa") %>% 
#   mutate(taxon = factor(taxon,
#                         levels = rev(c("Dermaptera", "Plecoptera"))))
# 
# 
# # assign text colour
# textcol <- "grey40"

# p.hc <- ggplot(phylo.int.tibble.heat, aes(x = ecol.cat, y = taxon, fill = countfactor)) +
#   #add border white colour of line thickness 0.25
#   geom_tile(colour="white",size=0.25) +
#   guides(fill=guide_legend(title=expression(italic("z")))) +
#   #remove x and y axis labels
#   labs(x="",y="") +
#   # remove free space at outer borders
#   scale_x_discrete(expand=c(0,0), position = "top") + #, breaks  = c("all", "Dermaptera", "Plecoptera")
#   #remove extra space
#   scale_y_discrete(expand=c(0,0)) +
#   # set colors
#   scale_fill_manual(values=colorRampPalette(brewer.pal(6,"Blues"))(6),na.value = "white") + # grey95
#   # scale_fill_manual(values=rev(c("#d53e4f", "#f46d43","#fdae61","#fee08b","#e6f598","#ddf1da")),na.value = "grey95") + # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da"),na.value = "grey90")
#   #theme options
#   theme(legend.position="right",legend.direction="vertical",
#         legend.title=element_text(colour=textcol,face="bold"),
#         legend.margin=margin(grid::unit(0,"cm")),
#         legend.text=element_text(colour=textcol,size=9,face="bold"),
#         legend.key.height=grid::unit(0.8,"cm"),
#         legend.key.width=grid::unit(0.2,"cm"),
#         axis.text.x=element_text(size=10,colour=textcol,face="bold", angle = 90, hjust = 0, vjust = -0.5),
#         axis.text.y=element_text(vjust=0.2,colour=textcol,face="bold"),
#         axis.ticks=element_line(size=0.4),
#         plot.background=element_blank(),
#         # panel.border=element_blank(),
#         # plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
#         plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"),
#         strip.text = element_text(face="bold", size=10)) +
#   facet_wrap(~ LM.set, nrow = 1, strip.position="bottom") +
#   # coord_equal()
#   coord_fixed(ratio = 1) +
#   geom_text(aes(label=round(z,2)), size=3.5)

# create heat map for phylogenetic integration
phylo.int.tibble.heat <- phylo.int.tibble %>% 
  select(taxon, ecol.cat, LM.set, p, z) %>% 
  filter(ecol.cat != "Csize.ant" & ecol.cat != "Csize.occ")  %>% # & taxon != "all")
  add_row(taxon = "Dermaptera", ecol.cat = "current.l", LM.set = "head.capsule", p = NA, z = NA) %>% 
  add_row(taxon = "Dermaptera", ecol.cat = "current.l", LM.set = "LMs.Mds", p = NA, z = NA) %>% 
  mutate(LM.set = factor(LM.set)) %>% 
  mutate(z=replace(z, p > 0.05, NA)) %>% 
  mutate(taxon = gsub("all", "both taxa", taxon), 
         LM.set = gsub("head.capsule", "head shape", LM.set),
         LM.set = gsub("LMs.Mds", "mandible shape", LM.set),
         ecol.cat = gsub(".l$", " nymphs", ecol.cat),
         ecol.cat = gsub(".a$", " adults", ecol.cat),
         taxon = factor(taxon,
                        levels = rev(c("both taxa", "Dermaptera", "Plecoptera"))),
         ecol.cat = gsub("wing adults", "wings", ecol.cat),
         ecol.cat = gsub("flight adults", "flight", ecol.cat),
         ecol.cat = factor(ecol.cat, 
                           levels = c("current nymphs", 
                                      "food nymphs", "food adults",
                                      "microhabitat nymphs", "microhabitat adults",
                                      "wings", "flight adults")))


phylo.int.tibble.heat <- phylo.int.tibble.heat %>% 
  filter(ecol.cat != "wings" & ecol.cat != "flight adults")
# , taxon != "both taxa")

# assign text colour
textcol <- "grey40"
p.hc <- ggplot(phylo.int.tibble.heat, aes(x = ecol.cat, y = taxon, fill = z)) +
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25) +
  # guides(fill=guide_legend(title=expression(italic("z")))) +
  #remove x and y axis labels
  labs(x="",y="") +
  # remove free space at outer borders
  scale_x_discrete(expand=c(0,0), position = "top") + #, breaks  = c("all", "Dermaptera", "Plecoptera")
  #remove extra space
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_continuous(high = colorRampPalette(brewer.pal(6,"Blues"))(6)[5], low = colorRampPalette(brewer.pal(6,"Blues"))(6)[1], na.value = "white") +
  # set colors
  # scale_fill_manual(values=colorRampPalette(brewer.pal(6,"Blues"))(6),na.value = "white") + # grey95
  # scale_fill_manual(values=rev(c("#d53e4f", "#f46d43","#fdae61","#fee08b","#e6f598","#ddf1da")),na.value = "grey95") + # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da"),na.value = "grey90")
  #theme options
  theme(
    # legend.position="right",legend.direction="vertical",
    #     legend.title=element_text(colour=textcol,face="bold"),
    #     legend.margin=margin(grid::unit(0,"cm")),
    #     legend.text=element_text(colour=textcol,size=9,face="bold"),
    #     legend.key.height=grid::unit(0.8,"cm"),
    #     legend.key.width=grid::unit(0.2,"cm"),
    axis.text.x=element_text(size=10,colour=textcol,face="bold", angle = 90, hjust = 0, vjust = -0.5),
    axis.text.y=element_text(vjust=0.2,colour=textcol,face="bold"),
    axis.ticks=element_line(size=0.4),
    plot.background=element_blank(),
    # panel.border=element_blank(),
    # plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
    plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"),
    strip.text = element_text(face="bold", size=10)) +
  facet_wrap(~ LM.set, nrow = 1, strip.position="bottom") +
  # coord_equal()
  coord_fixed(ratio = 1) +
  geom_text(aes(label=round(z,2)), size=3.5)

p.hc
# 7.63 3.21










########################################################################
# plot PCA with allometry correction

s=1
t=1
e=1
taxon.names <- c("all", "Dermaptera", "Plecoptera")
for(t in 1:1){ # length(taxon.names)
  for(s in 1:1){ # length(LM.subsets)
    curr.taxon.name <- taxon.names[t]
    # get subset name
    curr.subset.name <- LM.subset.names[s]
    message(curr.taxon.name)
    message(curr.subset.name)
    
    # get PCA allofree according to subset name
    # PCA <- get(paste0("PCA.uncorr.", curr.subset.name)) # PCA.allofree.phylcorr.
    PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
    
    curr.tree <- PCA$phy
    
    # get classifier according to subset name
    curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
    # curr.classifier <- curr.classifier[order(PCA$phy$tip.label),]
    
    # print PCA summary
    summary(PCA)
    
    # find tree cols
    # replace Kerschielloidea color with darkred
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Karschielloidea"] <- "#4682b4"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Pygidicranoidea"] <- "sandybrown"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Apachyoidea"] <- "indianred"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Anisolabidoidea"] <- "lightpink"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Labiduroidea"] <- "#e0d73b"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Hemimeroidea"] <- "#bca37d"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Forficuloidea"] <- "#66c1a4"
    
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Eusthenioidea"] <- "#ab98c8"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Gripopterygoidea"] <- "#87cefa"
    curr.classifier$col.superfamily[curr.classifier$genus == "Scopura"] <- "#f36b56"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Nemouroidea"] <- "#de8bc2"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Pteronarcyoidea"] <- "#ffc11a"
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Perloidea"] <- "#acd74f"
    
    # curr.classifier$col.superfamily[curr.classifier$superfamily == "Gripopterygoidea"] <- "#lightskyblue"
    
    tree.cols <- c()
    tree.legend.cols <- c()
    tree.point.shapes <- c()
    legend.lines <- c()
    # label.list <- c()
    label.list <- convert.IDs.to.ERCs(names(PCA$x[,1]), PBT)
    
    for(c in 1:length(label.list)){
      tree.cols[c] <- curr.classifier$col.superfamily[curr.classifier$ERC == label.list[c]] # col.order col.superfamily
      # tree.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      # tree.legend.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      tree.point.shapes[c] <- curr.classifier$point.shape.food.l[curr.classifier$ERC == label.list[c]]
      legend.lines[c] <- curr.classifier$superfamily[curr.classifier$ERC == label.list[c]]
    }
    tree.legend.cols <- unique(tree.cols)
    
    # # plot
    plot.PCA(PCA = PCA, colours = tree.cols, title = paste0("PCA ", curr.taxon.name, " ", curr.subset.name),
             label.list = label.list, legend.lines = legend.lines, legend.pos = "bottomright") # label.list
    
    axis.2 = 2
    # plot(c(min(PCA$x[,c(1,1)]),max(PCA$x[,c(1,1)])), c(min(PCA$x[,c(1,axis.2)]),max(PCA$x[,c(1,axis.2)])), type = "n")
    # lines(c(min(PCA$x[,c(1,1)]),max(PCA$x[,c(1,1)])), c(0,0), col = "grey80", lty = 2)
    # lines(c(0,0), c(min(PCA$x[,c(1,axis.2)]),max(PCA$x[,c(1,axis.2)])), col = "grey80", lty = 2)
    # points(PCA$x[,c(1,axis.2)], col = tree.cols, pch = 16, main = paste0("PCA ", curr.taxon.name, " ", curr.subset.name))
    # text(PCA$x[,c(1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
    
    phylomorpho.plot.data <- data.frame(PCA$x[,1:9])
    phylomorpho.plot.data$ERC <- rownames(phylomorpho.plot.data)
    phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.classifier, ERC, superfamily, food.larva, food.adult))
    rownames(phylomorpho.plot.data) <- phylomorpho.plot.data$ERC
    phylomorpho.plot.data$superfamily <- as.factor(phylomorpho.plot.data$superfamily)
    phylomorpho.plot.data$test <- "no"
    phylomorpho.plot.data$test[1:(nrow(phylomorpho.plot.data)/2)] <- "yes"
    phylomorpho.plot.data$test <- as.factor(phylomorpho.plot.data$test)
    
    # get percentages explained by PCA = Proportion of Variance (PoV)
    PoVs <- round(get.PoV(PCA = PCA), 2)
    sum(PoVs[1:25])
    
    tip.cols<-tree.cols
    names(tip.cols)<-curr.tree$tip.label
    cols<-c(tip.cols[curr.tree$tip.label],rep("grey90",curr.tree$Nnode))
    names(cols)<-1:(length(curr.tree$tip)+curr.tree$Nnode)
    edge.cols <- rep("#cccccc", nrow(curr.tree$edge))
    names(edge.cols) <- curr.tree$edge[,2]
    
    # PDF plotting for supplement
    pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/FIGURES/SUPPLEMENT/PCs/", gsub("_", "-", today()), "__PCs_", taxon.names.plot[t], ".pdf"),
        width=8.72, height=8.72) # paper = "a4", height=11.69
    # par(mfrow=c(2,2), mar=c(0.1, 0.1, 0.1, 0.1))
    legend.tibble <- tibble(unique(legend.lines), unique(tree.cols), c(7, 2, 5, 4, 8, 11, 1, 13, 9, 12, 10, 6, 3)) %>% 
      'colnames<-' (c("name", "colour", "position")) %>% 
      arrange(position)
    
    for(p in seq(1,8,2)){
      axis.1 = p
      axis.2 = p+1
      plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
           c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
           xlab = paste0("PC", axis.1, " (", PoVs[axis.1], "%)"), ylab = paste0("PC", axis.2, " (", PoVs[axis.2], "%)"))
      lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "grey80", lty = 2)
      lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "grey80", lty = 2)
      phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                       control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
      points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = tree.cols, bg = tree.cols, pch = 16, cex = 1.8, lwd=2) # tree.point.shapes
      # text(phylomorpho.plot.data[,c(axis.1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
      # if((p+1)%%4 == 0){
      legend("bottomright", legend = legend.tibble$name, pch=20, cex = .6, pt.cex = 1.5, bty = "n", col = legend.tibble$colour)
      # }
    }
    dev.off()
    
    # axis.2 = 3
    # plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
    #      c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n")
    # lines(c(min(phylomorpho.plot.data[,axis.1])-1,max(phylomorpho.plot.data[,axis.1])+1), c(0,0), col = "grey80", lty = 2)
    # lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2])-1,max(phylomorpho.plot.data[,axis.2])+1), col = "grey80", lty = 2)
    # phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
    #                  control = list(col.node = cols, col.edge = edge.cols), node.size=c(0,0), add = T) # 1.3
    # points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = tree.cols, bg = tree.cols, pch = 16, cex = 1.8, lwd=2) # tree.point.shapes
    # text(phylomorpho.plot.data[,c(axis.1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
    # par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
    
    # 9.19 18 inch
    # 123.072 mm * 234.328 mm 4.85 * 9.23 |*2| 9.7 18.46
    
    
    
    ####### PC1-3 plots with phylogeny and ecology
    curr.taxon.name <- "all" # all Dermaptera Plecoptera
    # get subset name
    curr.subset.name <- LM.subset.names[1]
    message(curr.taxon.name)
    message(curr.subset.name)
    curr.eco.cat <- "food.l" # food.l food.a wing.a flight.a
    # par(mfrow=c(1,2), mar=c(2, 2, 1, 1))
    axis.1 = 1
    axis.2 = 3
    # density.cols <- viridis(200)# colorRampPalette(c("white", "blue", "yellow", "red", "darkred"))(200)
    
    # larval food: predatory
    curr.plot.title <- NULL # "Larval food PCoA1"
    # get first 2 columns of PCoA results
    curr.PCoA_ <- get(paste0("PCoA.", curr.taxon.name, ".", curr.subset.name, ".", curr.eco.cat))
    # curr.PCoA.df <- get(paste0("PCoA.df.", curr.taxon.name, ".", curr.LM.subset.name, ".", curr.eco.cat))
    # biplot(curr.PCoA_, curr.PCoA.df, cex = c(0.5, 0.1), main = paste0("PCoA: ", curr.taxon.name, ": ", "Larval food"))
    
    if(curr.eco.cat != "wing.a" & curr.eco.cat != "flight.a" & curr.eco.cat != "Csize.ant" & curr.eco.cat != "Csize.occ"){
      curr.PCoA <- data.frame(curr.PCoA_$vectors[, 1:2])
    } else {
      curr.PCoA <- curr.PCoA_
      colnames(curr.PCoA) <- "Axis.1"
    }
    curr.PCoA$ERC <- rownames(curr.PCoA)
    
    curr.phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.PCoA, ERC, Axis.1), by = "ERC") %>%
      filter(!is.na(Axis.1))
    
    resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
    a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
                       xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
                       yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    
    plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
         c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
         main = curr.plot.title, xlab = "PC1", ylab = "PC2")
    
    custom.filled.contour(x=a[[1]], # tdn
                          y=a[[2]], # rdn
                          z=a[[3]], # idn
                          nlevels=100,
                          # color.palette="RdBu",# viridis colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
                          col = rev(get.Palette.RdBu(120)), # viridis(150)
                          plot.title=title(main="", # , sub="detail"
                                           xlab="z", ylab="y"),
                          add = T) #,
    rect(xleft = min(phylomorpho.plot.data[,axis.1]), xright = max(phylomorpho.plot.data[,axis.1]),
         ybottom = min(phylomorpho.plot.data[,axis.2]), ytop = max(phylomorpho.plot.data[,axis.2]),
         col= rgb(1,1,1,alpha=0.5),border=NA)
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    # phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
    #                  node.size=c(0,0), add = T) # 1.3 control = list(col.node = cols, col.edge = edge.cols)
    # points(phylomorpho.plot.data[,c(axis.1,axis.2)], cex = 0.8, lwd=2, pch = 16)
    
    # get PCA allofree according to subset name
    # PCA <- get(paste0("PCA.uncorr.", curr.subset.name)) # PCA.allofree.phylcorr.
    PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
    
    curr.tree <- PCA$phy
    
    # get classifier according to subset name
    curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
    # curr.classifier <- curr.classifier[order(PCA$phy$tip.label),]
    
    # print PCA summary
    summary(PCA)
    
    # find tree cols
    # replace Kerschielloidea color with darkred
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Karschielloidea"] <- "steelblue"
    curr.classifier$col.superfamily[curr.classifier$genus == "Scopura"] <- "#F5AE7D"
    tree.cols <- c()
    tree.legend.cols <- c()
    tree.point.shapes <- c()
    legend.lines <- c()
    # label.list <- c()
    label.list <- convert.IDs.to.ERCs(names(PCA$x[,1]), PBT)
    
    for(c in 1:length(label.list)){
      tree.cols[c] <- curr.classifier$col.superfamily[curr.classifier$ERC == label.list[c]]
      # tree.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      # tree.legend.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      tree.point.shapes[c] <- curr.classifier$point.shape.food.l[curr.classifier$ERC == label.list[c]]
      legend.lines[c] <- curr.classifier$superfamily[curr.classifier$ERC == label.list[c]]
    }
    tree.legend.cols <- unique(tree.cols)
    
    
    phylomorpho.plot.data <- data.frame(PCA$x[,1:3])
    phylomorpho.plot.data$ERC <- rownames(phylomorpho.plot.data)
    phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.classifier, ERC, superfamily, food.larva, food.adult))
    rownames(phylomorpho.plot.data) <- phylomorpho.plot.data$ERC
    phylomorpho.plot.data$superfamily <- as.factor(phylomorpho.plot.data$superfamily)
    phylomorpho.plot.data$test <- "no"
    phylomorpho.plot.data$test[1:(nrow(phylomorpho.plot.data)/2)] <- "yes"
    phylomorpho.plot.data$test <- as.factor(phylomorpho.plot.data$test)
    
    tip.cols<-tree.cols
    names(tip.cols)<-curr.tree$tip.label
    cols<-c(tip.cols[curr.tree$tip.label],rep("grey50",curr.tree$Nnode))
    names(cols)<-1:(length(curr.tree$tip)+curr.tree$Nnode)
    edge.cols <- rep("#cccccc", nrow(curr.tree$edge))
    names(edge.cols) <- curr.tree$edge[,2]
    
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = 1.2, lwd=1) # tree.point.shapes
    # text(phylomorpho.plot.data[,c(axis.1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
    
    
    
    axis.1 = 1
    axis.2 = 3
    density.cols <- viridis(200)# colorRampPalette(c("white", "blue", "yellow", "red", "darkred"))(200)
    curr.taxon.name <- "all"
    
    
    resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
    a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
                       xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
                       yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    
    plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
         c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
         main = curr.plot.title, xlab = "PC1", ylab = "PC2")
    
    custom.filled.contour(x=a[[1]], # tdn
                          y=a[[2]], # rdn
                          z=a[[3]], # idn
                          nlevels=100,
                          # color.palette="RdBu",# viridis colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
                          col = rev(get.Palette.RdBu(120)), # viridis(150)
                          plot.title=title(main="", # , sub="detail"
                                           xlab="z", ylab="y"),
                          add = T) #,
    rect(xleft = min(phylomorpho.plot.data[,axis.1]), xright = max(phylomorpho.plot.data[,axis.1]),
         ybottom = min(phylomorpho.plot.data[,axis.2]), ytop = max(phylomorpho.plot.data[,axis.2]),
         col= rgb(1,1,1,alpha=0.5),border=NA)
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = 1.2, lwd=1) # tree.point.shapes
    # text(phylomorpho.plot.data[,c(axis.1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
    par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
    
    
    
    # get PCA allofree according to subset name
    # PCA <- get(paste0("PCA.uncorr.", curr.subset.name)) # PCA.allofree.phylcorr.
    PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
    
    curr.tree <- PCA$phy
    
    # get classifier according to subset name
    curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
    # curr.classifier <- curr.classifier[order(PCA$phy$tip.label),]
    
    # print PCA summary
    summary(PCA)
    
    # find tree cols
    # replace Kerschielloidea color with darkred
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Karschielloidea"] <- "steelblue"
    curr.classifier$col.superfamily[curr.classifier$genus == "Scopura"] <- "#F5AE7D"
    tree.cols <- c()
    tree.legend.cols <- c()
    tree.point.shapes <- c()
    legend.lines <- c()
    # label.list <- c()
    label.list <- convert.IDs.to.ERCs(names(PCA$x[,1]), PBT)
    
    for(c in 1:length(label.list)){
      tree.cols[c] <- curr.classifier$col.superfamily[curr.classifier$ERC == label.list[c]]
      # tree.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      # tree.legend.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      tree.point.shapes[c] <- curr.classifier$point.shape.food.l[curr.classifier$ERC == label.list[c]]
      legend.lines[c] <- curr.classifier$superfamily[curr.classifier$ERC == label.list[c]]
    }
    tree.legend.cols <- unique(tree.cols)
    
    
    phylomorpho.plot.data <- data.frame(PCA$x[,1:3])
    phylomorpho.plot.data$ERC <- rownames(phylomorpho.plot.data)
    phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.classifier, ERC, superfamily, food.larva, food.adult))
    rownames(phylomorpho.plot.data) <- phylomorpho.plot.data$ERC
    phylomorpho.plot.data$superfamily <- as.factor(phylomorpho.plot.data$superfamily)
    phylomorpho.plot.data$test <- "no"
    phylomorpho.plot.data$test[1:(nrow(phylomorpho.plot.data)/2)] <- "yes"
    phylomorpho.plot.data$test <- as.factor(phylomorpho.plot.data$test)
    
    tip.cols<-tree.cols
    names(tip.cols)<-curr.tree$tip.label
    cols<-c(tip.cols[curr.tree$tip.label],rep("grey50",curr.tree$Nnode))
    names(cols)<-1:(length(curr.tree$tip)+curr.tree$Nnode)
    edge.cols <- rep("#cccccc", nrow(curr.tree$edge))
    names(edge.cols) <- curr.tree$edge[,2]
    
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = 1.2, lwd=1) # tree.point.shapes
    # text(phylomorpho.plot.data[,c(axis.1,axis.2)], labels = label.list, adj = c(0.5,1.5), cex=.5)
    par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
    
    
    # continuous color legend
    col.values <- curr.phylomorpho.plot.data[,axis.1]
    layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))
    colfunc <- colorRampPalette(c(rev(get.Palette.RdBu(120))[1], "white", rev(get.Palette.RdBu(120))[120]))
    
    par(mar=c(5.1,4.1,4.1,2.1))
    plot(1:10,ann=FALSE,type="n")
    grid()
    points(1:10,col=colfunc(10),pch=19,cex=1.5)
    
    xl <- 1
    yb <- 1
    xr <- 1.5
    yt <- 2
    
    par(mar=c(5.1,0.5,4.1,0.5))
    plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
    rect(
      xl,
      head(seq(yb,yt,(yt-yb)/10),-1),
      xr,
      tail(seq(yb,yt,(yt-yb)/10),-1),
      col=colfunc(10)
    )
    
    rect(xleft = xl,
         xright = xr,
         ybottom = 2,
         ytop = 1,
         col= rgb(1,1,1,alpha=0.5),border=NA)
    
    mtext(1:10,side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)
    #######################
    
    
    
    
    
    # plot ecol. characters as heatmap
    
    # lines
    # f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
    #             h = rep(0.02, 2), n = 200, lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
    #                                                min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    # contour(f1, xlab = "previous duration",
    #         ylab = "duration", levels  =  c(1, 10, 50, 100, 150))
    
    # color
    density.cols <- colorRampPalette(c(rgb(1,1,1,0),rgb(0,0,1,0.5),rgb(1,1,0,0.5),rgb(1,0,0,0.5),rgb(0.545,0,0,0.5)), alpha = TRUE)(200)
    par(mfrow=c(2,5), mar=c(2, 2, 1, 1))
    axis.1 = 1
    axis.2 = 2
    k = 20
    density.cols <- colorRampPalette(c("white", "dodgerblue", "khaki", "salmon", "tomato2"))(k) # colorRampPalette(c("white", "blue", "yellow", "red", "darkred"))(k)
    curr.classifier$col.order[curr.classifier$order == "Dermaptera"] <- rgb(72/255,145/255,0/255)
    curr.classifier$col.order[curr.classifier$order == "Plecoptera"] <- rgb(202/255,255/255,109/255)
    
    curr.classifier$col.superfamily[curr.classifier$superfamily == "Karschielloidea"] <- "steelblue"
    curr.classifier$col.superfamily[curr.classifier$genus == "Scopura"] <- "#F5AE7D"
    tree.cols <- c()
    tree.legend.cols <- c()
    tree.point.shapes <- c()
    legend.lines <- c()
    # label.list <- c()
    label.list <- convert.IDs.to.ERCs(names(PCA$x[,1]), PBT)
    
    for(c in 1:length(label.list)){
      tree.cols[c] <- curr.classifier$col.order[curr.classifier$ERC == label.list[c]] # col.superfamily
      # tree.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      # tree.legend.cols[c] <- curr.classifier$col.food.l[curr.classifier$ERC == label.list[c]]
      tree.point.shapes[c] <- curr.classifier$point.shape.food.l[curr.classifier$ERC == label.list[c]]
      legend.lines[c] <- curr.classifier$superfamily[curr.classifier$ERC == label.list[c]]
    }
    tree.legend.cols <- unique(tree.cols)
    
    
    phylomorpho.plot.data <- data.frame(PCA$x[,1:3])
    phylomorpho.plot.data$ERC <- rownames(phylomorpho.plot.data)
    phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.classifier, ERC, superfamily, food.larva, food.adult))
    rownames(phylomorpho.plot.data) <- phylomorpho.plot.data$ERC
    phylomorpho.plot.data$superfamily <- as.factor(phylomorpho.plot.data$superfamily)
    phylomorpho.plot.data$test <- "no"
    phylomorpho.plot.data$test[1:(nrow(phylomorpho.plot.data)/2)] <- "yes"
    phylomorpho.plot.data$test <- as.factor(phylomorpho.plot.data$test)
    
    tip.cols<-tree.cols
    names(tip.cols)<-curr.tree$tip.label
    cols<-c(tip.cols[curr.tree$tip.label],rep("grey50",curr.tree$Nnode))
    names(cols)<-1:(length(curr.tree$tip)+curr.tree$Nnode)
    edge.cols <- rep("#cccccc", nrow(curr.tree$edge))
    names(edge.cols) <- curr.tree$edge[,2]
    
    # larval food: predatory
    curr.plot.title <- "Larval food PCoA1"
    # get first 2 columns of PCoA results
    curr.PCoA_ <- get(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.l"))
    curr.PCoA.df <- get(paste0("PCoA.df.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.l"))
    # biplot(curr.PCoA_, curr.PCoA.df, cex = c(0.5, 0.1), main = paste0("PCoA: ", curr.taxon.name, ": ", "Larval food"))
    
    curr.PCoA <- data.frame(curr.PCoA_$vectors[, 1:2])
    curr.PCoA$ERC <- rownames(curr.PCoA)
    
    curr.phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.PCoA, ERC, Axis.1), by = "ERC") %>%
      filter(!is.na(Axis.1))
    
    # png(file=paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/FIGURES/ecology_PCAs/", gsub("_", "-", today()), " - PCoA1_nymphs.png"),
    #     width=1000, height=1000)
    # resolution <- 0.0001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
    # a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
    #                    xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
    #                    yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    # 
    plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
         c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
         main = curr.plot.title, xlab = "PC1", ylab = "PC2")
    # 
    # custom.filled.contour(x=a[[1]], # tdn
    #                       y=a[[2]], # rdn
    #                       z=a[[3]], # idn
    #                       # color.palette=colorRampPalette(c("blue", "cyan", "green", "yellow", "red")),
    #                       col = rev(get.Palette.RdBu(120)),
    #                       plot.title=title(main="", # , sub="detail"
    #                                        xlab="z", ylab="y"),
    #                       nlevels=100,
    #                       add = T) #,
    # rect(xleft = min(phylomorpho.plot.data[,axis.1]), xright = max(phylomorpho.plot.data[,axis.1]),
    #      ybottom = min(phylomorpho.plot.data[,axis.2]), ytop = max(phylomorpho.plot.data[,axis.2]),
    #      col= rgb(1,1,1,alpha=0.5),border=NA)
    # dev.off()
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    
    
    # larval food: non-feeding
    density.cols <- colorRampPalette(c("white", "dodgerblue", "khaki", "salmon", "tomato2"))(k)
    curr.plot.title <- "Larval non-feeding"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.larva == "non_feeding")
    # delete plot because no data for non-feeding
    # f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
    #             n = 200,
    #             lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
    #                      min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    # contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
         c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
         main = curr.plot.title, xlab = "PC1", ylab = "PC2")
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # larval food: predatory
    curr.plot.title <- "Larval predatory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.larva == "predatory")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # larval food: herbivory
    curr.plot.title <- "Larval herbivory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.larva == "herbivory")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # larval food: detritivory
    curr.plot.title <- "Larval detritivory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.larva == "detritivoric")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    
    
    
    # adult food PCoA1
    curr.plot.title <- "Adult food PCoA1"
    # get first 2 columns of PCoA results
    curr.PCoA_ <- get(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.a"))
    curr.PCoA.df <- get(paste0("PCoA.df.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.a"))
    # biplot(curr.PCoA_, curr.PCoA.df, cex = c(0.5, 0.1), main = paste0("PCoA: ", curr.taxon.name, ": ", "Adult food"))
    
    curr.PCoA <- data.frame(curr.PCoA_$vectors[, 1:2])
    
    curr.PCoA$ERC <- rownames(curr.PCoA)
    
    curr.phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.PCoA, ERC, Axis.1), by = "ERC") %>%
      filter(!is.na(Axis.1))
    
    # png(file=paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/FIGURES/ecology_PCAs/", gsub("_", "-", today()), " - PCoA1_adults.png"),
    #     width=1000, height=1000)
    # resolution <- 0.0001 
    # a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
    #                    xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
    #                    yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    # 
    
    plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
         c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n",
         main = curr.plot.title, xlab = "PC1", ylab = "PC2")
    # custom.filled.contour(x=a[[1]], # tdn
    #                       y=a[[2]], # rdn
    #                       z=a[[3]], # idn
    #                       col = rev(get.Palette.RdBu(120)),
    #                       plot.title=title(main="", # , sub="detail"
    #                                        xlab="z", ylab="y"),
    #                       nlevels=100,
    #                       add = T,
    #                       main = curr.plot.title) #,
    # rect(xleft = min(phylomorpho.plot.data[,axis.1]), xright = max(phylomorpho.plot.data[,axis.1]),
    #      ybottom = min(phylomorpho.plot.data[,axis.2]), ytop = max(phylomorpho.plot.data[,axis.2]),
    #      col= rgb(1,1,1,alpha=0.5),border=NA)
    # dev.off()
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    
    
    #adult food: non-feeding
    density.cols <- colorRampPalette(c("white", "dodgerblue", "khaki", "salmon", "tomato2"))(k)
    curr.plot.title <- "Adult non-feeding"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.adult == "non_feeding")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # adult food: predatory
    density.cols <- colorRampPalette(c("dodgerblue", "khaki", "salmon", "tomato2", "tomato2"))(k)
    curr.plot.title <- "Adult predatory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.adult == "predatory")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1, lwd=0.5)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # adult food: herbivory
    
    density.cols <- colorRampPalette(c("dodgerblue", "khaki", "salmon", "tomato2", "tomato2"))(k)
    curr.plot.title <- "Adult herbivory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.adult == "herbivory")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    # adult food: detritivory
    density.cols <- colorRampPalette(c("white", "white", "white", "dodgerblue", "khaki"))(k)
    curr.plot.title <- "Adult detritivory"
    curr.phylomorpho.plot.data <- subset(phylomorpho.plot.data, phylomorpho.plot.data$food.adult == "detritivoric")
    f1 <- kde2d(curr.phylomorpho.plot.data[,c(axis.1)], curr.phylomorpho.plot.data[,c(axis.2)],
                n = 200,
                lims = c(min(phylomorpho.plot.data[,axis.1])-0.02,max(phylomorpho.plot.data[,axis.1])+0.01,
                         min(phylomorpho.plot.data[,axis.2])-0.02,max(phylomorpho.plot.data[,axis.2]))+0.01)
    contour(f1, drawlabels = F, nlevels = k, col=density.cols, main = curr.plot.title)
    # image(f1, col = density.cols, main = curr.plot.title) # hcl.colors(25, "heat", rev = "true") , axes = F
    
    lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
                     control = list(col.node = cols, col.edge = edge.cols), node.size=c(0.0,0), add = T, lwd=0.1) # 1.3
    points(phylomorpho.plot.data[,c(axis.1,axis.2)], col = "black", bg = tree.cols, pch = 21, cex = .6, lwd=0.1) # tree.point.shapes
    
    
    # save PDF as 11.5 * 4.6 inch PDF
    
    par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
    
    
    # # PCoA heat maps
    # par(mfrow=c(2,1), mar=c(2, 2, 1, 1))
    # axis.1 = 1
    # axis.2 = 2
    # # larval food
    # # get first 2 columns of PCoA results
    # curr.PCoA <- data.frame(get(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.l"))$vectors[, 1:2])
    # curr.PCoA$ERC <- rownames(curr.PCoA)
    #
    # curr.phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.PCoA, ERC, Axis.1), by = "ERC") %>%
    #   filter(!is.na(Axis.1))
    #
    # resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
    # a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
    #                    xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
    #                    yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    #
    # plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
    #      c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n")
    # custom.filled.contour(x=a[[1]], # tdn
    #                y=a[[2]], # rdn
    #                z=a[[3]], # idn
    #                color.palette=colorRampPalette(c("blue", "cyan", "green", "yellow", "red")),
    #                plot.title=title(main="", # , sub="detail"
    #                                 xlab="z", ylab="y"),
    #                nlevels=100,
    #                add = T) #,
    # lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    # lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    # phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
    #                  node.size=c(0,0), add = T) # 1.3 control = list(col.node = cols, col.edge = edge.cols)
    # points(phylomorpho.plot.data[,c(axis.1,axis.2)], cex = 1.2, lwd=2, pch = 16)
    #
    # # adult food
    # # get first 2 columns of PCoA results
    # curr.PCoA <- data.frame(get(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", "food.a"))$vectors[, 1:2])
    # curr.PCoA$ERC <- rownames(curr.PCoA)
    #
    # curr.phylomorpho.plot.data <- left_join(phylomorpho.plot.data, select(curr.PCoA, ERC, Axis.1), by = "ERC") %>%
    #   filter(!is.na(Axis.1))
    #
    # resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
    # a <- akima::interp(x=curr.phylomorpho.plot.data[,axis.1], y=curr.phylomorpho.plot.data[,axis.2], z=curr.phylomorpho.plot.data$Axis.1,
    #                    xo=seq(min(curr.phylomorpho.plot.data[,axis.1]),max(curr.phylomorpho.plot.data[,axis.1]),by=resolution),
    #                    yo=seq(min(curr.phylomorpho.plot.data[,axis.2]),max(curr.phylomorpho.plot.data[,axis.2]),by=resolution), duplicate="mean")
    #
    # plot(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])),
    #      c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), type = "n")
    # custom.filled.contour(x=a[[1]], # tdn
    #                       y=a[[2]], # rdn
    #                       z=a[[3]], # idn
    #                       color.palette=colorRampPalette(c("blue", "cyan", "green", "yellow", "red")),
    #                       plot.title=title(main="", # , sub="detail"
    #                                        xlab="z", ylab="y"),
    #                       nlevels=100,
    #                       add = T) #,
    # lines(c(min(phylomorpho.plot.data[,axis.1]),max(phylomorpho.plot.data[,axis.1])), c(0,0), col = "#cccccc", lty = 1)
    # lines(c(0,0), c(min(phylomorpho.plot.data[,axis.2]),max(phylomorpho.plot.data[,axis.2])), col = "#cccccc", lty = 1)
    # phylomorphospace(tree = curr.tree, phylomorpho.plot.data[,c(axis.1,axis.2)], label = "off",
    #                  node.size=c(0,0), add = T) # 1.3 control = list(col.node = cols, col.edge = edge.cols)
    # points(phylomorpho.plot.data[,c(axis.1,axis.2)], cex = 1.2, lwd=2, pch = 16)
    # par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
    
    # curr.GPA <- get(paste0("GPA.", curr.subset.name))
    # plotTangentSpace(curr.GPA$coords, groups = curr.classifier$col.superfamily, warpgrids=TRUE) # <- this seems to accept a 3D mesh that may be warped
  }
}

.Internal(.invokeRestart(list(NULL, NULL), NULL))








########################################################################
# plot 3D ref to target
curr.taxon.name <- "all"
curr.subset.name <- LM.subset.names[1]
message(curr.taxon.name)
message(curr.subset.name)

curr.PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
link.matrix <- read.csv("link.matrix.dors.csv", header = T)

# with maximums and minimums: also contains variation from other PCs
# PC1
ref <- curr.PCA$shapes$shapes.comp1$min[c(1:16),] # , 42:50
target <- curr.PCA$shapes$shapes.comp1$max[c(1:16),] # , 42:50
plotRefToTarget(M1 = ref, M2 = target,method="vector",label = T) # , links = link.matrix
plotRefToTarget(M1 = ref, M2 = target,method="TPS",label = T)

# PC2
ref <- curr.PCA$shapes$shapes.comp2$min[c(1:16),] # , 42:50
target <- curr.PCA$shapes$shapes.comp2$max[c(1:16),] # , 42:50
plotRefToTarget(M1 = ref, M2 = target,method="vector",label = T) # , links = link.matrix

# PC3
ref <- curr.PCA$shapes$shapes.comp3$min[c(1:16),] # , 42:50
target <- curr.PCA$shapes$shapes.comp3$max[c(1:16),] # , 42:50
plotRefToTarget(M1 = ref, M2 = target,method="vector",label = T) # , links = link.matrix



# pairwise outlier comparison
link.matrix <- read.csv("link.matrix.dors.csv", header = T)
curr.GPA <- get(paste0("GPA.", curr.taxon.name, ".", curr.subset.name))

# PC1
ref.name <- "1587"
target.name <- "1479"

# PC2
ref.name <- "1612"
target.name <- "1626"

# PC3
ref.name <- "1612"
target.name <- "1707"

# ref <- mshape(curr.GPA$coords)

ref <- curr.GPA$coords[,,which(names(curr.GPA$Csize) == ref.name)]
target <- curr.GPA$coords[,,which(names(curr.GPA$Csize) == target.name)]

ref <- ref[c(1:16),] # , 42:50
target <- target[c(1:16),] # , 42:50

plotRefToTarget(ref, target, method = "vector", mag = 1, label = T) #, links = link.matrix points TPS vector surface # [1:16,]
plotRefToTarget(ref, target, method = "TPS", mag = 1, label = T, links = link.matrix) # points TPS vector surface # [1:16,]




plot(ref[,c(1,3)])
text(ref[,c(1,3)], label = rownames(ref))


########################################################################
# analyze single specimens' landmarks to find out why they are outliers
curr.subset.name <- LM.subset.names[s]
print(curr.subset.name)
curr.GPA <- get(paste0("GPA.", curr.taxon.name, ".", curr.subset.name))
consensus <- apply(curr.GPA$coords, c(1,2), mean)
custom.outlier.plot("1600", consensus, curr.GPA, .8)











########################################################################
# plot outliers
curr.taxon.name <- "Dermaptera"
LM.subset.names <- c("LMs.Mds") # head.capsule
for(s in 1:length(LM.subset.names)){ # get subset name
  # get subset name
  curr.subset.name <- LM.subset.names[s]
  message(curr.subset.name)
  
  curr.shape.allofree.phylcorr <- get(paste0("shape.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
  custom.plotOutliers(curr.shape.allofree.phylcorr, groups = as.factor(get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))$superfamily)) # , inspect.outliers = T , groups = as.factor(classifier$order)
}




########################################################################
# get all ERC numbers used
all.ERCs.analyzed <- unique(c(get(paste0("classifier.", "all", ".", LM.subset.names[1]))$ERC, 
                              get(paste0("classifier.", "all", ".", LM.subset.names[2]))$ERC))
setdiff(rownames(array_2D), all.ERCs.analyzed)

########################################################################
# 3D plot
pdf(paste0("//blanke-nas-1.zoologie.uni-koeln.de/DATA/PAPERS/PTR_Influence of the nymphal life history/R/results/PC_plots/phylomorphoplots/", gsub("_", "-", today()), " - phylomorphoplots_w_parasites.pdf"), 
    width = 10, height = 10)
LM.subset.names <- c("head.capsule", "LMs.Mds")
for (corrected in c(F, T)) {
  for(s in 1:length(LM.subsets)){
    for (t in 1:length(taxon.names)) {
      # get taxon.name
      curr.taxon.name <- taxon.names[t]
      # get subset name
      curr.subset.name <- LM.subset.names[s]
      message(corrected)
      message(curr.taxon.name)
      message(curr.subset.name)
      
      # load classifier
      curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
      
      # add point shpaes to classifier
      curr.classifier$food.larva
      
      # load tree
      curr.tree <- get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
      # plotTree(curr.tree)
      # load PCA
      # corrected <- T
      if(corrected == T) {
        PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
      } else {
        PCA <- get(paste0("PCA.uncorr.", curr.taxon.name, ".", curr.subset.name))
      }
      
      # calculate tree cols
      tree.cols <- c()
      label.list <- c()
      label.ERCs <- c()
      tree.legend.labels <- c()
      for(c in 1:length(PCA$phy$tip.label)){
        tree.cols[c] <- curr.classifier$col.superfamily[curr.classifier$ERC == PCA$phy$tip.label[c]]
        label.list[c] <- curr.classifier$ID[curr.classifier$ERC == PCA$phy$tip.label[c]]
        label.ERCs[c] <- curr.classifier$ERC[curr.classifier$ERC == PCA$phy$tip.label[c]]
        tree.legend.labels <- c(tree.legend.labels, curr.classifier$superfamily[curr.classifier$ERC == PCA$phy$tip.label[c]])
      }
      tree.legend.cols <- unique(tree.cols)
      tree.legend.labels <- unique(tree.legend.labels)
      
      
      # summary.PCA <- summary(PCA)
      # if(corrected == T) {
      #   title(main = paste0("Phylomorphospace (allo- & phylo-corr.): ", curr.taxon.name, ": ", curr.subset.name),
      #         xlab = paste("PC", axis.1, " (", round(summary.PCA[2,axis.1]*100, 2), ")"),
      #         ylab = paste("PC", axis.2, " (", round(summary.PCA[2,axis.2]*100, 2), ")"))
      # } else {
      #   title(main = paste0("Phylomorphospace (un-corr.): ", curr.taxon.name, ": ", curr.subset.name),
      #         xlab = paste("PC", axis.1, " (", round(summary.PCA[2,axis.1]*100, 2), ")"),
      #         ylab = paste("PC", axis.2, " (", round(summary.PCA[2,axis.2]*100, 2), ")"))
      # }
      # legend("bottomright", legend = tree.legend.labels, pch=20, cex = .5, pt.cex = 1.5, bty = "n", col = tree.legend.cols)
      
      # plot.PCA.3D.rgl(PCA = PCA, numbers = label.ERCs, labels = tree.labels, colors = tree.cols, rgl.close = F)
      
      plot.PCA.3D.plotly(PCA = PCA, classifier = curr.classifier, size = 7.5)
    }
  }
}
dev.off()



########################################################################
# phytools shape plots (PCs 1-3)
for(s in 1:length(LM.subsets)){
  # get subset name
  curr.subset.name <- LM.subset.names[s]
  curr.taxon.name <- "Plecoptera"
  message(curr.subset.name)
  
  classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
  curr.PCA <- get(paste0("PCA.allofree.phylcorr.", curr.taxon.name, ".", curr.subset.name))
  
  # load tree
  curr.tree <- get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
  curr.classifier <- order.classifier.by.tree(classifier, curr.tree)
  
  plot.discrete.character.tree.microhabitat.l(tree = curr.tree, classifier = curr.classifier, nsim = 500)
  
  
  
  for(pc in 1:3){
    anc.stat <- contMap(curr.tree, curr.PCA.data$x[,pc], plot=FALSE)
    plot(anc.stat, legend= 0.7*max(nodeHeights(curr.tree)), ftype="off", type= "fan", fsize=c(0.5,0.5), lwd = 6, outline = F) # , type= "fan"
    tiplabels(pie = current.l, piecol = palette()[c(5,4,3,1)], cex = 0.3)
  }
  par(mfrow=(c(1,1)))
  
  plotTree(curr.tree, ftype = "i", lwd = 1, fsize = 0.6, ylim = c(-4, Ntip(curr.tree)))
  nodes <- (1:curr.tree$Nnode + Ntip(curr.tree)[apply()])
  
  
  # PCoA contmaps
  PCoA.1.food.l <- setNames(curr.classifier$PCoA.1.food.l, curr.classifier$ERC)
  PCoA.1.food.l <- PCoA.1.food.l[!is.na(PCoA.1.food.l)]
  # eco anc.state.tree
  anc.stat.eco <- contMap(curr.tree, PCoA.1.food.l, plot=FALSE, method = "anc.ML")
  
  # tiplabels(pie = current.l, piecol = palette()[c(5,4,3,1)], cex = 0.3)
  
  # PC1 anc.state.tree
  anc.stat.shape.1 <- contMap(curr.tree, curr.PCA$x[,1], plot=FALSE, method = "anc.ML")
  
  layout(matrix(1:3,1,3), widths=c(0.45, 0.1, 0.45))
  plot(anc.stat.shape.1, lwd=2, ftype = "off", legend = 60, outline = F, fsize = c(0, 1.2))
  plot.new()
  plot.window(xlim=c(-0.1,0.1),
              ylim=get("last_plot.phylo",envir=.PlotPhyloEnv)$y.lim)
  par(cex=0.6)
  text(rep(0, length(anc.stat.eco$tree$tip.label)), 1:Ntip(anc.stat.eco$tree),
       curr.classifier$ID, font = 1, cex = 0.75)
  plot(anc.stat.eco, lwd=2, ftype = "off", legend = 60, outline = F, fsize = c(0, 1.2), direction = "leftwards")
}

########################################################################
# continous data on pruned tree
LM.subset.names <- c("head.capsule", "LMs.Mds")
taxon.names <- c("all", "Dermaptera", "Plecoptera")
eco.cats <- c("current.l", "food.l", "microhabitat.l",
              "food.a", "microhabitat.a", "wings", "flight") 

pdf(paste0("./results/", gsub("_", "-", today()), " - continuous.trait.trees.pdf"), width = 15, height = 12)
for(t in 1:length(taxon.names)){ # :length(taxon.names)
  # define taxon
  curr.taxon.name <- taxon.names[t]
  
  for (s in 1:length(LM.subset.names)){
    # define subset
    curr.LM.subset.name <- LM.subset.names[s]
    
    for (e in 1: length(eco.cats)){
      curr.eco.cat <- eco.cats[e]
      
      # message what's going on
      message(curr.taxon.name)
      message(curr.LM.subset.name)
      message(curr.eco.cat)
      
      # load tree
      curr.tree.full <- get(paste0("tree.", curr.taxon.name, ".", curr.LM.subset.name))
      
      
      if(curr.eco.cat == "wings"){
        curr.PCoA.1.tibble <- array.2D %>% 
          select(ERC, wings) %>% 
          drop_na(wings) %>% 
          mutate(wings = str_replace(wings, "macropterous", "1")) %>% 
          mutate(wings = as.numeric(str_replace(wings, "apterous", "0")))
        curr.PCoA.1 <- setNames(curr.PCoA.1.tibble$wings, curr.PCoA.1.tibble$ERC)
      } else if(curr.eco.cat == "flight"){
        curr.PCoA.1.tibble <- array.2D %>% 
          select(ERC, flight) %>% 
          drop_na(flight) %>% 
          mutate(flight = str_replace(flight, "agile", "1")) %>% 
          mutate(flight = as.numeric(str_replace(flight, "flightless", "0")))
        curr.PCoA.1 <- setNames(curr.PCoA.1.tibble$flight, curr.PCoA.1.tibble$ERC)
      } else{
        
        # get PCoA results
        curr.PCoA <- get(paste0("PCoA.", curr.taxon.name, ".", curr.LM.subset.name, ".", curr.eco.cat))
        
        # save PCoA 1 results
        curr.PCoA.1 <- curr.PCoA$vectors[, 1] # cbind(dimnames(curr.PCoA$vectors)[[1]], 
      }
      # get ERCs with available eco data
      curr.ERCs <- names(curr.PCoA.1)
      
      # prune tree according to available eco data
      curr.tree <- prune.tree(ERCs = names(curr.PCoA.1), tree = curr.tree.full)
      
      # check if taxa are missing in tree
      if(length(setdiff(curr.ERCs, curr.tree$tip.label) == 0)){
        missing.ERCs <- setdiff(curr.ERCs, curr.tree$tip.label)
        
        curr.PCoA.1 <- curr.PCoA.1[-which(names(curr.PCoA.1) %in% missing.ERCs)]
        
        message("!!!!!!!!!!!!!!!!!!!!!")
        message("Taxa missing in tree:")
        message(paste(missing.ERCs, collapse = " "))
        message("!!!!!!!!!!!!!!!!!!!!!")
      }
      
      # get array.3D according to subset name
      curr.array.3D <- get(paste0("array.3D.", curr.taxon.name, ".", curr.LM.subset.name))
      
      # delete list elements within array.3D that contain no data for PCoA1
      curr.array.3D <- prune.array.3D(ERCs = curr.ERCs, array.3D = curr.array.3D) 
      
      # run GPA (with sliding in case of "LMs.hc")
      if(curr.LM.subset.name == "head.capsule"){
        message("sliding...")
        curr.gpa <- gpagen(curr.array.3D, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.LM.subset.name)), print.progress = F)
      } else {
        message("no sliding...")
        curr.gpa <- gpagen(curr.array.3D, print.progress = F)
      }
      
      # create geomorph data frmae
      curr.gdf <- geomorph.data.frame(curr.gpa,
                                      phy = curr.tree)
      
      # simple allometry analysis
      curr.allometry.uncorr <- procD.lm(coords~log(Csize), data=curr.gdf, iter=9999)
      
      # Obtain allometry-adjusted residuals
      curr.shape.resid.allo.corr <- arrayspecs(curr.allometry.uncorr$residuals, p=dim(curr.gpa$coords)[1], k=dim(curr.gpa$coords)[2])
      
      # store allomtry-corrected shape
      curr.shape.allofree <- curr.shape.resid.allo.corr + array(curr.gpa$consensus, dim(curr.shape.resid.allo.corr)) # allometry-free shapes
      
      # run allometry-corrected PCA (phy is only added so it can be referenced)
      curr.pca <- gm.prcomp(A = curr.shape.allofree, phy = curr.tree)
      
      # save PC1
      curr.PC.1 <- curr.pca$x[,1]#*(-1)
      
      plot.shape.eco.continous.trees(tree = curr.tree, ecol.factor.vals = curr.PCoA.1, left.title = curr.LM.subset.name, central.title = curr.taxon.name, right.title = curr.eco.cat)
    }
  }
}

dev.off()

########################################################################
# plot consensus
for(s in 1:length(LM.subsets)){
  # get subset name
  curr.subset.name <- LM.subset.names[s]
  message(curr.subset.name)
  
  # get GPA according to subset name
  curr.GPA <- get(paste0("GPA.", curr.subset.name))
  
  # calculate consensus shape from GPA
  consensus <- apply(curr.GPA$coords, c(1,2), mean)
  
  # plot empty plot with limits of consensus
  plot(consensus, asp = 1, type = "n", main = curr.subset.name)
  
  # plot current subset LM points of all specimens
  for(i in 1:length(curr.GPA$data$coords.1.X)){
    points(curr.GPA$coords[,,i], pch = 16, cex = 0.5)
  }
  
  # plot current LM points of consensus on top in red
  points(consensus, col="Red", cex = 1, pch = 16)
}

########################################################################
# test for phylogenetic signal in shape data

# get subset name
curr.taxon.name <- "all" # all Dermaptera Plecoptera
LM.subset.names <- c("head.capsule")
curr.subset.name <- LM.subset.names[1]
message(curr.taxon.name)
message(curr.subset.name)

# get GPA, PCA and tree data
gpa <- get(paste0("GPA.", curr.taxon.name, ".", curr.subset.name))
pca <- get(paste0("PCA.allofree.uncorr.", curr.taxon.name, ".", curr.subset.name))
tree <- get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))

# the geomorph way
signal.phyl.GPA <- physignal(A=gpa$coords, phy=tree,iter=9999, print.progress = T)
summary(signal.phyl.GPA)

# univariate (as phytools phylosig does it):
# signal.phyl.PCA <- physignal(A=pca$x[,1], phy=tree,iter=999, print.progress = T)
# summary(signal.phyl.PCA)

sig.K <- phylosig(tree = tree, x = pca$x, method="K", test=T, nsim=10000)
sig.L <- phylosig(tree = tree, x = pca$x, method="lambda", test=T)
sig.K
sig.L

signal.phyl.GPA <- physignal(A=gpa$coords, phy=phytools:::lambdaTree(tree, 0.8718303),iter=9999, print.progress = T)
summary(signal.phyl.GPA)

# http://blog.phytools.org/2012/03/phylogenetic-signal-with-k-and.html
# Blomberg's own explanation:
# "A K less than one implies that relatives resemble each other less than expected under 
# Brownian motion evolution along the candidate tree. This could be caused by either departure 
# from Brownian motion evolution, such as adaptive evolution that is uncorrelated with the phylogeny 
# (i.e., homoplasy), or ‘‘measurement error’’ in the broad sense."

# λ measures the similarity of the covariances among species to the covariances expected under 
# Brownian motion; whereas K might be more usefully thought of as a measure of the partitioning of variance. 
# If K>1 then variance tends to be among clades; while if K<1 then variance is within clades (with BM as reference).

# http://www.phytools.org/Cordoba2017/ex/5/Cont-char-models.html
# λ is a scaling coefficient for the expected covariances between species. One way to interpret this is as a tree 
# transformation that stretches tip branches (and thus the predicted variances between species) relative to internal 
# branches (and predicted covariances). λ close to zero, means phylogenetic signal equivalent to that expected if the 
# data arose on a star phylogeny (that is, no phylogenetic signal). λ = 1 corresponds to a Brownian motion model.

# Another way to think about λ is as an implicit transformation of the tree in which λ = 0 is equivalente to a 
# star-phylogeny, whereas λ = 1 has covariances among related species that match those implied by the original phylogeny. 
# We can start by visualizing the implication of different values of λ on the predicted covariances between species, as 
# represented by a tree transformation:

# par(mfcol=c(1,3))
# plotTree(phytools:::lambdaTree(tree,1),ftype="i",
#          mar=c(0.1,0.1,4.1,0.1),fsize=0.6,lwd=1)
# title(main=expression(paste("Pagel's ",lambda," = 1.0",sep="")))
# plotTree(phytools:::lambdaTree(tree,0.5),ftype="i",
#          mar=c(0.1,0.1,4.1,0.1),fsize=0.6,lwd=1)
# title(main=expression(paste("Pagel's ",lambda," = 0.5",sep="")))
# plotTree(phytools:::lambdaTree(tree,0.1),ftype="i",
#          mar=c(0.1,0.1,4.1,0.1),fsize=0.6,lwd=1)
# title(main=expression(paste("Pagel's ",lambda," = 0.1",sep="")))


########################################################################
# test for phylogenetic signal in eco data

taxon.names <- c("all", "Dermaptera", "Plecoptera")
eco.cats <- c("current.l", "food.l", "microhabitat.l",
              "food.a", "microhabitat.a", "wings", "flight",
              "mech.adv", "md1.ins.size", "Csize.ant", "Csize.occ") 
LM.subset.names <- c("head.capsule", "LMs.Mds")



for(t in 1:1){ # length(taxon.names)
  if(t==1){
    phylo.sig.tibble <- create.empty.tibble(nrow = length(taxon.names)*length(LM.subset.names)*length(eco.cats), 
                                            names = c("taxon", "LM.subset", "ecol.cat", "K", "p.K", "lambda", "p.lambda"))
    l.counter <- 1
  }
  curr.taxon.name <- taxon.names[t]
  
  for(s in 1:1){ # length(LM.subset.names)
    curr.subset.name <- LM.subset.names[s]
    
    # get GPA, PCA and tree data
    tree <- get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
    classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
    
    for(e in 1:length(eco.cats)){ 
      curr.eco.cat <- eco.cats[e]
      message(curr.taxon.name)
      message(curr.subset.name)
      message(curr.eco.cat)
      
      if((curr.eco.cat == "current.l" & curr.taxon.name == "Dermaptera") | 
         (curr.eco.cat == "feeding_type.l"  & curr.taxon.name == "Dermaptera")){
        message(paste0("skipping ", curr.eco.cat, " for Dermaptera..."))
      } else {
        if(curr.eco.cat == "wings" | curr.eco.cat == "flight" | curr.eco.cat == "mech.adv" |
           curr.eco.cat == "md1.ins.size" | curr.eco.cat == "Csize.ant" | curr.eco.cat == "Csize.occ") {
          curr.eco.column <- which(colnames(classifier) == curr.eco.cat)
        } else {
          curr.eco.column <- which(colnames(classifier) == paste0("PCoA.1.", curr.taxon.name, ".", curr.eco.cat))
        }
        if(curr.eco.cat == "wings" | curr.eco.cat == "flight"){
          classifier[,curr.eco.column] <- gsub("apterous", "0", classifier[,curr.eco.column])
          classifier[,curr.eco.column] <- gsub("macropterous", "1", classifier[,curr.eco.column])
          classifier[,curr.eco.column] <- gsub("flightless", "0", classifier[,curr.eco.column])
          classifier[,curr.eco.column] <- gsub("agile", "1", classifier[,curr.eco.column])
        }
        curr.eco.data <- setNames(as.numeric(as.character(classifier[,curr.eco.column])), classifier$ERC)
        
        curr.sig.K <- phylosig(tree = tree, x = curr.eco.data, method="K", test=T, nsim=10000)
        curr.sig.L <- phylosig(tree = tree, x = curr.eco.data, method="lambda", test=T)
        
        phylo.sig.tibble$taxon[l.counter] <- curr.taxon.name
        phylo.sig.tibble$LM.subset[l.counter] <- curr.subset.name
        phylo.sig.tibble$ecol.cat[l.counter] <- curr.eco.cat
        phylo.sig.tibble$K[l.counter] <- curr.sig.K$K
        phylo.sig.tibble$p.K[l.counter] <- curr.sig.K$P
        phylo.sig.tibble$lambda[l.counter] <- curr.sig.L$lambda
        phylo.sig.tibble$p.lambda[l.counter] <- curr.sig.L$P
        
        l.counter <- l.counter+1
      }
    }
  }
}
phylo.sig.tibble <- phylo.sig.tibble[complete.cases(phylo.sig.tibble),]

write.xlsx2(as.data.frame(phylo.sig.tibble), paste0("./results/", today(), " - phylo.sig.ecocats.xlsx"), row.names = F)

########################################################################
# create heat map from phylo.sig.tibble


########################################################################
# Allometry

# get subset name
curr.subset.name <- LM.subset.names[1]
curr.taxon.name <- "all"
message(curr.subset.name)
message(curr.taxon.name)


# all analyses are allo-free and phylocorrected
# allometry "all"
curr.taxon.name <- "all"
curr.subset.name <- "head.capsule"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = gdf)
summary(PGLS)

# allometry "Dermaptera"
curr.taxon.name <- "Dermaptera"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = gdf)
summary(PGLS)

# allometry "Plecoptera"
curr.taxon.name <- "Plecoptera"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = gdf)
summary(PGLS)

########################################################################
# disparity analysis
# ALL
# disparity orders
curr.taxon.name <- "all"
curr.subset.name <- "head.capsule"
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = gdf)
morphol.disparity.orders <- morphol.disparity(PGLS, groups= ~order, data = gdf,
                                              iter = 9999, print.progress = FALSE)
# disparity superfamilies
new.gdf <- gdf
new.gdf$superfamily <- as.character(new.gdf$superfamily)
new.gdf$superfamily[new.gdf$superfamily != "Perloidea"] <- "non-Perloidea"
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = new.gdf)
morphol.disparity.superfamilies <- morphol.disparity(PGLS, groups= ~superfamily, data = new.gdf,
                                                     iter = 9999, print.progress = FALSE)
# disparity "Dermaptera" against "non-perloidean stoneflies"
new.gdf <- gdf
new.gdf$order <- as.character(new.gdf$order)
new.gdf$superfamily <- as.character(new.gdf$superfamily)
new.gdf$superfamily[new.gdf$superfamily != "Perloidea" & new.gdf$order == "Plecoptera"] <- "non-Perloidea"
new.gdf$superfamily[new.gdf$order == "Dermaptera"] <- "Dermaptera"
PGLS <- procD.pgls(f1 = coords~log(Csize), phy = phy, data = new.gdf)
morphol.disparity.superfamilies <- morphol.disparity(PGLS, groups= ~superfamily, data = new.gdf,
                                                     iter = 9999, print.progress = FALSE)


# food larvae all
curr.taxon.name <- "all"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
curr.classifier.eco <- curr.classifier[!is.na(curr.classifier$PCoA.1.all.food.l),]
curr.ERCs <- curr.classifier.eco$ERC
curr.coords <- prune.coords(curr.ERCs, gdf$coords)
curr.Csizes <- prune.Csizes(curr.ERCs, gdf$Csize)
curr.tree <-  get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
curr.tree <- prune.tree(curr.ERCs, curr.tree)
curr.classifier.eco <- order.classifier.by.tree(curr.classifier.eco, curr.tree)
groups <- curr.classifier.eco$food.larva
groups[which(groups != "predatory")] <- "non_predatory"

PGLS <- procD.pgls(f1 = curr.coords~curr.Csizes, phy = curr.tree)

morphol.disparity.food.larvae <- morphol.disparity(f1 = PGLS, groups= ~ groups,
                                                   iter = 9999, print.progress = FALSE)

# food larvae Dermaptera
curr.taxon.name <- "Dermaptera"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
curr.classifier.eco <- curr.classifier[!is.na(curr.classifier$PCoA.1.all.food.l),]
curr.ERCs <- curr.classifier.eco$ERC
curr.coords <- prune.coords(curr.ERCs, gdf$coords)
curr.Csizes <- prune.Csizes(curr.ERCs, gdf$Csize)
curr.tree <-  get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
curr.tree <- prune.tree(curr.ERCs, curr.tree)
curr.classifier.eco <- order.classifier.by.tree(curr.classifier.eco, curr.tree)
groups <- curr.classifier.eco$food.larva
groups[which(groups != "predatory")] <- "non_predatory"

PGLS <- procD.pgls(f1 = curr.coords~curr.Csizes, phy = curr.tree)

morphol.disparity.food.larvae <- morphol.disparity(f1 = PGLS, groups= ~ groups,
                                                   iter = 9999, print.progress = FALSE)

# food larvae Plecoptera
curr.taxon.name <- "Plecoptera"
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))
gdf <- get(paste0("gdf.", curr.taxon.name, ".", curr.subset.name))
curr.classifier.eco <- curr.classifier[!is.na(curr.classifier$PCoA.1.all.food.l),]
curr.ERCs <- curr.classifier.eco$ERC
curr.coords <- prune.coords(curr.ERCs, gdf$coords)
curr.Csizes <- prune.Csizes(curr.ERCs, gdf$Csize)
curr.tree <-  get(paste0("tree.", curr.taxon.name, ".", curr.subset.name))
curr.tree <- prune.tree(curr.ERCs, curr.tree)
groups <- curr.classifier.eco$food.larva
curr.classifier.eco <- order.classifier.by.tree(curr.classifier.eco, curr.tree)
groups[which(groups != "predatory")] <- "non_predatory"

PGLS <- procD.pgls(f1 = curr.coords~curr.Csizes, phy = curr.tree)

morphol.disparity.food.larvae <- morphol.disparity(f1 = PGLS, groups= ~ groups,
                                                   iter = 9999, print.progress = FALSE)

########################################################################
# ecological interactions
# small function to add procD.lm data into tibble
add.procD.lm.data.to.tibble <- function(eco.cat, taxon, procD.lm.results, procD.pgls.results, LM.subset.name){
  # procD.lm.results <- curr.procD.lm
  # LM.subset.name <- curr.LM.subset.name
  # create empty tibble with 3 rows and colnames s.below
  curr.ecol.interactions.tibble <- create.empty.tibble(nrow = 3, names = c("taxon", "ecol.cat", "LM.set", "n", 
                                                                           "Rsq", "F", "Z", "p", 
                                                                           "Rsq.corr", "F.corr", "Z.corr", "p.corr"))
  
  # set line number to 1
  l = 1
  
  # store current taxon.name in tibble line 1
  curr.ecol.interactions.tibble$LM.set[l] <- LM.subset.name
  
  # store current LM.subset.name in tibble line 1
  curr.ecol.interactions.tibble$taxon[l] <- taxon
  
  # set t to 1: row 1 in aov.table of ANOVA results
  t = 1
  
  # pull results from ANOVA aov.table and store in tibble in line 1
  curr.ecol.interactions.tibble$ecol.cat[l] <- eco.cat
  curr.ecol.interactions.tibble$n[l] <- nrow(curr.classifier)
  
  curr.ecol.interactions.tibble$Rsq[l] <- procD.lm.results$aov.table[t,4]
  curr.ecol.interactions.tibble$'F'[l] <- procD.lm.results$aov.table[t,5]
  curr.ecol.interactions.tibble$Z[l] <- procD.lm.results$aov.table[t,6]
  curr.ecol.interactions.tibble$p[l] <- procD.lm.results$aov.table[t,7]
  
  curr.ecol.interactions.tibble$Rsq.corr[l] <- procD.pgls.results$aov.table[t,4]
  curr.ecol.interactions.tibble$'F.corr'[l] <- procD.pgls.results$aov.table[t,5]
  curr.ecol.interactions.tibble$Z.corr[l] <- procD.pgls.results$aov.table[t,6]
  curr.ecol.interactions.tibble$p.corr[l] <- procD.pgls.results$aov.table[t,7]
  
  # set t to 2: row 2 in aov.table of ANOVA results and check if this row exists
  t = 2
  if(!is.na(curr.procD.lm$aov.table[t,7])){
    # pull results from ANOVA aov.table and store in tibble in next line
    l = l+1
    curr.ecol.interactions.tibble$ecol.cat[l] <- eco.cat
    curr.ecol.interactions.tibble$n[l] <- nrow(curr.classifier)
    
    curr.ecol.interactions.tibble$Rsq[l] <- procD.lm.results$aov.table[t,4]
    curr.ecol.interactions.tibble$'F'[l] <- procD.lm.results$aov.table[t,5]
    curr.ecol.interactions.tibble$Z[l] <- procD.lm.results$aov.table[t,6]
    curr.ecol.interactions.tibble$p[l] <- procD.lm.results$aov.table[t,7]
    
    curr.ecol.interactions.tibble$Rsq.corr[l] <- procD.pgls.results$aov.table[t,4]
    curr.ecol.interactions.tibble$'F.corr'[l] <- procD.pgls.results$aov.table[t,5]
    curr.ecol.interactions.tibble$Z.corr[l] <- procD.pgls.results$aov.table[t,6]
    curr.ecol.interactions.tibble$p.corr[l] <- procD.pgls.results$aov.table[t,7]
  }
  
  # set t to3: row 3 in aov.table of ANOVA results and check if this row exists
  t = 3
  if(!is.na(curr.procD.lm$aov.table[t,7])){
    # pull results from ANOVA aov.table and store in tibble in next line
    l = l+1
    curr.ecol.interactions.tibble$ecol.cat[l] <- eco.cat # eco.cat
    curr.ecol.interactions.tibble$n[l] <- nrow(curr.classifier)
    
    curr.ecol.interactions.tibble$Rsq[l] <- procD.lm.results$aov.table[t,4]
    curr.ecol.interactions.tibble$'F'[l] <- procD.lm.results$aov.table[t,5]
    curr.ecol.interactions.tibble$Z[l] <- procD.lm.results$aov.table[t,6]
    curr.ecol.interactions.tibble$p[l] <- procD.lm.results$aov.table[t,7]
    
    curr.ecol.interactions.tibble$Rsq.corr[l] <- procD.pgls.results$aov.table[t,4]
    curr.ecol.interactions.tibble$'F.corr'[l] <- procD.pgls.results$aov.table[t,5]
    curr.ecol.interactions.tibble$Z.corr[l] <- procD.pgls.results$aov.table[t,6]
    curr.ecol.interactions.tibble$p.corr[l] <- procD.pgls.results$aov.table[t,7]
  }
  
  # delete rows with NA (i.e., aov.table had less than 3 rows)
  curr.ecol.interactions.tibble <- curr.ecol.interactions.tibble[complete.cases(curr.ecol.interactions.tibble), ]
  return(curr.ecol.interactions.tibble)
}

# choose the taxa that should be analysed
taxon.names <- c("all", "Dermaptera", "Plecoptera")

# choose the LM.subset.names that should be analysed
LM.subsets.ecol.names <- c("head.capsule", "LMs.Mds") # "LMs.Mds", "LMs.Lbr", "LMs.Car", "LMs.Lab"

ecology.cat.names <- list("current.l", "food.l", "microhabitat.l",
                          "food.a", "microhabitat.a", "wings", "flight",
                          "mech.adv", "md1.ins.size", "Csize.ant", "Csize.occ") # , "md1.ins.size"

# create tibbles to save ANOVA results for corrected and uncorrected data
ecol.interactions.tibble <- create.empty.tibble(nrow = 0, names = c("taxon", "ecol.cat", "LM.set", "n", 
                                                                    "Rsq", "F", "Z", "p", 
                                                                    "Rsq.corr", "F.corr", "Z.corr", "p.corr")) # length(ecology.cat.names)*4
for(t in 1:length(taxon.names)){
  curr.taxon.name <- taxon.names[t]
  # run Procrustes PGLS / ANOVAs for all ecol cats and LM sets with corrected and uncorrected data
  for(s in 1:length(LM.subsets.ecol.names)){ # length(LM.subsets.ecol.names)
    # get current LM.subset name
    curr.LM.subset.name <- LM.subsets.ecol.names[s]
    
    for(e in 1:length(ecology.cat.names)){
      # get current eco cat
      curr.eco.cat <- unlist(ecology.cat.names[e])
      message(paste0(curr.taxon.name, ": ", curr.eco.cat, ": ", curr.LM.subset.name))
      
      if(curr.taxon.name == "Dermaptera" & curr.eco.cat == "current.l"){
        message("Dermaptera have only one value in current.larva PCoA1 and this cannot be tested.")
        ecol.interactions.tibble[nrow(ecol.interactions.tibble)+1, 1] = curr.taxon.name
        ecol.interactions.tibble$ecol.cat[nrow(ecol.interactions.tibble)] = curr.eco.cat
        ecol.interactions.tibble$LM.set[nrow(ecol.interactions.tibble)] = curr.LM.subset.name
        ecol.interactions.tibble$n[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$Rsq[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$"F"[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$Z[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$p[nrow(ecol.interactions.tibble)] = 1
        ecol.interactions.tibble$Rsq.corr[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$"F.corr"[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$Z.corr[nrow(ecol.interactions.tibble)] = 0
        ecol.interactions.tibble$p.corr[nrow(ecol.interactions.tibble)] = 1
      } else {
        # get tree
        curr.tree <- get(paste0("tree.", curr.taxon.name, ".", curr.LM.subset.name))
        
        # get classifier according to subset name
        curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.LM.subset.name))
        curr.classifier <- order.classifier.by.ERCs(curr.classifier, curr.tree$tip.label)
        
        
        # get PCoA results colnumber
        # check if eco.cat is wings or flight: these have only two character states and did thus not undergo a PCoA
        if(curr.eco.cat == "wings" | curr.eco.cat == "flight" | curr.eco.cat == "mech.adv" |
           curr.eco.cat == "md1.ins.size" | curr.eco.cat == "Csize.ant" | curr.eco.cat == "Csize.occ") {
          curr.PCoA.eco.cat.colnumber <- which(colnames(curr.classifier) == curr.eco.cat)
          
          # curr.PCoA.2.eco.cat.colnumber <- which(colnames(curr.classifier) == paste0("PCoA.2.", curr.taxon.name, ".", curr.eco.cat))
          
        } else {
          # find column number that contains the PCoA axis 1 of the current ecol.name, i.e. current.l
          curr.PCoA.eco.cat.colnumber <- which(colnames(curr.classifier) == paste0("PCoA.1.", curr.taxon.name, ".", curr.eco.cat))
        }
        
        # find rownumbers with NA values in PCoA1 column
        rown.numbers.with.na <- which(is.na(curr.classifier[, curr.PCoA.eco.cat.colnumber]))
        
        # prune tree
        tips.to.drop <- curr.classifier$ERC[rown.numbers.with.na]
        curr.tree.dropped <- drop.tip(curr.tree, tips.to.drop)
        
        # delete rows from classifier without eco data
        if(length(rown.numbers.with.na) > 0){
          curr.classifier <- curr.classifier[-rown.numbers.with.na,]
        }
        # 
        # # add pcoa results to classifier
        # curr.classifier$curr.PCoA.1 <- NA
        # for(f in 1:nrow(curr.classifier)){
        #   curr.ERC <- curr.classifier$ERC[f]
        #   curr.classifier$curr.PCoA.1[f] <- pcoa.curr.df$vectors[which(rownames(pcoa.curr.df$vectors) == curr.ERC),1]
        # }
        
        # get array.3D according to subset name
        curr.array.3D <- get(paste0("array.3D.", curr.taxon.name, ".", curr.LM.subset.name))
        
        # delete list elements within array.3D that contain no data for PCoA1
        curr.array.3D <- prune.array.3D(ERCs = curr.classifier$ERC, array.3D = curr.array.3D) 
        
        # run GPA (with sliding in case of "LMs.hc")
        if(curr.LM.subset.name == "head.capsule"){
          message("sliding...")
          curr.gpa <- gpagen(curr.array.3D, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.LM.subset.name)), print.progress = F)
        } else {
          message("no sliding...")
          curr.gpa <- gpagen(curr.array.3D, print.progress = F)
        }
        # names(curr.gpa$Csize)
        # plot(curr.gpa, label = T, plot.param = list(pt.cex = 0.1, txt.cex = 1))
        
        # sort classifier according to curr.gpa
        curr.classifier <- order.classifier.by.ERCs(curr.classifier, dimnames(curr.gpa$coords)[[3]])
        
        # # divide size-dependant values by Csize
        # pcoa.curr.df$vectors
        PCoA.1 = setNames(curr.classifier[, curr.PCoA.eco.cat.colnumber],
                          curr.classifier$ERC)
        
        # if(curr.eco.cat == "md1.ins.size" | curr.eco.cat == "Csize.ant" | curr.eco.cat == "Csize.occ"){
        #   Csizes.LMs <- curr.gpa$Csize
        #   PCoA.1 <- PCoA.1
        # }
        # build geomorph.data.frame
        curr.gdf <- geomorph.data.frame(curr.gpa,
                                        phy = curr.tree.dropped,
                                        PCoA.1 = PCoA.1) # 14
        
        
        # run Procrustes PGLS
        curr.procD.lm <- procD.lm(coords~PCoA.1, data = curr.gdf, iter=9999, print.progress = F) # this applies to Plecos only so no order group
        curr.procD.pgls <- procD.pgls(coords~PCoA.1, data = curr.gdf, phy = phy, iter=9999, print.progress = F)
        
        print(summary(curr.procD.pgls))
        
        # add proc.D.lm data into tibble
        # ecol.interactions.tibble$ecol.cat[nrow(ecol.interactions.tibble)+1] <- curr.LM.subset.name
        ecol.interactions.tibble <- rbind(ecol.interactions.tibble, 
                                          add.procD.lm.data.to.tibble(taxon = curr.taxon.name,
                                                                      eco.cat = curr.eco.cat, 
                                                                      procD.lm.results = curr.procD.lm, 
                                                                      procD.pgls.results = curr.procD.pgls, 
                                                                      LM.subset.name = curr.LM.subset.name))
      }
    }
  }
}

ecol.interactions.tibble <- ecol.interactions.tibble %>% mutate_at(vars(n, 
                                                                        Rsq, F, Z, p, 
                                                                        Rsq.corr, F.corr, Z.corr, p.corr), funs(as.double))

if(remove.specimens){
  write.xlsx2(as.data.frame(ecol.interactions.tibble), paste0("./results/", today(), " - ecol-interactions-shape_filtered.xlsx"), row.names = F)
} else {
  write.xlsx2(as.data.frame(ecol.interactions.tibble), paste0("./results/", today(), " - ecol-interactions-shape.xlsx"), row.names = F)
}

# plot ecological interactions table
ecol.interactions.tibble$ecol.cat <- factor(ecol.interactions.tibble$ecol.cat, 
                                            levels = c("current.l", "microhabitat.l", "microhabitat.a", "food.l",
                                                       "food.a", "wings", "flight", "mech.adv", "Csize.ant", "Csize.occ",
                                                       "md1.ins.size"))

ecol.interactions.tibble.plot <- ecol.interactions.tibble
ecol.interactions.tibble.plot$Rsq.corr[ecol.interactions.tibble.plot$p.corr > 0.05] <- 0
ecol.interactions.tibble.plot$Rsq[ecol.interactions.tibble.plot$p > 0.05] <- 0
# ecol.interactions.tibble.plot$col[ecol.interactions.tibble.plot$p.corr > 0.05] <- "grey50"
# ecol.interactions.tibble.plot$col[ecol.interactions.tibble.plot$p.corr <= 0.05] <- "#00BB1F"

ecol.interactions.tibble.plot.head <- ecol.interactions.tibble.plot %>% 
  filter(LM.set == "head.capsule")

ggplot(data = ecol.interactions.tibble.plot.head, aes(x = taxon, y = Rsq, fill = ecol.cat)) +
  geom_bar(stat="identity", position = position_dodge()) + 
  facet_wrap(~LM.set, ncol = 1) + 
  facet_wrap(~ecol.cat) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("uncorrected")

ggplot(data = ecol.interactions.tibble.plot.head, aes(x = taxon, y = Rsq.corr, fill = ecol.cat)) +
  geom_bar(stat="identity", position = position_dodge()) + 
  facet_wrap(~LM.set, ncol = 1) + 
  facet_wrap(~ecol.cat) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("corrected")

ecol.interactions.tibble.plot.mandibles <- ecol.interactions.tibble.plot %>% 
  filter(LM.set == "LMs.Mds")

ggplot(data = ecol.interactions.tibble.plot.mandibles, aes(x = taxon, y = Rsq, fill = ecol.cat)) +
  geom_bar(stat="identity", position = position_dodge()) + 
  facet_wrap(~LM.set, ncol = 1) + 
  facet_wrap(~ecol.cat) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("uncorrected")

ggplot(data = ecol.interactions.tibble.plot.mandibles, aes(x = taxon, y = Rsq.corr, fill = ecol.cat)) +
  geom_bar(stat="identity", position = position_dodge()) + 
  facet_wrap(~LM.set, ncol = 1) + 
  facet_wrap(~ecol.cat) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("corrected")


#####################################################
# multivariate testing: shape.coords~eco.PCoA
curr.taxon.name <- "all"
curr.eco.cat <- "food.l"
curr.subset.name <- "head.capsule"

# get classifier according to subset name
curr.classifier <- get(paste0("classifier.", curr.taxon.name, ".", curr.LM.subset.name))
# curr.classifier <- order.classifier.by.ERCs(curr.classifier, curr.tree$tip.label)

# get tree
curr.tree <- get(paste0("tree.", curr.taxon.name, ".", curr.LM.subset.name))

# load PCoA data from script 1
curr.PCoA <- get(paste0("PCoA.", curr.taxon.name, ".", curr.eco.cat))

# get PCoA IDs
PCoA.IDs <- rownames(curr.PCoA$vectors)

# convert PCoA IDs to ERCs
PCoA.ERCs <- convert.IDs.to.ERCs(PCoA.IDs, PBT.Derma.Pleco.LMs)

# change rownames of PCoA to its ERCs
rownames(curr.PCoA$vectors) <- PCoA.ERCs

# get array.3D according to subset name
curr.array.3D <- get(paste0("array.3D.", curr.taxon.name, ".", curr.LM.subset.name))

# delete list elements within array.3D that contain no data for PCoA
curr.array.3D <- prune.array.3D(ERCs = PCoA.ERCs, array.3D = curr.array.3D) 

# find rownumbers with NA values in PCoA1 column
rown.numbers.with.na <- which(is.na(curr.classifier[, curr.PCoA.eco.cat.colnumber]))

# prune tree
tips.to.drop <- curr.classifier$ERC[rown.numbers.with.na]
curr.tree.dropped <- drop.tip(curr.tree, tips.to.drop)

# delete rows from classifier without eco data
if(length(rown.numbers.with.na) > 0){
  curr.classifier <- curr.classifier[-rown.numbers.with.na,]
}

# run GPA (with sliding in case of "LMs.hc")
if(curr.LM.subset.name == "head.capsule"){
  message("sliding...")
  curr.gpa <- gpagen(curr.array.3D, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.LM.subset.name)), print.progress = F)
} else {
  message("no sliding...")
  curr.gpa <- gpagen(curr.array.3D, print.progress = F)
}
names(curr.gpa$Csize)

phylo.integration(curr.gpa$coords, curr.PCoA$vectors, phy = )
######################################################################### two block PLS ####

PLS.LM.subsets <- LM.subsets.all # c(head.capsule, LMs.Mds, PLS.LMs.hc_Md, PLS.LMs.hc_Car, PLS.LMs.hc_Lab)
PLS.LM.subset.names <- LM.subset.names.all # c("head.capsule", "LMs.Mds", "PLS.LMs.hc_Md", "PLS.LMs.hc_Car", "PLS.LMs.hc_Lab")

# choose the taxa that should be analysed
taxon.names <- c("all", "Dermaptera", "Plecoptera")

# pairwise comparison 'matrix' loop
for(t in 1:length(taxon.names)){
  curr.taxon.name <- taxon.names[t]
  if(t == 1){
    # create tibble to store two.b.PLS results
    two.b.PLS.df <- create.empty.tibble(nrow = 0, names = c("taxon", "block.1", "block.2", "n", "p.val", "r.PLS"))
    summary.counter = 1
  }
  for(s.1 in 1:length(PLS.LM.subset.names)){
    for(s.2 in 2:length(PLS.LM.subset.names)){
      if(s.2 - s.1 >= 1){
        message(curr.taxon.name)
        message(paste0(PLS.LM.subset.names[s.1], " --> ", PLS.LM.subset.names[s.2]))
        
        # get subset names
        curr.subset.name.1 <- PLS.LM.subset.names[s.1]
        curr.subset.name.2 <- PLS.LM.subset.names[s.2]
        
        # define array.3Ds 1 and 2
        array.3D.1 <- get(paste0("array.3D.", curr.taxon.name, ".", curr.subset.name.1))
        array.3D.2 <- get(paste0("array.3D.", curr.taxon.name, ".", curr.subset.name.2))
        
        # not sure why, but sometimes after first loop length(non.overlap.specimens) != 0
        for(p in 1:2){
          # get specimen names from both blocks
          specimens.block.1 <- dimnames(array.3D.1)[[3]]
          specimens.block.2 <- dimnames(array.3D.2)[[3]]
          
          # find specimens that are not present in both blocks
          non.overlap.specimens <- unique(c(setdiff(specimens.block.1, specimens.block.2), setdiff(specimens.block.2, specimens.block.1)))
          
          # go through each specimen of block 1
          for(n in 1:length(dimnames(array.3D.1)[[3]])){
            # get current specimen name
            curr.specimen.name <- dimnames(array.3D.1)[[3]][n]
            
            # delete specimen if it is not present in both blocks, i.e. if it is part of non.overlap.specimens
            if(curr.specimen.name %in% non.overlap.specimens){
              array.3D.1 <- array.3D.1[,,-n]
            }
          }
          
          # go through each specimen of block 2
          for(n in 1:length(dimnames(array.3D.2)[[3]])){
            # get current specimen name
            curr.specimen.name <- dimnames(array.3D.2)[[3]][n]
            
            # delete specimen if it is not present in both blocks, i.e. if it is part of non.overlap.specimens
            if(curr.specimen.name %in% non.overlap.specimens){
              array.3D.2 <- array.3D.2[,,-n]
            }
          }
        }
        
        # run GPAs in the array.3Ds
        GPA.1 <- gpagen(array.3D.1, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.subset.name.1)))
        GPA.2 <- gpagen(array.3D.2, curves = get(paste0("slid.matrix.", curr.taxon.name, ".", curr.subset.name.2)))
        
        # gett coords from GPAs
        block.1 <- GPA.1$coords
        block.2 <- GPA.2$coords
        
        # print summary and assign results to lists
        curr.summary <- two.b.pls(block.1, block.2, iter = 999)
        assign(paste0("PLS__", curr.taxon.name, "__", curr.subset.name.1, "__", curr.subset.name.2), curr.summary)
        summary.counter <- summary.counter+1
        two.b.PLS.df[(nrow(two.b.PLS.df)+1), 1] <- curr.taxon.name
        two.b.PLS.df$block.1[nrow(two.b.PLS.df)] <- curr.subset.name.1
        two.b.PLS.df$block.2[nrow(two.b.PLS.df)] <- curr.subset.name.2
        two.b.PLS.df$r.PLS[nrow(two.b.PLS.df)] <- curr.summary$r.pls
        two.b.PLS.df$p.val[nrow(two.b.PLS.df)] <- curr.summary$P.value
        two.b.PLS.df$n[nrow(two.b.PLS.df)] <- length(specimens.block.1)
        # if(length(curr.summary$Z > 0)) {two.b.PLS.df$eff.size[nrow(two.b.PLS.df)] <- curr.summary$Z} # maybe effect size Z is deprecated?
      }
    }
  }
}

all.PLS <- compare.pls(PLS__all__LMs.Mds__PLS.LMs.hc_Md, 
                       PLS__all__LMs.Car__PLS.LMs.hc_Car, 
                       PLS__all__LMs.Lab__PLS.LMs.hc_Lab,
                       PLS__all__LMs.Mds__LMs.Car,
                       PLS__all__LMs.Mds__LMs.Lab,
                       PLS__all__LMs.Car__LMs.Lab)

compare.pls(PLS__Dermaptera__LMs.Mds__PLS.LMs.hc_Md, 
            PLS__Dermaptera__LMs.Car__PLS.LMs.hc_Car, 
            PLS__Dermaptera__LMs.Lab__PLS.LMs.hc_Lab,
            PLS__Dermaptera__LMs.Mds__LMs.Car,
            PLS__Dermaptera__LMs.Mds__LMs.Lab,
            PLS__Dermaptera__LMs.Car__LMs.Lab)

compare.pls(PLS__Plecoptera__LMs.Mds__PLS.LMs.hc_Md, 
            PLS__Plecoptera__LMs.Car__PLS.LMs.hc_Car, 
            PLS__Plecoptera__LMs.Lab__PLS.LMs.hc_Lab,
            PLS__Plecoptera__LMs.Mds__LMs.Car,
            PLS__Plecoptera__LMs.Mds__LMs.Lab,
            PLS__Plecoptera__LMs.Car__LMs.Lab)

# write.xlsx2(as.data.frame(two.b.PLS.df), paste0("./results/", today(), " - two-b-PLS-results.xlsx"), row.names = F)



# two.b.PLS.df <- create.empty.tibble(nrow = 0, names = c("block.1", "block.2", "n", "p.val", "r.PLS"))
# 
# array.3D.1 <- array.3D.PLS.LMs.hc_Md
# array.3D.1 <- array.3D.LMs.Mds
# 
# 
# curr.summary <- two.b.pls(block.1, block.2, iter = 999)
# two.b.PLS.df[(nrow(two.b.PLS.df)+1), 1] <- curr.subset.name.1
# two.b.PLS.df$block.2[nrow(two.b.PLS.df)] <- curr.subset.name.2
# two.b.PLS.df$r.PLS[nrow(two.b.PLS.df)] <- curr.summary$r.pls
# two.b.PLS.df$p.val[nrow(two.b.PLS.df)] <- curr.summary$P.value
# two.b.PLS.df$n[nrow(two.b.PLS.df)] <- length(specimens.block.1)

# get some data from the classifiers <- this had to be caluclated
classifier.infos <- create.empty.tibble(length(LM.subset.names), c("LM.set", "n"))
ERC.numbers.in.LM.subsets <- list()
for(t in 1:length(LM.subset.names)){
  curr.subset.name <- LM.subset.names[t]
  curr.classifier <- get(paste0("classifier.", curr.subset.name))
  
  classifier.infos$LM.set[t] <- curr.subset.name
  classifier.infos$n[t] <- nrow(curr.classifier)
  
  ERC.numbers.in.LM.subsets[t] <- list(curr.classifier$ERC)
}
names(ERC.numbers.in.LM.subsets) <- LM.subset.names


#######################################
# Coverage
###### load species file taxonomy #####
SF.taxonomy <- load.SF.taxonomy() %>% 
  filter(order == "Dermaptera" | order == "Plecoptera") %>% 
  mutate(ID = paste0(genus, "_", species))

curr.taxon.name <- taxon.names[1]
# define subset
curr.subset.name <- LM.subset.names[2]
# load classifier
# curr.classifier <- as_tibble(get(paste0("classifier.", curr.taxon.name, ".", curr.subset.name))) %>% 
#   select(ID, ERC, genus, species, order, superfamily, family, subfamily)
curr.classifier <- array.2D %>% 
  select(ID, ERC, genus, species, order, superfamily, family, subfamily)

for(t in 2:length(taxon.names[2:3])){
  curr.taxon = taxon.names[t]
  superfamilies <- SF.taxonomy %>% 
    filter(order == curr.taxon) %>% 
    distinct(superfamily) %>% 
    drop_na()
  
  superfamilies.pres <- curr.classifier %>% 
    filter(order == curr.taxon) %>% 
    distinct(superfamily) %>% 
    drop_na()
  
  families <- SF.taxonomy %>% 
    # filter(order == curr.taxon) %>% 
    distinct(family) %>% 
    drop_na()
  
  families.pres <- curr.classifier %>% 
    # filter(order == curr.taxon) %>% 
    distinct(family) %>% 
    drop_na()
  
  subfamilies <- SF.taxonomy %>% 
    # filter(order == curr.taxon) %>% 
    distinct(subfamily) %>% 
    drop_na()
  
  subfamilies.pres <- curr.classifier %>% 
    # filter(order == curr.taxon) %>% 
    distinct(subfamily) %>% 
    drop_na()
  
  genera <- SF.taxonomy %>% 
    filter(order == curr.taxon) %>% # comment for "all"
    distinct(genus) %>% 
    drop_na()
  
  genera.pres <- curr.classifier %>% 
    filter(order == curr.taxon) %>% # comment for "all"
    distinct(genus) %>% 
    drop_na()
  
  species <- SF.taxonomy %>% 
    filter(order == curr.taxon) %>% 
    distinct(species) %>% 
    drop_na()
  
  species.pres <- curr.classifier %>% 
    filter(order == curr.taxon)
  
}

writeClipboard(sort(unlist(genera[,1])))
writeClipboard(sort(unlist(genera.pres[,1])))

# zotero stuff
zotero.file <- "N:/PAPERS/PTR_Influence of the nymphal life history/R/Literature_list_assembly/2020-08-18 - Zotero_export.csv"
refs.zotero.all <- read_csv(zotero.file) 
refs.zotero <- refs.zotero.all %>% 
  # select relevant columns only
  select("Short Title", Author, Editor, "Publication Year", Title, "Publication Title", Volume, Issue, Pages, "Num Pages", 
         "Item Type", "Conference Name", "DOI", "Edition", Number, Place, Publisher, Series, "Series Title", 
         Url, Version, Type) %>%
  # replace italics starts and stops in Title column
  mutate(Title = gsub("<i>", "_", Title)) %>% 
  mutate(Title = gsub("</i>", "_", Title))

colnames(refs.zotero)[which(colnames(refs.zotero) == "Publication Year")] <- "Publication_Year"

refs.zotero.pages <- refs.zotero$Pages
refs.zotero.pages <- refs.zotero.pages[!is.na(refs.zotero.pages)]
pagenumbers <-c()
for(o in 1:length(refs.zotero.pages)){
  curr.pages <- refs.zotero.pages[o]
  curr.pages.num <- as.numeric(str_split(curr.pages, pattern  ="-")[[1]])
  if(length(curr.pages.num) > 1){
    curr.pagenumber <- curr.pages.num[2] - curr.pages.num[1]
  } else if (length(curr.pages.num) == 1){
    curr.pagenumber <- 1
  }
  pagenumbers[o] <- curr.pagenumber
}
sum(pagenumbers, na.rm = T)
