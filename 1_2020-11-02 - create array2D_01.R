# plot_cols = "order" # order family subfamily
# order_to_plot = c("Dermaptera", "Plecoptera") # all Dermaptera Plecoptera Blattodea Odonata
remove.specimens = F
analyse_eyes = F
analyse_antennae = F
recalculate_eco_struff = T
mirrored = F
orders_to_analyze = c("Dermaptera", "Plecoptera") # "Plecoptera", "Dermaptera", "Psocodea", "Zoraptera" "all"
# plot_alometry = T
# plot_LMs_3D = T

source("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/functions_PTR.R")


############## 1 #############
start.time <- Sys.time()
wd <- "//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/all_Checkpoint_files/" # test_add_eye_curve/
setwd(wd)

# load LM set for current subgroup <- use all possible landmarks because it makes more sense to filter after array_2D creation
if(mirrored == F){
  subgroup_landmarkset <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/all_possible_head_landmarks.csv", 
                                                header = F)$V1)
} else {
  subgroup_landmarkset <- as.character(read.csv("//blanke-nas-1/PROTOCOLS/_SCRIPTS/R/Peter/checkpoint_LMs/all_possible_head_landmarks_mirr.csv", 
                                                header = F)$V1)
}

# load all checkpoint files in wd
file_list <- list.files(wd, pattern = "*.ckpt$", recursive = F, full.names = T)

# load basetable
PBT <- load.PBT()
PBT_Derma_Pleco <- PBT[PBT$order=="Plecoptera" | PBT$order=="Dermaptera" & PBT$LMs != "-",]

ERCs_Derma_Pleco <- PBT_Derma_Pleco$ERC[complete.cases(PBT_Derma_Pleco$ERC)]

# find empty checkpoint files
no_LM_files <- find.empty.checkpoint.files(file.list = file_list)

# remove empty checkpoint files
file_list <- setdiff(file_list, no_LM_files)

# reduce to file names
file_names <- basename(file_list)

# get ERC numbers from filenames
ERC_numbers_all <- sub("^(\\d+)_.+$", "\\1", file_names)

# remove some taxa
if(remove.specimens){
  Hemimeridae <- c("1604", "1744", "0015", "0017", "1602", "1603")
  Arixeniidae <- c("1601", "0657", "1715", "1600", "1743")
  parasites <- c(Hemimeridae, Arixeniidae)
  list.positions.parasites <- which(ERC_numbers_all %in% parasites)
  ERC_numbers_all <- ERC_numbers_all[-list.positions.parasites]
  file_list <- file_list[-list.positions.parasites]
  PBT_Derma_Pleco <- PBT_Derma_Pleco %>% 
    filter(ERC %!in% parasites)
  message(paste0("removed ", paste(parasites, collapse = ", "), " from PBT and file_list."))
}

# filter out files that are new or have been modified
# old_file_infos <- read.csv("file_infos.csv")
# for(f in 1:length(file_list)){
#   if(f==1){
#     files_to_analyze <- c()
#   }
#   curr_mod_time <- file.info(file_list[f])[1,4]
#   curr_old_mod_time <- as.character(old_file_infos$mtime[old_file_infos$file %in% file_names[f]])
#   time_diff <- floor(as.numeric(difftime(curr_mod_time, curr_old_mod_time)))
#   
#   if(time_diff != 0){
#     files_to_analyze <- c(files_to_analyze, ERC_numbers_all[f])
#   }
# }

if(mirrored == F){
  landmarkset_list <- get.checkpoint.LMs(file_list = file_list, orders_to_analyze = orders_to_analyze, mirror = F)
} else {
  landmarkset_list <- get.checkpoint.LMs(file_list = file_list, orders_to_analyze = orders_to_analyze, mirror = T)
}

array_2D <- array.2D.from.LM.list(LM.list = landmarkset_list, LM.names = subgroup_landmarkset)

# check dims of array_2D
dim(array_2D)

# store a copy of array_2D to play with
array_2D_ <- array_2D
# array_2D <- array_2D_

# create interpolated equidistant eye type 3 LMs
if(analyse_eyes==T){
  # define the number of  type 3 LMs that should represent the occipital suture in the end
  no_eye_LMs = 20
  
  # define the number of interpolated points
  no_curve_LMs_before_thrsh = 1000 # 1000!
  
  # add columns to array_2D to house the equidistant occS LM x, y, z coords
  array_2D[, (ncol(array_2D)+1):(ncol(array_2D)+no_eye_LMs*3)] <- NA
  
  # add colnames to those columns
  LM.counter = 1
  for(o in seq((ncol(array_2D)-(no_eye_LMs*3)+1), (ncol(array_2D)), 3)){
    colnames(array_2D)[o] <- paste0("occS_shape_eq_", LM.counter, "_X")
    colnames(array_2D)[o+1] <- paste0("occS_shape_eq_", LM.counter, "_Y")
    colnames(array_2D)[o+2] <- paste0("occS_shape_eq_", LM.counter, "_Z")
    LM.counter = LM.counter+1
  }
  
  # go through every row of array_2D
  for(r in 1:nrow(array_2D)){
    show.progress(r, nrow(array_2D))
    
    # check if row contains occS landmarks and mandibular condyle
    if(!is.na(array_2D$occS_shape_1_X[r]) & !is.na(array_2D$mdcond_ant_X[r])){
      
      # get subset of array_2D that contains occ. suture data of current ERC
      curr.occS.coords <- array_2D[r, ] %>% 
        select(matches("occS_shape_\\d+")) %>% 
        t()
      
      # create nicer dataframe from the subset
      current_eye_LMs <- create_empty_df(nrow = nrow(curr.occS.coords)/3+1, names = c("x", "y", "z", "name", "count"))
      df.row.counter = 1
      for(t in seq(1, nrow(curr.occS.coords), 3)){
        current_eye_LMs$x[df.row.counter] <- curr.occS.coords[t,1]
        current_eye_LMs$y[df.row.counter] <- curr.occS.coords[t+1,1]
        current_eye_LMs$z[df.row.counter] <- curr.occS.coords[t+2,1]
        # current_eye_LMs$name[df.row.counter] <- paste0("occS_shape_", df.row.counter)
        df.row.counter <- df.row.counter+1
      }
      
      # take first row and copy it to be the last row as well to close the loop to prevent weird interpolation behaviour
      current_eye_LMs[nrow(current_eye_LMs),] <- current_eye_LMs[1,]
      
      # add LM names to name columne
      current_eye_LMs$name <- paste0("occS_shape_", 1:nrow(current_eye_LMs))
      
      # add numbers into coutner column
      current_eye_LMs$count <- 1:nrow(current_eye_LMs)
      
      # create sequence of numbers that range from 1 (= min(current_eye_LMs$count)) to ncol(current_eye_LMs) (= max(current_eye_LMs$count) with the length "no_curve_LMs_before_thrsh"
      current_eye_LMs_ts <- seq(from = min(current_eye_LMs$count), max(current_eye_LMs$count), length = no_curve_LMs_before_thrsh)
      
      # create interpolated curve along the curve LMs' coordinates
      current_eye_LMs_curve <- apply(current_eye_LMs[,-c(4,5)], 2, function(u) spline(current_eye_LMs$count, u, xout = current_eye_LMs_ts)$y)
      
      # get coordinates of mdcond_ant LM
      md_ant_coords <- array_2D[r, ] %>% 
        select(matches("frontS_X_coronS")) # !! here:! changed!
      
      # find closest eye-curve-point to anterior Md condyle
      for(l in 1:nrow(current_eye_LMs_curve)){
        # before first loop:
        if(l == 1){
          # set the current minimal distance to infinity
          min_dist = Inf
          
          # create empty list of distances
          dists <- c()
        }
        
        # get current curve interpolated point coordinates
        curr_curvepoint_coords <- current_eye_LMs_curve[l,]
        
        # calculate the distance between these coordinates and the mdcond_ant coordinates
        curr_dist <- dist(rbind(md_ant_coords,curr_curvepoint_coords))
        # print(paste0(l, " --> ", curr_dist))
        
        # add this value to the list of distances
        dists[l] <- curr_dist
        
        # check if current distance is smaller then the minimal distance that was found before
        if(curr_dist < min_dist){
          # save new minimal distance
          min_dist = as.numeric(curr_dist)
          # save row number of curve point with new minimal distance
          min_dist_row <- l
          # print(paste0("MIN: ", l, " --> ", min_dist, " (", min_dist_row, ")"))
        }
      }
      
      # sort eye-curve according to minimal distance
      current_eye_LMs_sorted <- rbind(current_eye_LMs_curve[min_dist_row:nrow(current_eye_LMs_curve),],
                                      current_eye_LMs_curve[1:(min_dist_row-1),])
      
      # create dataframe of curve landmarks to calculate the distances between them in order to decrease number of curve points
      sorted.LMs.curve.df <- data.frame(current_eye_LMs_sorted)      # sorted.LMs.curve.df[,"dist"] <- NA 
      
      # calculate all distances to previous curve LMs
      system.time({
        sorted.LMs.curve.df$dist <- 0 
        for(l in 2:nrow(sorted.LMs.curve.df)){
          sorted.LMs.curve.df$dist[l] <- dist(rbind(sorted.LMs.curve.df[(l-1),1:3], sorted.LMs.curve.df[l,1:3]))
        }
      })
      
      # calculate total distance between all curve points ~~ circumference of occipital suture
      current_total_eye_dist <- sum(sorted.LMs.curve.df$dist)
      
      # calculate the distance threshold to reduce curve points to "no_eye_LMs" equidistant points
      eye_threshold <- current_total_eye_dist/no_eye_LMs
      
      # print original LMs and curve in 3D
      # plot3d( current_eye_LMs_curve, type="l", lwd=5, col="navy" )
      # spheres3d(current_eye_LMs[,-c(4,5)], radius=4, col="orange")
      # spheres3d(md_ant_coords, radius=5, col="red")
      # distance_line <- segments3d(x=c(md_ant_coords[1,1],current_eye_LMs_sorted[1,1]),
      #                             y=c(md_ant_coords[1,2],current_eye_LMs_sorted[1,2]),
      #                             z=c(md_ant_coords[1,3],current_eye_LMs_sorted[1,3]))
      # aspect3d("iso")
      
      # reduce eye curve to "no_eye_LMs" LMs by checking if cumulative distance of neighboring curve points crossed eye_threshold-distance
      for(d in 1:nrow(sorted.LMs.curve.df)){
        if(d==1){
          curr_cumul_dist = 0
          row.numbers <- c(d)
          # spheres3d(current_eye_LMs_sorted[d,], radius=5, col="green")
        } else{
          curr_cumul_dist = curr_cumul_dist+sorted.LMs.curve.df$dist[d]
          
          if(curr_cumul_dist >= eye_threshold){
            # spheres3d(current_eye_LMs_sorted[d,], radius=5, col="green")
            row.numbers <- c(row.numbers, d)
            curr_cumul_dist = 0
          }
        }
      }
      
      # reduce sorted.LMs.curve.df to contain only the equidistant LMs
      eq.LMs.df <- sorted.LMs.curve.df[row.numbers, 1:3]
      
      # convert this df into a vector of all coords
      eq.LMs.vector <- as.vector(t(eq.LMs.df))
      
      # convert array_2D back into data.frame
      array_2D <- as.data.frame(array_2D)
      
      # insert this vector into the end of the current row of the array_2D
      array_2D[r, (ncol(array_2D)-(no_eye_LMs*3)+1):(ncol(array_2D))] <- eq.LMs.vector
    }
  }
}


# create interpolated equidistant antenna type 3 LMs
if(analyse_antennae==T){
  # define the number of  type 3 LMs that should represent the occipital suture in the end
  no.eq.LMs = 20
  
  # define the number of interpolated points
  no_curve_LMs_before_thrsh = 1000 # 1000!
  
  # add columns to array_2D to house the equidistant occS LM x, y, z coords
  array_2D[, (ncol(array_2D)+1):(ncol(array_2D)+no.eq.LMs*3)] <- NA
  
  # add colnames to those columns
  LM.counter = 1
  for(o in seq((ncol(array_2D)-(no.eq.LMs*3)+1), (ncol(array_2D)), 3)){
    colnames(array_2D)[o] <- paste0("antS_shape_eq_", LM.counter, "_X")
    colnames(array_2D)[o+1] <- paste0("antS_shape_eq_", LM.counter, "_Y")
    colnames(array_2D)[o+2] <- paste0("antS_shape_eq_", LM.counter, "_Z")
    LM.counter = LM.counter+1
  }
  
  # go through every row of array_2D
  for(r in 1:nrow(array_2D)){
    show.progress(r, nrow(array_2D))
    
    # check if row contains antennal landmarks and mandibular condyle LMs
    if(!is.na(array_2D$antS_shape_1_X[r]) & !is.na(array_2D$mdcond_ant_X[r])){
      
      # get subset of array_2D that contains occ. suture data of current ERC
      curr.shape.coords <- array_2D[r, ] %>% 
        select(matches("antS_shape_\\d+")) %>% 
        t()
      
      # create nicer dataframe from the subset
      current.LMs.df <- create_empty_df(nrow = nrow(curr.shape.coords)/3+1, names = c("x", "y", "z", "name", "count"))
      df.row.counter = 1
      for(t in seq(1, nrow(curr.shape.coords), 3)){
        current.LMs.df$x[df.row.counter] <- curr.shape.coords[t,1]
        current.LMs.df$y[df.row.counter] <- curr.shape.coords[t+1,1]
        current.LMs.df$z[df.row.counter] <- curr.shape.coords[t+2,1]
        # current.LMs.df$name[df.row.counter] <- paste0("antS_shape_", df.row.counter)
        df.row.counter <- df.row.counter+1
      }
      
      # take first row and copy it to be the last row as well to close the loop to prevent weird interpolation behaviour
      current.LMs.df[nrow(current.LMs.df),] <- current.LMs.df[1,]
      
      # add LM names to name columne
      current.LMs.df$name <- paste0("antS_shape_", 1:nrow(current.LMs.df))
      
      # add numbers into coutner column
      current.LMs.df$count <- 1:nrow(current.LMs.df)
      
      # create sequence of numbers that range from 1 (= min(current.LMs.df$count)) to ncol(current.LMs.df) (= max(current.LMs.df$count) with the length "no_curve_LMs_before_thrsh"
      current.LMs.df.ts <- seq(from = min(current.LMs.df$count), max(current.LMs.df$count), length = no_curve_LMs_before_thrsh)
      
      # create interpolated curve along the curve LMs' coordinates
      current.LMs.df.curve <- apply(current.LMs.df[,-c(4,5)], 2, function(u) spline(current.LMs.df$count, u, xout = current.LMs.df.ts)$y)
      
      # get coordinates of mdcond_ant LM
      md_ant_coords <- array_2D[r, ] %>% 
        select(matches("mdcond_ant"))
      
      # find closest eye-curve-point to anterior Md condyle
      for(l in 1:nrow(current.LMs.df.curve)){
        # before first loop:
        if(l == 1){
          # set the current minimal distance to infinity
          min_dist = Inf
          
          # create empty list of distances
          dists <- c()
        }
        
        # get current curve interpolated point coordinates
        curr_curvepoint_coords <- current.LMs.df.curve[l,]
        
        # calculate the distance between these coordinates and the mdcond_ant coordinates
        curr_dist <- dist(rbind(md_ant_coords,curr_curvepoint_coords))
        # print(paste0(l, " --> ", curr_dist))
        
        # add this value to the list of distances
        dists <- c(dists, curr_dist)
        
        # check if current distance is smaller then the minimal distance that was found before
        if(curr_dist < min_dist){
          # save new minimal distance
          min_dist = as.numeric(curr_dist)
          # save row number of curve point with new minimal distance
          min_dist_row <- l
          # print(paste0("MIN: ", l, " --> ", min_dist, " (", min_dist_row, ")"))
        }
      }
      # sort eye-curve according to minimal distance
      current.LMs.df.sorted <- rbind(current.LMs.df.curve[min_dist_row:nrow(current.LMs.df.curve),],
                                     current.LMs.df.curve[1:(min_dist_row-1),])
      
      # create dataframe of curve landmarks to calculate the distances between them in order to decrease number of curve points
      sorted.LMs.curve.df <- data.frame(current.LMs.df.sorted)
      
      # add NA column with name "dist"
      sorted.LMs.curve.df[,"dist"] <- NA
      sorted.LMs.curve.df$dist <- 0
      
      # calculate all distances to previous curve LMs
      for(l in 2:nrow(sorted.LMs.curve.df)){
        sorted.LMs.curve.df$dist[l] <- dist(rbind(sorted.LMs.curve.df[(l-1),1:3], sorted.LMs.curve.df[l,1:3]))
      }
      
      # calculate total distance between all curve points ~~ circumference of occipital suture
      current_total_eye_dist <- sum(sorted.LMs.curve.df$dist)
      
      # calculate the distance threshold to reduce curve points to "no.eq.LMs" equidistant points
      eye_threshold <- current_total_eye_dist/no.eq.LMs
      
      # print original LMs and curve in 3D
      # plot3d( current.LMs.df.curve, type="l", lwd=5, col="navy" )
      # spheres3d(current.LMs_df[,-c(4,5)], radius=4, col="orange")
      # spheres3d(md_ant_coords, radius=5, col="red")
      # distance_line <- segments3d(x=c(md_ant_coords[1,1],current.LMs.df.sorted[1,1]),
      #                             y=c(md_ant_coords[1,2],current.LMs.df.sorted[1,2]),
      #                             z=c(md_ant_coords[1,3],current.LMs.df.sorted[1,3]))
      # aspect3d("iso")
      
      # reduce eye curve to "no.eq.LMs" LMs by checking if cumulative distance of neighboring curve points crossed eye_threshold-distance
      for(d in 1:nrow(sorted.LMs.curve.df)){
        if(d==1){
          curr_cumul_dist = 0
          row.numbers <- c(d)
          # spheres3d(current.LMs.df.sorted[d,], radius=5, col="green")
        } else{
          curr_cumul_dist = curr_cumul_dist+sorted.LMs.curve.df$dist[d]
          
          if(curr_cumul_dist >= eye_threshold){
            # spheres3d(current.LMs.df.sorted[d,], radius=5, col="green")
            row.numbers <- c(row.numbers, d)
            curr_cumul_dist = 0
          }
        }
      }
      
      # reduce sorted.LMs.curve.df to contain only the equidistant LMs
      eq.LMs.df <- sorted.LMs.curve.df[row.numbers, 1:3]
      
      # convert this df into a vector of all coords
      eq.LMs.vector <- as.vector(t(eq.LMs.df))
      
      # convert array_2D back into data.frame
      array_2D <- as.data.frame(array_2D)
      
      # insert eq.LMs.vector into the end of the current row of the array_2D      
      array_2D[r, (ncol(array_2D)-(no.eq.LMs*3)+1):(ncol(array_2D))] <- eq.LMs.vector
    }
  }
}  

# save a current version of the array_2D to play with
array_2D_save1 <- array_2D

# put ERC numbers back as rownames and delete ERC column
array_2D <- as.data.frame(array_2D)
rownames(array_2D) <- array_2D$ERC
array_2D$ERC <- NULL

# save a current version of the array_2D to play with
array_2D_save2 <- array_2D








# # create 2D array to check which landmarks are present: delete all y- and z-coord. columns
# array_2D.check <- array_2D[, seq(1,ncol(array_2D),3)]
# 
# # delete columns so that curve landmarks are only represented by one the x-coord. column
# for (i in 2:28) {
#   delete.col <- paste0("antS_shape_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# for (i in 2:35) {
#   delete.col <- paste0("occS_shape_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# for (i in 2:10) {
#   delete.col <- paste0("clypeus_shape_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# for (i in 2:20) {
#   delete.col <- paste0("frons_shape_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# for (i in 2:20) {
#   delete.col <- paste0("vertex_shape_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# for (i in 2:20) {
#   delete.col <- paste0("occS_shape_eq_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
#   delete.col <- paste0("antS_shape_eq_", i, "_X")
#   array_2D.check <- array_2D.check[, -which(names(array_2D.check) %in% delete.col)]
# }
# 
# # create array 2D to easily identify missing LMs: replace all non-NAs with x
# array_2D_x <- array_2D.check
# array_2D_x[!is.na(array_2D_x)] <- "x"
# 
# write.xlsx2(as.data.frame(array_2D_x), paste0(today(), "_array.2D.LMs.x.xlsx"), row.names = T)

# # clean array from NAs (each LANDMARK with a single NA will be deleted)
# completed_array_2D_max_specimen <- array_2D[,colMeans(is.na(array_2D)) == 0]  # use only landmarks without NAs
# 
# # clean array from NAs (each SPECIMEN with a single NA will be deleted)
# completed_array_2D_max_LMs<- array_2D[rowMeans(is.na(array_2D)) == 0,]  # use only landmarks without NAs
# 


if(remove.specimens){
  write.xlsx2(as.data.frame(array_2D), paste0(today(), "_array.2D.LMs_filtered.xlsx"), row.names = T)
  message(paste0("all done! Copy data from ", paste0(today(), "_array.2D.LMs_filtered.xlsx"), " into sheet tab 'array.2D.LM."))
} else {
  write.xlsx2(as.data.frame(array_2D), paste0(today(), "_array.2D.LMs.xlsx"), row.names = T)
  message(paste0("all done! Copy data from ", paste0(today(), "_array.2D.LMs.xlsx"), " into sheet tab 'array.2D.LM."))
  
}




##### 2 #####

if (recalculate_eco_struff == T){
  ###### load species file taxonomy #####
  SF.taxonomy <- load.SF.taxonomy() %>% 
    filter(order == "Dermaptera" | order == "Plecoptera") %>% 
    mutate(ID = paste0(genus, "_", species))
  
  ###### load PBT #####
  PBT <- load.PBT()
  
  # filter Dermas and Plecos that are in array.2D; create IDs (also for GBFIf tax.infos); select columns of interest only
  PBT.Derma.Pleco.LMs <- PBT %>% 
    filter(order == "Dermaptera" | order == "Plecoptera") %>% 
    # filter(LMs == "x" | LMs == "a" | LMs ==  "g" | LMs ==  "gg" | LMs == "x (in Public)") %>% 
    mutate(ID = paste0(genus_GBIF, "_", species_GBIF)) %>% 
    # mutate(ID_GBIF = paste0(genus_GBIF, "_", species_GBIF)) %>% 
    dplyr::select(ERC, genus_GBIF, species_GBIF, LMs, ID) %>%#genus, species, order, suborder, infraorder, superfamily, family, subfamily, 
    left_join(select(SF.taxonomy, order, suborder, infraorder, superfamily, family, subfamily, genus, species), 
              by = c("genus_GBIF" = "genus", "species_GBIF" = "species"))
  
  # add taxon infos for species that have no species name (e.g. Kamimura sp.)
  for (i in which(is.na(PBT.Derma.Pleco.LMs$order))){
    curr.PBT.row <- which(SF.taxonomy$genus == PBT.Derma.Pleco.LMs$genus_GBIF[i])[1]
    PBT.Derma.Pleco.LMs$order[i] <- SF.taxonomy$order[curr.PBT.row]
    PBT.Derma.Pleco.LMs$suborder[i] <- SF.taxonomy$suborder[curr.PBT.row]
    PBT.Derma.Pleco.LMs$infraorder[i] <- SF.taxonomy$infraorder[curr.PBT.row]
    PBT.Derma.Pleco.LMs$superfamily[i] <- SF.taxonomy$superfamily[curr.PBT.row]
    PBT.Derma.Pleco.LMs$family[i] <- SF.taxonomy$family[curr.PBT.row]
    PBT.Derma.Pleco.LMs$subfamily[i] <- SF.taxonomy$subfamily[curr.PBT.row]
  }
  
  # delete rows that contain NA
  PBT.Derma.Pleco.LMs <- delete.rows.with.na.in.column(PBT.Derma.Pleco.LMs, 6) # col 6 = order
  
  # remove lines from PBT that contain data on non-landmarked species
  PBT.Derma.Pleco.LMs <- PBT.Derma.Pleco.LMs %>% 
    filter(ERC %in% rownames(array_2D))
  
  # remove 'GBIF' from colnames
  colnames(PBT.Derma.Pleco.LMs)[2:3] <- c("genus", "species")
  
  ###### load CHW #####
  CWH <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1aP6E7rKyUBn7WhX_rQLDXlFFS0egGN3tFkeCUeQa2yo#gid=0')}))
  # delete first four columns which contain taxonomic infos
  CWH <- CWH[, -c(1:4)]
  # use second row elements as colnames
  colnames(CWH) <- CWH[1,]
  # change first to column names to genus and species
  colnames(CWH)[1:2] <- c("genus", "species")
  # delete first row; only use certain columns
  CWH <- CWH[-1,1:29]
  # change data column types to double and replace NAs with 0
  CWH <- CWH %>% 
    mutate_at(vars(current_fast,current_moderate,current_slow,current_none,
                   aquatic_on_under_stones_logs, aquatic_in_sand_mud_detritus, aquatic_on_plants_incl_moss, 
                   on_stones_bark_plants_ice, in_leaf_litter, under_stones_bark_etc, epizoic, detritus,
                   macropterous, brachypterous, micropterous, apterous, yes, 
                   yes_no_further_infos, agile, marginal, flightless), funs(as.double)) %>% 
    mutate_at(vars(current_fast,current_moderate,current_slow,current_none,
                   aquatic_on_under_stones_logs, aquatic_in_sand_mud_detritus, aquatic_on_plants_incl_moss, on_stones_bark_plants_ice, 
                   in_leaf_litter, under_stones_bark_etc, epizoic, detritus,
                   macropterous, brachypterous, micropterous, apterous, yes,
                   yes_no_further_infos, agile, marginal, flightless), ~replace_na(., 0)) %>% 
    mutate(ID = paste0(genus, "_", species))
  
  # remove lines from CWH that contain data on non-landmarked species
  CWH <- CWH %>% 
    filter(ID %in% PBT.Derma.Pleco.LMs$ID)
  
  for(l in 1:nrow(CWH)){
    # fuse micropterous and brachypterous into brachypterous
    if(!is.na(CWH$micropterous[l])){
      CWH$brachypterous[l] <- sum(CWH$brachypterous[l], CWH$micropterous[l], na.rm = T)
    }
    # fuse micropterous and apterous into apterous
    if(!is.na(CWH$brachypterous[l])){
      CWH$apterous[l] <- sum(CWH$apterous[l], CWH$brachypterous[l], na.rm = T)
    }
    # fuse yes_no_further_infos and agile into agile
    if(!is.na(CWH$yes_no_further_infos[l])){
      CWH$agile[l] <- sum(CWH$yes_no_further_infos[l], CWH$agile[l], na.rm = T)
    } 
    # fuse marginal and agile into agile
    if(!is.na(CWH$marginal[l])){
      CWH$agile[l] <- sum(CWH$marginal[l], CWH$agile[l], na.rm = T)
    }
    # fuse aquatic_on_under_stones_logs and under_stones_bark_etc into under_stones_bark_etc
    if(!is.na(CWH$aquatic_on_under_stones_logs[l])){
      CWH$under_stones_bark_etc[l] <- sum(CWH$aquatic_on_under_stones_logs[l], CWH$under_stones_bark_etc[l], na.rm = T)
    }
    # fuse aquatic_in_sand_mud_detritus and detritus into detritus
    if(!is.na(CWH$aquatic_in_sand_mud_detritus[l])){
      CWH$detritus[l] <- sum(CWH$aquatic_in_sand_mud_detritus[l], CWH$detritus[l], na.rm = T)
    }
    # fuse aquatic_on_plants_incl_moss and on_stones_bark_plants_ice into on_stones_bark_plants_ice
    if(!is.na(CWH$aquatic_on_plants_incl_moss[l])){
      CWH$on_stones_bark_plants_ice[l] <- sum(CWH$aquatic_on_plants_incl_moss[l], CWH$on_stones_bark_plants_ice[l], na.rm = T)
    }
    # fuse current_fast and current_moderate into current_fast
    if(!is.na(CWH$aquatic_on_plants_incl_moss[l])){
      CWH$current_fast [l] <- sum(CWH$current_moderate[l], CWH$current_fast [l], na.rm = T)
    }
    # fuse in_leaf_litter and detritus into detritus
    if(!is.na(CWH$in_leaf_litter[l])){
      CWH$detritus [l] <- sum(CWH$in_leaf_litter[l], CWH$detritus [l], na.rm = T)
    }
    show.progress(l, nrow(CWH))
  }
  
  # add discrete lines for l and a where d is value of larva_adult
  for(l in 1:nrow(CWH)){
    curr.larva_adult <- CWH$larva_adult[l]
    if (curr.larva_adult == "b"){
      # print("yo")
      CWH$larva_adult[l] <- "l"
      CWH[(nrow(CWH)+1), ] <- CWH[l, ]
      CWH$larva_adult[nrow(CWH)] <- "a"
    }
    show.progress(l, nrow(CWH))
  }
  
  # extract species level data only
  CWH.species <- CWH %>% 
    filter(taxonrank == "species") %>% 
    mutate(ID = paste0(genus, "_", species))
  
  # get taxon infos (order, family) from PBT into CWH; extract the species that have landmarks as well and discard all other rows
  CWH.species <- CWH.species %>% 
    left_join(select(PBT.Derma.Pleco.LMs, order, family, ID), by = c("ID" = "ID")) %>% 
    filter(!is.na(order))
  
  
  ###### load WHW #####
  WHW <- as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1XLVK4xOpIwXsZkxIIWEX8pnkTTHS7V8pHhwTwopqPb8#gid=0')}))
  # delete first four columns which contain taxonomic infos
  WHW <- WHW[, -c(1:4)]
  # use second row elements as colnames
  colnames(WHW) <- WHW[1,]
  # change first to column names to genus and species
  colnames(WHW)[1:2] <- c("genus", "species")
  # delete first row; only use certain columns
  WHW <- WHW[-1,1:28]
  # change data column types to double
  WHW <- WHW %>% 
    mutate_at(vars(shredders, scrapers_grazers, collector_browser, collector_scraper, predator, wood_gouger, filter_feeders, 
                   fruitivory, fungivory, phytophagy, herbivory, pollen, 
                   predatory, scavenger, parasitoid_endop, ectoparasitic, diatoms, algae, 
                   lichens, detritivoric, omnivore, yes_unspecified, non_feeding), funs(as.double)) %>% 
    mutate(ID = paste0(genus, "_", species))
  
  # remove lines from CWH that contain data on non-landmarked species
  WHW <- WHW %>% 
    filter(ID %in% PBT.Derma.Pleco.LMs$ID)
  
  # delete rows with food: yes_unspecified
  WHW <- WHW %>% 
    filter(is.na(yes_unspecified))
  
  # fuse/change columns 
  for(l in 1:nrow(WHW)){
    # wood_gauger and shredders into shredders
    if(!is.na(WHW$wood_gouger[l])){
      WHW$shredders[l] <- sum(WHW$shredders[l], WHW$wood_gouger[l], na.rm = T)
    }
    # collector_browser and shredders into shredders
    if(!is.na(WHW$collector_browser[l])){
      WHW$shredders[l] <- sum(WHW$shredders[l], WHW$collector_browser[l], na.rm = T)
    }
    # collector_scraper and scrapers_grazers to scrapers_grazers
    if(!is.na(WHW$collector_scraper[l])){
      WHW$scrapers_grazers[l] <- sum(WHW$scrapers_grazers[l], WHW$collector_scraper[l], na.rm = T)
    }
    # # phytophagy and herbivory to herbivory
    # if(!is.na(WHW$phytophagy[l])){
    #   WHW$herbivory[l] <- sum(WHW$herbivory[l], WHW$phytophagy[l], na.rm = T)
    # }
    # # fruitivory and herbivory to herbivory
    # if(!is.na(WHW$fruitivory[l])){
    #   WHW$herbivory[l] <- sum(WHW$herbivory[l], WHW$fruitivory[l], na.rm = T)
    # }
    # # pollen and herbivory to herbivory
    # if(!is.na(WHW$pollen[l])){
    #   WHW$herbivory[l] <- sum(WHW$herbivory[l], WHW$pollen[l], na.rm = T)
    # }
    
    # pollen and herbivory to herbivory
    if(!is.na(WHW$fruitivory[l]) | !is.na(WHW$fungivory[l]) | !is.na(WHW$phytophagy[l]) | !is.na(WHW$pollen[l]) | 
       !is.na(WHW$diatoms[l]) | !is.na(WHW$algae[l]) | !is.na(WHW$lichens[l])){
      WHW$herbivory[l] <- sum(WHW$herbivory[l], 
                              WHW$fruitivory[l], WHW$fungivory[l], WHW$phytophagy[l], WHW$pollen[l], WHW$diatoms[l], WHW$algae[l],WHW$lichens[l],
                              na.rm = T)
    }
    show.progress(l, nrow(WHW))
  }
  
  # add discrete lines for l and a where d is value of larva_adult
  for(l in 1:nrow(WHW)){
    curr.larva_adult <- WHW$larva_adult[l]
    if (curr.larva_adult == "b"){
      # print("yo")
      WHW$larva_adult[l] <- "l"
      WHW[(nrow(WHW)+1), ] <- WHW[l, ]
      WHW$larva_adult[nrow(WHW)] <- "a"
    }
    show.progress(l, nrow(WHW))
  }
  
  # extract species level data only
  WHW.species <- WHW %>% 
    filter(taxonrank == "species") %>% 
    mutate(ID = paste0(genus, "_", species))
  
  # get taxon infos (order, family) from PBT into WHW; extract the species that have landmarks as well and discard all other rows
  WHW.species <- WHW.species %>% 
    left_join(select(PBT.Derma.Pleco.LMs, order, family, ID), by = c("ID" = "ID")) %>% 
    filter(!is.na(order))
  
  
  
  ###### Analysis CHW #####
  # calculate the percentage sums for each microhabitat item for each species in larval and in adult stage
  microhabitat <-  CWH.species %>% 
    filter(type == "m") %>% 
    select(ID, larva_adult, on_stones_bark_plants_ice, under_stones_bark_etc, epizoic, detritus) %>% 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.on_stones_bark_plants_ice = sum(on_stones_bark_plants_ice, na.rm = T)*100/sum(on_stones_bark_plants_ice, under_stones_bark_etc, 
                                                                                             epizoic, detritus, na.rm = T)) %>%
    mutate(sum.under_stones_bark_etc = sum(under_stones_bark_etc, na.rm = T)*100/sum(on_stones_bark_plants_ice, under_stones_bark_etc, 
                                                                                     epizoic, detritus, na.rm = T)) %>%
    mutate(sum.epizoic = sum(epizoic, na.rm = T)*100/sum(on_stones_bark_plants_ice, under_stones_bark_etc, 
                                                         epizoic, detritus, na.rm = T)) %>%
    mutate(sum.detritus = sum(detritus, na.rm = T)*100/sum(on_stones_bark_plants_ice, under_stones_bark_etc, 
                                                           epizoic, detritus, na.rm = T)) %>%
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.on_stones_bark_plants_ice, 
           sum.under_stones_bark_etc, sum.epizoic, sum.detritus, ID) %>%
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    distinct()
  
  # calculate the percentage sums for each current item for each species in larval and in adult stage
  current <-  CWH.species %>% 
    filter(type == "c" & larva_adult == "l") %>% 
    select(ID, larva_adult, current_fast, current_slow, current_none) %>% 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.current_fast = sum(current_fast, na.rm = T)*100/sum(current_fast, current_slow, current_none, na.rm = T)) %>% 
    mutate(sum.current_slow = sum(current_slow, na.rm = T)*100/sum(current_fast, current_slow, current_none, na.rm = T)) %>% 
    mutate(sum.current_none = sum(current_none, na.rm = T)*100/sum(current_fast, current_slow, current_none, na.rm = T)) %>% 
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.current_fast, sum.current_slow, sum.current_none, ID) %>%
    mutate_all(~replace(., is.nan(.), NA))  %>% 
    distinct()
  
  # calculate the percentage sums for each wing item for each species in larval and in adult stage
  # wing <-  CWH.species %>% 
  #   filter(type == "w" & larva_adult == "a") %>% 
  #   select(ID, larva_adult, macropterous, brachypterous, apterous, yes) %>% 
  #   group_by(ID, larva_adult)  %>% 
  #   summarise_all(funs(sum(., na.rm = T))) %>% 
  #   group_by(ID, larva_adult)  %>%
  #   mutate(sum.macropterous = sum(macropterous, na.rm = T)*100/sum(macropterous, brachypterous, apterous, na.rm = T)) %>% 
  #   mutate(sum.brachypterous = sum(brachypterous, na.rm = T)*100/sum(macropterous, brachypterous, apterous, na.rm = T)) %>% 
  #   mutate(sum.apterous = sum(apterous, na.rm = T)*100/sum(macropterous, brachypterous, apterous, na.rm = T)) %>% 
  #   left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
  #   select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.macropterous, sum.brachypterous, sum.apterous, ID) %>%
  #   mutate_all(~replace(., is.nan(.), NA)) %>% 
  #   distinct()
  wing <-  CWH.species %>% 
    filter(type == "w" & larva_adult == "a") %>% 
    select(ID, larva_adult, macropterous, apterous) %>% 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.macropterous = sum(macropterous, na.rm = T)*100/sum(macropterous, apterous, na.rm = T)) %>% 
    mutate(sum.apterous = sum(apterous, na.rm = T)*100/sum(macropterous, apterous, na.rm = T)) %>% 
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.macropterous, sum.apterous, ID) %>%
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    distinct()
  
  # calculate the percentage sums for each flight item for each species in larval and in adult stage
  flight <-  CWH.species %>% 
    filter(type == "f" & larva_adult == "a") %>% 
    select(ID, larva_adult, agile, flightless) %>% 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.agile = sum(agile, na.rm = T)*100/sum(agile, flightless, na.rm = T)) %>% 
    mutate(sum.flightless = sum(flightless, na.rm = T)*100/sum(agile, flightless, na.rm = T)) %>% 
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.agile, sum.flightless, ID) %>%
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    distinct()
  
  
  ###### Analysis WHW #####
  # calculate the percentage sums for each feeding type item for each species in larval and in adult stage
  feeding_type <-  WHW.species %>% 
    filter(type == "p" & larva_adult == "l") %>% 
    select(ID, larva_adult, shredders, scrapers_grazers, predator, filter_feeders) %>% 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.shredders = sum(shredders, na.rm = T)*100/sum(shredders, scrapers_grazers, predator, 
                                                             filter_feeders, na.rm = T)) %>% 
    mutate(sum.scrapers_grazers = sum(scrapers_grazers, na.rm = T)*100/sum(shredders, scrapers_grazers, predator, 
                                                                           filter_feeders, na.rm = T)) %>% 
    mutate(sum.predator = sum(predator, na.rm = T)*100/sum(shredders, scrapers_grazers, predator, 
                                                           filter_feeders, na.rm = T)) %>% 
    mutate(sum.filter_feeders = sum(filter_feeders, na.rm = T)*100/sum(shredders, scrapers_grazers, predator, 
                                                                       filter_feeders, na.rm = T)) %>% 
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, sum.shredders, sum.scrapers_grazers, 
           sum.predator, sum.filter_feeders, ID) %>%
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    distinct()
  
  # calculate the percentage sums for each food item for each species in larval and in adult stage
  food <-  WHW.species %>% 
    filter(type == "g") %>% 
    select(ID, larva_adult, herbivory, 
           predatory, ectoparasitic, scavenger, detritivoric, non_feeding) %>%  # omnivore, yes_unspecified, phytophagy, 
    group_by(ID, larva_adult)  %>% 
    summarise_all(funs(sum(., na.rm = T))) %>% 
    group_by(ID, larva_adult)  %>%
    mutate(sum.herbivory = sum(herbivory, na.rm = T)*100/sum(herbivory, predatory,
                                                             ectoparasitic, scavenger, detritivoric, 
                                                             non_feeding, na.rm = T)) %>% 
    mutate(sum.predatory = sum(predatory, na.rm = T)*100/sum(herbivory, predatory,
                                                             ectoparasitic, scavenger, detritivoric, 
                                                             non_feeding, na.rm = T)) %>% 
    mutate(sum.ectoparasitic = sum(ectoparasitic, na.rm = T)*100/sum(herbivory, predatory,
                                                                     ectoparasitic, scavenger, detritivoric, 
                                                                     non_feeding, na.rm = T)) %>% 
    mutate(sum.scavenger = sum(scavenger, na.rm = T)*100/sum(herbivory, predatory,
                                                             ectoparasitic, scavenger, detritivoric, 
                                                             non_feeding, na.rm = T)) %>% 
    mutate(sum.detritivoric = sum(detritivoric, na.rm = T)*100/sum(herbivory, predatory,
                                                                   ectoparasitic, scavenger, detritivoric, 
                                                                   non_feeding, na.rm = T)) %>% 
    mutate(sum.non_feeding = sum(non_feeding, na.rm = T)*100/sum(herbivory, predatory,
                                                                 ectoparasitic, scavenger, detritivoric, 
                                                                 non_feeding, na.rm = T)) %>% 
    # mutate(sum.omnivore = sum(omnivore, na.rm = T)*100/sum(fungivory, phytophagy, herbivory, pollen, predatory, lignivorous,
    #                                                        ectoparasitic, scavenger, diatoms, algae, lichens, detritivoric, 
    #                                                        non_feeding, omnivore, yes_unspecified, na.rm = T)) %>% 
    left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID), by = "ID") %>% 
    select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult, 
           sum.herbivory, sum.predatory, sum.ectoparasitic, sum.scavenger, sum.detritivoric, 
           sum.non_feeding, ID) %>% # , sum.omnivore
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    distinct()
  
  
  ##### create separated tibbles for larvae and adults so that they can have separate columns names for the huge 2D df ####
  current.l <- current %>%
    filter(larva_adult == "l") %>% 
    ungroup()
  colnames(current.l)[10:ncol(current.l)-1] <- paste0(colnames(current.l)[10:ncol(current.l)-1], ".l")
  
  
  feeding_type.l <- feeding_type %>%
    filter(larva_adult == "l")  %>% 
    ungroup()
  colnames(feeding_type.l)[10:ncol(feeding_type.l)-1] <- paste0(colnames(feeding_type.l)[10:ncol(feeding_type.l)-1], ".l")
  
  
  food.l <- food %>%
    filter(larva_adult == "l")  %>% 
    ungroup()
  colnames(food.l)[10:ncol(food.l)-1] <- paste0(colnames(food.l)[10:ncol(food.l)-1], ".l")
  
  microhabitat.l <- microhabitat %>%
    filter(larva_adult == "l")  %>% 
    ungroup()
  colnames(microhabitat.l)[10:ncol(microhabitat.l)-1] <- paste0(colnames(microhabitat.l)[10:ncol(microhabitat.l)-1], ".l")
  
  flight.a <- flight %>%
    filter(larva_adult == "a")  %>% 
    ungroup()
  # flight.a$sum.both <- 0
  for(l in 1:nrow(flight.a)){
    if(flight.a$sum.agile[l] != 0 & flight.a$sum.flightless[l] != 0){
      # flight.a$sum.both[l] <- 100
      flight.a$sum.agile[l] <- 100 # 0
      flight.a$sum.flightless[l] <- 0
    }
    else if(flight.a$sum.agile[l] != 0 & flight.a$sum.flightless[l] == 0){
      flight.a$sum.agile[l] <- 100
    }
    else if(flight.a$sum.agile[l] == 0 & flight.a$sum.flightless[l] != 0){
      flight.a$sum.flightless[l] <- 100
    }
  }
  # flight.a <- flight.a[, colnames(flight.a[c(1:(ncol(flight.a)-2), ncol(flight.a), ncol(flight.a)-1)])]
  colnames(flight.a)[10:ncol(flight.a)-1] <- paste0(colnames(flight.a)[10:ncol(flight.a)-1], ".a")
  
  food.a <- food %>%
    filter(larva_adult == "a")  %>% 
    ungroup()
  colnames(food.a)[10:ncol(food.a)-1] <- paste0(colnames(food.a)[10:ncol(food.a)-1], ".a")
  
  microhabitat.a <- microhabitat %>%
    filter(larva_adult == "a")  %>% 
    ungroup()
  colnames(microhabitat.a)[10:ncol(microhabitat.a)-1] <- paste0(colnames(microhabitat.a)[10:ncol(microhabitat.a)-1], ".a")
  
  wing.a <- wing %>%
    filter(larva_adult == "a")  %>% 
    ungroup()
  colnames(wing.a)[10:ncol(wing.a)-1] <- paste0(colnames(wing.a)[10:ncol(wing.a)-1], ".a")
  
  
  ##### create one single 2D dataframe for all analyses to follow
  array.2D.full <- PBT.Derma.Pleco.LMs %>%
    left_join(., select(current.l, colnames(current.l[10:ncol(current.l)])), by = "ID") %>%
    left_join(., select(feeding_type.l, colnames(feeding_type.l[9:ncol(feeding_type.l)])), by = "ID") %>%
    left_join(., select(food.l, colnames(food.l[9:ncol(food.l)])), by = "ID") %>%
    left_join(., select(food.a, colnames(food.a[9:ncol(food.a)])), by = "ID") %>%
    left_join(., select(flight.a, colnames(flight.a[9:ncol(flight.a)])), by = "ID") %>%
    left_join(., select(microhabitat.l, colnames(microhabitat.l[9:ncol(microhabitat.l)])), by = "ID") %>%
    left_join(., select(microhabitat.a, colnames(microhabitat.a[9:ncol(microhabitat.a)])), by = "ID") %>%
    left_join(., select(wing, colnames(wing[9:ncol(wing)])), by = "ID")
  
  # replace all zeros with NA
  array.2D.full[array.2D.full == 0] <- NA
  
  
  ### calculate pcoas for ecological data and create stacked plots PDF
  # little function to plot stacked bar plots for current subsets
  plot.stacked.bar <- function(df){
    plot.df <- df %>%
      select(ID, 10:(ncol(df)-1))
    
    plot.df.long <- gather(data = plot.df, key = attribute, value = percentage, 2:ncol(plot.df))
    
    
    print(
      ggplot(data = plot.df.long, aes(fill=attribute, y = percentage, x = ID))+
        geom_bar(position = "stack", stat = "identity") +
        coord_flip()
    )
  }
  
  # create list of tibbles with ecological data for stack plotting and PCoA calculation
  ecology.cats <- list(current.l, food.l, microhabitat.l,
                       food.a, microhabitat.a) # , flight.a, wing.a
  ecology.cat.names <- list("current.l",  "food.l", "microhabitat.l",
                            "food.a", "microhabitat.a") # , "flight.a", "wing.a"
  
  # go through all ecological category tibbles, stack plot into PDF and calculate PCoA
  pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/R/", gsub("_", "-", today()), "_stacked.pdf"), height = 20)
  # pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/R/", gsub("_", "-", today()), "__PCoAs.pdf"), paper = "a4")
  # par(mfrow=c(2,2), mar=c(2, 2, 1, 1))
  # par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
  taxon.names <- c("all", "Dermaptera", "Plecoptera")
  for(t in 1:length(taxon.names)){ # :length(taxon.names)
    for(e in 1:length(ecology.cats)){
      # define taxon
      curr.taxon.name <- taxon.names[t]
      message(curr.taxon.name)
      curr.eco.cat <- ecology.cat.names[e]
      message(curr.eco.cat)
      
      # skip Dermaptera current
      if((curr.eco.cat == "current.l" & curr.taxon.name == "Dermaptera") | (curr.eco.cat == "feeding_type.l"  & curr.taxon.name == "Dermaptera")){
        message(paste0("skipping ", curr.eco.cat, " for Dermaptera..."))
      } else {
        # get current ecology tibble
        curr.df <- ecology.cats[[e]]
        
        # get only curr.taxon data from curr.df
        if(curr.taxon.name != "all"){
          curr.df <- curr.df %>%
            filter(order == curr.taxon.name)
        }
        # stack plot into PDF
        plot.stacked.bar(curr.df)
        
        if(ecology.cat.names[e] != "wing.a" & ecology.cat.names[e] != "flight.a" & ecology.cat.names[e] != "Csize.occ" & ecology.cat.names[e] != "Csize.ant"){
          
          # convert relevant ecology tibble columns into data.frame
          curr.df <- as.data.frame(curr.df[, 10:ncol(curr.df)])
          
          # delete duplicate rows resulting from multiple scans per species
          curr.df <- distinct(curr.df)
          
          # set rownames as ID and delet ID column
          rownames(curr.df) <- curr.df$ID
          curr.df$ID <- NULL
          
          # # delete rows that contain no data # should not be necessary because it is already filtered
          # replace all Nas with zeros
          curr.df[is.na(curr.df)] <- 0
          # ind <- find.all.NA.rows(curr.df)
          # curr.df <- curr.df[ !ind, ]
          
          # calculate distance matrix and store as distance object
          # dist.food.l.d <- as.dist(dist(food.l.df))
          
          # calculate distance matrix and store as matrix object
          # dist.curr.df <- as.matrix(dist(curr.df))
          
          # plot(hclust(dist.curr.df))
          # heatmap(dist.curr.df)
          # levelplot(dist.curr.df, pretty = T)
          
          # levelplot for upper triangle
          # upper <- get.upper.tri(dist.curr.df)
          # myPal <- colorRampPalette(brewer.pal('YlOrRd', n=9))(ncol(upper))
          # myTheme <- rasterTheme(region = myPal)
          # levelplot(upper, par.settings = myTheme, scales=list(x=list(rot=90)), pretty = T)
          
          # calculate dissimilarity index of community
          diss.curr.df <- vegdist(curr.df)
          
          # calculate $ plot PCoA
          pcoa.curr.df <- pcoa(diss.curr.df)
          # biplot(pcoa.curr.df, curr.df, cex = c(0.5, 0.1), main = paste0("PCoA: ", curr.taxon.name, ": ", curr.eco.cat))
          
          # assign PCoA data for multivariate testing against shape
          assign(paste0("PCoA.", curr.taxon.name, ".", curr.eco.cat), pcoa.curr.df)
          
          # add ID column again for left_join
          curr.df$ID <- rownames(curr.df)
          
          # add 1st two PCoA axes to curr.df
          curr.df$PCoA.1 <- pcoa.curr.df$vectors[,1]
          curr.df$PCoA.2 <- pcoa.curr.df$vectors[,2]
          
          # rename the PCoA columns according to current ecology category
          colnames(curr.df)[(ncol(curr.df)-1) : ncol(curr.df)] <-
            c(paste0("PCoA.1.", curr.taxon.name, ".", ecology.cat.names[e]), paste0("PCoA.2.", curr.taxon.name, ".", ecology.cat.names[e]))
          
          curr.df <- as_tibble(curr.df)
          
          array.2D.full <- array.2D.full %>%
            left_join(., select(curr.df, colnames(curr.df[(ncol(curr.df)-2):ncol(curr.df)])), by = "ID")
        }
      }
    }
  }
  
  dev.off()
  
  ###############
  # beautiful plotting for supplement
  # go through all ecological category tibbles, stack plot into PDF and calculate PCoA
  # pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/R/", gsub("_", "-", today()), "_stacked.pdf"), height = 20)
  # pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/R/", gsub("_", "-", today()), "__PCoAs.pdf"), paper = "a4")
  # par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
  ecology.cats.plot <- list(current.l, food.l, food.a,  microhabitat.l,
                       microhabitat.a)
  ecology.cat.names.plot <- list("current.l",  "food.l", "food.a",
                                 "microhabitat.l", "microhabitat.a")
  taxon.names.plot <- c("all", "Dermaptera", "Plecoptera")

  # pdf(paste0("//blanke-nas-1/DATA/PAPERS/PTR_Influence of the nymphal life history/FIGURES/SUPPLEMENT/PCoAs/", gsub("_", "-", today()), "__PCoAs_", taxon.names.plot[t], ".pdf"),
  #     width=8.72, height=11.69) # paper = "a4",
  # par(mfrow=c(2,2), mar=c(0.1, 0.1, 0.1, 0.1))
  #
  for(t in 1:length(taxon.names.plot)){
    for(e in 1:length(ecology.cat.names.plot)){
      # define taxon
      curr.taxon.name <- taxon.names.plot[1]
      message(curr.taxon.name)
      curr.eco.cat <- ecology.cat.names.plot[e]
      message(curr.eco.cat)

      # skip Dermaptera current
      if((curr.eco.cat == "current.l" & curr.taxon.name == "Dermaptera")){
        message(paste0("skipping ", curr.eco.cat, " for Dermaptera..."))
      } else {
        # get current ecology tibble
        curr.df <- ecology.cats.plot[[e]]

        # get only curr.taxon data from curr.df
        if(curr.taxon.name != "all"){
          curr.df <- curr.df %>%
            filter(order == curr.taxon.name)
        }
        # stack plot into PDF
        # plot.stacked.bar(curr.df)

        # convert relevant ecology tibble columns into data.frame
        curr.df <- as.data.frame(curr.df[, 10:ncol(curr.df)])

        # delete duplicate rows resulting from multiple scans per species
        curr.df <- distinct(curr.df)

        # set rownames as ID and delet ID column
        rownames(curr.df) <- gsub("_", " ", curr.df$ID)
        curr.df$ID <- NULL

        # give better names for plots
        new.col.names <- gsub("sum.", "", colnames(curr.df))
        new.col.names <- gsub("\\.a", "", new.col.names)
        new.col.names <- gsub("\\.l", "", new.col.names)
        new.col.names <- gsub("on_stones_bark_plants_ice", "on surfaces", new.col.names)
        new.col.names <- gsub("under_stones_bark_etc", "under objects", new.col.names)
        new.col.names <- gsub("detritus", "in detritus", new.col.names)
        new.col.names <- gsub("current_fast", "high", new.col.names)
        new.col.names <- gsub("current_slow", "low", new.col.names)
        new.col.names <- gsub("current_none", "none", new.col.names)
        new.col.names <- gsub("herbivory", "herbivores", new.col.names)
        new.col.names <- gsub("predatory", "predators", new.col.names)
        new.col.names <- gsub("ectoparasitic", "ectoparasites", new.col.names)
        new.col.names <- gsub("scavenger", "scavengers", new.col.names)
        new.col.names <- gsub("detritivoric", "detritivores", new.col.names)
        new.col.names <- gsub("non_feeding", "non-feeders", new.col.names)
        colnames(curr.df) <- new.col.names


        # # delete rows that contain no data # should not be necessary because it is already filtered
        # replace all Nas with zeros
        curr.df[is.na(curr.df)] <- 0

        # calculate dissimilarity index of community
        diss.curr.df <- vegdist(curr.df)

        # calculate $ plot PCoA
        pcoa.curr.df <- pcoa(diss.curr.df)

        # plot
        # change taxon names for plotting
        curr.taxon.name.plot <- gsub("all", "all taxa", curr.taxon.name)

        # get info if nymphs or adults
        adult.or.nymph <- NULL
        if(grepl("\\.a", curr.eco.cat)){
          adult.or.nymph <- "adults"
        } else if(grepl("\\.l", curr.eco.cat)){
          adult.or.nymph <- "nymphs"
        }

        # change eco cut plot name
        curr.eco.cat.plot <- gsub("\\.l", "", curr.eco.cat)
        curr.eco.cat.plot <- gsub("\\.a", "", curr.eco.cat.plot)

        custom.biplot.pcoa(pcoa.curr.df, curr.df, cex = c(0.5, 0.1), 
               main = paste0(curr.taxon.name.plot, ": ", curr.eco.cat.plot, " ", adult.or.nymph),
               xlabs = "", ylabs = "", 
               cex = rep(par("cex"), 5))
      }
    }
  }
  #
  # dev.off()
  ##############
  
  ##### converge eco data #####
  # create little function to find and crop eco type
  get.curr.type <- function(array.2D.full, col.names, curr.values, l){
    curr.type <- col.names[which(curr.values == max(curr.values, na.rm = T))]
    curr.type <- sub("^sum\\.", "", curr.type)
    curr.type <- sub("\\.a$", "", curr.type)
    curr.type <- sub("\\.l$", "", curr.type)
    # print(paste0(l, ": ", paste(curr.type, collapse = ".")))
    return(curr.type)
  }
  
  # feeding type
  array.2D.full$feeding.type <- NA
  col.names <- c("sum.shredders.l", "sum.scrapers_grazers.l", "sum.predator.l",
                 "sum.filter_feeders.l")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$feeding.type[l] <- curr.type
    }
  }
  
  # larval food
  array.2D.full$food.larva <- NA
  col.names <- c("sum.herbivory.l", 
                 "sum.predatory.l", "sum.ectoparasitic.l", "sum.scavenger.l",
                 "sum.detritivoric.l", "sum.non_feeding.l") # , "sum.omnivore.l"
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$food.larva[l] <- curr.type
    }
  }
  
  # adult food
  array.2D.full$food.adult <- NA
  col.names <- c("sum.herbivory.a",
                 "sum.predatory.a", "sum.ectoparasitic.a", "sum.scavenger.a",
                 "sum.detritivoric.a", "sum.non_feeding.a") # , "sum.omnivore.l"
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$food.adult[l] <- curr.type
    }
  }
  
  # larval microhabitat
  array.2D.full$microhabitat.larva <- NA
  col.names <- c("sum.on_stones_bark_plants_ice.l", "sum.under_stones_bark_etc.l", "sum.detritus.l")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$microhabitat.larva[l] <- curr.type
    }
  }
  
  # adult microhabitat
  array.2D.full$microhabitat.adult <- NA
  col.names <- c("sum.on_stones_bark_plants_ice.a", "sum.under_stones_bark_etc.a", "sum.detritus.a")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$microhabitat.larva[l] <- curr.type
    }
  }
  
  # larval current
  array.2D.full$current <- NA
  col.names <- c("sum.current_fast.l", "sum.current_slow.l", "sum.current_none.l")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$current[l] <- curr.type
    }
  }
  
  # # adult wings
  # array.2D.full$wings <- NA
  # col.names <- c("sum.macropterous", "sum.brachypterous", "sum.micropterous", "sum.apterous")
  # col.numbers <- which(colnames(array.2D.full) %in% col.names)
  # for(l in 1:nrow(array.2D.full)){
  #   curr.values <- array.2D.full[l,col.numbers]
  #   if(mean(is.na(curr.values)) != 1){
  #     curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
  #     array.2D.full$wings[l] <- curr.type
  #   }
  # }
  # adult wings
  array.2D.full$wings <- NA
  col.names <- c("sum.macropterous", "sum.apterous")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$wings[l] <- curr.type
    }
  }
  
  # adult flight
  array.2D.full$flight <- NA
  col.names <- c("sum.agile.a", "sum.flightless.a")
  col.numbers <- which(colnames(array.2D.full) %in% col.names)
  for(l in 1:nrow(array.2D.full)){
    curr.values <- array.2D.full[l,col.numbers]
    if(mean(is.na(curr.values)) != 1){
      curr.type <- get.curr.type(array.2D.full, col.names, curr.values,l)
      array.2D.full$flight[l] <- curr.type
    }
  }
}

# add mech.advantage and m1.size columns so they won't be at end of final df
array.2D.full$mech.adv <- NA
array.2D.full$md1.ins.size <- NA
array.2D.full$Csize.ant <- NA
array.2D.full$Csize.occ <- NA

# load landmark array_2D from LM analysis script (kind of deprecated)
array.2D.LMs <- array_2D # as_tibble(suppressWarnings({gsheet2tbl('https://docs.google.com/spreadsheets/d/1BjS1ejG5ikzlTCz1sOc-Vcjz5ofDPBsyC_6Am8pisZA#gid=421482542')}))
array.2D.LMs$ERC <- rownames(array_2D)

# join everything WUAAA
# colnames(array.2D.LMs)[1] <- "ERC"
array.2D.everything <- array.2D.full %>% 
  left_join(., array.2D.LMs, by = "ERC") %>%
  arrange(ERC)


###### mechanical advantage
for(l in 1:nrow(array.2D.everything)){
  curr.coordinates.dist.inc <- c(array.2D.everything$dist_incisiv_X[l],
                                 array.2D.everything$dist_incisiv_Y[l],
                                 array.2D.everything$dist_incisiv_Z[l])
  curr.coordinates.md.cond.ant <- c(array.2D.everything$mdcond_ant_X[l],
                                    array.2D.everything$mdcond_ant_Y[l],
                                    array.2D.everything$mdcond_ant_Z[l])
  curr.coordinates.md.cond.post <- c(array.2D.everything$mdcond_post_X[l],
                                     array.2D.everything$mdcond_post_Y[l],
                                     array.2D.everything$mdcond_post_Z[l])
  curr.coordinates.md1.ant <- c(array.2D.everything$md1_insterion_ant_X[l],
                                array.2D.everything$md1_insterion_ant_Y[l],
                                array.2D.everything$md1_insterion_ant_Z[l])
  curr.coordinates.md1.post <- c(array.2D.everything$md1_insterion_post_X[l],
                                 array.2D.everything$md1_insterion_post_Y[l],
                                 array.2D.everything$md1_insterion_post_Z[l])
  curr.coordinates.md1.cent <- c(mean(c(curr.coordinates.md1.ant[1],
                                        curr.coordinates.md1.post[1])),
                                 mean(c(curr.coordinates.md1.ant[2],
                                        curr.coordinates.md1.post[2])),
                                 mean(c(curr.coordinates.md1.ant[3],
                                        curr.coordinates.md1.post[3])))
  
  
  curr.out.lever <- dist3d(curr.coordinates.dist.inc, curr.coordinates.md.cond.ant, curr.coordinates.md.cond.post)
  curr.in.lever <- dist3d(curr.coordinates.md1.cent, curr.coordinates.md.cond.ant, curr.coordinates.md.cond.post)
  curr.mech.advantage <- curr.in.lever/curr.out.lever
  curr.md1.size <- dist(rbind(t(as.matrix(curr.coordinates.md1.ant)), t(as.matrix(curr.coordinates.md1.post))))
  
  curr.ERC <- array.2D.everything$ERC[l]
  array.2D.everything$mech.adv[array.2D.everything$ERC == curr.ERC] <- curr.mech.advantage
  array.2D.everything$md1.ins.size[array.2D.everything$ERC == curr.ERC] <- curr.md1.size
}

###### Csize occ
Csize.occ.tibble <- create.empty.tibble(names = c("ERC", "Csize.occ"), nrow = 0) %>% 
  mutate(ERC = as.character(ERC))

l.counter.tibble <- 1
for(l in 1:nrow(array.2D.everything)){
  # get subset of array_2D that contains occ. suture data of current ERC
  curr.occS.coords <- array.2D.everything[l, ] %>% 
    select(matches("^occS_shape_\\d+_\\w+$")) %>% 
    t()
  if(mean(is.na(curr.occS.coords)) != 1){
    curr.occS.coords <- delete.rows.with.na.in.column(curr.occS.coords)
    
    curr.occS.coords.df <- create.empty.tibble(nrow = length(curr.occS.coords)/3, names = c("x", "y", "z"))
    l.counter <- 1
    for(c in seq(1, length(curr.occS.coords), 3)){
      curr.occS.coords.df$x[l.counter] <- curr.occS.coords[c]
      curr.occS.coords.df$y[l.counter] <- curr.occS.coords[c+1]
      curr.occS.coords.df$z[l.counter] <- curr.occS.coords[c+2]
      
      l.counter <- l.counter+1
    }
    curr.Csize.occ <- cSize(curr.occS.coords.df)
    
    Csize.occ.tibble[l.counter.tibble, 1] <- array.2D.everything$ERC[l]
    Csize.occ.tibble$Csize.occ[l.counter.tibble] <- curr.Csize.occ
    l.counter.tibble <- l.counter.tibble+1
    
    array.2D.everything$Csize.occ[l] <- curr.Csize.occ
    # print(paste0(l, ": ", array.2D.everything$ERC[l], ": ", curr.Csize.occ))
  } else {
    array.2D.everything$Csize.occ[l] <- NA
    # print(paste0(l, ": ", array.2D.everything$ERC[l], ": no eye LMs"))
  }
}

Csize.occ.tibble$larva_adult.l <- "b"
Csize.occ <- Csize.occ.tibble %>% 
  left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID, ERC), by = "ERC") %>% 
  select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult.l, Csize.occ, ID) %>% # , sum.omnivore
  mutate_all(~replace(., is.nan(.), NA)) %>% 
  distinct()


###### Csize ant
Csize.ant.tibble <- create.empty.tibble(names = c("ERC", "Csize.ant"), nrow = 0) %>% 
  mutate(ERC = as.character(ERC))
l.counter.tibble <- 1
for(l in 1:nrow(array.2D.everything)){
  # get subset of array_2D that contains ant. suture data of current ERC
  curr.antS.coords <- array.2D.everything[l, ] %>% 
    select(matches("^antS_shape_\\d+_\\w+$")) %>% 
    t()
  if(mean(is.na(curr.antS.coords)) != 1){
    curr.antS.coords <- delete.rows.with.na.in.column(curr.antS.coords)
    
    curr.antS.coords.df <- create.empty.tibble(nrow = length(curr.antS.coords)/3, names = c("x", "y", "z"))
    l.counter <- 1
    for(c in seq(1, length(curr.antS.coords), 3)){
      curr.antS.coords.df$x[l.counter] <- curr.antS.coords[c]
      curr.antS.coords.df$y[l.counter] <- curr.antS.coords[c+1]
      curr.antS.coords.df$z[l.counter] <- curr.antS.coords[c+2]
      
      l.counter <- l.counter+1
    }
    curr.Csize.ant <- cSize(curr.antS.coords.df)
    
    Csize.ant.tibble[l.counter.tibble, 1] <- array.2D.everything$ERC[l]
    Csize.ant.tibble$Csize.ant[l.counter.tibble] <- curr.Csize.ant
    l.counter.tibble <- l.counter.tibble+1
    
    array.2D.everything$Csize.ant[l] <- curr.Csize.ant
    # print(paste0(l, ": ", array.2D.everything$ERC[l], ": ", curr.Csize.ant))
  } else {
    array.2D.everything$Csize.ant[l] <- NA
    # print(paste0(l, ": ", array.2D.everything$ERC[l], ": no antennal LMs"))
  }
}

Csize.ant.tibble$larva_adult.l <- "b"
Csize.ant <- Csize.ant.tibble %>% 
  left_join(., select(PBT.Derma.Pleco.LMs, order, suborder, infraorder, superfamily, family, subfamily, genus, species, ID, ERC), by = "ERC") %>% 
  select(order, suborder, infraorder, superfamily, family, subfamily, genus, species, larva_adult.l, Csize.ant, ID) %>% # , sum.omnivore
  mutate_all(~replace(., is.nan(.), NA)) %>% 
  distinct()



if(remove.specimens){
  write.xlsx2(as.data.frame(array.2D.everything), paste0(today(), "_array.2D.everything_filtered.xlsx"), row.names = F)
  # write.table(as.data.frame(array.2D.everything), "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
  message(paste0("all done! Saved to ", paste0(today(), "_array.2D.everything_filtered.xlsx"), " and clipboard: paste into sheet tab 'array.2D.everything."))
} else {
  write.xlsx2(as.data.frame(array.2D.everything), paste0(today(), "_array.2D.everything.xlsx"), row.names = F)
  # write.table(as.data.frame(array.2D.everything), "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
  message(paste0("all done! Saved to ", paste0(today(), "_array.2D.everything.xlsx"), " and clipboard: paste into sheet tab 'array.2D.everything."))
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken










# 
# array.2D.everything$ID[array.2D.everything$family == "Perlidae"]
# 
# array.2D.everything$sum.macropterous[array.2D.everything$genus == "Amphinemura"]
# 
# array.2D.everything$current[array.2D.everything$genus == "Amphinemura"]
# 
# array.2D.everything$food.larva[array.2D.everything$genus == "Anacroneuria"]
# 
# array.2D.everything$food.adult[array.2D.everything$genus == "Perlodes"]
# 
# array.2D.everything$ID[array.2D.everything$genus == "Anacroneuria"]
# 
# # # calculate eye area
# ERC <- 0004
# eye.tip.LM.col.number <- grep("tip_of_eye", colnames(array.2D.LMs))
# eye.curve.LM.col.numbers <- grep("occS_shape_eq", colnames(array.2D.LMs))
# eye.curve.LMs <- array.2D.LMs[,c(eye.tip.LM.col.number, eye.curve.LM.col.numbers)]
# curr.eye.curve.LMs <- eye.curve.LMs[1,]
# 
# tip.coords <- curr.eye.curve.LMs[,1:3]
# curve.coords.row <- curr.eye.curve.LMs[,-c(1:3)]
# 
# dims = 3
# df <- curve.coords.row
# if(dims == 2){dim.names <- c("x", "y")}
# if(dims == 3){dim.names <- c("x", "y", "z")}
# curve.coords.cols <- create.empty.tibble(nrow = ncol(df)/dims, names = dim.names)
# for(r in seq(1, ncol(curve.coords.row), 3))
# 
# # add one more row with same coords as first coord to close loop circle
# curve.coords[nrow(curve.coords)+1, ] <- curve.coords[1,]
