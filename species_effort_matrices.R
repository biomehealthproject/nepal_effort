library(dplyr)

all_cams <-
  read.csv("Surevey_effort_30minthreshold.csv", stringsAsFactors = FALSE)

species <-
  read.csv("Latest_species_meta.csv", stringsAsFactors = FALSE)

#replacing 0s with NAs - NAs indicate when the cameras were not on
#We want to build a matrix for each species where 1 = species detected, 0 = camera on but species not detected, NA =camera not on
all_cams[all_cams == 0] <- NA

sp_dates <- species %>%
  select(CommonName, site_cam.x , date_fixed) %>%
  arrange(CommonName) %>%
  distinct()

sp_dates$date_fixed <-
  as.Date(sp_dates$date_fixed, format = "%d/%m/%Y")

no_sp <-
  which(!colnames(all_cams)[2:ncol(all_cams)] %in% unique(sp_dates$site_cam.x)) #which cams are these?

all_cams <-
  all_cams[,-no_sp + 1] #getting rid of columns with cameras with no sp detections


d <- sp_dates
colnames(d) <- c("Species", "Site", "DateTime")

calcOcc <-
  function(species,
           d = d,
           startDate = as.Date("2019-03-15"),
           endDate = as.Date("2019-04-15")) {
    # Make a vector of breaks ###Can we specify different start dates for different points?
    brks <- seq(startDate, endDate, by = "day")
    brks <- brks[-length(brks)]
    
    # Breaks with final end date as well.
    brksLong <- c(brks, brks[length(brks)] + timeStep)
    
    # Create an empty matrix of dim sites x time periods
    occ <-
      matrix(0, nrow = length(unique(d$Site)), ncol = length(brksLong))
    rownames(occ) <- sort(unique(d$Site))
    colnames(occ) <- strftime(brksLong, format = "%Y-%m-%d")
    
    
    for (s in unique(d$Site)) {
      seen <- NA
      captures <- d$DateTime[d$Species == species & d$Site == s]
      
      # Were animals seen at the site
      seen <- brksLong %in% captures
      st <- startDate
      end <- endDate
      
      # If the species was seen, occ = 1
      
      col_i <- which(colnames(occ) == s)
      occ[seen, col_i] <- 1
      occ <- occ * all_cams[, 2:ncol(all_cams)]
      
      paste0(species, " done!")
      species_name <- gsub(" ", "", species)
      row.names(occ) <- brksLong
      if (!file.exists(here::here("matrices_out", paste0(species_name, "_tt_effort.csv")))) {
        write.csv(occ, here::here("matrices_out", paste0(species_name, "_tt_effort.csv")))
      }
      
    }
    return(occ)
    
    
  }

# This lapply function will create a effort matrix for each species

lapply(
  X = unique(species$CommonName),
  FUN = calcOcc,
  d = d,
  startDate = as.Date("2019-03-15"),
  endDate = as.Date("2019-04-15")
)

#If you just want it for one use this:

calcOcc(
  species = "Chital",
  d = d,
  startDate = as.Date("2019-03-15"),
  endDate = as.Date("2019-04-15")
)


#####This section for compressing the matrices into difference time chunks####


chital <-
  read.csv(here::here("matrices_out", "Chital_tt_effort.csv"))

row.names(chital) <- chital$X

chital <- chital[, -1]

#na_mode = "include" means that NAs will effectively be treated as zeros.
#if na_mode = anything apart from "include" an NA in a time step will count the whole timestep as NA

#have just done an example with the Chital data but could set it up as above to create for all sp.

timestepper <- function(occ_in, timestep, na_mode = "include") {
  if (na_mode == "include") {
    occ_in[is.na(occ_in)] <- 0
  }
  
  if (timestep > nrow(occ_in) / 2) {
    print(paste(
      "Time step is too large! Please reduce to",
      nrow(occ_in) / 2 ,
      "or less."
    ))
  } else {
    start <- seq(1, nrow(occ_in), by = timestep)
    end <- seq(timestep, nrow(occ_in), by = timestep)
    
    if (length(start) > length(end)) {
      start <- start[-length(start)]
    }
    
    timesteps <- matrix(nrow = length(start), ncol = ncol(occ_in))
    colnames(timesteps) <- colnames(occ_in)
    rownames(timesteps) <-
      paste(rownames(occ)[start], rownames(occ_in)[end], sep = ":")
    
    for (i in 1:length(start)) {
      timestep_out <- colSums(occ_in[start[i]:end[i], ])
      timesteps[i, ] <- timestep_out
      timesteps[timesteps > 0] <- 1
    }
    
    return(timesteps)
  }
  
}

timestepper(occ_in = chital,
            timestep = 4,
            na_mode = "include")


timestepper(occ_in = chital,
            timestep = 4,
            na_mode = "exclude")
