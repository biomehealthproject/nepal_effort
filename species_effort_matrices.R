library(dplyr)

all_cams <-
  read.csv("Surevey_effort_30minthreshold.csv", stringsAsFactors = FALSE)

species <- read.csv("Latest_species_meta.csv", stringsAsFactors = FALSE)

#replacing 0s with NAs - NAs indicate when the cameras were not on
#We want to build a matrix for each species where 1 = species detected, 0 = camera on but species not detected, NA =camera not on
all_cams[all_cams == 0] <- NA

sp_dates <- species %>%
  select(CommonName, site_cam.x , date_fixed) %>%
  arrange(CommonName) %>%
  distinct()

sp_dates$date_fixed <- as.Date(sp_dates$date_fixed, format = "%d/%m/%Y")

no_sp <-
  which(!colnames(all_cams)[2:ncol(all_cams)] %in% unique(sp_dates$site_cam.x)) #which cams are these?

all_cams <-
  all_cams[, -no_sp + 1] #getting rid of columns with cameras with no sp detections


d <- sp_dates
colnames(d) <- c("Species", "Site", "DateTime")

calcOcc <-
  function(species,
           d = d,
           timeStep = 1,
           startDate = as.Date("2019-03-15"),
           endDate = as.Date("2019-04-15")) {
    # Make a vector of breaks ###Can we specify different start dates for different points?
    brks <- seq(startDate, endDate, by = paste(timeStep, 'day'))
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
      write.csv(occ, here::here("matrices_out", paste0(species_name, "_tt_effort.csv")))
      
    }
    return(occ)
    
    
  }

lapply(
  X = unique(species$CommonName),
  FUN = calcOcc,
  d = d,
  timeStep = 1,
  startDate = as.Date("2019-03-15"),
  endDate = as.Date("2019-04-15")
)



occ<-read.csv(here::here("matrices_out", "Chital_tt_effort.csv"))

row.names(occ)<-occ$X

occ<-occ[,-1]

piv_occ<-t(occ)


#na_mode = "include" means that NAs will effectively be treated as zeros. 
#na_mode = "exclude" means that an NA in a time step will count the whole timestep as NA

timestepper<-function(piv_occ, timestep, na_mode = "include"){
  
  start <- seq(1, ncol(piv_occ), by = timestep)
  end <-seq(timestep, ncol(piv_occ), by = timestep)
 #end[length(end)]<-ncol(piv_occ)
  
  if (length(start) > length(end)){
    start<-start[-length(start)]
    }
  
  print(start)
  print(end)
  
  }

timestepper(piv_occ = piv_occ, timestep = 10)






