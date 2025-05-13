library(readr)
library(geojsonR)
library(sp)
library(readxl)
library(glmnet)
library(data.table)
library(readr)
library(sf)
library(rstan)

## Reading Data
file_js <- FROM_GeoJson(url_file_string = "Neighbourhoods - historical 140 - 4326.geojson")
tmc_summary_data <- read.csv("tmc_summary_data.csv", header = T)
bike_js <-  FROM_GeoJson(url_file_string = "cycling-network - 4326.geojson")
demographic_data <- read.csv("neighbourhood-profiles-2016-140-model.csv")

# Set to 158 if using 2021 census data
nbd_num <- 140

poly_list <- list()
for(i in 1:length(file_js$features)){
  poly_test <- Polygon(file_js$features[[i]]$geometry$coordinates[[1]])
  first_poly_test <- Polygons(list(poly_test), ID = i)
  poly_list <- append(poly_list,first_poly_test)
}

toronto_spatial_polys <- SpatialPolygons(poly_list)

# 5897 is the index to get to the start of 2021 data collection
tmc_subset <- tmc_summary_data[1:5897,]


## Combining Intersections in Neighbourhoods

neighbourhood_counts <- rep(0,nbd_num)
neighbourhood_durations <- rep(0,nbd_num)
neighbourhood_peds <- rep(0,nbd_num)
neighbourhood_totals <- rep(0,nbd_num)

for(i in 1:length(tmc_subset$count_id)){
  x <- tmc_subset$longitude[i]
  y <- tmc_subset$latitude[i]
  
  for(j in 1:length(toronto_spatial_polys)){
    coords <- toronto_spatial_polys@polygons[[j]]@Polygons[[1]]@coords
    result <- point.in.polygon(x,y,coords[,1], coords[,2])
    
    if(result != 0){
      
      neighbourhood_counts[j] = neighbourhood_counts[j] + tmc_subset$total_vehicle[i]
      neighbourhood_peds[j] = neighbourhood_peds[j] + tmc_subset$total_pedestrian[i]
      neighbourhood_totals[j] = neighbourhood_totals[j] + tmc_subset$total_vehicle[i] + tmc_subset$total_bike[i] + tmc_subset$total_heavy_pct[i] + tmc_subset$total_pedestrian[i]
      
      time <- tmc_subset$count_duration[i]
      if(time == "8S" | time == "8R"){
        neighbourhood_durations[j] = neighbourhood_durations[j] + 8
      }
      else if (time == "14") {
        neighbourhood_durations[j] = neighbourhood_durations[j] + 14
      }
      else{
        print("error")
      }
    }
  }
}

neighbourhood_persontime <- neighbourhood_totals*neighbourhood_durations

## Putting together Demographic Data
neighbourhood_order <- rep("", nbd_num)
for(i in 1:nbd_num){
  neighbourhood_order[i] = file_js$features[[i]]$properties$AREA_NAME
}

## Hard coding edge cases for 2021 census data
# neighbourhood_order[1] = "South Eglinton Davisville"
# neighbourhood_order[61] = "Yonge St. Clair"
# neighbourhood_order[82] = "North St. James Town"
# neighbourhood_order[85] = "Cabbagetown South St. James Town"
# neighbourhood_order[94] = "East End Danforth"
# neighbourhood_order[95] = "Taylor Massey"
# neighbourhood_order[97] = "Danforth-East York"
# neighbourhood_order[131] = "O Connor Parkview"
# checklist <- gsub(" ", "", colnames(demographic_data), fixed = TRUE)
# for(i in 1:nbd_num){
#   checkname <- gsub("-", " ", neighbourhood_order[i], fixed = TRUE)
#   checkname <- gsub("/", ".", checkname, fixed = TRUE)
#   checkname <- gsub(" ", ".", checkname, fixed = TRUE)
#   checkname <- gsub("'", ".", checkname, fixed = TRUE)
#   
#   
#   permutation[i] = which(checklist == checkname)-1
# }


permutation <- rep(-1,nbd_num)
checklist <- as.numeric(demographic_data[1,-c(1,2,3,4,5,6)])
for(i in 1:nbd_num){
  checknum <- as.numeric(file_js$features[[i]]$properties$AREA_SHORT_CODE)
  permutation[i] = which(checklist == checknum)
}


## Reordering Data so it works with demographics file
neighbourhood_counts = neighbourhood_counts[order(permutation)]
neighbourhood_durations = neighbourhood_durations[order(permutation)]
neighbourhood_peds = neighbourhood_peds[order(permutation)]
neighbourhood_persontime = neighbourhood_persontime[order(permutation)]


## LASSO Regression to pick covariates

# -c(427:1949,2145:2208,2265:2564)

#demographic_t_subset <- demographic_data[c(2,3,4,9,19,34,63,64,93:103,212,217:224,402,1488,1494,1966,1991,2221:2230,2571,2573,2577,2578,2579,2580,2581,2584:2588),]
#demographic_subset <- matrix(nrow = nbd_num, ncol = 55)

topics <- demographic_data$Topic
subset_index <- which(topics == "Population and dwellings" |
                      topics == "Age characteristics" |
                      topics == "Income of individuals in 2025" |
                      topics == "Highest certificate, diploma or degree" |
                      topics == "Labour force status" |
                      topics == "Main mode of commuting" |
                      topics == "Commuting duration")

demographic_t_subset <- demographic_data[subset_index,]
demographic_subset <- matrix(nrow = nbd_num, ncol = length(subset_index))

for(i in 1:nrow(demographic_t_subset)){
  temp <- c()
  for(j in 1:140){
    temp[j] = demographic_t_subset[i,6+j]
  }
  demographic_subset[,i] <- parse_number(temp)
}

demographic_subset <- data.frame(demographic_subset)
colnames(demographic_subset) <- demographic_t_subset$Characteristic
rownames(demographic_subset) <- colnames(demographic_data)[-c(1:6)]


lasso_data <- demographic_subset
colnames(lasso_data) <- c(1:length(subset_index))

# colnames(lasso_data) <- c("TSNS","Population","14_Years","19_Years", "Over 65",
#                           "Average Age", "Median Income", "Average Income", "Income_10",
#                           "Income_20", "Income_30", "Income_40",
#                           "Income_50","Income_60","Income_70","Income_80", "Income_90","Income_100","Income_more",
#                           "Gini", "Single_Detached","Semi_Detached","Row_House","Apartment_Duplex","Apartment_5",
#                           "Apartment_More", "Other_house", "Movable", "Average_children", "Immigrate_1980",
#                           "Immigrate_2016","Employed", "Bachelor", "Job_0","Job_1","Job_2","Job_3","Job_4",
#                           "Job_5","Job_6","Job_7","Job_8","Job_9","Commute_1","Commute_2","Car_drive",
#                           "Car_pass", "Transit","Walk","Bike","Commute_less","Commute_29",
#                           "Commute_44","Commute_59","Commute_60")

X <- as.matrix(lasso_data)

cv_model <- cv.glmnet(y = neighbourhood_counts/neighbourhood_durations, x = X, alpha = 1)
lasso_model <- glmnet(y = neighbourhood_counts/neighbourhood_durations, x = X, alpha = 1,
                      lambda = cv_model$lambda.1se)
lasso_model$beta
colnames(demographic_subset)[which(lasso_model$beta != 0)]

## Bad Models (very high dispersion, few significant covariates)
naive_poisson <- glm(neighbourhood_counts ~ . , family = poisson, data= lasso_data)
quasi_poisson <- glm(sqrt(neighbourhood_counts) ~ . , family = quasipoisson, data= lasso_data[,-c(1:6)])

## Processing Bike Lane Data

line_list <- list()
lane_types <- c()

for(i in 1:length(bike_js$features)){
  for(j in 1:length(bike_js$features[[i]]$geometry$coordinates[[1]])){
    points <- SpatialPoints(bike_js$features[[i]]$geometry$coordinates[[1]][j])
    
    lane_types <- c(lane_types, bike_js$features[[i]]$properties$INFRA_LOWORDER)
    line_list <- append(line_list,points)  
  }
}

major_lane_counts <- rep(0,nbd_num)
minor_lane_counts <- rep(0,nbd_num)
for(i in 1:length(line_list)){
  for(j in 1:length(toronto_spatial_polys)){
    out <- over(line_list[[i]],toronto_spatial_polys[j])
    if(1 %in% out){
      if(lane_types[i] == "Cycle Track" || lane_types[i] == "Bike Lane"
         || lane_types[i] == "Bike Lane - Buffered" || 
         lane_types[i] == "Cycle Track - Contraflow"
         || lane_types[i] == "Contra-Flow Bike Lane"){
        
          major_lane_counts[j] = major_lane_counts[j] + 1
      }
      else {
        minor_lane_counts[j] = minor_lane_counts[j] + 1
      }
    }
  }
}

## Getting neighbourhood intersections to find adjacents

nbd_matrix <- matrix(0,nrow = nbd_num, ncol = nbd_num)
for(i in 1:nbd_num){
  for(j in 1:(i-1)){
    out <- st_intersects(st_as_sf(toronto_spatial_polys[i]),st_as_sf(toronto_spatial_polys[j]))
    if(length(unlist(out)) > 0) {
      nbd_matrix[i,j] = 1
      nbd_matrix[j,i] = 1
    }
  }
}

nbd_matrix[1,1] = 0

## Getting distance from Neighbourhoods to city centre

kensington_poly <- toronto_spatial_polys[which(neighbourhood_order == "Kensington-Chinatown (78)")] 
coords <- kensington_poly@polygons[[1]]@Polygons[[1]]@coords
centroid <- c(mean(coords[,1]), mean(coords[,2]))


neighbourhood_centers <- matrix(0,nrow = 2, ncol = nbd_num)
neighbourhood_distances <- rep(0,nbd_num)

for(i in 1:length(toronto_spatial_polys)){
  coords <- toronto_spatial_polys@polygons[[i]]@Polygons[[1]]@coords
  neighbourhood_centers[1,i] = mean(coords[,1])
  neighbourhood_centers[2,i] = mean(coords[,2])
  neighbourhood_distances[i] = sqrt((centroid[1] - mean(coords[,1]))^2 + 
                                      (centroid[2] - mean(coords[,2]))^2)
}

neighbourhood_distances = neighbourhood_distances[order(permutation)]

## Bayes Models

rstan_options(auto_write = TRUE) #save compiled model

data = list(
  n = nbd_num,
  y = neighbourhood_counts/neighbourhood_durations,
  popchange = demographic_subset$`Population Change 2011-2016`,
  seniors = demographic_subset$`Older Seniors (85+ years)`,
  collegeDeg = demographic_subset$`    College, CEGEP or other non-university certificate or diploma`,
  labourParticp = demographic_subset$`Participation rate`,
  distance = neighbourhood_distances,
  majorBikeLanes = major_lane_counts,
  minorBikeLanes = minor_lane_counts,
  wmat = nbd_matrix)

stan_fit = stan("NormalBayesModel.stan", 
                data = data,
                iter = 15000,
                warmup = 7500,
                chain = 2, refresh=5)

## Beta 1 hits ceiling of 30 too often.
mcpost <- as.data.frame(stan_fit)
save(stan_fit, file="NormalBayesModel_Job5.rda")

data2 = list(
  n = nbd_num,
  y = neighbourhood_counts,
  popchange = demographic_subset$`Population Change 2011-2016`,
  distance = neighbourhood_distances,
  majorBikeLanes = major_lane_counts,
  minorBikeLanes = minor_lane_counts,
  logPT = log(neighbourhood_persontime),
  wmat = nbd_matrix)

stan_fit2 = stan("PoissonBayesModel.stan", 
                data = data2,
                iter = 4200,
                warmup = 2500,
                chain = 1, refresh=10)

mcpost2 <- as.data.frame(stan_fit2)
save(stan_fit, file="PoissonBayesModel_Job2.rda")


## Plots for Report

hist(neighbourhood_counts, main = "Histogram of Neighbourhood Vehicle Counts",
     xlab = "Number of Vehicles", col = 'steelblue')
hist(neighbourhood_durations, main = "Histogram of Time Observed at Neighbourhoods",
     xlab = "Number of Vehicles", col = 'steelblue')
hist(neighbourhood_counts/neighbourhood_durations, 
     main = "Histogram of Time Observed at Neighbourhoods",
     xlab = "Number of Vehicles", col = 'steelblue')


## Creating Map of Toronto

plot(toronto_spatial_polys, main = "Toronto Map", xlab = "Longitude",ylab = "Latitude")


par(mfrow = c(1,2))

## Top Quantile of Peak AM Traffic
plot(toronto_spatial_polys, main = "Peak Morning Vehicle Counts \n in Toronto",
     xlim = c(-79.63926,-79.12607), ylim = c(43.59,43.85))
plot_col <- c()
top_quantile <- quantile(tmc_subset$am_peak_vehicle,0.75, na.rm = T)
max_quantile <- max(tmc_subset$am_peak_vehicle, na.rm = T)
for(i in 1:length(tmc_subset$am_peak_vehicle)){
  if(is.na(tmc_subset$am_peak_vehicle[i])){
    plot_col[i] = rgb(0,0,1,alpha = 0.1)
  }
  else if(tmc_subset$am_peak_vehicle[i] > top_quantile) {
    plot_col[i] = rgb(1*(tmc_subset$am_peak_vehicle[i]/max_quantile),0,0) 
  }
  else{
    plot_col[i] = rgb(0,0,1,alpha = 0.1)
  }
}

points(tmc_subset$longitude,tmc_subset$latitude,col = plot_col, pch = 16)

legend(legend = c('Highest','Lowest'),
       fill = c('red', 'blue'), "bottom", pt.cex = 0.3, cex = 0.8, bty = 'n',
       horiz = T, inset = c(0,-0.3))

## Neighbourhood Traffic Plot
rbPal <- colorRampPalette(c('red','black'))
plot_col <- rbPal(10)[cut(neighbourhood_counts/neighbourhood_durations, breaks = 10)]
plot(toronto_spatial_polys, main = "Toronto Neighbourhood Traffic \n Adjusted for Time", 
     col = plot_col,
     xlim = c(-79.63926,-79.12607), ylim = c(43.59,43.85))
legend(legend = c('Highest','Lowest'),
       fill = c('black', 'red'), "bottomright", pt.cex = 0.3, cex = 0.8,horiz = T,
       inset = c(0,-0.3),bty = 'n')

dev.off()
par(mfrow=c(1,3))
hist(neighbourhood_counts, main = "Vehicle Counts",
     xlab = "Vehicles", col = 'steelblue')
hist(neighbourhood_durations, main = "Time Observed at \n Neighbourhoods",
     xlab = "Hours", col = 'steelblue')
hist(neighbourhood_counts/neighbourhood_durations, 
     main = "Vehicles Per Hour",
     xlab = "Rate", col = 'steelblue')

dev.off()

par(mfrow = c(1,2))
spatial_effects <- rep(0,140)
for(i in 1:140){
  spatial_effects[i] = mean(mcpost[,i])
}

spatial_effects_p <- rep(0,140)
for(i in 1:140){
  spatial_effects_p[i] = mean(mcpost2[,i])
}

hist(spatial_effects, main = "Spatial Effects \n from Normal Model", xlab = "Spatial Effect",
     col = 'steelblue')
hist(spatial_effects_p, main = "Spatial Effects \n from Poisson Model", xlab = "Spatial Effect",
     col = 'steelblue')


## Confidence Intervals

lower_normal <- rep(0,7)
upper_normal <- rep(0,7)
for(i in 1:7){
  lower_normal[i] = quantile(mcpost[,140+i], prob = 0.025)
  upper_normal[i] = quantile(mcpost[,140+i], prob = 0.975)
}

means_normal <- c(mean(mcpost$beta1),mean(mcpost$beta2),mean(mcpost$beta3),mean(mcpost$beta4),mean(mcpost$beta5),mean(mcpost$beta6),mean(mcpost$beta7))

lower_poisson <- rep(0,3)
upper_poisson <- rep(0,3)
for(i in 1:3){
  lower_poisson[i] = quantile(mcpost2[,280+i], probs = 0.025)
  upper_poisson[i] = quantile(mcpost2[,280+i], probs = 0.975)
}

means_poisson <- c(mean(mcpost2$beta5),mean(mcpost2$beta6),mean(mcpost2$beta7))


## Inverse Gamma Tests

beta <- 6000
alpha <- 20
pts <- seq(0,900, by = 0.01)
pdf <- beta^alpha/gamma(alpha)*pts^(-alpha-1)*exp(-beta/pts)
plot(pts,pdf, type = 'l')
