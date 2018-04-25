# Name: Paige Kelley
# Course: 44-149 Scientific Computing
# Assignment # Project 3
# Due Date: 04/20/2018
# Brief: Population Clustering
# By submitting this, I pledge that the code in this file was written by the author idicated above,
# and that ll assistance was correctly attributed in comments.  Additionally, I
# agree to abide by the rules expressed in the CSIS Academic Honest Policy

census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'),]

plot(contiguous$longitude, contiguous$latitude, type='p', col=contiguous$state)

N<- 8

chosen_counties <- sample(1:nrow(contiguous), N)
center <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  center[i, 1] = contiguous[chosen_counties[i], 'latitude']
  center[i, 2] = contiguous[chosen_counties[i], 'longitude']
}

#the following can be used if the for loop doesn't work
#center[,1] = contiguous$latitude[chosen_counties]
#center[,2] = contiguous$longitude[chosen_counties]

#print(center)

#print(contiguous[1,])
#print(center[1,])

distance_squared <- function(county, center_point) {
  deltax <- county[1, 'latitude'] - center_point[1]
  deltay <- county[1, 'longitude'] - center_point[2]
  deltax^2 + deltay^2
}

#deltax <- contiguous[1, 'latitude'] - center[1,1]
#deltay <- contiguous[1, 'longitude'] - center[1,2]

#print(distance_squared(contiguous[1,], centers[1,]))

#belongs_to[i] means the ith county belongs_to the cluster at
#belongs_to[i]
belongs_to <- rep(0, nrow(contiguous))


ITERS <- 10

for (x in 1: ITERS){
#figure out closest cluster
for (county in 1: nrow(contiguous)) {
  closest_center <- 1
  closest_distance <- distance_squared(contiguous[county,], center[1,])

  for (cluster in 2:N) {
    d <- distance_squared(contiguous[county,], center[cluster,])
    if (d < closest_distance) {
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county] <- closest_center
}

plot(contiguous$longitude, contiguous$latitude, type='p', col=belongs_to)

#calculate new cluster centers
for (i in 1:N) {
cluster_of_interest <- contiguous[belongs_to==i,]
total_population <- sum(cluster_of_interest$population)
new_latitude <- sum(cluster_of_interest$latitude * cluster_of_interest$population) / total_population
new_longitude <- sum (cluster_of_interest$longitude * cluster_of_interest$population) / total_population
center[i, 1] <- new_latitude
center[i, 2] <- new_longitude
}
  plot(contiguous$longitude, contiguous$latitude, col=belongs_to)
}
