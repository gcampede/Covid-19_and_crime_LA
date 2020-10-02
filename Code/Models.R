''' Causal impact models: to obtain the files (time series) to perform the models
you have to first run the 'data_preprocessing.R' script also contained in this folder.
For illustrative purposes, here are presented the models having the March 4th-March28th
time window as the post-intervention period. To evaluate the impact of the policies in 
the March 4th-March16th time window, modify accordingly the 'post.period' line below'''

pre.period <- as.Date(c("2017-01-01", "2020-03-03"))
post.period <- as.Date(c("2020-03-04", "2020-03-28")) # modify in "2020-03-16" to evaluate models in the first post-intervention window


##''''''''''''''''''''''''' ASSAULT '''''''''''''''''''''''''''''''''##


## univariate

assault_impact <- CausalImpact(assault_ts, pre.period, 
                               post.period, 
                               model.args = list(nseasons = 7, season.duration = 1))
plot(assault_impact, c( "pointwise", "cumulative"))
summary(assault_impact)

summary(assault_impact, "report")

## covariates

assault_cov_impact <- CausalImpact(assault_merged, pre.period, 
                                   post.period, 
                                   model.args = list(nseasons = 7, season.duration = 1))
plot(assault_cov_impact, c( "pointwise", "cumulative"))
summary(assault_cov_impact)

summary(assault_cov_impact, "report")


##''''''''''''''''''''''''' BATTERY '''''''''''''''''''''''''''''''''##

## univariate

battery_impact <- CausalImpact(battery_ts, pre.period, 
                               post.period, 
                               model.args = list(nseasons = 7, season.duration = 1))
plot(battery_impact, c( "pointwise", "cumulative"))
summary(battery_impact)

summary(battery_impact, "report")

## covariates

battery_cov_impact <- CausalImpact(battery_merged, pre.period, 
                                   post.period, 
                                   model.args = list(nseasons = 7, season.duration = 1))
plot(battery_cov_impact, c( "pointwise", "cumulative"))
summary(battery_cov_impact)

summary(battery_cov_impact, "report")





##''''''''''''''''''''''' BURGLARY '''''''''''''''''''''''''''''''''''##

## univariate

burglary_impact <- CausalImpact(burglary_ts, pre.period, 
                                post.period, 
                                model.args = list(nseasons = 7, season.duration = 1))
plot(burglary_impact, c( "pointwise", "cumulative"))
summary(burglary_impact)

summary(burglary_impact, "report")

## covariates

burglary_cov_impact <- CausalImpact(burglary_merged, pre.period, 
                                    post.period, 
                                    model.args = list(nseasons = 7, season.duration = 1))
plot(burglary_cov_impact, c( "pointwise", "cumulative"))
summary(burglary_cov_impact)

summary(burglary_cov_impact, "report")



##''''''''''''''''''''''' INTIMATE '''''''''''''''''''''''''''''''''''##
## univariate

intimate_impact <- CausalImpact(intimate_ts, pre.period, 
                                post.period, 
                                model.args = list(nseasons = 7, season.duration = 1))
plot(intimate_impact, c( "pointwise", "cumulative"))
summary(intimate_impact)

summary(intimate_impact, "report")

## covariates

intimate_cov_impact <- CausalImpact(intimate_merged, pre.period, 
                                    post.period, 
                                    model.args = list(nseasons = 7, season.duration = 1))
plot(intimate_cov_impact, c( "pointwise", "cumulative"))
summary(intimate_cov_impact)

summary(intimate_cov_impact, "report")



##''''''''''''''''''''''' ROBBERY '''''''''''''''''''''''''''''''''''## 

## univariate

robbery_impact <- CausalImpact(robbery_ts, pre.period, 
                               post.period, 
                               model.args = list(nseasons = 7, season.duration = 1))
plot(robbery_impact, c( "pointwise", "cumulative"))
summary(robbery_impact)

summary(robbery_impact, "report")

## covariates

robbery_cov_impact <- CausalImpact(robbery_merged, pre.period, 
                                   post.period, 
                                   model.args = list(nseasons = 7, season.duration = 1))
plot(robbery_cov_impact, c( "pointwise", "cumulative"))
summary(robbery_cov_impact)

summary(robbery_cov_impact, "report")



##''''''''''''''''''''''' SHOPLIFTING '''''''''''''''''''''''''''''''''''## 


## univariate

shoplifting_impact <- CausalImpact(shoplifting_ts, pre.period, 
                                   post.period, 
                                   model.args = list(nseasons = 7, season.duration = 1))
plot(shoplifting_impact, c( "pointwise", "cumulative"))
summary(shoplifting_impact)

summary(shoplifting_impact, "report")

## covariates

shoplifting_cov_impact <- CausalImpact(shoplifting_merged, pre.period, 
                                       post.period, 
                                       model.args = list(nseasons = 7, season.duration = 1))
plot(shoplifting_cov_impact, c( "pointwise", "cumulative"))
summary(shoplifting_cov_impact)

summary(shoplifting_cov_impact, "report")



##''''''''''''''''''''''' THEFT '''''''''''''''''''''''''''''''''''## 


## univariate

theft_impact <- CausalImpact(theft_ts, pre.period, 
                             post.period, 
                             model.args = list(nseasons = 7, season.duration = 1))
plot(theft_impact, c( "pointwise", "cumulative"))
summary(theft_impact)

summary(theft_impact, "report")

## covariates

theft_cov_impact <- CausalImpact(theft_merged, pre.period, 
                                 post.period, 
                                 model.args = list(nseasons = 7, season.duration = 1))
plot(theft_cov_impact, c( "pointwise", "cumulative"))
summary(theft_cov_impact)

summary(theft_cov_impact, "report")





##''''''''''''''''''''''' STOLEN VEHICLE '''''''''''''''''''''''''''''''''''## 

## univariate

vehicle_impact <- CausalImpact(vehicle_ts, pre.period, 
                               post.period, 
                               model.args = list(nseasons = 7, season.duration = 1))
plot(vehicle_impact, c( "pointwise", "cumulative"))
summary(vehicle_impact)

summary(vehicle_impact, "report")

## covariates

vehicle_cov_impact <- CausalImpact(vehicle_merged, pre.period, 
                                   post.period, 
                                   model.args = list(nseasons = 7, season.duration = 1))
plot(vehicle_cov_impact, c( "pointwise", "cumulative"))
summary(vehicle_cov_impact)

summary(vehicle_cov_impact, "report")

##''''''''''''''''''''''' HOMICIDE '''''''''''''''''''''''''''''''''''## 

# univariate first version
homicide_first_impact <- CausalImpact(homicide_first, pre.period, 
                                      post.period, 
                                      model.args = list(nseasons = 7, season.duration = 1))
plot(homicide_first_impact, c( "pointwise", "cumulative"))
summary(homicide_first_impact)


# covariate second version
homicide_first_merged_impact <-CausalImpact(homicide_first_merged, pre.period, 
                                            post.period, 
                                            model.args = list(nseasons = 7, season.duration = 1))
plot(homicide_first_merged_impact, c( "pointwise", "cumulative"))
summary(homicide_first_merged_impact)

# univariate
homicide_impact <- CausalImpact(homicide_ts, pre.period, 
                                post.period, 
                                model.args = list(nseasons = 7, season.duration = 1))
plot(homicide_impact, c( "pointwise", "cumulative"))
summary(homicide_impact)

summary(homicide_impact, "report")

## covariates

homicide_cov_impact <- CausalImpact(homicide_merged, pre.period, 
                                    post.period, 
                                    model.args = list(nseasons = 7, season.duration = 1))
plot(homicide_cov_impact, c( "pointwise", "cumulative"))+theme_bw(base_size=20)
summary(homicide_cov_impact)

summary(homicide_cov_impact, "report")

##''''''''''''''''''''''' OVERALL CRIMES '''''''''''''''''''''''''''''''''''##
## univariate

all_impact <- CausalImpact(la_ts, pre.period, 
                           post.period, 
                           model.args = list(nseasons = 7, season.duration = 1))
plot(all_impact, c( "pointwise", "cumulative"))
summary(all_impact)

summary(all_impact, "report")

## covariates

all_cov_impact <- CausalImpact(merged, pre.period, 
                               post.period, 
                               model.args = list(nseasons = 7, season.duration = 1))
plot(all_cov_impact, c( "pointwise", "cumulative"))
summary(all_cov_impact)

summary(all_cov_impact, "report")
