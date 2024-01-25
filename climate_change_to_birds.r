
# Load in the tidyverse, raster, and sf packages
library(tidyverse)
library(raster)
library(sf)

# Read the climate data from an rds file
climate <- read_rds("datasets/climate_raster.rds")

# Have a look at the variables in the climate data
colnames(climate)

# Convert to SpatialPixelDataFrame for plotting
climate_df <- mutate(
  .data = climate, 
  rasters = map(
    .x = rasters, 
    ~ as_tibble(as(.x, "SpatialPixelsDataFrame")))) %>%
  unnest(cols = c(rasters))

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

check_climate <- read_rds("datasets/climate_raster.rds")

run_tests({
    test_that("the correct package is loaded", {
        expect_true("tidyverse" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("raster" %in% .packages(), info = "Did you load the raster package?")
        expect_true("sf" %in% .packages(), info = "Did you load the sf package?")
    })
    test_that("the dataset is loaded correctly", {
        expect_true(
            exists("climate"),
            info = "Variable `climate` is not defined, did you import the rds file correctly?")
        expect_identical(
            colnames(climate), colnames(check_climate), 
            info = "Column names of the climate dataset do not look right. Did you import it with the read_rds() function?")
        expect_identical(
            climate, check_climate, 
            info = "The climate dataset contains the wrong values. Did you import the rds file correctly?")
    })
})

library(ggthemes)

# Filter the data to plot
ggp_temperature <- climate_df %>%
  filter(decade %in% c(1970,2010)) %>%
  # Create the plot
  ggplot(aes(x = x, y = y)) + geom_tile(aes(fill = minimum.temperature)) +
  # Style the plot with options ideal for maps
  theme_map() + coord_equal() +
  facet_grid(~ decade) + scale_fill_distiller(palette = "Spectral") + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Minimum of Average Monthly Temperature (Celsius)", caption = 'Source: MetOffice UK')

# Display the map
ggp_temperature

# One or more tests of the student's code
check_climate_df <- mutate(
  .data = check_climate, 
  rasters = map(
    .x = rasters, 
    ~ as_tibble(as(.x, "SpatialPixelsDataFrame")))) %>%
  unnest()

check_ggp_temperature <- check_climate_df %>%
  filter(decade %in% c(1970, 2010)) %>%
  ggplot(aes(x = x, y = y)) + geom_tile(aes(fill = minimum.temperature)) +
  theme_map() + coord_equal() + 
  facet_grid(~ decade) + scale_fill_distiller(palette = "Spectral") + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Minimum temperature (Celsius)", caption = 'Source: MetOffice UK')

run_tests({
    test_that("the plot was generated", {
        expect_true(
            all(class(ggp_temperature) == c("gg", "ggplot")),
            info = "Did you create the plot?")
    })
    test_that("the plot contains the correct data", {
        expect_named(
            object = ggp_temperature[["data"]],
            expected = c('decade', 'minimum.temperature', 'maximum.temperature', 'rainfall', 'wind.speed', 'snow.lying', 'air.frost', 'x', 'y'),
            info = "did you create the plot with the correct input data?")
    })
    test_that("the theme is set correctly", {
        expect_identical(
            check_ggp_temperature$theme, ggp_temperature$theme,
            info = "the theme settings of the plot are not as expected.\nMake sure you style the plot as a map and maintain an equal ratio between x and y values.")
            
    })

})

library(rgbif)
source("datasets/occ_search.R")

# Call the API to get the occurrence records of this species
gbif_response <- occ_search(
  scientificName = "Loxia scotica", country = "GB",
  hasCoordinate = TRUE, hasGeospatialIssue = FALSE, limit = 2000)

# Inspect the class and names of gbif_response
  class(gbif_response)
  names(gbif_response)

# Print the first six lines of the data element in gbif_response
  head(gbif_response[["data"]])

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

run_tests({
    test_that("the string is correct", {
        expect_true(
            exists("gbif_response"), 
            info = "Could not find gbif_response.\nDid you call the GBIF API with occ_search()?")
        expect_true(
            class(gbif_response) == "gbif",
            info = "gbif_response doesn't look like a GBIF API response object.\nDid you source the occ_search function and did you run it with the input parameters that were already provided?")
    })
})

library(lubridate)

birds_dated <- mutate(
  .data = gbif_response$data,
  # Create a new column specifying the decade of observation
  decade = ymd_hms(eventDate) %>% round_date("10y") %>% year())

birds_cleaned <- birds_dated %>%
  filter(
    issues == "" &
    str_detect(license, "http://creativecommons.org/") &
    # No records before 1970s decade or after 2010s decade
    decade >=1970 & decade <= 2010
  ) %>%
  transmute(decade = decade, x = decimalLongitude, y = decimalLatitude) %>%
  arrange(decade)

check_birds_dated <- mutate(
  .data = gbif_response$data,
  # Create a new column specifying the decade of observation
  decade = ymd_hms(eventDate) %>% round_date("10y") %>% year())

check_birds_cleaned <- check_birds_dated %>%
  filter(
    issues == "" &
    str_detect(license, "http://creativecommons.org/") &
    # No records before 1970s decade or after 2010s decade
    decade >= 1970 & decade <= 2010
  ) %>%
  transmute(decade = decade, x = decimalLongitude, y = decimalLatitude) %>%
  arrange(decade)

run_tests({
    test_that("decade column is as expected", {
        expect_true(
            all(birds_dated[["decade"]] == check_birds_dated[["decade"]] | is.na(birds_dated[["decade"]]) & is.na(check_birds_dated[["decade"]])), 
            info = "Something is wrong with the newly created decade column.\nMake sure you use the right column with ymd_hms() to create it.")
    })
    test_that("occurrences are filtered by date", {
        expect_true(
            min(birds_cleaned[["decade"]]) == 1970,
            info = "Minimum date is earlier than expected.\nAre you sure you filtered the date range as specified in the instructions?")
        expect_true(
            max(birds_cleaned[["decade"]]) == 2010,
            info = "Maximum date is later than expected.\nAre you sure you filtered the date range as specified in the instructions?")
        expect_equal(
            unique(birds_cleaned[["decade"]]), c(1970, 1980, 1990, 2000, 2010),
            info = "The range of decades with data is not as expected.\nAre you sure you filtered the date range as specified in the instructions?")

    })
})

# "Nest" the bird data
birds_nested <- birds_cleaned %>%
    group_by(decade) %>%
    nest(.key = "presences")

head(birds_nested)

# Calculate the total number of records per decade
birds_counted <- birds_nested %>%
  mutate(n = map_dbl(.x = presences, .f = nrow))

head(birds_counted)

# "Nest" the bird data
check_birds_nested <- check_birds_cleaned %>%
  group_by(decade) %>% 
  nest(.key = "presences")

# Calculate the total number of records per decade
check_birds_counted <- check_birds_nested %>%
  mutate(n = map_dbl(.x = presences, .f = nrow))

run_tests({
    test_that("birds_nested was created", {
        expect_true(
            exists("birds_nested"),
            info = "Could not find birds_nested. Did you create it from birds_cleaned as instructed?")
    })
    test_that("birds_nested is as expected", {
        expect_true(
            nrow(birds_nested) == 5,
            info = "birds_nested doesn't have the expected number of rows\nDid you nest the data grouped by decade?")
        expect_true(
            ncol(birds_nested) == 2,
            info = "birds_nested doesn't have the expected number of columns.\nDid you create the columns decade and presences?")
        expect_true(
            all(colnames(birds_nested) %in% c("decade", "presences")),
            info = "The column names of birds_nested aren't named as expected")
    })
    test_that("birds_counted was created", {
        expect_true(
            exists("birds_counted"),
            info = "Could not find birds_counted Did you create it from birds_cleaned as instructed?")
    })
    test_that("columns of birds 1", {
        expect_true(
            nrow(birds_counted) == 5,
            info = "birds_counted doesn't have the expected number of rows\nDid you nest the data grouped by decade?")
    })
    test_that("columns of birds 2", {
        expect_true(
            ncol(birds_counted) == 3,
            info = "birds_counted doesn't have the expected number of columns.\nDid you create the columns decade and presences?")
    })
    test_that("columns of birds 3", {
        expect_true(
            all(colnames(birds_counted) %in% c("decade", "presences", "n")),
            info = "The column names of birds_counted aren't named as expected")
    })
})

# Define geographical projections
proj_latlon <- st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj_ukgrid <- st_crs("+init=epsg:27700")

# Convert records to spatial points and project them
birds_presences <- mutate(birds_counted,
  presences = map(presences, ~ .x %>%
    # Specify the current projection
    st_as_sf(coords = c("x", "y"), crs = proj_latlon) %>%
    # Transform to new projection
    st_transform(crs = proj_ukgrid)))

check_proj_latlon <- st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
check_proj_ukgrid <- st_crs("+init=epsg:27700")

check_birds_presences <- mutate(check_birds_counted,
  presences = map(presences, ~ .x %>%
    # Specify the current projection
    st_as_sf(coords = c("x", "y"), crs = check_proj_latlon) %>%
    # Transform to new projection
    st_transform(crs = check_proj_ukgrid)))

run_tests({
    test_that("CRS strings were created", {
        expect_true(
            exists("proj_latlon"),
            info = "Could not find proj_latlon. Did you run the code to create it?")
        expect_true(
            exists("proj_ukgrid"),
            info = "Could not find proj_ukgrid. Did you run the code to create it?")
    })
    test_that("proj_latlon is correct", {
        expect_s3_class(proj_ukgrid, class = "crs")
        expect_identical(
            proj_latlon, check_proj_latlon,
            info = "The content of proj_ukgrid isn't as expected. Have you changed the original CRS string?")
    })
    test_that("proj_ukgrid is correct", {
        expect_s3_class(proj_ukgrid, class = "crs")
        expect_identical(
            proj_ukgrid, check_proj_ukgrid,
            info = "The content of proj_ukgrid isn't as expected. Have you changed the original CRS string?")
    })
    
    test_that("birds_presences was creates", {
        expect_true(
            exists("birds_presences"),
            info = "Could not find bird_presences.\nDid you successfully create it?")
    })
    test_that("birds_presences is correct", {
        expect_named(
            birds_presences, names(check_birds_presences),
            info = "Columns of birds_presences are not as expected.\nDid you use the right CRS definitions to project and transform the data?")
    })
})

# Combine the bird data and the climate data in one data frame
birds_climate <- full_join(birds_presences, climate, by = "decade")

presence_data <- map2_df(
  .x = birds_climate[["rasters"]],
  .y = birds_climate[["presences"]],
  # extract the raster values at presence locations
  ~ raster::extract(x= .x, y= .y) %>% 
    as_tibble() %>% 
    mutate(observation = "presence"))

# Combine the bird data and the climate data in one data frame
check_birds_climate <- full_join(check_birds_presences, check_climate, by = "decade")

check_presence_data <- map2_df(
  .x = check_birds_climate$rasters,
  .y = check_birds_climate$presences,
  # extract the raster values at presence locations
  ~ raster::extract(x = .x, y = .y) %>%
    as_tibble() %>% 
    mutate(observation = "presence"))


run_tests({
    test_that("birds_climate is as expected", {
        expect_true(
            exists("birds_climate"),
            info = "Could not find birds_climate.\nDid you successfully run the code to create it?")
        expect_named(
            object = birds_climate,
            expected = names(check_birds_climate),
            info = "Column names of birds_climate are not as expected.\nDid you join the right data frames?")
        expect_identical(
            birds_climate, check_birds_climate,
            info = "birds_presences is not as expected.\nDid you use the right CRS definitions to project and transform the data?")

    })
    test_that("presence_data is as expected", {
        expect_true(
            exists("presence_data"),
            info = "Could not find presence_data\nDid you successfully run the code to create it?")
        expect_named(
            object = presence_data,
            expected = names(check_presence_data),
            info = "Column names of presence_data are not as expected.\nDid you use the right functions to create it?")
        expect_identical(
            presence_data, check_presence_data,
            info = "presence_data is not as expected.\nDid you correctly extract the raster values ayt presence locations?")
    })
})

# Define helper function for creating pseudo-absence data
create_pseudo_absences <- function(rasters, n, ...) {
    set.seed(12345)
    sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
    raster::extract(rasters, .) %>% as_tibble() %>%
    mutate(observation = "pseudo_absence")
}

# Create pseudo-absence proportional to the total number of records per decade
pseudo_absence_data <- pmap_df(.l = birds_climate, .f = create_pseudo_absences)

# Combine the two datasets
model_data <- bind_rows(presence_data, pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()

# Define helper function for creating pseudo-absence data
check_create_pseudo_absences <- function(rasters, n, ...) {
    set.seed(12345)
    sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
    raster::extract(rasters, .) %>% as_tibble() %>%
    mutate(observation = "pseudo_absence")
}

# Create pseudo-absence proportional to the total number of records per decade
check_pseudo_absence_data <- pmap_df(.l = check_birds_climate, .f = check_create_pseudo_absences)

# Combine the two datasets
check_model_data <- bind_rows(check_presence_data, check_pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()

run_tests({
    test_that("helper function was defined as specified", {
        expect_identical(
            object = create_pseudo_absences,
            expected = check_create_pseudo_absences,
            info = "The helper function is not defined as expected. Did you replace the dots (...) with something? They need to remain unchanged in this case.")
    })
    test_that("absence data was generated", {
        expect_true(
            exists("pseudo_absence_data"),
            info = "Could not find pseudo_absence_data\nDid you successfully run the code to create it?")
        expect_named(
            object = pseudo_absence_data,
            expected = c('minimum.temperature', 'maximum.temperature', 'rainfall', 'wind.speed', 'snow.lying', 'air.frost', 'observation'),
            info = "Column names of pseudo_absence_data are not as expected.\nDid you use the right map function?")
        expect_identical(
            pseudo_absence_data, check_pseudo_absence_data,
            info = "pseudo_absence_data is not as expected.\nDid you correctly map to the helper function you defined earlier?")
    })
    test_that("model data was collected", {
        expect_true(
            exists("model_data"),
            info = "Could not find model_data\nDid you successfully run the code to create it?")
        expect_named(
            object = model_data,
            expected = names(check_model_data),
            info = "Column names of model_data are not as expected.\nDid you use the right function to bind the rows of the two data frames, presence_data and pseudo_absence_data?")
        expect_identical(
            model_data, check_model_data,
            info = "model_data is not as expected.\nDid you use the right function to bind the rows of the two data frames, presence_data and pseudo_absence_data?")
    })
})

# Load caret and set a reproducible seed
library(caret)
set.seed(12345)

# Create a tuning grid with sets of hyperparameters to try
tuneGrid <- expand.grid(alpha = c(0, 0.5, 1), lambda = c(.003, .01, .03, .06))

# Create settings for model training
trControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 1,
  classProbs = TRUE, verboseIter = FALSE, summaryFunction = twoClassSummary)

# Fit a statistical model to the data and plot
model_fit <- train(
  observation ~ ., data = model_data,
  method = "glmnet", family = "binomial", metric = "ROC",
  tuneGrid = tuneGrid, trControl = trControl)

plot(model_fit)

# Load caret and set a reproducible seed
library(caret)
set.seed(12345)

# Create a tuning grid with sets of hyperparameters to try
check_tuneGrid <- expand.grid(alpha = c(0, 0.5, 1), lambda = c(.003, .01, .03, .06))

# Create settings for model training
check_trControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 1,
  classProbs = TRUE, verboseIter = FALSE, summaryFunction = twoClassSummary)

# Fit a statistical model to the data and plot
check_model_fit <- train(
  observation ~ ., data = check_model_data,
  method = "glmnet", family = "binomial", metric = "ROC",
  tuneGrid = check_tuneGrid, trControl = check_trControl)


run_tests({
    test_that("tuneGrid and trControl were specified correctly", {
        expect_true(
            exists("tuneGrid"),
            info = "Could not find tuneGrid.\nDid you successfully run the code to create it?")
        expect_named(
            object = tuneGrid,
            expected = names(check_tuneGrid),
            info = "Names of tuneGrid are not as expected.\nDid you specify the correct names for the hyperparameters?")
        expect_identical(
            object = tuneGrid,
            expected = check_tuneGrid,
            info = "tuneGrid is not as expected.\nDid you change the specified values?")

        expect_true(
            exists("trControl"),
            info = "Could not find trControl\nDid you successfully run the code to create it?")
        expect_named(
            object = trControl,
            expected = names(check_trControl),
            info = "Names of trControl are not as expected.\nDid you call the right function to create it?")
        expect_identical(
            object = trControl,
            expected = check_trControl,
            info = "trControl is not as expected.\nDid you change the specified values?")
    })
    
    test_that("model was fitted successfully", {
        expect_true(
            exists("model_fit"),
            info = "Could not find model_fit\nDid you successfully run the code to create it?")
        expect_named(
            object = model_fit,
            expected = names(check_model_fit),
            info = "Names of model_fit are not as expected.\nDid you change the model type?")
        expect_identical(
            object = c(model_fit$modelType, model_fit$method, model_fit$terms),
            expected = c(check_model_fit$modelType, check_model_fit$method, check_model_fit$terms),
            info = "model_fit is not as expected.\nDid you run it with the right tuneGrid, trControl, and function inputs?")
    })
})

# Use our model to make a prediction
climate_df[["prediction"]] <- predict(
    object = model_fit,
    newdata = climate_df,
    type = "prob")[["presence"]]

head(climate_df)

check_climate_df$prediction <- predict(
    object = check_model_fit,
    newdata = check_climate_df,
    type = "prob")[["presence"]]

run_tests({
test_that("prediction column was created correctly", {
        expect_true(
            !is.null(climate_df[["prediction"]]),
            info = "Could not find column prediction in climate_df\nDid you successfully run the code to create it?")
        expect_identical(
            object = climate_df[["prediction"]],
            expected = check_climate_df[["prediction"]],
            info = "prediction values are not as expected.\nDid you use the right model and data to predict?")
    })
})

library(viridis)

# Create the plot
ggp_changemap <- ggplot(data= climate_df, aes(x = x,y = y, fill = prediction)) + 
  geom_tile() +
  # Style the plot with the appropriate settings for a map
  theme_map() + coord_equal() +
  scale_fill_viridis(option = "A") + theme(legend.position = "bottom") +
  # Add faceting by decade
  facet_grid(~ decade) +
  labs(title = 'Habitat Suitability', subtitle = 'by decade',
       caption = 'Source:\nGBIF data and\nMetOffice UK climate data',
       fill = 'Habitat Suitability [0 low - high 1]')

# Display the plot
ggp_changemap

ggp_changemap_check <- ggplot(data = check_climate_df, aes(x = x, y = y, fill = prediction)) +
  geom_tile() + theme_map() + coord_equal() + scale_fill_viridis(option = "A") +
  theme(legend.position = "bottom") + facet_grid(~ decade) +
  labs(title = 'Habitat Suitability', subtitle = 'by decade',
       caption = 'Source:\nGBIF data and\nMetOffice UK climate data',
       fill = 'Habitat Suitability [0 low - high 1]')

run_tests({
    test_that("the correct package is loaded", {
        expect_true("viridis" %in% .packages(), info = "Did you load the viridis package?")
    })
    test_that("the plot was generated", {
        expect_true(
            all(class(ggp_changemap) == c("gg", "ggplot")),
            info = "Did you create the plot?")
    })
    test_that("the plot is set up correctly", {
        expect_equal(
            ggp_changemap$coordinates, ggp_changemap_check$coordinates,
            info = "The theme settings of the plot are not as expected.\nMake sure you style the plot as a map and maintain an equal ratio between x and y values.")
        expect_equal(
            ggp_changemap$facet, ggp_changemap_check$facet,
            info = "The faceting of the plot seems to be set up incorrectly. Did you specify to facet across rows using decade?")
            
    })
    
})
