#Test for aggregate to england function
test_that("Test that the number for England prevalence is equal to 15.6", 
                      {CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
                      england_prevalence <- aggregate_prevalence_to_England(CCG_prevalence)
                      expect_equal(england_prevalence, 15.6)})

#Test for aggregate to region function
test_that("Test that the number of  NHS regions is 14",
                      {CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
                      region_prevalence <- aggregate_prevalence_to_region(CCG_prevalence)
                      number_of_rows <- nrow(region_prevalence)
                      expect_equal(number_of_rows, 14)})

#Test for function to mainpulate regions to match shapefile
#Our dataset had 14 regions but the shapefile combined two of these (greater manchester and lancashire) so only had 13 regions
#We created a function to combine these two regions in our dataset so we were left with 13 regions, matching the shapefile
test_that("Test that the number of  NHS regions is 13",
          {CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
          region_prevalence <- aggregate_prevalence_to_region(CCG_prevalence)
          thirteen_level_NHS_regional_prevalence <- manipulate_regions_for_shapefile(region_prevalence)
          number_of_rows <- nrow(thirteen_level_NHS_regional_prevalence)
          expect_equal(number_of_rows, 13)})