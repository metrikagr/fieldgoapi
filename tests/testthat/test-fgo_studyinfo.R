test_that("test get studyinfo with complete data", {

    # Set seed for reproducibility
    set.seed(123)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Combine the two named lists into a single named list
    cont <- list(Status = status, data = values)
    cont <- jsonlite::toJSON(cont)
    out <- fieldgoapi::as_data_frame_fgo(cont)
    testthat::expect_equal(10, nrow(out))
    testthat::expect_equal("data.frame", class(out))
})


test_that("test get studyinfo with NA values", {

    # Set seed for reproducibility
    set.seed(123)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Randomly select 3 positions in the second list to replace with NA
    na_positions <- sample(1:10, 5)

    for(pos in na_positions) {
        values[[pos]] <- NA
    }

    # Combine the two named lists into a single named list
    cont <- list(Status = status, data = values)
    cont <- jsonlite::toJSON(cont)
    out <- fieldgoapi::as_data_frame_fgo(cont)
    testthat::expect_equal(10, nrow(out))
    testthat::expect_equal("data.frame", class(out))
})


test_that("test get studyinfo with all NA values", {

    # Set seed for reproducibility
    set.seed(123)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Randomly select 3 positions in the second list to replace with NA
    na_positions <- sample(1:10, 10)

    for(pos in na_positions) {
        values[[pos]] <- NA
    }

    # Combine the two named lists into a single named list
    cont <- list(Status = status, data = values)
    cont <- jsonlite::toJSON(cont)
    out <- fieldgoapi::as_data_frame_fgo(cont)
    testthat::expect_equal(10, nrow(out))
    testthat::expect_equal("data.frame", class(out))
})
