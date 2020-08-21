context("Get Terra IDs")
test_that( "getTerraID", {
  expect_equal(
    getValues(demo_rec@reckoning, "terra-submission-id"),
    "terra-196d3163-4eef-46e8-a7e6-e71c0012003d")
})

