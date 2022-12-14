load(file = system.file("extdata", "exdata.rda", package = "spmodel"))
load(system.file("extdata", "exdata_poly.rda", package = "spmodel"))

test_local <- FALSE # FALSE for CRAN

##### CRAN test
test_that("tidy works geo", {
  spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord)
  expect_s3_class(tidy(spmod), "tbl")
})

##### local test
if (test_local) {

  test_that("tidy works geo", {
    spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord)
    expect_s3_class(tidy(spmod), "tbl")
    expect_equal(ncol(tidy(spmod)), 5)
    expect_s3_class(tidy(spmod, effects = "spcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "spcov")), 3)
    expect_null(tidy(spmod, effects = "randcov"))
    expect_s3_class(tidy(anova(spmod)), "tbl")
    expect_equal(ncol(tidy(anova(spmod))), 4)
  })

  test_that("tidy works geo", {
    spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord, random = ~group)
    expect_s3_class(tidy(spmod), "tbl")
    expect_equal(ncol(tidy(spmod)), 5)
    expect_s3_class(tidy(spmod, effects = "spcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "spcov")), 3)
    expect_s3_class(tidy(spmod, effects = "randcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "randcov")), 3)
    expect_s3_class(tidy(anova(spmod)), "tbl")
    expect_equal(ncol(tidy(anova(spmod))), 4)
  })

  test_that("tidy works auto", {
    spmod <- spautor(y ~ x, exdata_poly, "car")
    expect_s3_class(tidy(spmod), "tbl")
    expect_equal(ncol(tidy(spmod)), 5)
    expect_s3_class(tidy(spmod, effects = "spcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "spcov")), 3)
    expect_null(tidy(spmod, effects = "randcov"))
    expect_s3_class(tidy(anova(spmod)), "tbl")
    expect_equal(ncol(tidy(anova(spmod))), 4)
  })

  test_that("tidy works auto", {
    spmod <- spautor(y ~ x, exdata_poly, "car", random = ~group)
    expect_s3_class(tidy(spmod), "tbl")
    expect_equal(ncol(tidy(spmod)), 5)
    expect_s3_class(tidy(spmod, effects = "spcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "spcov")), 3)
    expect_s3_class(tidy(spmod, effects = "randcov"), "tbl")
    expect_equal(ncol(tidy(spmod, effects = "randcov")), 3)
    expect_s3_class(tidy(anova(spmod)), "tbl")
    expect_equal(ncol(tidy(anova(spmod))), 4)
  })
}
