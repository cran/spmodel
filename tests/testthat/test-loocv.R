load(file = system.file("extdata", "exdata.rda", package = "spmodel"))
load(system.file("extdata", "exdata_poly.rda", package = "spmodel"))
load(system.file("extdata", "exdata_Mpoly.rda", package = "spmodel"))

test_local <- FALSE # FALSE for CRAN

#### CRAN tests
test_that("loocv works geo", {
  spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord)
  expect_vector(loocv(spmod))
  expect_vector(loocv(spmod, local = TRUE))
  # cores 2 for cran check
  spmod <- spautor(y ~ x, exdata_poly, "car")
  expect_vector(loocv(spmod))
})

#### local tests
if (test_local) {

  test_that("loocv works geo", {
    spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord)
    expect_vector(loocv(spmod))
    expect_vector(loocv(spmod, local = TRUE))
    # cores 2 for cran check
    if (test_local) { ##### local test
      expect_vector(loocv(spmod, local = list(parallel = TRUE, ncores = 2)))
      expect_equal(length(loocv(spmod, cv_predict = TRUE)), 2)
      expect_equal(length(loocv(spmod, cv_predict = TRUE, local = TRUE)), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE)), 3)
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE, local = TRUE)), 3)
    if (test_local) { ##### local test
      expect_equal(length(loocv(spmod, se.fit = TRUE)), 2)
      expect_equal(length(loocv(spmod, se.fit = TRUE, local = TRUE)), 2)
    }
    # cores 2 for cran check
    if (test_local) { ##### local test
      expect_equal(length(loocv(spmod, cv_predict = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
      expect_equal(length(loocv(spmod, cv_predict = TRUE, local = list(parallel = TRUE, method = "all", ncores = 2))), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 3)
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE, local = list(parallel = TRUE, method = "all", ncores = 2))), 3)
    if (test_local) { ##### local test
      expect_equal(length(loocv(spmod, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
      expect_equal(length(loocv(spmod, se.fit = TRUE, local = list(parallel = TRUE, method = "all", ncores = 2))), 2)
    }

    # random effects
    spmod <- splm(y ~ x, exdata, "exponential", xcoord, ycoord, random = ~group)
    expect_vector(loocv(spmod))
    expect_vector(loocv(spmod, local = TRUE))
  })

  test_that("loocv works auto", {
    spmod <- spautor(y ~ x, exdata_poly, "car")
    expect_vector(loocv(spmod))
    # cores 2 for cran check
    if (test_local) {
      expect_vector(loocv(spmod, local = list(parallel = TRUE, ncores = 2)))
      expect_equal(length(loocv(spmod, cv_predict = TRUE)), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE)), 3)
    # cores 2 for cran check
    if (test_local) {
      expect_equal(length(loocv(spmod, cv_predict = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
      expect_equal(length(loocv(spmod, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 3)


    # random effects
    spmod <- spautor(y ~ x, exdata_poly, "car", random = ~group)
    expect_vector(loocv(spmod))
    expect_vector(loocv(spmod, local = TRUE))


    # missing data
    spmod <- spautor(y ~ x, exdata_Mpoly, "car")
    expect_vector(loocv(spmod))
    # cores 2 for cran check
    if (test_local) {
      expect_vector(loocv(spmod, local = list(parallel = TRUE, ncores = 2)))
      expect_equal(length(loocv(spmod, cv_predict = TRUE)), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE)), 3)
    # cores 2 for cran check
    if (test_local) {
      expect_equal(length(loocv(spmod, cv_predict = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
      expect_equal(length(loocv(spmod, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 2)
    }
    expect_equal(length(loocv(spmod, cv_predict = TRUE, se.fit = TRUE, local = list(parallel = TRUE, ncores = 2))), 3)

  })

}
