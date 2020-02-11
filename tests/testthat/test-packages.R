test_that("packages not in neuroconductor", {

  expect_error(neuro_install("worstPackageEver", release = "stable",
                             release_repo = "github"))

})