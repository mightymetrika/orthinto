test_that("orthinto works", {
  oio <- orthinto()
  expect_s3_class(oio, "oio")
})
