library(mw3dlib)
context("rgbToStringColor")

test_that("rgbToStringColor works", {
  expect_equal(rgbToStringColor(1,0,0),"#ff0000")
  expect_equal(rgbToStringColor(0,1,0),"#00ff00")
  expect_equal(rgbToStringColor(0,0,1),"#0000ff")
  expect_equal(rgbToStringColor(0.5,0.5,0.5),"#808080")
  expect_equal(rgbToStringColor(c(1,0,1),c(1,0,0),c(1,0,0.5)),
                                c("#ffffff","#000000","#ff0080"))
})