context("ggplotUtils tests")
library(standardPrintOutput)
library(ggplot2)
library(dplyr)
library(tibble)

test_that("save figure", {
  theme_set(defaultFigureLayout())
  fig <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  expect_s3_class(fig, 'gg')
  fig %>% saveHalfPageFigure("cars")
  fs::file_delete("cars.png")
  fs::file_delete("cars.pdf")
  fs::file_delete("cars.eps")
})

test_that("save draft figure", {
  theme_set(defaultFigureLayout())
  fig <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + watermark()
  expect_s3_class(fig, 'gg')
  fig %>% saveHalfPageFigure("draft_cars")
  fs::file_delete("draft_cars.png")
  fs::file_delete("draft_cars.pdf")
  fs::file_delete("draft_cars.eps")
})

test_that("save table", {
  tab <- mtcars %>% rownames_to_column() %>% group_by(gear,carb) %>% mergeCells()
  expect_true(is_huxtable(tab))
  tab %>% saveTable("table_cars")
  fs::file_delete("table_cars.html")
  fs::file_delete("table_cars.png")
  fs::file_delete("table_cars.pdf")
})
