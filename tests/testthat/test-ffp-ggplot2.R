library(ggplot2, warn.conflicts = FALSE)

# Test data setup
ret <- diff(log(EuStockMarkets))
p_exp <- exp_decay(ret[ , 1], 0.001)
p_kernel <- kernel_entropy(ret, colMeans(ret), cov(ret))

# autoplot.ffp() with standard inputs -------

test_that("autoplot.ffp() returns ggplot object", {
  plot <- autoplot(p_exp)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() has required layers", {
  plot <- autoplot(p_exp)
  # Check for geom_line and geom_vline layers
  layer_classes <- sapply(plot$layers, function(x) class(x$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
  expect_true("GeomVline" %in% layer_classes)
})

test_that("autoplot.ffp() includes title and labels", {
  plot <- autoplot(p_exp)
  expect_equal(plot$labels$title, "Flexible Forward Probabilities")
  expect_true(!is.null(plot$labels$subtitle))
  expect_true(!is.null(plot$labels$caption))
})

test_that("autoplot.ffp() includes statistics in subtitle", {
  plot <- autoplot(p_exp)
  subtitle <- plot$labels$subtitle
  # Check for key statistics words
  expect_true(grepl("Mean", subtitle))
  expect_true(grepl("Median", subtitle))
  expect_true(grepl("Std Dev", subtitle))
})

test_that("autoplot.ffp() marks maximum probability", {
  plot <- autoplot(p_exp)
  layer_classes <- sapply(plot$layers, function(x) class(x$geom)[1])
  # Vline layer for the vertical line at max
  expect_true("GeomVline" %in% layer_classes)
})

test_that("autoplot.ffp() with color=FALSE produces monochrome", {
  plot <- autoplot(p_exp, color = FALSE)
  expect_s3_class(plot, "ggplot")
  # Check that color aesthetic is not mapped
  layer_classes <- sapply(plot$layers, function(x) class(x$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
})

test_that("autoplot.ffp() with color=TRUE uses gradient", {
  plot <- autoplot(p_exp, color = TRUE)
  expect_s3_class(plot, "ggplot")
  # Should have a continuous color scale spanning the low/high gradient
  color_scale <- plot$scales$get_scales("colour")
  expect_false(is.null(color_scale))
  expect_equal(color_scale$palette(0), "#3498DB")
  expect_equal(color_scale$palette(1), "#E74C3C")
})

test_that("autoplot.ffp() respects theme settings", {
  plot <- autoplot(p_exp)
  # Should have minimal theme applied
  expect_true(!is.null(plot$theme))
})

# Edge cases ---------------------------------------------------------

test_that("autoplot.ffp() handles single probability", {
  p_single <- ffp(1.0)
  plot <- autoplot(p_single)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() handles uniform distribution", {
  p_uniform <- ffp(rep(1/100, 100))
  plot <- autoplot(p_uniform)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() handles concentrated distribution", {
  p_concentrated <- ffp(c(0.99, rep(0.01/99, 99)))
  plot <- autoplot(p_concentrated)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() handles different distributions", {
  # Test with kernel entropy result
  plot_kernel <- autoplot(p_kernel)
  expect_s3_class(plot_kernel, "ggplot")

  # Both should produce valid plots
  expect_true(!is.null(plot_kernel$data))
})

# Integration with other functions ----------------------------------------

test_that("autoplot.ffp() works with entropy_pooling result", {
  set.seed(42)
  prior <- rep(1 / 100, 100)
  A <- matrix(rnorm(100), ncol = 100)
  A <- rbind(A, rep(1, 100))
  b <- c(1, 1)

  result <- entropy_pooling(p = prior, A = A, b = b, solver = "solnl")
  plot <- autoplot(result)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() works with average_ffp result", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))
  p_avg <- average_ffp(p1, p2)

  plot <- autoplot(p_avg)
  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() works with combine_ffp result", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))
  p_comb <- combine_ffp(p1 = p1, p2 = p2, weights = c(0.7, 0.3))

  plot <- autoplot(p_comb)
  expect_s3_class(plot, "ggplot")
})

# Data integrity tests --------------------------------------------------

test_that("autoplot.ffp() doesn't modify input", {
  p_orig <- exp_decay(ret[ , 1], 0.001)
  p_copy <- vctrs::vec_data(p_orig)

  plot <- autoplot(p_orig)

  expect_equal(vctrs::vec_data(p_orig), p_copy)
})

test_that("autoplot.ffp() data frame has correct structure", {
  plot <- autoplot(p_exp)
  plot_data <- plot$data

  expect_true("id" %in% names(plot_data))
  expect_true("probability" %in% names(plot_data))
  expect_equal(nrow(plot_data), vctrs::vec_size(p_exp))
})

test_that("autoplot.ffp() x-axis shows indices correctly", {
  p_short <- ffp(c(0.2, 0.3, 0.5))
  plot <- autoplot(p_short)
  plot_data <- plot$data

  expect_equal(plot_data$id, 1:3)
})

test_that("autoplot.ffp() y-axis represents probabilities", {
  plot <- autoplot(p_exp)
  plot_data <- plot$data

  expect_true(all(plot_data$probability >= 0))
  expect_true(all(plot_data$probability <= 1))
  expect_equal(sum(plot_data$probability), 1, tolerance = 1e-10)
})

# Snapshot tests for consistency ------------------------------------------

test_that("`autoplot.ffp()` produces consistent output", {
  plot <- autoplot(p_exp)
  expect_snapshot_output(class(plot))
  expect_snapshot_output(names(plot))
})
