library("testthat")


test_that("Simple era construction", {
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei"), 
                     observationPeriodId = c(1,1), 
                     conceptId = c(10,11),
                     startDay = c(50,25),
                     endDay = c(50,75))
  result <- convertToSccs.data.frame(cases, eras)
  expect_equal(result$outcomes$rowId, c(0,1))
  expect_equal(result$outcomes$stratumId, c(1,1))
  expect_equal(result$outcomes$time, c(50,51))
  expect_equal(result$outcomes$y, c(0,1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(11))
})

test_that("One day era", {
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei"), 
                     observationPeriodId = c(1,1), 
                     conceptId = c(10,11),
                     startDay = c(50,25),
                     endDay = c(50,25))
  result <- convertToSccs.data.frame(cases, eras)
  expect_equal(result$outcomes$rowId, c(0,1))
  expect_equal(result$outcomes$stratumId, c(1,1))
  expect_equal(result$outcomes$time, c(100,1))
  expect_equal(result$outcomes$y, c(1,0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(11))
})

test_that("Merging overlapping eras", {
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei","hei"), 
                     observationPeriodId = c(1,1,1), 
                     conceptId = c(10,11,11),
                     startDay = c(50,25,70),
                     endDay = c(50,75,80))
  result <- convertToSccs.data.frame(cases, eras)
  expect_equal(result$outcomes$rowId, c(0,1))
  expect_equal(result$outcomes$stratumId, c(1,1))
  expect_equal(result$outcomes$time, c(45,56))
  expect_equal(result$outcomes$y, c(0,1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(11))
})

test_that("Concomitant drug use", {
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei","hei"), 
                     observationPeriodId = c(1,1,1), 
                     conceptId = c(10,11,12),
                     startDay = c(50,25,70),
                     endDay = c(50,75,70))
  result <- convertToSccs.data.frame(cases, eras)
  expect_equal(result$outcomes$rowId, c(0,1,2))
  expect_equal(result$outcomes$stratumId, c(1,1,1))
  expect_equal(result$outcomes$time, c(50,50,1))
  expect_equal(result$outcomes$y, c(0,1,0))
  expect_equal(result$covariates$rowId, c(1,2,2))
  expect_equal(result$covariates$stratumId, c(1,1,1))
  expect_equal(result$covariates$covariateId, c(11,11,12))
})

test_that("Concomitant drug use (3 drugs", {
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hoi","hei","hei","hei"), 
                     observationPeriodId = c(1,1,1,1,1), 
                     conceptId = c(10,9,11,12,13),
                     startDay = c(50,85,25,70,70),
                     endDay = c(NA,NA,75,80,77))
  result <- convertToSccs.data.frame(cases, eras)
  expect_equal(result$outcomes$rowId, c(0,0,1,1,2,2,3,3,4,4))
  expect_equal(result$outcomes$stratumId, c(1,1,1,1,1,1,1,1,1,1))
  expect_equal(result$outcomes$time, c(45,45,45,45,3,3,2,2,6,6))
  expect_equal(result$outcomes$y, c(1,0,0,1,0,0,0,0,0,0))
  expect_equal(result$covariates$rowId, c(1,2,3,3,4,4,4))
  expect_equal(result$covariates$stratumId, c(1,1,1,1,1,1,1))
  expect_equal(result$covariates$covariateId, c(11,12,12,13,11,12,13))
})

f <- function(){
  writeLines(paste("expect_equal(result$outcomes$rowId, c(", paste(result$outcomes$rowId, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$outcomes$stratumId, c(", paste(result$outcomes$stratumId, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$outcomes$time, c(", paste(result$outcomes$time, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$outcomes$y, c(", paste(result$outcomes$y, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$covariates$rowId, c(", paste(result$covariates$rowId, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$covariates$stratumId, c(", paste(result$covariates$stratumId, collapse=","),"))",sep=""))
  writeLines(paste("expect_equal(result$covariates$covariateId, c(", paste(result$covariates$covariateId, collapse=","),"))",sep=""))
}