test_that("test define_ophiuroid_size", {
  a <- data.frame(Taxon = rep("Ophiuroidea", 6),
                  Size = runif(6,min = 1, max = 5),
                  Note = c("Dics-1", rep("Arm-1", 4), "Arm"))
  a <- cbind(Station = c(rep('a',5),'b'), a)

  aa <- define_ophiuroid_size(data = a, protocol = "all_arms", grouping_variables = "Station")
  la <- define_ophiuroid_size(data = a, protocol = "longest_arm", grouping_variables = "Station")

  expect_equal(length(aa$Size) == 1,  TRUE)
  expect_equal(length(la$Size) == 1,  TRUE)
  expect_equal(la$Size> aa$Size ,  TRUE)


})
