test_that("test define_ophouroid_size", {
  # a <- readxl::read_xlsx("testdata.xlsx")
  # b <- assign_method(a, method_file = NULL)
  # c <- calculate_biovolume(b)
  #
  # aa <-
  #   c %>%
  #   filter(Taxon == "Ophiuroidea") %>%
  #   define_ophiuroid_size(protocol_ophiuroid = "longest_arm")
  #
  # la <-
  #   c %>%
  #   filter(Taxon == "Ophiuroidea") %>%
  #   define_ophiuroid_size(protocol_ophiuroid = "all_arms")

  a <- data.frame(Taxon = rep("Ophiuroidea", 6),
                  Size = runif(6,min = 1, max = 5),
                  Note = c("Dics-1", rep("Arm-1", 4), "Arm"))
  a <- cbind(Station = c(rep("A", 5), "B"), a)
  aa <- define_ophiuroid_size(data = a, protocol = "all_arms", grouping_variables = "Station")
  la <- define_ophiuroid_size(data = a, protocol = "longest_arm", grouping_variables = "Station")

  expect_equal(length(aa$Size) == 1, TRUE)
  expect_equal(is.numeric(aa$Size), TRUE)

  expect_equal(length(la$Size) == 1, TRUE)
  expect_equal(is.numeric(la$Size), TRUE)

  expect_equal(la$Size > aa$Size, TRUE)
})
