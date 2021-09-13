test_that("Test the outcome of assign_method", {
  a <- readxl::read_xlsx("testdata.xlsx")
  result <- assign_method(a, method_file = NULL)

  # check output columns
  expect_equal("Method" %in% colnames(result), T)
  expect_equal("C" %in% colnames(result), T)

  # simple
  # This test is dumb
  # need a new one
  expect_equal(unique(result$Method[result$Taxon == "Polychaeta"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Oligochaeta"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Isopoda"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Tanaidacea"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Amphipoda"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Scaphopoda"]), "Cone")
  expect_equal(unique(result$Method[result$Taxon == "Gastropoda"]), "Cone")
  expect_equal(unique(result$Method[result$Taxon == "Bivalvia"]), "LWR")
  expect_equal(unique(result$Method[result$Taxon == "Aplacophora"]), "Cylinder")
  expect_equal(unique(result$Method[result$Taxon == "Echinoidea"]), "Ellipsoid")
  expect_equal(unique(result$Method[result$Taxon == "Ophiuroidea"]), "Cylinder")

  # exceptional
  expect_equal(unique(result$Method[result$Taxon == "Hydrozoa" & result$Note == "Polyp"]), "LWR")
  expect_equal(unique(result$C[result$Taxon == "Hydrozoa" & result$Note == "Polyp"]), 0.385)
  expect_equal(unique(result$Method[result$Taxon == "Hydrozoa" & result$Note == "Colony"]), "Cylinder")
  expect_equal(unique(result$Method[result$Taxon == "Bryozoa" & result$Note == "Colony"]), "Cylinder")
})
