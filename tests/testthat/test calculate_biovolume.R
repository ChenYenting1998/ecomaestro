test_that("Test the outcome of calculate_biovolume", {
  a <- readxl::read_xlsx("testdata.xlsx")
  b <- assign_method(a, method_file = NULL)
  c <- calculate_biovolume(b)
  size_LWR <- c %>% filter(Method == "LWR")
  size_con <- c %>% filter(Method == "Cone")
  size_cyl <- c %>% filter(Method == "Cylinder")
  size_ell <- c %>% filter(Method == "Ellipsoid")

  expect_equal(all(is.numeric(c$Size)), TRUE)

  # check cylinder
  expect_equal(all(size_cyl$Size == Cylinder(size_cyl$L, size_cyl$W)), TRUE)
  expect_equal(all(size_ell$Size == Ellipsoid(size_ell$L, size_ell$W)), TRUE)
  expect_equal(all(size_con$Size == Cone(size_con$L, size_con$W)), TRUE)
  expect_equal(all(size_LWR$Size == LWR(size_LWR$L, size_LWR$W, size_LWR$C)), TRUE)
})
