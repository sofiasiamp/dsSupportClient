


test_that("ds.wrapper Errors", {

  case1 <- ds.wrapper(df = "D", ds_function = ds.class, datasources=conns[1:3])

  df_comparison <- data.frame(Server1.class = c("integer", "factor", "integer", "numeric"),
                              Server2.class = c("integer", "factor", "integer", "numeric"),
                              Server3.class = c("integer", "factor", "integer", "numeric"))

  row.names(df_comparison) <- c("ID", "Sex", "Age", "Weight")

  expect_equal(case1, df_comparison)

  ## Case 2: there is a problem with the coding somewhere?
  case2 <- ds.wrapper(df = "D", ds_function = ds.class, datasources = conns[c(1:4)])

  df_comparison2 <- data.frame(Server1.class = c("integer", "factor", "integer", "numeric"),
                               Server2.class = c("integer", "factor", "integer", "numeric"),
                               Server3.class = c("integer", "factor", "integer", "numeric"),
                               Server4.class = c("integer", "factor", "integer", "character"))

  row.names(df_comparison2) <- c("ID", "Sex", "Age", "Weight")

  expect_equal(case2, df_comparison2)


  #case3 <- ds.wrapper(df = "D", ds_function = ds.numNA)

  #### Not true, since the class difference comes to play
  #expect_silent(case3)






})
