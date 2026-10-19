test_that("make_cache_key is stable", {
  k1 <- make_cache_key("hello", "sys", "gpt-4")
  k2 <- make_cache_key("hello", "sys", "gpt-4")
  expect_equal(k1, k2)
  expect_true(nchar(k1) == 64)
})

test_that("chatcache_init creates database", {
  tmp <- tempfile()
  dir.create(tmp)
  con <- chatcache_init(cache_dir = tmp)
  expect_true(DBI::dbIsValid(con))
  expect_true(file.exists(file.path(tmp, "chatcache.sqlite")))
  DBI::dbDisconnect(con)
})
