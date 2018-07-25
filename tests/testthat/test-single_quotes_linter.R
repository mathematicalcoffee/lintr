context("single_quotes_linter")
test_that("returns the correct linting", {
  expect_lint('blah', NULL, single_quotes_linter)

  expect_lint('\'blah\'', NULL, single_quotes_linter)

  expect_lint('\'"blah\'', NULL, double_quotes_linter)

  expect_lint('\'blah"\'', NULL, double_quotes_linter)

  expect_lint('\'blah"\'', NULL, double_quotes_linter)

  expect_lint('\'"blah"\'', NULL, single_quotes_linter)

  expect_lint('"\'"', NULL, single_quotes_linter)
  expect_lint('"\'blah\'"', NULL, single_quotes_linter)

  expect_lint('"blah"',
    rex("Only use single-quotes."),
    single_quotes_linter)

  expect_lint('fun("blah")',
    rex("Only use single-quotes."),
    single_quotes_linter)

  expect_lint('{"blah"}',
    rex("Only use single-quotes."),
    single_quotes_linter)
})
