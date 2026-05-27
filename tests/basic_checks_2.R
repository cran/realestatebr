if (interactive()) {
  load_all()

  # Critical errors: always returns the same table.
  # should return different tables
  # Minor issue: this function should support some kind of 'all' or 'full' option to return the entire dataset.
  d1 <- get_dataset("bcb_realestate")
  d2 <- get_dataset("bcb_realestate", "application")
  d3 <- get_dataset("bcb_realestate", "indices")
  d4 <- get_dataset("bcb_realestate", "sources")
  d5 <- get_dataset("bcb_realestate", "units")

  all.equal(d1, d2)
  all.equal(d2, d3)
  all.equal(d3, d4)
  all.equal(d4, d5)

  # Minor issue: duplicate messaging
  # Downloading real estate data from BCB API
  # Downloading real estate data from the Brazilian Central Bank.
  # Minor issue: unwanted naming message
  # New names:
  # • `ivg-r_br` -> `ivg.r_br`
  # • `mvg-r_br` -> `mvg.r_br`
  get_dataset("bcb_realestate", source = "fresh")

  get_dataset("bcb_realestate", "application", source = "fresh")

  # Major issue: the get_dataset("bcb_realestate") function is missing several important tables
  # as a simple workaround we should support the option 'full'/'all' that retuns the entire dataset, allowing the user more control.
  # In the future, we should support all possible tables

  # Critical error: bcb_series is failing with source = 'fresh'
  get_dataset("bcb_series", source = "fresh")

  # Critical error: bcb_series cache is wrong/outdated
  # Minor issue: bcb_series should also support a 'all'/'full' option
  d1 <- get_dataset("bcb_series", table = "credit")
  d2 <- get_dataset("bcb_series", table = "price")
  d3 <- get_dataset("bcb_series", table = "activity")

  # Critical error: these datasets should not be the same
  all.equal(d1, d2)
  # This is ok
  isTRUE(all.equal(d2, d3))

  # Inspecting d3 reveals the table logic isn't working
  head(d3)
  # d3 includes data from several different categories
  # it should only return category == 'activity'
  unique(d3$bcb_category)
}
