
# assuming all your data is in the same format
process_test_samples <- function (file) {

  # this is the line you're already familiar with that reads in your data
  df <- read.table(file, fill = TRUE, header = FALSE, skip = 46,
                   skipNul = TRUE, fileEncoding="UTF-16LE")

  # assigns column names
  colnames(df) <- c("time", "temp", "weight", "deriv")

  # It looks like the initial mass is stored on line 12, so this line just
  # reads that so it can be passed into the next function automatically
  init_mass <- read.table(file, nrows = 1, fill = TRUE, header = FALSE, skip = 11,
                          skipNul = TRUE, fileEncoding="UTF-16LE")[,2]

  # process data given the initial mass, and column names given
  tmp <- mixchar::process(df, init_mass = init_mass,
                          temp = 'temp',
                          mass = 'weight')

  # return this object
  tmp
}
