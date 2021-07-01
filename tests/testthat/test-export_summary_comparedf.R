context("Test export.summary.comparedf")
test_dir <- gsub("/tests/testthat", "", getwd())
output_path <- file.path(test_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

test_that("Rmd file from export.summary.comparedf", {
  
  file <- "test_rmd"
  mockstudy2 <- mockstudy %>% 
    mutate(age = ifelse(age < 50, age - 5, age), bmi = ifelse(bmi < 20, bmi - 1, bmi)) %>% 
    filter(!is.na(race)) 
  # run the export compare function - save in expected
  sumComp <- summary(arsenal::comparedf(mockstudy,mockstudy2, by = "case", int.as.num = TRUE, factor.as.char = TRUE))
  export(x = sumComp, type = "github", folder = output_path, subfolder = "expected", filename = file, deleteRmd = TRUE)
  # run the export compare function - save in got
  export(x = sumComp, type = "github", folder = output_path, subfolder = "got", filename = file, deleteRmd = TRUE)
  # read the two html files
  md_expected <- paste0(output_path, "/expected/", file, ".md")
  md_got <- paste0(output_path, "/got/", file, ".md")
  # read in the two html files
  exp <- readLines(md_expected)
  got <- readLines(md_got)
  # remove elements for date and author
  # and also the other header lines since the file extension is for some reason missing when run in R cmd check...?
  exp_nodateauth <- c(exp[5:length(exp)])
  got_nodateauth <- c(got[5:length(got)])
   # compare the two files without time stamps and author
  expect_equal(exp_nodateauth, got_nodateauth)
})
  
