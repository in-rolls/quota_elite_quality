if (!exists("report", inherits = FALSE)) {
  source("R/analysis.R")
  report <- run_analysis()
  source("R/paper.R")
}

sys.source("tests/test_bihar_winners.R", envir = environment())
sys.source("tests/test_winners.R", envir = environment())
sys.source("tests/test_delhi.R", envir = environment())
sys.source("tests/test_literature.R", envir = environment())
sys.source("tests/test_summary.R", envir = environment())
sys.source("tests/test_occupation.R", envir = environment())
sys.source("tests/test_meta.R", envir = environment())
