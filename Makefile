.PHONY: restore paper lint format test check manuscript

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

paper manuscript:
	Rscript build.R

format:
	Rscript -e 'styler::style_file("build.R"); styler::style_dir("R"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- c(lintr::lint("build.R"), unlist(lapply(c("R", "tests"), lintr::lint_dir), recursive = FALSE)); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript tests/run.R

check: lint
	Rscript build.R --test
