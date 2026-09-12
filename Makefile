.PHONY: restore analysis figures paper lint format test check manuscript

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript scripts/check_inputs.R
	Rscript scripts/bihar_winners.R
	Rscript scripts/mumbai.R
	Rscript scripts/delhi.R
	Rscript scripts/rural_winners.R
	Rscript scripts/missing_education.R
	Rscript scripts/inventory.R

figures: analysis
	Rscript scripts/figures.R

paper: figures
	$(MAKE) manuscript

manuscript:
	Rscript -e 'options(tinytex.install_packages = FALSE); rmarkdown::render("manuscript/main.Rmd", knit_root_dir = getwd(), quiet = TRUE)'

format:
	Rscript -e 'styler::style_dir("R"); styler::style_dir("scripts"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript tests/run.R

check: paper lint test
