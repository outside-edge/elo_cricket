.PHONY: sync fetch prepare analyze visualize publish update lint test check ci-docker

sync:
	Rscript -e 'renv::restore(prompt = FALSE)'

fetch prepare analyze visualize publish update:
	Rscript scripts/pipeline.R $@ $(AS_OF) $(FLAGS)

lint:
	Rscript scripts/check.R

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: lint test

ci-docker:
	docker run --rm -v "$(CURDIR):/app" -w /app -e RENV_PATHS_LIBRARY=/tmp/library -e RENV_PATHS_CACHE=/tmp/cache rocker/r-ver:4.6.0 sh -c 'apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev && make sync check'
