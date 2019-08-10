
.PHONY: all
all: README.md \
     documentation

## Recreate README

README.md : README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'


## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

