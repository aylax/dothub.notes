help:
	@echo "command:"
	@grep '^[a-z]' Makefile | awk '{print $$1}' | sed 's/^/- /;s/:$$//'

build: compile clean mount
	@stack exec site build
	@echo "site :: build -> success"

mount:
	@sh scripts/create-metadata

clean:
	@stack exec site clean
	@sh scripts/delete-metadata
	@echo "site :: clean -> success"

preview: compile clean mount
	@stack exec site watch
	@echo "site :: preview -> success"

publish:
	@echo "site :: publish -> success"


compile:
	@stack build
	@echo "site :: compile -> success"

.PHONY: help build mount clean preview publish compile

