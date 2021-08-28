help:
	@echo "command:"
	@grep '^[a-z]' Makefile | awk '{print $$1}' | sed 's/^/- /;s/:$$//'


build: compile clean
	@stack exec site build
	@echo "site :: build -> success"

clean:
	@stack exec site clean
	@echo "site :: clean -> success"

preview: compile clean
	@stack exec site watch
	@echo "site :: preview -> success"

publish:
	@echo "site :: publish -> success"

compile:
	@stack build
	@echo "site :: compile -> success"

.PHONY: help build clean preview publish compile

