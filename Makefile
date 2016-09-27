# Copyright (C) 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


APP = timonier

SHELL = /bin/bash

EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VAGRANT = vagrant

VERSION=$(shell \
        grep Version timonier.el \
        |awk -F':' '{print $$2}' \
	|sed -e "s/[^0-9.]//g")

PACKAGE_FOLDER=$(APP)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar

ELS = $(shell find . -name "*.el")
OBJECTS = $(ELS:.el=.elc)

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)    : initialize development environment"
	@echo -e "$(WARN_COLOR)- build$(NO_COLOR)   : build project"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)    : launch unit tests"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR)   : cleanup"
	@echo -e "$(WARN_COLOR)- package$(NO_COLOR) : packaging"

init:
	@echo -e "$(OK_COLOR)[$(APP)] Initialize environment$(NO_COLOR)"
	@$(CASK) --dev install

elpa:
	@echo -e "$(OK_COLOR)[$(APP)] Build$(NO_COLOR)"
	@$(CASK) install
	@$(CASK) update
	@touch $@

.PHONY: build
build : elpa $(OBJECTS)

test: build
	@echo -e "$(OK_COLOR)[$(APP)] Unit tests$(NO_COLOR)"
	@$(CASK) exec ert-runner

.PHONY: virtual-test
virtual-test: check-env
	@$(VAGRANT) up
	@$(VAGRANT) ssh -c "source /tmp/.emacs-timonier.rc && make -C /vagrant EMACS=$(EMACS) clean init test"

.PHONY: virtual-clean
virtual-clean:
	@$(VAGRANT) destroy

.PHONY: clean
clean :
	@echo -e "$(OK_COLOR)[$(APP)] Cleanup$(NO_COLOR)"
	@rm -fr $(OBJECTS) elpa $(APP)-pkg.el $(APP)-pkg.elc $(ARCHIVE).gz

reset : clean
	@rm -rf .cask

pkg-file:
	$(CASK) pkg-file

pkg-el: pkg-file
	$(CASK) package

package: clean pkg-el
	@echo -e "$(OK_COLOR)[$(APP)] Packaging$(NO_COLOR)"
	cp dist/$(ARCHIVE) .
	gzip $(ARCHIVE)
	rm -fr dist

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
