# SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
# SPDX-License-Identifier: Apache-2.0

REBAR ?= $(shell which rebar3 2>/dev/null || echo ./rebar3)

.PHONY: all compile test clean dialyzer xref fmt check-fmt docs

all: compile

compile:
	$(REBAR) compile

## SpiderMonkey registers an atexit() handler in JS_Init() that tears down
## internal helper threads.  When loaded as a NIF inside the BEAM VM, the
## BEAM's exit sequence may race with this handler, causing a benign
## segfault *after* all tests have passed.  Unlike Firefox and GJS which
## own the entire process and control shutdown ordering, a NIF cannot
## guarantee that SpiderMonkey's atexit handler runs before BEAM's
## schedulers and thread pools are torn down.
##
## We work around this by checking the actual test output rather than the
## exit code.
test: all
	@LOGFILE=$$(mktemp); \
	$(REBAR) eunit > $$LOGFILE 2>&1 || true; \
	cat $$LOGFILE; \
	grep -q ', 0 failures' $$LOGFILE; \
	RESULT=$$?; rm -f $$LOGFILE; exit $$RESULT

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

fmt:
	$(REBAR) fmt

check-fmt:
	$(REBAR) fmt --check

docs:
	$(REBAR) edoc

clean:
	$(REBAR) clean
	rm -rf _build

check: test dialyzer xref check-fmt

