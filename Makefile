PROJECT = lqueue
ERLC_OPTS = -Werror +debug_info +warn_export_all

all: compile

compile:
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -I include/ -o ebin src/*.erl

clean:
	rm -rf ebin test/*.beam erl_crash.dump .$(PROJECT).plt

clean-all: clean
	rm -rf logs/

# Tests

CT_RUN = ct_run \
	-pa ebin \
	-dir test \
	-logdir logs

test: clean compile
	@mkdir -p logs/
	@$(CT_RUN) -suite lqueue_SUITE

# Dialyzer

build-plt: compile
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
	--apps kernel stdlib

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
	-Werror_handling -Wrace_conditions -Wunmatched_returns
