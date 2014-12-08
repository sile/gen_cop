APP=gen_cop

DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns
LIBS=$(ERL_LIBS):deps

all: compile xref eunit dialyze

init:
	@eval "if ! [ -f 'src/${APP}.app.src' ]; then ./rebar create-app appid=${APP}; fi"
	@./rebar prepare-deps

compile:
	@./rebar -r compile skip_deps=true

refresh:
	@./rebar refresh-deps
	@rm -f .dialyzer.plt

xref:
	@./rebar -r xref skip_deps=true

clean:
	@./rebar -r clean skip_deps=true
	@rm -f .dialyzer.plt

distclean:
	@git clean -d -f -x

eunit:
	@./rebar -r eunit skip_deps=true

edoc:
	@./rebar -r doc skip_deps=true

start: compile
	@ERL_LIBS=$(LIBS) erl +stbt db +K true -pz ebin -eval 'catch reloader:start().' -eval 'erlang:display(application:ensure_all_started($(APP))).' -eval 'catch logi_tty:install(info).'

.dialyzer.plt:
	touch .dialyzer.plt
	ERL_LIBS=$(LIBS) dialyzer --build_plt --plt .dialyzer.plt --apps erts \
	$(shell ERL_LIBS=$(LIBS) erl -noshell -pa ebin -eval '{ok, _} = application:ensure_all_started($(APP)), [erlang:display(Name) || {Name, _, _} <- application:which_applications(), Name =/= $(APP)], halt().')

dialyze: .dialyzer.plt compile
	ERL_LIBS=$(LIBS) dialyzer -pa ebin --plt .dialyzer.plt -I deps -r ebin $(DIALYZER_OPTS)
