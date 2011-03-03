all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar skip_deps=true dialyze

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	if [ -f test.config ]; then\
		erl -config test -pa ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse -s ebitly;\
	else\
		erl -pa ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse -s ebitly;\
	fi

shell: all
	erl -pa ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse
