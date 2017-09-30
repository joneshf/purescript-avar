.PHONY: ps erl all run test

all: ps erl

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

ps:
	psc-package sources | xargs purs compile 'src/**/*.purs'

ps_test:
	psc-package sources | xargs purs compile 'src/**/*.purs' 'test/**/*.purs'

ps_watch:
	while true; do psc-package sources | xargs purs compile 'src/**/*.purs' ; inotifywait -qre close_write .; done

run:
	erl -pa ebin -noshell -eval '(main@ps:main@c())(unit)' -eval 'init:stop()'

test: ps_test erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'
