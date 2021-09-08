.PHONY: clean

out/step0_repl.js: src/mal/step0_repl.cljs
	npx shadow-cljs compile :step0

out/step1_read_print.js: src/mal/step1_read_print.cljs
	npx shadow-cljs compile :step1

out/step2_eval.js: src/mal/step2_eval.cljs
	npx shadow-cljs compile :step2

clean:
	rm -rf out/
