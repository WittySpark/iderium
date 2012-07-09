.PHONY: run check build clean show

run: build
	@./demo > demo.log; less demo.log

check: build
	@valgrind --leak-check=full ./demo

build:
ifdef demo
	@if [ -e .demo ]; then old=`cat .demo`; cp demo.adb demos/$${old}.demo; fi
	@cp demos/$(demo).demo demo.adb; echo $(demo) > .demo
endif
	@mkdir -p rubbish
	@gprbuild -Pdemo
	@rm -rf rubbish

clean:
	@gprclean -Piderium
	@rm -f demo

show:
	@octave demo.m
