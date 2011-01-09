all: compile

ebin:
	mkdir ebin

compile: ebin
	cd src;erl -pa ../../../ebin -make

clean:
	rm -rf ebin