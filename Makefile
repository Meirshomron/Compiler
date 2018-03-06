%:
	echo '(load "project/project.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q

	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o -ggdb

	gcc -m64 -Wall -g $(MAKECMDGOALS).o -o $(MAKECMDGOALS)
	
clean :
	rm -rf inon.o inon.s inon