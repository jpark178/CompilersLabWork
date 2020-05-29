make: 
	bison -d -v mini_l.y
	flex mini_l.lex
	gcc -o parser mini_l.tab.c lex.yy.c -lfl

clean:
	rm mini_l.output mini_l.tab.h mini_l.tab.c parser.exe lex.yy.c