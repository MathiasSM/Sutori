Main: Lexer
	ghc --make -w ../app/Main.hs

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f ../app/*.o
	-rm -f ../app/*.hi
	-rm -f Lexer.hs
	-rm -f Parser.hs
	-rm -f ../app/Main
