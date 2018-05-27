Main: Lexer OurMonad Parser AST  
	ghc --make -w ../app/Main.hs

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

AST: AST.hs
	ghc --make ./AST.hs

OurMonad: OurMonad.hs
	ghc --make ./OurMonad.hs

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f AST
	-rm -f OurMonad
	-rm -f ../app/*.o
	-rm -f ../app/*.hi
	-rm -f Lexer.hs
	-rm -f Parser.hs
	-rm -f ../app/Main
