#target: dependencies
#	action/recipe

Lexer: PreprocessingToken.hi PreprocessingToken.o Lexer.hs
	echo $(HC)
	$(HC) -i../ Lexer.hs

Preprocessor:
	$(HC) -i../../char_token_parsers/ PreprocessingToken.hs

clean:
	rm *.o
	rm *.hi

