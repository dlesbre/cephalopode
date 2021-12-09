all: grammar
	ghc --make Main.hs -o bifrost

grammar: cleangrammar
	bnfc --haskell -p Lang Lang.cf
	happy -gca Lang/ParLang.y
	alex -g Lang/LexLang.x

# No display of executed commands.
$(VERBOSE).SILENT:

#testgrammar: grammar
#	runhaskell Lang/TestLang.hs < test/newtest.prog

cleangrammar:
	-rm -rf Lang

clean: cleangrammar
	-rm -f *.hi *.o

distclean: clean
	-rm -f bifrost
	-rm -f examples/*/*.prog.fl
