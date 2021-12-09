all:
	ghc --make Main.hs -o bifrost

grammar:
	-rm -rf Lang
	bnfc --haskell -p Lang Lang.cf
	happy -gca Lang/ParLang.y
	alex -g Lang/LexLang.x

# No display of executed commands.
$(VERBOSE).SILENT:

#testgrammar: grammar
#	runhaskell Lang/TestLang.hs < test/newtest.prog

clean:
	-rm -f *.hi *.o
	-rm -f Lang/*.hi Lang/*.o Lang/*.x Lang/*.y

distclean: clean
	-rm -f bifrost
	-rm -f examples/*/*.prog.fl
