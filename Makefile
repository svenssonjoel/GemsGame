


all: main 



main: Main.hs Gems.hs 
	ghc Main.hs -o main