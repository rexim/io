LHSS=$(wildcard *.lhs)

all: index.pdf

index.pdf: $(LHSS) index.o
	pdflatex -shell-escape index.lhs

index.o: index.lhs
	ghc -c index.lhs

watch: $(LHSS)
	while inotifywait -q -e modify,move_self $(LHSS); do \
		ghc -c index.lhs && pdflatex -halt-on-error -shell-escape index.lhs;   \
	done
