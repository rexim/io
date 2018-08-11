LHSS=$(wildcard *.lhs)

all: index.pdf

index.pdf: $(LHSS) index
	pdflatex -shell-escape index.lhs

index: index.lhs
	ghc -c index.lhs

watch: $(LHSS)
	while inotifywait -q -e modify,move_self $(LHSS); do \
		ghc -c index.lhs && pdflatex -halt-on-error -shell-escape index.lhs;   \
	done
