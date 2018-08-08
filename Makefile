LHSS=$(wildcard *.lhs)

all: index.pdf

index.pdf: $(LHSS)
	pdflatex -shell-escape index.lhs

watch: $(LHSS)
	while inotifywait -q -e modify,move_self $(LHSS); do \
		pdflatex -halt-on-error -shell-escape index.lhs;   \
	done
