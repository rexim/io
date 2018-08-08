PDFS=$(patsubst %.lhs,%.pdf,$(wildcard *.lhs))

all: $(PDFS)

%.pdf: %.lhs
	pdflatex -shell-escape $<

watch: index.lhs
	while inotifywait -q -e modify,move_self index.lhs; do \
		pdflatex -halt-on-error -shell-escape index.lhs;   \
	done
