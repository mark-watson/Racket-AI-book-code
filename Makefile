clean:
	rm -r -f */compiled */*.rkt~

repopack:
	repopack --ignore kgn/data,nlp/data
