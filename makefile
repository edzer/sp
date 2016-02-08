default:
	vi sp_gallery.Rmd
	make index.html

index.html:	sp_gallery.html
	cp sp_gallery.html index.html

sp_gallery.html:	sp_gallery.Rmd
	Rscript -e "library(knitr); knit2html(\"sp_gallery.Rmd\", options=\"toc\")"

commit:
	git commit -a

push:
	git add figure/*
	make commit
	git push origin gh-pages
