default:
	vi sp_gallery.Rmd
	make index.html

index.html:	sp_gallery.html
	cp sp_gallery.html index.html

sp_gallery.html:	sp_gallery.Rmd
	# Rscript -e "library(knitr); purl(\"sp_gallery.Rmd\"); knit2html(\"sp_gallery.Rmd\", options=\"toc\")"
	Rscript -e "knitr::purl(\"sp_gallery.Rmd\"); rmarkdown::render(\"sp_gallery.Rmd\")"

commit:
	git commit -a

push:
	git add figure/*
	make commit
	git push origin gh-pages

view:
	google-chrome sp_gallery.html
