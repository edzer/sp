default:
	vi sp_gallery.Rmd
	make index.html

index.html:	sp_gallery.html
	cp sp_gallery.html index.html

sp_gallery.html:	sp_gallery.Rmd
	# Rscript -e "library(knitr); purl(\"sp_gallery.Rmd\"); knit2html(\"sp_gallery.Rmd\", options=\"toc\")"
	Rscript -e "knitr::purl(\"sp_gallery.Rmd\"); rmarkdown::render(\"sp_gallery.Rmd\", output_options = list(self_contained = FALSE))"

commit:
	git commit -a

push:
	git add sp_gallery_files/* sp_gallery_files/*/*
	make commit
	git push origin gh-pages

view:
	google-chrome sp_gallery.html

onefile:
	Rscript -e "knitr::purl(\"sp_gallery.Rmd\"); rmarkdown::render(\"sp_gallery.Rmd\")"
