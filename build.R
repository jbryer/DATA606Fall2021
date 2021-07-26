# usethis::gh_token_help()
# remotes::install_github("jhelvy/xaringanBuilder")
# credentials::set_github_pat()

# Builds the course website.

library(xaringanBuilder)

# First copy the Slides and supplemental materials to the docs/folder
tocopy <- c(list.files('Slides', pattern = '.html'),
			list.dirs('Slides', recursive = FALSE, full.names = FALSE))
for(i in tocopy) {
	from <- paste0('Slides/', i)
	to <- paste0('docs/slides/', i)
	cat(paste0('Copying ', from, ' to ', to, '...\n'))

	success <- FALSE
	if(i %in% c('draft')) {
		cat(paste0('Ignoring ', i, '...\n'))
		success <- TRUE
	} else if(!file_test("-f", from)) { # Directory
		dir.create(to, recursive = TRUE)
		success <- file.copy(from, 'docs/slides/', recursive = TRUE, overwrite = TRUE)
	} else { # File
		success <- file.copy(from, to, overwrite = TRUE)
	}
	if(!success) {
		cat(paste0('ERROR: ', i, ' did not copy!\n'))
	}

	if(tolower(tools::file_ext(from)) == 'html') {
		pdf <- paste0(tools::file_path_sans_ext(from), '.pdf')

		build_pdf <- !file.exists(pdf) | file.info(from)$mtime >	file.info(pdf)$mtime

		if(build_pdf) {
			wd <- setwd('Slides/')
			tryCatch({
				build_pdf(i,
						  complex_slides = TRUE,
						  partial_slides = FALSE)
			}, error = function(e) {
				cat(paste0('Error generating PDF from ', from))
				print(e)
			}, finally = { setwd(wd) })
		}
	}
}
wd <- setwd('website')
blogdown::build_site(build_rmd = TRUE)
setwd(wd)
