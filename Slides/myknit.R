myknit <- function(inputFile, encoding) {
	# Destination where the slides should be copied after knitting
	dest.dir <- '../docs/slides';
	# Any directories that should be copied over (i.e. any dependencies)
	# dependencies <- c('assets/', 'libs/', 'images/');
	dependencies <- list.dirs('.', recursive = FALSE) # This will copy all directories over

	input.dir <- normalizePath(dirname(inputFile));
	dest.dir <- normalizePath(dest.dir)
	rmarkdown::render(input = inputFile, encoding = encoding, quiet = FALSE);
	file.copy(from = paste0(tools::file_path_sans_ext(basename(inputFile)), '.html'),
			  to = paste0(dest.dir, '/', tools::file_path_sans_ext(basename(inputFile)), '.html'));
	for(i in dependencies) {
		if(dir.exists(paste0(input.dir, '/', i))) {
			if(!dir.exists(paste0(dest.dir, '/', i))) {
				dir.create(paste0(dest.dir, '/', i), recursive = TRUE)
			}
			file.copy(from = paste0(input.dir, '/', i),
					  to = paste0(dest.dir, '/'),
					  recursive = TRUE);
		} else {
			file.copy(from = paste0(input.dir, '/', i),
					  to = paste0(dest.dir, '/', i));
		}
	}
}
