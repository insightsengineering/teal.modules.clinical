library(httr)
print("starting r script")

html_template = function(fname = ""){

  dir_level = length(strsplit(fname, '/')[[1]])-1
  dir_level = paste(rep("../", dir_level), sep="", collapse="")

  a = '<!-- start_releases -->'
  a = c(a, '<li class="dropdown">
	<a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">')
  a = c(a, "Other releases")
  a = c(a, '<span class="caret"></span></a><ul class="dropdown-menu" role="menu">')
  a = c(a, paste0('   <li><a href="', dir_level, 'index.html">latest</a></li>'))
  a = c(a, paste0('   <li><a href="', dir_level, releases, '/index.html">', releases, '</a></li>'))
  a = c(a, '</ul></li>')
  a = c(a, '<!-- end_releases -->')
  print(a)
  return(a)
}


args = commandArgs(trailingOnly=TRUE)
print(args)
if(length(args) != 2){
  stop('2 arguments for executing the script are needed, including reponame: 
  (e.g. "teal.modules.clinical") and release_name: (e.g. "latest" or "v1.0.0")
  
       Rscript pkgdown-multisite.R "teal.modules.clinical" "latest"')
}
repo_name = args[1]
setwd(repo_name)

release_name = args[2] # latest/main, pre-release or new_tag_string (e.g. "v0.8.0")

cat(paste("repo_name:", repo_name, "\n"))
cat(paste("release_name:", release_name, "\n"))

# get available dirs for tags:
releases = list.dirs(recursive = FALSE, full.names = FALSE)
releases = sort(grep("^pre-release|^v", releases, value = TRUE), decreasing = TRUE)
# extra correction for single vs. double length chars in tags (e.g. v0.1.1 is not before v0.1.10)
releases = rev(releases[order(nchar(releases), releases)])

# if pre-release is present change the order to take it on the top
if(any(grep(x = releases, "pre-release"))){
  releases = unique(c("pre-release", releases))
}
cat(c("releases:", releases))

# create directory structure for pre-release or new-tag with
# the current main directory and without previous tags
release_name = ifelse(release_name == "main", "latest", release_name)

if(release_name != "latest"){
  dir.create(release_name, showWarnings = FALSE)
  files = dir(include.dirs = FALSE, recursive = TRUE)
  files = files[-grep(x = files, pattern = "^pre-release|^v")] # exclude tags and pre-release

  # create structure of directories:
  dirs = unique(dirname(files))
  lapply(dirs, function(x) dir.create(paste0(release_name, "/", x),
                                       showWarnings = FALSE, recursive = TRUE))
  # and copy there:
  file.copy(files, paste0(release_name, "/", files), overwrite = TRUE)
  # update release drop-down menu list:
  releases = unique(c(release_name, releases))
}


# get list of files to be modified and create changes where needed
html_files = dir(pattern = ".html$", include.dirs = FALSE, recursive = TRUE)

for(f in html_files){

  html_code = readLines(f)
  # try to detect whether previous chunk of code for archived version had been added
  # and remove it if needed before updating:
  start_release_line = grep(pattern = '<!-- start_releases -->', x = html_code)

  if(length(start_release_line) > 0) {
    end_release_line = grep(pattern = '<!-- end_releases -->', x = html_code)
    html_code = html_code[-(start_release_line:end_release_line)]
  }

  # add new archived_version section after changelog:
  start_line = grep(pattern = 'index.html">Changelog</a>', x = html_code) + 1

  if(length(start_line == 1)){
      html_code = c(html_code[1:start_line], html_template(fname = f), html_code[(start_line+1):length(html_code)])
      writeLines(html_code, f)
  } else {
      message(paste(f, "didnt find place to put dropdown menu for archived releases"))
    }
}
