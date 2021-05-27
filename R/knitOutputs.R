# TODO: figure this out.
# multipleWithDates = function(path) {
#   function(inputFile, encoding,...) {
#   rmarkdown::render(
#     inputFile, 
#     encoding = encoding, 
#     #output_dir = path, 
#     output_file=paste0(path,'-',Sys.Date(),'.pdf')) 
#   }
# }

# needs preamble containing:
# preamble: >
#   \usepackage{amsmath}
#   \usepackage{minted}
#   \usemintedstyle{emacs}
#   \setminted[java]{fontsize=\footnotesize,tabsize=3}
#   \setminted[xml]{fontsize=\footnotesize,tabsize=3}

options(tinytex.engine_args = '-shell-escape')
codeSnippet = function(type,filename,starts=0,ends=Inf,sep="\n...\n") {
  
  lines = readLines(filename)
  
  if (isTRUE(getOption("knitr.in.progress")) && knitr::opts_knit$get('rmarkdown.pandoc.to')=="html") {
    cat(
      paste0(
        "~~~{.",type,"}\n",
        paste0(
          lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
          collapse=sep),
        "\n~~~"
      ),
      sep="")
  } else {
    cat(
      paste0(
        "\\begin{minted}{",type,"}\n",
        paste0(
          lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
          collapse=sep),
        "\n\\end{minted}"
      ),
      sep="")
  }
}