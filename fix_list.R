#!/usr/bin/env Rscript
# Fix template_defs.R: add commas between D() calls and close TEMPLATES list

lines <- readLines("template_defs.R", warn = FALSE)
n <- length(lines)

# Find the GAP-FILL PHASE 2 comment
gap_start <- grep("GAP-FILL PHASE 2", lines)[1]
cat(sprintf("Gap-fill section starts at line %d\n", gap_start))

# Find all D() call starts after gap_start
d_starts <- grep('^D\\("', lines)
new_starts <- d_starts[d_starts >= gap_start]
cat(sprintf("New D() calls to fix: %d\n", length(new_starts)))

# For each D() call (except the last), find its ending ) and add comma
for (i in seq_along(new_starts)) {
  start <- new_starts[i]
  if (i < length(new_starts)) {
    end_search <- new_starts[i + 1] - 1
  } else {
    end_search <- n
  }

  # Find the last non-blank, non-comment line
  end <- end_search
  while (end > start && grepl("^\\s*$|^#", lines[end])) {
    end <- end - 1
  }

  # Add comma if this is not the last D() call
  if (i < length(new_starts)) {
    if (grepl("\\)\\s*$", lines[end]) && !grepl(",\\s*$", lines[end])) {
      lines[end] <- paste0(lines[end], ",")
    }
  }
}

# Add closing of TEMPLATES list after the last entry
lines <- c(lines, "", ") # END TEMPLATES list", "",
           'cat(sprintf("Defined %d templates\\n", length(TEMPLATES)))')

writeLines(lines, "template_defs.R", useBytes = TRUE)

# Verify
check <- readLines("template_defs.R", warn = FALSE)
cat(sprintf("Total lines: %d\n", length(check)))
