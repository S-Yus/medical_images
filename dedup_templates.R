#!/usr/bin/env Rscript
# Remove duplicate template IDs from template_defs.R
# Strategy: keep the version with a special type, or the first occurrence if both are xy

lines <- readLines("template_defs.R", warn = FALSE)

# Find all D() calls and their line ranges
d_starts <- grep('^D\\("', lines)
cat(sprintf("Found %d D() calls\n", length(d_starts)))

# For each D() call, determine its end line (before next D() or section comment)
entries <- list()
for (i in seq_along(d_starts)) {
  start <- d_starts[i]
  end <- if (i < length(d_starts)) d_starts[i + 1] - 1 else length(lines)
  # Trim trailing blank lines and comments from end
  while (end > start && grepl("^$|^#|^\\s*$", lines[end])) end <- end - 1

  block <- paste(lines[start:end], collapse = "\n")

  # Extract ID
  id <- sub('^D\\("([^"]+)".*', '\\1', lines[start])

  # Extract type
  has_type <- grepl('type="', block)
  type_val <- if (has_type) sub('.*type="([^"]+)".*', '\\1', block) else "xy"

  entries[[length(entries) + 1]] <- list(
    id = id, start = start, end = end, type = type_val, index = i
  )
}

# Find duplicates and decide which to keep
ids <- sapply(entries, `[[`, "id")
dup_ids <- unique(ids[duplicated(ids)])
cat(sprintf("Found %d duplicate IDs\n", length(dup_ids)))

lines_to_remove <- integer()

for (did in dup_ids) {
  idx <- which(ids == did)
  group <- entries[idx]

  # Prefer the entry with a special type (non-xy)
  special <- which(sapply(group, `[[`, "type") != "xy")

  if (length(special) > 0) {
    # Keep the first special type entry, remove others
    keep <- idx[special[1]]
    remove <- setdiff(idx, keep)
  } else {
    # All xy: keep the first one
    keep <- idx[1]
    remove <- idx[-1]
  }

  for (r in remove) {
    e <- entries[[r]]
    # Include any trailing blank/comment lines
    end_ext <- e$end
    while (end_ext < length(lines) && grepl("^\\s*$", lines[end_ext + 1])) {
      end_ext <- end_ext + 1
    }
    lines_to_remove <- c(lines_to_remove, e$start:end_ext)
    cat(sprintf("  Remove %s at lines %d-%d (keep %s at %d, type=%s)\n",
                e$id, e$start, end_ext,
                entries[[keep]]$id, entries[[keep]]$start,
                entries[[keep]]$type))
  }
}

# Also remove section headers that become empty
cat(sprintf("\nRemoving %d lines total\n", length(unique(lines_to_remove))))

# Remove lines
clean_lines <- lines[-unique(lines_to_remove)]

# Write cleaned file
writeLines(clean_lines, "template_defs.R", useBytes = TRUE)

# Verify
d_count <- length(grep('^D\\("', clean_lines))
cat(sprintf("Cleaned file: %d templates, %d lines\n", d_count, length(clean_lines)))

# Check no remaining duplicates
clean_ids <- sub('^D\\("([^"]+)".*', '\\1', clean_lines[grep('^D\\("', clean_lines)])
remaining_dups <- clean_ids[duplicated(clean_ids)]
if (length(remaining_dups) > 0) {
  cat(sprintf("WARNING: %d remaining duplicates: %s\n",
              length(remaining_dups), paste(remaining_dups, collapse=", ")))
} else {
  cat("No remaining duplicates - clean!\n")
}
