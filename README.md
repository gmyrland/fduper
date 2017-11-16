# fduper

fduper is an R package that allows you to create custom, re-usable, and extensible workflows for removing duplicate files.

# Overview

There are many existing file deduplicators, however, they typically don't allow for nuanced file deduping strategies.
While they typically provide a means to add and ignore folders recursively, it tends to be difficult to cut through the noise of legitimate dupes to focus on the real ones.
Additionally, casting a wide net by searching a large number of files to find all duplicates can be computationally intensive.
If the task is simply check a handful of orphaned files in a folder, but their duplicates may be anywhere on the drive, it can lead to significant time wasted computing hashes of duplicate pairs that do not inlcude the orphaned files in quesion.

A solution to this is to increase the flexibility and availability of the constratints in the deduplication process.
For example, it may be nice to do a a workflow such as "search entire drive for all image files, but only flag duplicate pairs if at least one is in the ~/images. In that case, delete the other file, but not if it's in a path that contains a .git folder because that is likely to be a legitimate duplicate."

At its heart, fduper extends dplyr to include methods relevant to the file deduplication process.
The underlying data object is a tibble of arbitrary file information (path, size, hashes, etc.).
Since this structure will be familiar to any R user, it should be relatively easy to add custom steps to further extend fduper for your custom workflow.
The intent of fduper is not to be the fastest deduper, but to provide a readily understandable and hackable user experience.
And in practice, a well crafted workflow can often save substantial unnecessary computation by file dedupers that don't allow for more finesse in their process.

fduper is under development and should be used with caution.

# Usage

To use fduper, build a workflow using the fduper grammar.
The grammar is in development, but it looks something like this:

``` r
fduper() %>%
    add_files('~') %>%      # Add files home directory recursively
    add_size() %>%          # Add file size to fduper dataset
    filter(                 # Use size to narrow scope of search
        size > 0,
        size < 1e6
    ) %>%
    reduce(size) %>%        # Eliminate files with unique size in bytes
                            # This forgoes computing unecessary hashes
    add_hash() %>%          # Compute md5sum hash of remaining files
    identify(hash) %>%      # Flag files with matching md5sums as duplicates
    validate %>%            # Perform thorough comparison of dupes
    to_text_file('out.txt') # Output duplicate sets to file
```

or

``` r
fduper("~/lots_of_text_files") %>%  # Add directory full of text files
    add_size %>%                    # Add file size to fduper dataset
    filter(size > 0) %>%            # Ignore 0 byte files
    reduce(size) %>%                # Elimite files with unique size
    add_dos(default=NA) %>%         # Compute hash of file with dos line endings
    filter(!is.na(dos)) %>%         # Eliminate files identified as binary
    add_unix(default=NA) %>%        # Compute hash of file with unix line endings
    filter(!is.na(unix)) %>%        # Eliminate files identified as binary (should be none)
    filter(dos != unix) %>%         # Eliminate files where dos line endings is the same as unix
    identify(unix, dos) %>%         # Flag files with matching dosunix hashes as duplicates
    to_text_file('out.txt', overwrite=TRUE) # Output duplicate sets to file

# The result is specifically files that are identical except that their
# line endings vary, possibly because they travelled back and forth between Windows
# and linux.
```

The key feature is the flexibility of designing your own deduplication workflow.

Additional grammar allows (or will allow for) operations such as:

- Keeping files by regex pattern
- Removing files by regex pattern
- Ensuring groups contain / or do not contain at least one file matching a path
- Interactive file deletion with user prompts
- Summary stats of dupes to inform where to direct efforts

Dplyr functions such as `filter`, `group_by`, etc. can be used to manipulate the fduper object.
As the underlying data is a tibble, custom manipulations should be straight-forward for those familiar with R.

Finally, as fduper is fully extensible, there is no reason why it cannot be extended to be used in completely different ways.
For example, rather than elimiating duplicates using duplicate sets, it could be used to collect file paths and hashes to compare against a saved blacklist of hashes.

# Installation

The development version of the package can be downloaded or cloned from GitHub ([gmyrland/fduper](https://github.com/gmyrland/fduper)).

Alternatively, it can be installed using devtools:

``` r
# install.packages("devtools")
devtools::install_github("gmyrland/fduper")
```

# Author

Glen Myrland
