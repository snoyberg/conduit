#!/bin/sh
#
# Convert a literate Haskell program (README.lhs) to GitHub Flavored
# Markdown (README.md).
#
# For more on GitHub Flavored Markdown see:
#
#     http://github.github.com/github-flavored-markdown/
#

# add a disclaimer
echo '<!-- Generated from README.lhs, do not modify! -->' > README.md

cat README.lhs | \

    # GFM does not ignore line breaks in paragraphs, so we remove them
    perl -0777 -pe  's/([]a-zA-Z,.`])\n([[a-zA-Z`])/\1 \2/g' | \

    # convert Bird-style literate Haskell to Markdown code blocks
    perl -pe 's/^> /    /' | \
    perl -pe 's/^>$//' >> README.md
