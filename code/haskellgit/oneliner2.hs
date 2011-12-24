## List all the authors with commits
git log --format='%aN' | ghc -e 'script $ uniq . lines'

