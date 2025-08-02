function gfrom --wraps='git fetch && git rebase origin/master' --description 'alias gfrom=git fetch && git rebase origin/master'
  git fetch && git rebase origin/master $argv
        
end
