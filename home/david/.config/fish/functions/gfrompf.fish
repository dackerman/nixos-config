function gfrompf --wraps='git fetch && git rebase origin/master && git push -f' --description 'alias gfrompf=git fetch && git rebase origin/master && git push -f'
  git fetch && git rebase origin/master && git push -f $argv
        
end
