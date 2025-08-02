function gclear --wraps='git add . && git reset --hard HEAD' --description 'alias gclear=git add . && git reset --hard HEAD'
  git add . && git reset --hard HEAD $argv
        
end
