#!/usr/bin/env bash
#   __     __                                                     _
#  / _|___/ _|       ___ ___  _ __ ___  _ __ ___   __ _ _ __   __| |___
# | ||_  / |_ _____ / __/ _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
# |  _/ /|  _|_____| (_| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
# |_|/___|_|        \___\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

# thesaitama@ fzf-commands.sh

# ------------------------------------------------------------------------
# file open

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# fep preview + open editor
fep() {
  local file
  file=$(fzf --preview "pygmentize {}" --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# ------------------------------------------------------------------------
# grep

# eg fuzzy grep open via ag
eg() {
  local file

  file="$(ag --nobreak --noheading $@ | fzf -0 -1 | awk -F: '{print $1 " +" $2}')"

  if [[ -n $file ]]
  then
     e $file
  fi
}

# ------------------------------------------------------------------------
# history

# fh - repeat history
fh() {
  eval $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s | sed 's/ *[0-9]* *//')
}

# ------------------------------------------------------------------------
# process

# fkill - kill process
fkill() {
  ps -ef | sed 1d | fzf -m | awk '{print $2}' | xargs kill -${1:-9}
}

# ------------------------------------------------------------------------
# git

# fbr - checkout git branch (including remote branches),
#       sorted by most recent commit, limit 30 last branches
fbr() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

# fco_preview - checkout git branch/tag,
#               with a preview showing the commits between the tag/branch and HEAD
fco() {
  local tags branches target
  tags=$(
git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
git branch --all | grep -v HEAD |
sed "s/.* //" | sed "s#remotes/[^/]*/##" |
sort -u | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
(echo "$tags"; echo "$branches") |
    fzf --no-hscroll --no-multi --delimiter="\t" -n 2 \
        --ansi --preview="git log -200 --pretty=format:%s $(echo {+2..} |  sed 's/$/../' )" ) || return
  git checkout $(echo "$target" | awk '{print $2}')
}

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fstash - easier way to deal with stashes
# type fstash to get a list of your stashes
# enter shows you the contents of the stash
# ctrl-d shows a diff of the stash against your current HEAD
# ctrl-b checks the stash out as a branch, for easier merging
fstash() {
  local out q k sha
  while out=$(
    git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
    fzf --ansi --no-sort --query="$q" --print-query \
        --expect=ctrl-d,ctrl-b);
  do
    mapfile -t out <<< "$out"
    q="${out[0]}"
    k="${out[1]}"
    sha="${out[-1]}"
    sha="${sha%% *}"
    [[ -z "$sha" ]] && continue
    if [[ "$k" == 'ctrl-d' ]]; then
      git diff $sha
    elif [[ "$k" == 'ctrl-b' ]]; then
      git stash branch "stash-$sha" $sha
      break;
    else
      git stash show -p $sha
    fi
  done
}

# ------------------------------------------------------------------------
# tmux

# ftpane - switch pane (@george-b)
ftpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse)

  if [ -n "$target" ]; then
    target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
    target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

    if [[ $current_window -eq $target_window ]]; then
      tmux select-pane -t ${target_window}.${target_pane} && exit
    else
      tmux select-pane -t ${target_window}.${target_pane} &&
        tmux select-window -t $target_window && exit
    fi
  fi

}

# ------------------------------------------------------------------------
# fasd

av() {
    [ $# -gt 0 ] && fasd -f -e ${EDITOR} "$*" && return
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && vi "${file}" || return 1
}

if type z >/dev/null 2>&1; then
  unalias z
fi
z() {
  if [[ -z "$*" ]]; then
    cd "$(fasd_cd -d | fzf -1 -0 --no-sort --tac +m | sed 's/^[0-9,.]* *//')"
  else
    cd "$(fasd_cd -d | fzf --query="$*" -1 -0 --no-sort --tac +m | sed 's/^[0-9,.]* *//')"
  fi
}

# ------------------------------------------------------------------------
# Mac OSX Only

if [ "$(uname)" == 'Darwin' ]; then

  # b - browse chrome bookmarks
  b() {
    bookmarks_path=~/Library/Application\ Support/Google/Chrome/Default/Bookmarks
    jq_script='def ancestors: while(. | length >= 2; del(.[-1,-2]));
               . as $in | paths(.url?) as $key | $in | getpath($key) | {name,url, path: [$key[0:-2] | ancestors as $a | $in | getpath($a) | .name?] | reverse | join("/") } | .path + "/" + .name + "\t" + .url'

    jq -r "$jq_script" < "$bookmarks_path" \
      | sed -E $'s/(.*)\t(.*)/\\1\t\x1b[36m\\2\x1b[m/g' \
      | fzf --ansi \
      | cut -d$'\t' -f2 \
      | xargs open
  }

  # app - osx appluncher (old)
  uapp() {
    app_path=$(find /Applications -maxdepth 3 -type d |
                 grep '\.app$' |
                 sed 's/\/Applications\///' |
                 sed 's/\.app$//' |
                 fzf --query="$1" --prompt="App > " --exit-0)
    if [ -n "$app_path" ]; then
      open -a "/Applications/$app_path.app"
      # in emacs terminal excute and exit
      [ $TERM == 'eterm-color' ] && exit
    fi
  }
  # app - osx appluncher
  app() {
    sapp_list=$(find /System/Applications -maxdepth 3 -type d |
                 grep '\.app$' |
                 sed 's/\/System\/Applications\///' |
                 sed 's/\.app$//' |
                 sed 's/^/S::/')
    aapp_list=$(find /Applications -maxdepth 3 -type d |
                 grep '\.app$' |
                 sed 's/\/Applications\///' |
                 sed 's/\.app$//' |
                 sed 's/^/A::/')
    uapp_dest="/Users/$(whoami)/Applications"
    uapp_dest_sed="s/\/Users\/$(whoami)\/Applications\///"
    # echo $uapp_dest_sed
    uapp_list=$(find $uapp_dest -maxdepth 3 -type d |
                 grep '\.app$' |
                 sed $uapp_dest_sed |
                 sed 's/\.app$//' |
                 sed 's/^/U::/')
    # echo -e $uapp_list
    app_path=$(echo -e "$sapp_list\n$aapp_list\n$uapp_list" | fzf --query="$1" --prompt="App > " --exit-0)
    if [ -n "$app_path" ]; then
      open_path_u="s/U::/\/Users\/$(whoami)\/Applications\//"
      open_path=$(echo "$app_path" |
                    sed 's/S::/\/System\/Applications\//' |
                    sed 's/A::/\/Applications\//' |
                    sed $open_path_u)
      # echo $open_path
      open -a "$open_path.app"
      # in tmux, emacs terminal excute and exit
      # [ -n "$TMUX" ] && exit
      # [ $TERM == 'eterm-color' ] && exit
      # preventing open command returns not 0
      :
    fi
  }
fi
