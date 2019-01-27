# thesaitama dotfile

## System requirements

* macOS 10.14 / Linux
* bash 4.4
* MacPorts

Emacs configuration compatible with Windows 7-10

## How to install

```bash
git clone https://github.com/thesaitama/dotfiles.git ~/dotfiles
cd ~/dotfiles
sh ./dotfileslink.sh
```

* for Windows Emacs user: simply run `setup-emacs.bat`

## Note

* diff-hignight

```bash
cd /opt/local/share/git/contrib/diff-highlight/
sudo make
sudo ln -s /opt/local/share/git/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
```

* ranger

```bash
sudo port install ranger
sudo port install mediainfo atool libcaca imlib2 highlight
ranger --copy-config=all
```
