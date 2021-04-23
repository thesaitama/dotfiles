# MacPorts

* update
  ``` bash
  sudo port selfupdate
  sudo port upgrade
  ```

 * clean
  ``` bash
  sudo port clean --all all
  sudo port uninstall inactive
  ```

## diff-hignight

``` bash
cd /opt/local/share/git/contrib/diff-highlight/
sudo make
sudo ln -s /opt/local/share/git/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
```

## ranger

``` bash
sudo port install ranger
sudo port install mediainfo atool libcaca imlib2 highlight
ranger --copy-config=all
```

## Python27

``` bash
sudo port install python27
sudo port install py27-setuptools
```

### optional

``` bash
sudo port -f activate py27-distribute
```

``` bash
sudo port install py27-pip
sudo port select --set pip pip27
sudo port install py27-virtualenv py27-virtualenvwrapper
sudo port select --set virtualenv virtualenv27
```

### check site-package

``` bash
python -c "import site; print(site.getsitepackages())"
```

### check distribute

``` bash
python -c "import distutils.sysconfig as s; print(s.get_python_lib())"
```

### virtualenv wrapper(.bashrc)

``` bash
export VIRTUALENVWRAPPER_PYTHON='/opt/local/bin/python2.7'
export VIRTUALENVWRAPPER_VIRTUALENV='/opt/local/bin/virtualenv-2.7'
export VIRTUALENVWRAPPER_VIRTUALENV_CLONE='/opt/local/bin/virtualenv-clone-2.7'
source /opt/local/bin/virtualenvwrapper.sh-2.7
```

### IPython27

``` bash
sudo port install py27-ipython
sudo port select --set ipython2 py27-ipython
```

## wcwidth-cjk

``` bash
git clone https://github.com/fumiyas/wcwidth-cjk.git
cd wcwidth-cjk
sudo port install libtool autoconf automake
glibtoolize
aclocal
autoheader
automake --add-missing
autoconf -v -f -i
./configure --prefix=/usr/local
make
sudo make install
```
