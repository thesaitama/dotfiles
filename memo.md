
# MacPorts
update
```
sudo port selfupdate
sudo port upgrade
```

clean
```
sudo port clean --all all
sudo port uninstall inactive
```

## Python27
```
sudo port install python27
sudo port install py27-setuptools
```
### optional
```
sudo port -f activate py27-distribute
```

```
sudo port install py27-pip
sudo port select --set pip pip27
sudo port install py27-virtualenv py27-virtualenvwrapper
sudo port select --set virtualenv virtualenv27
```

### check site-package
```
python -c "import site; print(site.getsitepackages())"
```
### check distribute
```
python -c "import distutils.sysconfig as s; print(s.get_python_lib())"
```

### virtualenv wrapper(.bashrc)
```
export VIRTUALENVWRAPPER_PYTHON='/opt/local/bin/python2.7'
export VIRTUALENVWRAPPER_VIRTUALENV='/opt/local/bin/virtualenv-2.7'
export VIRTUALENVWRAPPER_VIRTUALENV_CLONE='/opt/local/bin/virtualenv-clone-2.7'
source /opt/local/bin/virtualenvwrapper.sh-2.7
```

## IPython27
```
sudo port install py27-ipython
sudo port select --set ipython2 py27-ipython
```
