# pakell

This project aims to be a small CLI tool to find some comments notes like
(FIXME, TODO, etc) into specified file or directory.

## Installation


For now, pakell can only be installed with stack. stack is a program for
developping Haskell project. Look at the stack website for installation
instruction: https://docs.haskellstack.org/en/stable/README/#how-to-install


Once stack is installed, you need to clone, build and install pakell :

```bash
$ git clone https://github.com/gacou54/pakell.git
$ cd pakell
$ stack install
```

Now pakell should be installed :)

## Utilisation

pakell initialize a config file in .config directory, pakell.conf.
It should be empty the first time. Keywords in there are the ones that
pakell will look for when parsing file/directory.

To add a keyword:

```bash
$ pakell add TODO
```

You just added a new keyword. You can look for it in a file or directory
that way:


```bash
$ pakell look <directoryOrFilePath>
```
or (l is an alias for look command)

```bash
$ pakell l <directoryOrFilePath>
```

or to look in current directory

```bash
$ pakell
```

To remove a keyword:

```bash
$ pakell remove TODO
```

To list current keywords:

```bash
$ pakell list
```

To remove all keywords:

```bash
$ pakell clear
```

## Example

![basic example](/imgsExample/basicExample.png)

### NOTE
IN DEVELOPMENT.
* This project has just been started. Also, this is my first Haskell
project. Once functional, this project will probably have to be reworked.


### TODO

* Add fonctionnalities
* Correct error about file that can't be open
* Exception management: I developped this to work on my system.
        I don't know if pakell works well everywhere.
        For example I use a 256 color terminal, which is not the case for
        everybody.

