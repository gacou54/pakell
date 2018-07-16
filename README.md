# pakell

This project aims to be a small CLI tool to find keywords like
(`FIXME`, `TODO`, etc) into specified file or directory. The utilization aims
to be easy. First you have to add your keywords, then look for them.


It is also possible to use pakell as a higher level tool than grep. For example,
the command `pakell lookfor <aWord>` allow you to find a specific word in
the current directory, and "pakell lookfor `-p <path> <aWord>` to find a
specific word at a specific path.


## Installation


For now, pakell can only be installed with stack. stack is a program for
developping Haskell projects. Look at the stack website for installation
instructions: https://docs.haskellstack.org/en/stable/README/#how-to-install


Once stack is installed, you need to clone, build and install pakell :

```bash
$ git clone https://github.com/gacou54/pakell.git
$ cd pakell
$ stack install
```

Now pakell should be installed :)

## Utilization

pakell initialize a config file in the .config directory, pakell.conf.
It should be empty at the first utilization. Keywords in there are the ones that
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

Notes
* You can recursively parse directories with the `-r` option.
* Some command don't need a specified path because it parse the current directory, but you can speficy a path with the `-p <aPath>` option, where `<aPath>` is your path like home/
* By default, hidden file/directory are not parsed. But you can allow it by -d option



## Example

![basic example](/imgsExample/basicExample.png)

### NOTE
IN DEVELOPMENT.
* This is a new project and may change. By now pakell works well on my computer, but I have no garanty that it will work everywhere


### TODO

* Add fonctionnalities
* Exception management: I developped this to work on my system.
        I don't know if pakell works well everywhere.
        For example I use a 256 color terminal, which is not the case for
        everybody.

