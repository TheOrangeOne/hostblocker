# hostblocker
use your hostsfile as a domain blocker to block ads, malware, porn, gambling, social and whatever other sites you wish to block!

# usage

initialize a specified hostsfile

```shell
$ touch myhostsfile # create a new hostsfile
$ racket hostblocker.rkt --init myhostsfile
```

add an awesome default source (unified hosts = ads+malware) from [StevenBlack's hosts project](https://github.com/StevenBlack/hosts) with `--defaults`

```shell
$ racket hostblocker.rkt --file myhostsfile --defaults
$ racket hostblocker.rkt --file myhostsfile --list # view the added source
```

or add whatever remote or local sources you wish with `--add`

```shell
$ racket hostblocker.rkt --file myhostsfile --add http://adaway.org/hosts.txt adaway
```

# installation

hostblocker requires racket, and the build tool raco

## from source
```shell
$ sudo pacman -S racket # install racket
$ raco pkg install # install dependencies
$ sudo make
```

# TODO

- [x] `--add` add remote or local sources
- [x] `--remove` remove hosts based on the source
- [x] `--update` update all source
- [x] `--defaults` provide good defaults for ads, social, porn and gambling
- [ ] `--add-tag` add a tag to a host
- [ ] `--enable-by-tag` enable all hosts with a tag
- [ ] `--disable-by-tag` disable all hosts with a tag
- [ ] add tests and documentation
