# hostblocker
block websites and ads efficiently and absolutely using hosts file

# adding a source

for example, let's block known facebook domains using the hostsfile provided by [blocklists](https://github.com/jmdugan/blocklists)

the hostsfile to block 'all' of facebook's domains is found at `https://github.com/jmdugan/blocklists/blob/master/corporations/facebook/all`

add it to your hostsfile using the following command:

```racket hostblocker.rkt -a https://github.com/jmdugan/blocklists/blob/master/corporations/facebook/all```

# good sources to --add
- `http://someonewhocares.org/hosts/hosts`
- `http://adaway.org/hosts.txt`

## redesign
- be fast
- be easy to use
- don't mess with the existing hostsfile
  - read hostsfile storing lines in before-list
  - if cookie is read, read in hosts, tags etc
  - if endcookie is read, continue storing line in after-list
  - output before-list then output resulting data then output after-list
- easily update hostsfile and add sources
- commands
  - `$ hb --update`
  - `$ hb --add`
  - `$ hb --block`
  - `$ hb --unblock`
  - `$ hb --block-by-tag`
  - `$ hb --unblock-by-tag`
  - `$ hb --delete`
  - `$ hb --list`
  - `$ hb --list-by-tag`


# testing

```$ racket tests.rkt```

no output is a good thing!
