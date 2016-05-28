# hostblocker
block websites and ads efficiently and absolutely using hosts file

# adding a source

for example, let's block known facebook domains using the hostsfile provided by [blocklists][https://github.com/jmdugan/blocklists]

the hostsfile to block 'all' of facebook's domains is located at `https://github.com/jmdugan/blocklists/blob/master/corporations/facebook/all`

we can add it to our hostsfile:

```racket hostblocker.rkt -a https://github.com/jmdugan/blocklists/blob/master/corporations/facebook/all```


# TODO
- `-a` add source from remote or local [x]
- `-u` updating from sources
- `-d` fetch default sources from github or something
- `-t` list by tag / add tags hashtable
- `-l` list sources in hostsfile (categorize by local or remote)
- `-o` output to hostfile [x]
- `-v` print version
- `-r` remove source
- `-rt` remove by tag
- `-et` enable tag (uncomment line)
- `-dt` disable tag (comment line)
- `-ee` enable entry/host
- `-de` disable entry/host
- give sources nicknames to provide easier access
- add optional default sources
- clean up code and write documentation cause watching game of thrones and programming does not result in clean code
- come up with a way to maintain the state of the hostsfile (keep existing entries not in a source) and still be able to update sources then instead of deleting/appending to the hostsfile overwrite entirely [x]


# testing

```$ racket tests.rkt```
