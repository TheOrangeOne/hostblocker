# hostblocker
block websites and ads efficiently and absolutely using hosts file


# TODO
- `-a` add source from remote or local [x]
- `-u` updating from sources
- `-d` fetch default sources from github or something
- `-t` list by tag / add tags hashtable
- `-l` list sources in hostsfile (categorize by local or remote)
- `-o` output to hostfile
- `-v` print version
- `-r` remove source
- `-rt` remove by tag
- `-et` enable tag (uncomment line)
- `-dt` disable tag (comment line)
- `-ee` enable entry/host
- `-de` disable entry/host
- add optional default sources
- clean up code and write documentation cause watching game of thrones and programming does not result in clean code
- come up with a way to maintain the state of the hostsfile (keep existing entries not in a source) and still be able to update sources then instead of deleting/appending to the hostsfile overwrite entirely
  - idea: `(listof (anyof string? hash?))` where the strings would be the raw lines of the hostsfile that are not contained within `#! src` blocks; hash tables would be the sources (the order of the entries within the sources does not matter, but the order of the entries not contained within the sources might eg. localhost definition)


# testing

```$ racket tests.rkt```
