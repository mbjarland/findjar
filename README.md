# findjar

[![Build Status](https://github.com/mbjarland/findjar/actions/workflows/ci.yml/badge.svg)](https://github.com/mbjarland/findjar/actions)
[![Version](https://img.shields.io/badge/version-1.0.98-brightgreen)](https://img.shields.io/badge/version-1.0.98-brightgreen)
[![License](https://img.shields.io/badge/License-EPL_2.0-green.svg)](https://www.eclipse.org/legal/epl-2.0/)

findjar - a tool for searching through files, including files inside jars 

![alt text](https://github.com/mbjarland/findjar/doc/findjar_manifest_jdk_created_by.png)

findjar searches for files/content in any disk structure. It is capable of looking both for/in normal files and also for/in files inside zip/jar files. It is capable of regex matching both for file name/path and for content within the files. 
 
This tool is in essence an improvement of the unix find command geared towards solving a common problem for programmers on the JVM: finding that specific file or class in your maven repo, classpath, etc when that file can reside either directly on disk or inside a jar archive. 
 
Also this tool can be useful in detecting what version a specific class file or source file inside a library jar changed between a number of versions of the library file. 
 
Note that this tool is capable of a few extra tricks such as colorizing matches, printing line numbers, printing a configurable number of surrounding context lines, printing out the contents of matched files inside jar files and calculating md5 or sha1 hashes of the content of matched files inside jar files. 

## Features

- searches both normal files (like \*nix find) and files inside .jar and .zip archives.
- supports regex matching for file names, paths, and content within the files.
- support printing contents of matched files, both normal files and files within jar/zip archives.
- calculates md5, sha1, sha512, crc32, and sha256 hashes for matched files.
- ANSI coloring for better visualization, with an option to toggle off.

## Installation / Building

This project uses:

  * java - tested against java 11, 17, and 21. 
  * clojure - clojure installation instructions can be found [here](https://clojure.org/guides/install_clojure)
  
assuming the above requirements of java and clojure are met, you can build the project using: 

```bash
 ─➤ clj -T:build uber
```

which will generate an uber/fat jar in the `target` directory after which you can run the tool as described in the section below. 

## Running

Once you have the jar file available you can execute the tool using something like the below: 

```
 ─➤ java -jar target/findjar-1.0.78-standalone.jar ~/.m2/repository/ -n math.clj -g Hickey -x 10
 ```

If you are on osx or \*nix and you want to make life easier you can create an alias: 

```
 ─➤ alias findjar="$JAVA_HOME/bin/java -jar ~/findjar/target/findjar-1.0.98-standalone.jar $@"
```
(replacing ~/findjar with wherever your findjar jar file is and replacing JAVA_HOME with a valid java installation path if you don't have JAVA_HOME set).

After this you can run the tool as you would a normal command: 

```
 ─➤ findjar ~/.m2 --help
```


## Basic Usage

```bash
 ─➤ findjar <search-root> [options]
```

## Command line help

```bash
─➤ findjar --help
 
findjar - a tool for searching through files, including files inside jars 
 
usage: findjar <search-root> [-p <path-pattern>] [-g <content-pattern>] [...] 
 
findjar searches for files/content in any disk structure. It is capable of 
looking both for/in normal files and also for/in files inside zip/jar files. 
It is capable of regex matching both for file name/path and for content 
within the files. 
 
This tool is in essence an improvement of the unix find command geared 
towards solving a common problem for programmers on the JVM: finding that 
specific file or class in your maven repo, classpath, etc when that file can 
reside either directly on disk or inside a jar archive. 
 
Also this tool can be useful in detecting what version a specific class file 
or source file inside a library jar changed between a number of versions of 
the library file. 
 
Note that this tool is capable of a few extra tricks such as writing out the 
contents of matched files inside jar files and calculating md5 or sha1 hashes 
of matched files inside jar files. 
 
For regular files the path pattern (-p) matches against the entire path, 
including the file name, i.e.: 
 
 ~> findjar ~/.m2 -p '.*asm/asm/3.2.*pom' 
 
 repository/asm/asm/3.2/asm-3.2.pom 
 
whereas for files within jar files, the path pattern matches the string: 
 
 <path-to-jar-file>@<path-within-jar-file> 
 
i.e: 
 
 ~> findjar ~/.m2 -p '.*asm/asm.*Edge.class' 
 
 repository/asm/asm/3.2/asm-3.2.jar@org/objectweb/asm/Edge.class 
 
Command line switches can be provided either using short form i.e. '-t j' or 
long form i.e. '--type j'. 
 
For usage examples: 
 
 ~> findjar --examples 
 
Author: Matias Bjarland / mbjarland@gmail.com 
 
 
findjar 1.0.98 - fa4efc9 - 2023.09.21 16:11:18 + 
 
Options: 
  -p, --path <regex>     a pattern to match against the relative path 
                         (including file name) starting from search-root
  -a, --apath <regex>    a pattern to match against the absolute path 
                         (including file name)
  -n, --name <regex>     a pattern to match against file names
  -g, --grep <regex>     a pattern to match against file content lines
  -t, --types <n|j|z>    restrict the files searched to only the type(s) 
                         specified. The list of supported file types is 
                         extensible. Available file types: n - normal files, 
                         j - files in jar files, z - files in zip files. 
                         Default: nj
  -x, --context <#>      If -g is given, show <# of lines> lines of context 
                         around the match, defaults to 0
  -o, --out-file <path>  when using -c (cat file), write the contents of the 
                         located file(s) to the output file
  -f, --flags <flags>    turns on regex flags for all matches used. Example: 
                         '-f i' turns on case insensitive matching for both 
                         file names and content. See oracle javadocs on 
                         java.util.regex.Pattern (special constructs > match 
                         flags) for details on java regex flags
  -c, --cat              cat file. For matching files, print the entire file 
                         contents on the console
  -m, --monochrome       turn off ansi-coloring of matching content lines
  -s, --hash <algo>      calculate file hash(es) for matched files. Available 
                         algorithms: md5, sha1, sha512, crc32, sha256
      --profile          internal developer option - enable profiling
      --examples         print out usage examples
  -h, --help             show usage information

```

### Examples

To retrieve examples of how to use `findjar`:

```bash
findjar --examples
```

or to see a browser compatible list, please refer to the [examples page](https://github.com/mbjarland/findjar/blob/master/examples.md).

## License

The project is released under the [Eclipse Public License - v2.0](https://www.eclipse.org/legal/epl-2.0/)

## Author

Matias Bjarland / [mbjarland@gmail.com](mailto:mbjarland@gmail.com)

