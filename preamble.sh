: # This is a special script which intermixes both sh
: # and cmd code. It is written this way because it is
: # used in system() shell-outs directly in otherwise
: # portable code. See https://stackoverflow.com/questions/17510688
: # for details.

:; hash drip  >/dev/null 2>&1 # make sure the command call on the next line has an up to date worldview
:;if command -v drip >/dev/null 2>&1 ; then CMD=drip; else CMD=java; fi
:;CMD="${JAVA_HOME}/bin/java"
:;exec $CMD -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:-OmitStackTraceInFastThrow -Xss16M -client -Xbootclasspath/a:"$0" findjar.main "$@"; exit $?
@echo off
java -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:-OmitStackTraceInFastThrow -client -Xbootclasspath/a:%1 findjar.main "%~f0" %*
goto :eof
