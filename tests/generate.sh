#!/bin/sh

../pandoc -r native -s -w native testsuite.native > writer.native
../pandoc -r native -s -w markdown testsuite.native > writer.markdown
../pandoc -r native -s -w rst testsuite.native > writer.rst      
../pandoc -r native -s -w html testsuite.native > writer.html
../pandoc -r native -s -w html -S testsuite.native > writer.smart.html
../pandoc -r native -s -w latex testsuite.native > writer.latex
../pandoc -r native -s -w rtf testsuite.native > writer.rtf

