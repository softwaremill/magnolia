#!/bin/zsh

source ../build.zsh

PROJECT=magnolia

TEST_DEPENDENCIES=$PROJECT-core.jar:$PROJECT-examples.jar:../estrapade/estrapade-macros.jar:../estrapade/estrapade-core.jar:../contextual/contextual-core.jar:../contextual/contextual-data.jar
build core && \
build examples -cp $PROJECT-core.jar && \
build tests -cp $TEST_DEPENDENCIES && \
runtests tests -cp $TEST_DEPENDENCIES:$PROJECT-tests.jar
