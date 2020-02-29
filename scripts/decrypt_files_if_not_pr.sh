#!/usr/bin/env bash

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$TRAVIS_REPO_SLUG" == "propensive/magnolia" ]]; then
    openssl aes-256-cbc -K $encrypted_a92a7858a56e_key -iv $encrypted_a92a7858a56e_iv -in secrets.tar.enc -out secrets.tar -d
    tar xvf secrets.tar
fi
