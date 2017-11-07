#!/bin/sh
mkdir -p 2.12/lib 2.11/lib
for V in 2.12 2.11; do
  wget -O $V/lib/export-hook.jar "http://repo1.maven.org/maven2/org/typelevel/export-hook_$V/1.2.0/export-hook_$V-1.2.0.jar"
  wget -O $V/lib/alleycats-core.jar "http://repo1.maven.org/maven2/org/typelevel/alleycats-core_$V/1.0.0-RC1/alleycats-core_$V-1.0.0-RC1.jar"
  wget -O $V/lib/machinist.jar "http://repo1.maven.org/maven2/org/typelevel/machinist_$V/0.6.2/machinist_$V-0.6.2.jar"
  wget -O $V/lib/macro-compat.jar "http://repo1.maven.org/maven2/org/typelevel/macro-compat_$V/1.1.1/macro-compat_$V-1.1.1.jar"
  wget -O $V/lib/shapeless.jar "http://repo1.maven.org/maven2/com/chuusai/shapeless_$V/2.3.2/shapeless_$V-2.3.2.jar"
  wget -O $V/lib/cats-macros.jar "http://repo1.maven.org/maven2/org/typelevel/cats-macros_$V/1.0.0-RC1/cats-macros_$V-1.0.0-RC1.jar"
  wget -O $V/lib/cats-kernel.jar "http://repo1.maven.org/maven2/org/typelevel/cats-kernel_$V/1.0.0-RC1/cats-kernel_$V-1.0.0-RC1.jar"
  wget -O $V/lib/cats-core.jar "http://repo1.maven.org/maven2/org/typelevel/cats-core_$V/1.0.0-RC1/cats-core_$V-1.0.0-RC1.jar"
  wget -O $V/lib/kittens.jar "http://repo1.maven.org/maven2/org/typelevel/kittens_$V/1.0.0-RC1/kittens_$V-1.0.0-RC1.jar"
done
