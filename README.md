# Running Haskell AWS Lambda Functions

## Content

* http://aws.amazon.com/fr/blogs/compute/running-executables-in-aws-lambda/
* `ghc-centos`: Docker container for building Haskell binaries compatible with [Amazon's Linux AMI](http://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html). Does not seem to be a good idea in general as there are quite a few differences between CentOS and Linux AMI.
* `run.js`: Javascript wrapper to run binary in a child process
