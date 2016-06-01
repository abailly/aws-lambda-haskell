# Running Haskell AWS Lambda Functions

This repository contains code and directions to run Haskell executables over [AWS Lambda]() "serverless" infrastructure. This experiment was triggered by reading description of [apex](http://apex.run) provide a wrapper to run Go code.

## Howto - The Manual Way

### Prepare environment

* Ensure access to AWS Lambda service:
    * use `aws configure` to define credentials and region to deploy to
    * create or use role for executing code on AWS Lambda, e.g. something like `arn:aws:iam::259394719635:role/lambda`

## Simple Build ##

* Build docker container for building Haskell code that is supposed to be runnable on Amazon's Linux AMI
    
    ```
    cd ghc-centos
    docker build -t haskell-centos .
    cd ..
    ```

* Build haskell code:

    ```
    docker run -ti -v $(pwd):/build -w /build --name=haskell-build haskell-centos stack build --allow-different-user
    ...
    CONTAINER_ID=$(docker ps -a | grep haskell-centos | head -1 | cut -d ' ' -f 1)
    docker run --volumes-from=$CONTAINER_ID busybox dd if=/build/.stack-work/install/x86_64-linux/ghc-7.10.3/7.10.3/bin/main > main
    ```

* Build zip file containing Javascript wrapper and main app
  
  ```
  zip lambda.zip run.js main
  ```

## Complex Build

A lot of interesting pieces of code rely on external C libraries. For example, [hmatrix](https://github.com/albertoruiz/hmatrix) relies on LAPACK and BLAS libraries for efficient matrix operations and compiled executable will need to be dynamically (or statically) linked to those libraries for proper execution. The above directions should be updated to take into account those extraneous libraries:

* Add the needed libraries into the build container description, e.g.

   ```
   RUN yum install -y lapack-devel blas-devel
   ```

* Build code as before, using the custom image,
* Export from container the needed libraries (the will be packed as part of the code shipped to AWS Lambda):

   ```
   $ docker run --volumes-from=$CONTAINER_ID haskell-centos ldd /build/.stack-work/install/x86_64-linux/ghc-7.10.3/7.10.3/bin/main > libs
   ... extract list of library files to copy...
   $ for i in $(cat libraryFiles); do
      docker run --volumes-from=$CONTAINER_ID haskell-centos dd if=$i > $(basename $i)
     done
   ```
* Modify the `run.js` wrapper to set correctly `LD_LIBRARY_PATH` environment:
     
   ```
   process.env['LD_LIBRARY_PATH'] = process.env['LAMBDA_TASK_ROOT']
   ctx = context
   ```
* Pack everything into `lambda.zip`: Javascript wrapper, libraries, executable...

## Deploy to AWS Lambda

* Create function on Lambda:

    ```
    $ aws lambda create-function --function-name hello-test --runtime nodejs4.3 --zip-file fileb://./aws-lambda.zip  --handler run.handle --role arn:aws:iam::259394719635:role/lambda
    {
        "CodeSha256": "QYKOebaDN/fqEzb1nmaV3ByNDZK3JvD0kWX6YQnPpjE=", 
        "FunctionName": "hello-test", 
        "CodeSize": 265356, 
        "MemorySize": 128, 
        "FunctionArn": "arn:aws:lambda:eu-west-1:259394719635:function:hello-test", 
        "Version": "$LATEST", 
        "Role": "arn:aws:iam::259394719635:role/lambda", 
        "Timeout": 20, 
        "LastModified": "2016-05-23T10:32:38.126+0000", 
        "Handler": "run.handle", 
        "Runtime": "nodejs4.3", 
        "Description": ""
    }
    ```

* Run function on Lambda:

    ```
    $ aws lamdba invoke-function --function-name hello-test
    {
        "StatusCode": 200
    }
    $ cat test
    "child process exited with code 0"
    ```

The provided `main.hs` simply output its input to its output. There should be an execution trace in the logs hosted on CloudWatch:

![](cloudwatch.png)
    
## Manifest

* `ghc-centos`: Docker container for building Haskell binaries compatible with [Amazon's Linux AMI](http://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html). Does not seem to be a good idea in general as there are quite a few differences between CentOS and Linux AMI, but in practice it kind works...
* `run-tmpl.js`: Template Javascript wrapper to run binary in a child process. The `$$main$$` string should be replaced by the name of the packed executable,
* `test.js`: Javascript test wrapper, invoke the handler simulating what AWS Lambda does
* `stack.yaml`, `main.cabal`, `main.hs`: Basic structure for building Haskell code

## References

* [Running executables in AWS Lambda](http://aws.amazon.com/fr/blogs/compute/running-executables-in-aws-lambda/)
* [Child processes in node](https://nodejs.org/api/child_process.html)
* [AWS Lambda documentation](http://docs.aws.amazon.com/lambda/latest/dg/nodejs-create-deployment-pkg.html)
