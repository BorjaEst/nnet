# nnet
Erlang Neural Networks (nnet) is an application to represent neural networks in erlang terms.


## Installation
Create your own project with rebar3.
 ```sh
 $ rebar3 new app yourapp
 ```

Then in your project path find rebar.config file and add nnet as dependency under the deps key:
```erlang
{deps, 
    [
        {nnet, {git, "https://github.com/BorjaEst/nnet.git", {tag, "<version>"}}}
    ]}.
```

Then using compile command, rebar3 will fetch the defined dependencies and compile them as well for your application.
```sh
$ rebar3 compile
```

At the end for making a release you first need to create your release structure and then making a release with following commands.
```sh
$ rebar3 new release yourrel
$ rebar3 release
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 


## Usage
Load the app using your prefered method. For example in the project folder executing  rebar3 shell:
```sh
$ rebar3 shell
===> Booted nnet
```


All user functions are defined inside the module [src/nnet](https://github.com/BorjaEst/nnet/blob/master/src/nnet.erl), however here is an example:


## TBD


## More examples


## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.


### Improvement ideas and requests
TBD


## License
This software is under [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

