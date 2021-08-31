% SOCKS-SERVER(1) SOCKS5 Proxy Server | SOCKS5 Proxy Server
% Peter Trsko
% 31 August 2021

# NAME

`socks-server` - Simple SOCKS5 proxy server.


# USAGE

\[CONFIG=*EXPR*] socks-server \[\--config-typecheck] \[\--config=*EXPR*]

socks-server \--config-print-type


# DESCRIPTION

Simple [SOCKS5](https://en.wikipedia.org/wiki/SOCKS) server configured using
[Dhall configuration language](https://dhall-lang.org/).

Either `CONFIG=`*EXPR* environment variable (see *ENVIRONMENT VARIABLES*
section) needs to be defined or `--config=`*EXPR* option (see *OPTIONS*
section) passed. The *EXPR* value is a [Dhall](https://dhall-lang.org/)
expression, see *CONFIGURATION* section.


# OPTIONS

\--config=*EXPR*
:   Set configuration to *EXPR*, where *EXPR* is a [Dhall](https://dhall-lang.org/)
    expression; if application fails to parse or typecheck the EXPR then it
    terminates with exit code `1`.

    Alternatively configuration can be passed via `CONFIG` environment variable
    (see *ENVIRONMENT VARIABLES* section). If both `CONFIG=`*EXPR* and
    `--config=`*EXPR* then the option has pariority and the value passed that
    way is used.

\--config-typecheck
:   Typecheck the configuration and exit; exit code `0` is used on success and
    exit code `1` on failure to typecheck.

\--config-print-type
:   Print [Dhall](https://dhall-lang.org/) type of configuration accepted by
    the application.


# ENVIRONMENT VARIABLES

`CONFIG=`*EXPR*
:   Dhall *EXPR*ession to be used as a configuration. For more information
    about the *EXPR*ession see *CONFIGURATION* section.

    This environment variable has lower priority than than `--config=`*EXPR*
    command line option (see *OPTIONS* section). See also *CONFIGURATION* for
    full configuration resolution algorithm.


# CONFIGURATION

Application takes configuration Dhall expression from only one source by
following following resolution algorithm:

1. If `--config=`*EXPR* is specified then Dhall *EXPR*ession is used as
   configuration. See *OPTIONS* section for more details.

   Try the next step if the command line option is not specified.

2. If `CONFIG=`*EXPR* is defined then Dhall *EXPR*ession is used as
   configuration. See *ENVIRONMENT VARIABLES* section for more details.

   When we get here and `CONFIG` environment variable is not defined then we
   fail with appropriate error message and exit code `1`.

Simple configuration can look like:

```dhall
-- | Listen on `localhost:1080` where 1080 is default port for SOCKS proxies.
{ listen = { host = "localhost", port = 1080 }
, verbosity = < Annoying | Normal | Silent | Verbose >.Normal
}
```

To listen on all ports (useful inside a Docker container):

```dhall
-- | Listen on port `1080` (default SOCKS proxy port) on all interfaces.
{ listen = { host = "*", port = 1080 }
, verbosity = < Annoying | Normal | Silent | Verbose >.Normal
}
```


# SEE ALSO

* [Dhall configuration language](https://dhall-lang.org/)
* [Configure services with Dhall
  ](https://trskop.github.io/articles/2020-10-11-configure-services-with-dhall.html)


# BUGS

<https://github.com/trskop/socks-server/issues>
