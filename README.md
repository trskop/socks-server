# SOCKS Server

Simple [SOCKS5](https://en.wikipedia.org/wiki/SOCKS) server configured using
Dhall.


## Usage

```
[CONFIG=EXPR] socks-server [--typecheck] [--config=EXPR]
              socks-server --print-config-type
```

Either `CONFIG=EXPR` environment variable needs to be defined or
`--config=EXPR` option passed in the first usage case. The `EXPR` value is a
Dhall expression, see "Configuration" section.


## Configuration

Simple default configuration can look like:

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

More complicated example that can be turned into a library can be found in
[`config.dhall`](./config.dhall) file.


## Features and Limitations

* Supports only connect command. What it means that it's okay to use as a proxy
  server for HTTP, but not anything that needs to bind port for connections
  that go the other way or UDP.

* Authentication is not supported.

* All types of addresses are supported: IPv4, IPv6, and hostname. When hostname
  is used then DNS resolution is done by the SOCKS server instead of the
  client. Some command applications need to be told to use this feature, for
  example `curl` can use either `socks5h://` URI schema or `--socks5-hostname`
  option for this purpose.

* Logging is not yet implemented even though `Verbosity` is already in the
  config.

* Error handling needs polishing.

* Support listening on UNIX domain socket. This should be fairly simple, but
  will change the configuration type.
