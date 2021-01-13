-- | Configuration file for `socks-server` that uses `localhost:1080` to listen
-- for new connection.
--
-- While the whole expression int this file can be significantly reduced what
-- we have here can be easily turned into a library that provides defaults for
-- missing values.
--
-- For example, we can just change the last line to listen on all interfaces
-- like this:
--
-- ```
-- in Config::{ listen = Listen::{ host = "*" } }
-- ```

let Listen =
      { Type = { host : Text, port : Natural }
      , default = { host = "localhost", port = 1080 }
      }

let Verbosity = < Silent | Normal | Verbose | Annoying >

let Config =
      { Type = { listen : Listen.Type, verbosity : Verbosity }
      , default = { listen = Listen::{=}, verbosity = Verbosity.Normal }
      }

in  Config::{=}
