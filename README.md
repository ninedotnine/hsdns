# devious-dns

`devious-dns` is a DNS server which responds to requests with... *inaccurate* IP addresses.

This is likely not useful for anything (except testing?) but it was fun to write.

## structure

The main module of the server is `src/hsdns-server.hs`.

```
src/
├── HSDNS/
│   ├── Client/
│   │   └── Network.hs
│   ├── Common/
│   │   ├── Decoding.hs
│   │   ├── Encoding.hs
│   │   ├── Packing.hs
│   │   └── Typedefs.hs
│   ├── Server/
│   │   ├── Answering.hs
│   │   └── Network.hs
│   └── Settings.hs
├── hsdns-client.hs
└── hsdns-server.hs
```

## dependencies

`attoparsec` and `network`

## building

`make`.

If the build fails with an error such as "Could not find module ‘Prelude’. There are files missing in the 'base' package" then try:

`make HSFLAGS=-static`
