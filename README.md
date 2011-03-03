# eBitLy
Access the [**Bit.ly API**](http://code.google.com/p/bitly-api/wiki/ApiDocumentation) using [**ibrowse**](https://github.com/cmullaparthi/ibrowse)

## Overview
**ebitly** is a wrapper over the Bit.ly API

## Usage
You can start the app using `ebitly:start/0` or include `ebitly_sup` in your supervisor tree. To use it, just use the functions on `ebitly`.

## Limitations
We've only implemented the v3/shorten method, because that's the only thing we needed by now.