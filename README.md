# perspectives-apitypes
Types to use in Purescript programs that communicate with Perspectives-core over TCP. The module also defines a serialization format of contexts, roles and properties that is used to create Contexts and Roles over the API.

This module is used in:
* [perspectives-core](https://github.com/joopringelberg/perspectives-core)
* [perspectives-arc-languageserver](https://github.com/joopringelberg/perspectives-arc-languageserver)

## Installation
Install with npm **(Not yet available)**:

```
$ npm install perspectives-apitypes
```

## Use the types in Purescript
The module `Perspectives.ApiTypes` defines a Reqest and a Response type. These are simple records that are serialized prior to exchanging them over the TCP channel. The type `RequestType` enumerates the allowed type of requests, such as `GetRol`, `GetRolBinding`, etc.
