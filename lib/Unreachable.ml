open! Base

exception Error of string

let default = Error "Unreachable code reached"
